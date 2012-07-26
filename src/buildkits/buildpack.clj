(ns buildkits.buildpack
  (:require [buildkits.db :as db]
            [clojure.java.io :as io]
            [clojure.java.jdbc :as sql]
            [cheshire.core :as json]
            [clojure.data.codec.base64 :as base64]
            [environ.core :as env]
            [compojure.core :refer [defroutes GET PUT POST DELETE ANY]])
  (:import (org.jets3t.service.security AWSCredentials)
           (org.jets3t.service.acl AccessControlList)
           (org.jets3t.service.impl.rest.httpclient RestS3Service)
           (org.jets3t.service.acl.gs AllUsersGrantee)
           (org.jets3t.service.acl Permission)
           (org.jets3t.service.model S3Object)))

;; for actions coming from the heroku-buildpacks plugin
(defn check-api-key [username key]
  (try (= username (.getEmail (.getUserInfo (com.heroku.api.HerokuAPI. key))))
       (catch com.heroku.api.exception.RequestFailedException _)))

(defn org-member? [username org]
  (sql/with-query-results [member] [(str "SELECT * FROM memberships, organizations "
                                         "WHERE email = ? AND organizations.name = ?"
                                         " AND organization_id = organizations.id")
                                    username org]
    (boolean member)))

(defn org-exists? [org]
  (sql/with-query-results [o] ["SELECT * FROM organizations WHERE name = ?" org]
    (boolean o)))

(defn create-org [org username]
  (when-not (re-find #"^[-\w]+$" org)
    (throw (ex-info "Org names must contain only alphanumerics and dashes."
                    {:org org})))
  (let [{:keys [id]} (sql/insert-record "organizations" {:name org})]
    (sql/insert-record "memberships" {:email username :organization_id id})))

(defn s3-put [org buildpack-name content]
  (when-let [access_key (env/env :aws-access-key)]
    (let [s3 (RestS3Service. (AWSCredentials. access_key
                                              (env/env :aws-secret-key)))
          bucket (env/env :aws-bucket)
          key (format "buildpacks/%s/%s.tgz" org buildpack-name)
          obj (doto (S3Object. key content)
                (.setAcl (AccessControlList/REST_CANNED_PUBLIC_READ)))]
      (.putObject s3 bucket obj))))

;; why is this not in clojure.java.io?
(defn get-bytes [input]
  (let [baos (java.io.ByteArrayOutputStream.)]
    (io/copy input baos)
    (when (instance? java.io.File input)
      (.delete input))
    (.toByteArray baos)))

(defn create [buildpack-name username org content]
  (let [bytes (get-bytes content)
        rev-id (db/create username org buildpack-name bytes)]
    (s3-put org buildpack-name bytes)
    {:status 201 :body (json/encode {:revision rev-id})}))

(defn update [username org buildpack content]
  (let [bytes (get-bytes content)
        rev-id (db/update username (:id buildpack) bytes)]
    (s3-put org (:name buildpack) bytes)
    {:status 200 :body (json/encode {:revision rev-id})}))

(defn- rollback-query [org buildpack-id target]
  (if (= target "previous")
    [(str "SELECT * FROM revisions WHERE buildpack_id = ?"
          " ORDER BY id DESC OFFSET 1 LIMIT 1") buildpack-id]
    ["SELECT * FROM revisions WHERE buildpack_id = ? AND id = ?"
     buildpack-id (Integer. target)]))

(defn rollback [username org buildpack target]
  (sql/with-query-results [rev] (rollback-query org (:id buildpack) target)
    (if [rev]
      (update username org buildpack (:tarball rev))
      {:status 404})))

(defn revisions [_ _ buildpack]
  (sql/with-query-results revs [(str "SELECT id, published_by, created_at"
                                     " FROM revisions"
                                     " WHERE buildpack_id = ? ORDER BY id")
                                (:id buildpack)]
    {:status 200 :body (json/encode revs)}))

(defn share [_ org email]
  (sql/with-query-results [{:keys [id]}]
    ["SELECT * FROM organizations WHERE name = ?" org]
    ;; TODO: ensure user exists
    (when-not (re-find #"@" email)
      (throw (ex-info "Invalid email" {:email email})))
    (sql/with-query-results [member] [(str "SELECT * FROM memberships WHERE"
                                           " email = ? AND organization_id = ?")
                                      email id]
      (if member
        {:status 409}
        (do (sql/insert-record "memberships" {:email email :organization_id id})
            {:status 201})))))

(defn unshare [_ org email]
  (sql/with-query-results [membership]
    [(str "SELECT * FROM memberships, organizations WHERE email = ? AND"
          " organization_id = organizations.id AND organizations.name = ?")
     email org]
    (if membership
      (do (sql/delete-rows "memberships" ["id = ?" (:id membership)])
          {:status 200})
      {:status 404})))

(def ^:dynamic *not-found* (constantly {:status 404}))

(defn check-auth [headers org callback & args]
  (if-let [authorization (get headers "authorization")]
    (let [[username key] (-> authorization (.split " ") second
                             .getBytes base64/decode String. (.split ":"))]
      (sql/with-connection db/db
        (if (check-api-key username key)
          (if (org-member? username org)
            (apply callback username org args)
            (sql/transaction
             (if (org-exists? org)
               {:status 403}
               (do (create-org org username)
                   (apply callback username org args)))))
          {:status 401})))
    {:status 401}))

(defn check-pack-auth [headers org buildpack-name callback & args]
  (let [pack-callback (fn [username org & args]
                        (if-let [pack (db/get-buildpack org buildpack-name)]
                          (apply callback username org pack args)
                          (apply *not-found* username org args)))]
    (apply check-auth headers org pack-callback args)))

(defroutes app
  (GET "/buildpacks" []
       {:status 200 :headers {"content-type" "application/json"}
        :body (sql/with-connection db/db
                (json/encode (db/get-buildpacks)))})
  (GET "/buildpacks/:org/:name/revisions" {{:keys [org name]} :params
                                           headers :headers}
       (check-pack-auth headers org name revisions))
  (POST "/buildpacks/:org/:name" {{:keys [org name buildpack]} :params
                                  headers :headers}
        (binding [*not-found* (partial create name)]
          (check-pack-auth headers org name update (:tempfile buildpack))))
  (POST "/buildpacks/:org/:name/revisions/:target"
        {{:keys [org name target]} :params headers :headers}
        (check-pack-auth headers org name rollback target))
  (POST "/buildpacks/:org/share/:email" {{:keys [org email]} :params
                                         headers :headers}
        (check-auth headers org share email))
  (DELETE "/buildpacks/:org/share/:email" {{:keys [org email]} :params
                                           headers :headers}
          (check-auth headers org unshare email)))

;; This is intended to be run by hand to make the s3 contents match the DB
(defn update-s3-tarballs []
  (sql/with-connection db/db
    (sql/with-query-results revisions
      [(str "SELECT buildpacks.*, revisions.tarball"
            " FROM buildpacks, revisions"
            " WHERE revisions.buildpack_id = buildpacks.id"
            " AND revisions.id IN "
            " (SELECT MAX(revisions.id) FROM revisions"
            "   GROUP BY buildpack_id);")]
      (doseq [{:keys [tarball organization_id name id attributes]} revisions]
        (sql/with-query-results [org] ["SELECT name FROM organizations WHERE id = ?"
                                       organization_id]
          (update (get attributes "owner") (:name org)
                  {:name name :id id} tarball))))))