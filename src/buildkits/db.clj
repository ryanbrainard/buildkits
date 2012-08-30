(ns buildkits.db
  (:refer-clojure :exclude [flatten])
  (:require [clojure.java.jdbc :as sql]
            [clojure.java.io :as io]
            [clojure.walk :as walk]
            [environ.core :as env])
  (:import (org.openstreetmap.osmosis.hstore PGHStore)))

(def db (env/env :database-url "postgres://localhost:5432/buildkits"))

(defn hstore [m]
  (PGHStore. (walk/stringify-keys m)))

(defn unhstore [h]
  (into {} (for [[k v] h]
             [k (if (instance? PGHStore v)
                  (walk/keywordize-keys (into {} v)) v)])))

(defn flatten [buildpack]
  (merge (dissoc buildpack :attributes :organization_id)
         (:attributes buildpack)))

(defn get-buildpack
  ([org-and-buildpack-name]
     (apply get-buildpack (.split org-and-buildpack-name "/")))
  ([org buildpack-name]
     (sql/with-query-results [b] [(str "SELECT buildpacks.*, organizations.name as org"
                                       "  FROM buildpacks, organizations"
                                       " WHERE organizations.name = ?"
                                       " AND buildpacks.name = ?")
                                  org buildpack-name]
       (if b
         (assoc (flatten (unhstore b))
           :tar_link (format "http://%s.s3.amazonaws.com/buildpacks/%s/%s.tgz"
                             (env/env :aws-bucket) org buildpack-name))))))

(defn get-buildpacks []
  (sql/with-query-results buildpacks
    [(str "SELECT buildpacks.*, organizations.name as org"
          " FROM buildpacks, organizations"
          " WHERE buildpacks.organization_id = organizations.id"
          " ORDER BY organization_id, name")]
    (mapv (comp flatten unhstore) buildpacks)))

(defn get-kit [name]
  (if name
    (sql/with-query-results buildpacks
      [(str "SELECT buildpacks.*, revisions.tarball, organizations.name as org"
            " FROM buildpacks, revisions, kits, organizations"
            " WHERE revisions.buildpack_id = buildpacks.id"
            " AND kits.kit = ?"
            " AND buildpacks.id = kits.buildpack_id"
            " AND buildpacks.organization_id = organizations.id"
            " AND revisions.created_at IN "
            " (SELECT MAX(revisions.created_at) FROM revisions"
            "   GROUP BY buildpack_id);") name]
      (if (seq buildpacks)
        (mapv (comp flatten unhstore) buildpacks)))))

(defn add-to-kit [username org buildpack-name position]
  (if-let [{:keys [id]} (get-buildpack org buildpack-name)]
    (sql/insert-record :kits {:kit username :buildpack_id id
                              :position position})
    (throw (ex-info "Bulidpack not found" {:buildpack buildpack-name}))))

(def defaults ["clojure" "gradle" "grails" "java" "logo" "nodejs" "php"
               "play" "python" "ruby" "scala"])

(defn create-kit [name]
  (doseq [buildpack defaults]
    (try (add-to-kit name "heroku" buildpack 0)
         (catch clojure.lang.ExceptionInfo _
           (println "WARNING: creating kit and missing" buildpack))))
  (get-kit name))

(defn remove-from-kit [kit org buildpack-name]
  (let [{:keys [id]} (get-buildpack org buildpack-name)]
    (sql/delete-rows :kits ["kit = ? AND buildpack_id = ?" kit id])))

(defn update [username buildpack-id content]
  (sql/transaction
   (sql/with-query-results [{:keys [max]}]
     ["SELECT max(id) FROM revisions WHERE buildpack_id = ?" buildpack-id]
     (let [rev-id (inc (or max 0))]
       (sql/insert-record :revisions {:buildpack_id buildpack-id :id rev-id
                                      :published_by username :tarball content})
       rev-id))))

(defn create [username org buildpack-name content]
  (sql/transaction
   (sql/with-query-results [{:keys [id]}]
     ["SELECT id FROM organizations WHERE name = ?" org]
     (let [{:keys [id]} (sql/insert-record :buildpacks
                                           {:name buildpack-name
                                            :attributes (hstore {:owner username})
                                            :organization_id id})]
       (update username id content)))))
