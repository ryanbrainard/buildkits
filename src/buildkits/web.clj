(ns buildkits.web
  (:require [ring.adapter.jetty :as jetty]
            [ring.middleware.session.cookie :as cookie]
            [ring.middleware.resource :as resource]
            [ring.middleware.stacktrace :as trace]
            [ring.util.response :as res]
            [noir.util.middleware :as noir]
            [environ.core :as env]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [compojure.core :refer [defroutes GET PUT POST DELETE ANY]]
            [compojure.route :as route]
            [compojure.handler :as handler]
            [clojure.java.jdbc :as sql]
            [clojure.java.io :as io]
            [clojure.data.codec.base64 :as base64]
            [buildkits.db :as db]
            [buildkits.html :as html]
            [buildkits.kit :as kit]
            [buildkits.log :as log]
            [buildkits.buildpack :as buildpack]))

;; for operations coming from the CLI client
(defn wrap-auth [handler]
  (fn [{:keys [headers params] :as req}]
    (let [authorization (or (get headers "authorization")
                            (throw (ex-info "Unauthorized" {:status 401})))
          [username key] (-> authorization (.split " ") second
                             .getBytes base64/decode String. (.split ":"))]
      (when-not (try (= username (-> (com.heroku.api.HerokuAPI. key)
                                     (.getUserInfo) (.getEmail)))
                     (catch com.heroku.api.exception.RequestFailedException _))
        (throw (ex-info "Forbidden" {:status 401})))
      (handler (update-in req [:params] assoc :username username)))))

(defn get-token [code]
  (-> (http/post "https://api.heroku.com/oauth/token"
                 {:form-params {:client_id (env/env :oauth-client-id)
                                :client_secret (env/env :oauth-client-secret)
                                :code code :grant_type "authorization_code"}})
      (:body) (json/decode true) :access_token))

(defn get-username [token]
  (-> (http/get "https://api.heroku.com/user"
                {:headers {"Authorization" (str "Bearer " token)}})
      (:body) (json/decode true) :email))

(defroutes app
  (GET "/" {{username :username} :session :as req}
       {:body (sql/with-connection db/db
                (html/dashboard (db/get-buildpacks) username
                                (if username
                                  (or (db/get-kit username)
                                      (db/create-kit username)))))})
  (GET "/buildkit/:name.tgz" [name]
       (log/info :download name)
       (sql/with-connection db/db
         (if-let [kit (db/get-kit name)]
           {:status 200
            :headers {"Content-Type" "application/octet-stream"}
            :body (kit/compose name kit)}
           {:status 404})))
  (GET "/oauth" [code]
       (if code
         (let [username (get-username (get-token code))]
           (log/info :login username)
           (assoc (res/redirect "/")
             :session {:username username}))
         {:status 403}))
  (GET "/logout" {{username :username} :session}
       (log/info :logout username)
       (assoc (res/redirect "/") :session nil))
  (PUT "/buildkit/:org/:buildpack/:pos" [org buildpack pos :as
                                         {{:keys [username]} :session}]
       (when-not username
         (throw (ex-info "Must log in" {:status 403})))
       (sql/with-connection db/db
         (log/info :add-to-kit (str org "/" buildpack) :username username)
         (db/add-to-kit username org buildpack (Integer. pos)))
       (res/redirect "/"))
  (DELETE "/buildkit/:org/:buildpack/:pos" [org buildpack :as
                                            {{:keys [username]} :session}]
          (when-not username
            (throw (ex-info "Must log in" {:status 403})))
          (sql/with-connection db/db
            (log/info :remove-from-kit (str org "/" buildpack) :username username)
            (db/remove-from-kit username org buildpack))
          (res/redirect "/"))
  #'buildpack/public-app
  (wrap-auth #'buildpack/edit-app)
  (wrap-auth #'kit/app)
  (route/not-found "Not found"))

(defn- accepts-html? [req]
  (re-find #"html" (get (:headers req) "accept" "")))

(defn wrap-exceptions [handler]
  (fn [req]
    (try
      (handler req)
      ;; TODO: extract this into a library; this is silly
      (catch clojure.lang.ExceptionInfo e
        (if (accepts-html? req)
          {:status (:status (ex-data e) 400)
           :headers {"Content-Type" "text/html"}
           :body (slurp (io/resource (if (= (:status (ex-data e)) 404)
                                       "404.html"
                                       "500.html")))}
          {:status (:status (ex-data e) 400)
           :headers {"Content-Type" "application/json"}
           :body (json/encode (assoc (ex-data e) :message (.getMessage e)))}))
      (catch Exception e
        (.printStackTrace e)
        (if (accepts-html? req)
          {:status 500
           :headers {"Content-Type" "text/html"}
           :body (slurp (io/resource "500.html"))}
          {:status 500
           :headers {"Content-Type" "application/json"}
           :body (json/encode {:message (.getMessage e)})})))))

(defn -main [& [port]]
  (let [port (Integer. (or port (env/env :port) 5000))
        store (cookie/cookie-store {:key (env/env :session-secret)})]
    (jetty/run-jetty (-> #'app
                         (resource/wrap-resource "static")
                         (wrap-exceptions)
                         ((if (env/env :dev)
                            trace/wrap-stacktrace
                            noir/wrap-force-ssl))
                         (handler/site {:session {:store store}}))
                     {:port port :join? false})))

;; (.stop s)
;; (def s (-main 5000))
