(ns buildkits.log
  (:require [clojure.string :as str]))

;; taken from pulse

(defn- unparse1 [[k v]]
  (str (name k) "="
       (cond
        (or (true? v) (false? v))
        v
        (keyword? v)
        (name v)
        (float? v)
        (format "%.3f" v)
        (number? v)
        v
        (string? v)
        (cond
         (re-find #"^[a-zA-Z0-9\:\.\-\_]+$" v)
         v
         (neg? (.indexOf ^String v "\""))
         (str "\"" v "\"")
         :else
         (str "'" v "'"))
        :else
        "?")))

(defn- unparse [data]
  (->> data
       (partition 2)
       (map unparse1)
       (str/join " ")))

(def *info* true)

(defn info [& data]
  (when *info*
    (let [msg (unparse (list* :app "buildkits" data))]
      (locking *out*
        (println msg)))))
