(ns cspbox.tools.config
  (:require [clojure.edn :as edn]))

;; load and read config
(defn load-config
  "Given a filename, load & return a config file"
  [filename & [flag]]
  (when filename
    (if flag
      (edn/read-string (slurp filename))
      (let [source (clojure.java.io/file filename)]
        (when (.exists source)
          (edn/read-string (slurp filename)))))))

(defn select-read-conf
  "open local directory first then from jar"
  [config-name]
  (let [conf (load-config config-name)]
    (if conf
      conf
      (load-config (clojure.java.io/resource config-name) :direct))))
