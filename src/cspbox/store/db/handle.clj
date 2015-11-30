(ns cspbox.store.db.handle
  (:require [taoensso.carmine :as car :refer (wcar)]))


;;will move to db file
(def server-conn {:pool {} :spec {}}) ; See `wcar` docstring for opts
(defmacro wcar* [& body] `(car/wcar server-conn ~@body))

(defn db-ready?[]
  (try
    (= (wcar* (car/ping)) "PONG")
    (catch Exception e (str "caught exception: " (.getMessage e)))))

