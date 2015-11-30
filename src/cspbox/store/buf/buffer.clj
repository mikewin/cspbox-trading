(ns cspbox.store.buf.buffer
  (:require [taoensso.carmine :as car :refer (wcar)]
            [cspbox.store.db.handle :refer [wcar*]]))

;; a buffer could be local or remote (DB)

(defn local-buf
  ""
  []
  (atom nil))

(defn remote-buf
  ""
  [remote-id]
  (wcar* (car/lpush remote-id nil))
   remote-id)

(defn get-remote-buf
  "get the buffer from db"
  [remote-id]
  (let [res (wcar* (car/lrange remote-id 0 -1))]
    (drop-last res)))

(defn put-remote-buf
  "push a value in the first place in list"
  [value remote-id]
  (let [res (wcar* (car/lpushx remote-id value)
                   (car/lrange remote-id 0 -1))]
    (drop-last (second res))))

(defn pop-remote-buf
  "remove first value in list"
  [remote-id]
  (let [res (wcar* (car/lpop remote-id)
                   (car/lrange remote-id 0 -1))]
    (drop-last (second res))))

(defn remove-remote-buf-item
  "remove item from list"
  [remote-id item]
  (let [res (wcar* (car/lrem remote-id 0 item)
                   (car/lrange remote-id 0 -1))]
    (drop-last (second res))))

(defn make-buf
  "make a buf (local or remote)"
  [& [buf-type remote-id]]
  (let [remote-id (or remote-id (keyword (gensym "remote-buf")))
        buffer    (if (= buf-type :local) (local-buf) (remote-buf remote-id))]
    (with-meta
      (fn
        ([] (if (= buf-type :local)  ;; todo change to mult-method or macro
               @buffer
               (get-remote-buf remote-id)))
        ([value] (if (= buf-type :local)
                   (swap! buffer conj value)
                   (put-remote-buf value remote-id))))
      {:buf buffer})))

(defn pop-buf
  "remove the value on the top"
  [buf]
  (let [buffer  (:buf (meta buf))]
    (cond (keyword buffer) (pop-remote-buf buffer)
          :else (swap! buffer pop))))

(defn remove-item
  "remove the value on the top"
  [buf item]
  (let [buffer  (:buf (meta buf))]
    (cond (keyword buffer) (remove-remote-buf-item buffer item)
          :else (swap! buffer (partial remove #(= % item))))))
;;test



(comment
(def b (make-buf :local))
(def b (make-buf ))
(b 3)
(b 4)
(pop-buf b)
(b 6)
(b)

  (defn tt
  []
  (let [ta  (make-buf :local)]
    [(fn g []
       (ta))
     (fn p [data]
       (ta data))]))
(def t (tt))
(def t1 (first t))
(def t2 (second t))

  )
