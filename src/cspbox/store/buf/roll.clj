(ns cspbox.store.buf.roll)

(defn roll-buffer [buffer val buffer-size]
  "copy from reactive clojure book"
  (let [buffer (conj buffer val)]
    (if (> (count buffer) buffer-size)
      (pop buffer)
      buffer)))


(defn make-sliding-buffer [buffer-size]
  "generate functon wrap a sliding buffer"
  (let [buffer (atom clojure.lang.PersistentQueue/EMPTY)]
    (fn [n]
      (swap! buffer roll-buffer n buffer-size)
      @buffer)))


(defn make-lookback-buffer [buffer-size & [flag]]
  "generate functon wrap a sliding buffer, flag control return buffer or value"
  (let [buffer (atom clojure.lang.PersistentQueue/EMPTY)]
    (fn
      ([]  (if (= buffer-size 1) (last @buffer) @buffer))
      ([n] (swap! buffer roll-buffer n buffer-size)
           (if flag @buffer n)))))

