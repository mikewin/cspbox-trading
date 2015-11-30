(ns cspbox.conv.utils.macro)

(defmacro make-fn [m]
 `(fn [& args#]
    (eval
      (cons '~m args#))))

(defmacro to-map
  [& args]
  (let [k (map keyword args)]
    (zipmap k args)))
