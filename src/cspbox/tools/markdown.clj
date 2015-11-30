(ns cspbox.tools.markdown)

(defn md-header
  "write header"
  [content]
  (str "# " content " \n "))

(defn md-list
  "write a list"
  [list-cont]
  (clojure.string/join " \n " (mapv #(str "+ " %) list-cont)))

(defn md-table
  "write a table with head and body"
  [table-head table-body]
  (let [row-fn     (fn [row] (apply str "|" (mapv (fn [item] (str "  " item "  |")) row)))
        row-num    (count table-head)
        header     (row-fn table-head)
        spliter    (apply str "|" (repeat row-num (apply str (concat (repeat 10 '-) "|"))))
        body     (clojure.string/join " \n " (mapv row-fn table-body))]
    (str header " \n " spliter " \n " body " \n ")))
