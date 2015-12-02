(ns cspbox.trading.order.record
  (:require [cspbox.runtime.tools.csv :refer [write-vector-csv]]
            [cspbox.runtime.tools.date :refer [timestamp-date]]
            [taoensso.timbre :as log]))

(defn record-order
  "write order to file or db"
  [order & [{:keys [file]}]]
  (log/trace "order:" order)
  (when file
    (let [date  (timestamp-date (:timestamp order))]
      (write-vector-csv [(merge order {:date date})] {:file file :append true}))))
