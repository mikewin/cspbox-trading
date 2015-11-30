(ns cspbox.trading.order.record
  (:require [cspbox.tools.csv :refer [write-vector-csv]]
            [cspbox.tools.date :refer [timestamp-date]]
            [clojure.tools.logging  :as log]))

(defn record-order
  "write order to file or db"
  [order & [{:keys [file]}]]
  (log/info "order:" order)
  (when file
    (let [date  (timestamp-date (:timestamp order))]
      (write-vector-csv [(merge order {:date date})] {:file file :append true}))))
