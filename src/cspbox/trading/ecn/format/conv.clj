(ns cspbox.trading.ecn.format.conv
  (:require [semantic-csv.core :refer [->float]]))

;; convert data format to meet requir ment

(defn select-bar-data
  "select one field from bar data (open close high low volum)"
  [format-conf & [field-id]]
  (assert format-conf "conf should be given")
  (let [field-id (or field-id :close)
        field-num (get format-conf field-id)
        date-place (get format-conf :date)]
    (fn [data]
      (when data
        {:date (nth data date-place) :price (->float (nth data field-num))}))))
