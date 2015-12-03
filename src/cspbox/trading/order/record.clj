(ns cspbox.trading.order.record
  (:require [cspbox.runtime.tools.date :refer [timestamp-date]]
            [cspbox.runtime.tools.csv :refer [write-vector-csv conv-csv-md read-parse-cvs]]
            [cspbox.runtime.tools.write :refer [write-file]]
            [semantic-csv.core :as sc :refer [->float ->int]]
            [taoensso.timbre :as log]))

(defn record-order
  "write order to file or db"
  [order & [{:keys [file]}]]
  (let [date  (timestamp-date (:timestamp order))]
    (log/debug "order:" (merge order {:date date}))
    (when file
      (write-vector-csv {:file file :append true} [(merge order {:date date})]))))


(defn stats-order
  "for backtest, read order from csv convert to md, add stats info"
  [{:keys [file]}]
  (when file
    (let [cast-map {:price ->float :quantity ->int}
          conv-map {:oqt :quantity :opc :price}
          orders  (read-parse-cvs file {:conv-map conv-map :cast-map cast-map})
          md-file (conv-csv-md file)
          order-count    (count orders)
          quantity       (mapv :quantity orders)
          price          (mapv :price  orders)
          total-q        (apply + quantity)
          new-quantity   (conj quantity (-  total-q))
          new-price      (conj price (last price))
          pl-list        (mapv #(* %1 (- %2)) new-price new-quantity)
          trade-pl       (apply + pl-list)
          percent        (-> trade-pl (/ (first pl-list)) (* 100.0) Math/abs)
          stats          (str "- order count: " order-count "\n - total pl: " trade-pl
                              "\n - percent : " percent "% \n")]
      (log/debug stats)
      (write-file md-file nil stats {:append true})
      md-file)))


;; test
(comment
(stats-order {:file "e:/work/autorun/cspbox/draft/log/order.csv"})
  )
