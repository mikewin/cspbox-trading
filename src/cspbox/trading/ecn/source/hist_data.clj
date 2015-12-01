(ns cspbox.trading.ecn.source.hist-data
  (:require [cspbox.trading.ecn.local.hist :refer [read-local-hist-day read-local-hist-min]]
            [cspbox.runtime.tools.csv :refer [change-keyword]]
            [cspbox.trading.ecn.format.conv :refer [select-bar-data]]))

;; generate data source for DSL using

(defn pop-buf
  "a fn wrap a buffer used like pop"
  [init-buf]
  (let [buf (atom init-buf)]
    (fn []
      (let [value (first @buf)]
        (swap! buf next)
        value))))


(defn pre-proc-data
  [data field conv-fn]
  (->> data
       reverse  ;; from old date
       (map #(select-keys % field)) ;; select value need
       conv-fn  ;; change the keyword name
       pop-buf))



(defn data-source-hist-day
  "read local csv file output date and price"
  [{:keys [symbol count field conv]}]
  (assert symbol "data source should give symbol")
  (let [number    (or count 20)
        data     (read-local-hist-day symbol {:number number :parse true})
        conv-fn  (change-keyword conv)
        data-buf (pre-proc-data data field conv-fn)]
    (fn []
      (data-buf))))


(defn data-srouce-hist-minute
  "read local csv file output minute data (date and price)"
  [{:keys [symbol count field conv]}]
  (assert symbol "data source should have symbol")
  (let [numbers (or count 100)
        data (read-local-hist-min symbol numbers)
        conv-fn  (change-keyword conv)
        data-buf (pre-proc-data data field conv-fn)]
    (fn []
      (data-buf))))


;; test

(comment
 (def ds (data-source-hist-day-price {:symbol "000301" :count 50}))
  (def ds (data-source-hist-day-bar {:symbol "000301" :count 50 :field [:date :open :close :high :low :volume]}))
 (ds)
 (def gen-day-price (data-source-hist-day-bar {:symbol "000301" :count 50 :field [:date :close] :conv {:close :price}}))
  (def gen-min-price (data-srouce-hist-minute {:symbol "00700.XHKG" :count 300
                                             :field [:date :close] :conv {:close :price}}))
(gen-min-price)

(def gen-min-bar (data-srouce-hist-minute {:symbol "00700.XHKG" :count 300
                                             :field [:date :open :close :high :low :volume] }))
(gen-min-bar)
  )

