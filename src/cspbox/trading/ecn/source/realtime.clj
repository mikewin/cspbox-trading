(ns cspbox.trading.ecn.source.realtime
  (:require [cspbox.trading.ecn.web.wmcloud.data :refer [get-rt-bar read-config]]
            [cspbox.store.buf.roll :refer [make-lookback-buffer]]
            [clojure.string :as str]
            [clj-time.local :as l]
            [clj-time.core :as t]
            [semantic-csv.core :as sc :refer [->int]]
            [cspbox.trading.order.market :refer [market-open market-close]]
            [cspbox.conv.utils.macro :refer [to-map]]))

;; also see web grab data.clj

(defn pipe-buf
  "a fn wrap a buffer used like pop"
  [& [init-buf]]
  (let [buf (atom (or init-buf []))]
    (fn
      ([]
        (let [value (first @buf)]
          (swap! buf (comp vec next))
          value))
      ([new-buf]
        (swap! buf into new-buf)))))

(defn get-new-data
  ""
  [symbol last-min start conf data-buf]
  (let [last-time (or (last-min) start)
        [now-h now-m]  ((juxt t/hour t/minute) (l/local-now))
        [last-h last-m]  (mapv ->int (str/split last-time #":"))]
    (when  (or (< last-h now-h) (and (= last-h now-h) (< last-m now-m)))
      (let [[last-h last-m]  (if (= last-m 59) [(inc last-h) 0] [last-h (inc last-m)])
            inc-last (str last-h ":" last-m)
            data  (get-rt-bar conf symbol {:start inc-last})]
        (when-not (empty? data)
          (let [minute  (:barTime (last data))]
            (data-buf data)
            (last-min minute)))))))

(defn data-source-intraday-min
  "require today minute data"
  [{:keys [symbol from]}] ;;stop
  (assert symbol "data source should give symbol")
  (let [last-min  (make-lookback-buffer 1) ;; last
        start     (or from market-open) ;;todo for recovery
        data-buf  (pipe-buf)
        conf      (read-config)]
    {:price
     (fn []
       (let [_   (get-new-data symbol last-min start conf data-buf)
             out-data  (data-buf)]
         (when out-data
           {:date (:barTime out-data) :price (:closePrice out-data)})))
     :bar
     (fn[]
       (let [_   (get-new-data symbol last-min start conf data-buf)
             out-data  (data-buf)
             [date open close high low volume] ((juxt :barTime :closePrice :openPrice
                                                      :highPrice :lowPrice :totalVolume) out-data)]
         (when out-data
           (to-map date open close high low volume))))}))

;;test

(comment
(def gen-m (:price (data-source-intraday-min {:symbol "00700.XHKG" :from "14:50"})))

(gen-m)
  )


