(ns cspbox.trading.ind.range-projection-mean-reversion
  (:require [cspbox.store.buf.roll :refer [make-lookback-buffer]]
            [cspbox.trading.ind.base :refer [make-sma-ind avg]]
            [cspbox.trading.order.market :refer [market-hours market-closed-p]]
            [cspbox.tools.date :refer [timestamp-day-of-week seconds-per-hour sec-of]]
            [clj-time.core :as t]
            [cspbox.conv.utils.macro :refer [to-map]]
            [clojure.tools.logging  :as log]))

;;; In order for this trading strategy to work correctly, the event stream must be at a faster
;;; frequency than the projection interval since the projected range is based on the opening value
;;; of the projection interval.  Additionally, the smoothed trading range limits must be re-calculated
;;; only at the projection interval, so an instance of the TIME-BAR-GENERATOR that will generate bars
;;; at the projection interval must be created to supply market updates to the strategy as well.

(defn range-projection-mean-reversion
  "projection-interval :week :day :hour, input bar data,todo: market-on-close"
  [{:keys [N projection-interval]}]
  (let [pivot-ma-ind     (make-sma-ind N)
        tr-ma-ind        (make-sma-ind N)
        pivot-ma-history (make-lookback-buffer N :push)
        tr-ma-buf        (make-lookback-buffer 1)
        L-buf            (make-lookback-buffer 1)
        S-buf            (make-lookback-buffer 1)
        SFL-buf          (make-lookback-buffer 1)
        SFS-buf          (make-lookback-buffer 1)]
    {:range ;;use this function before market open time
      (fn [{:keys [high low open close]}]
        (let [pivot (avg [open close high low])
              trading-range  (/ (* 2 (- high low)) (+ high low))
              prev-tr-ma (tr-ma-buf)
              pivot-ma  (pivot-ma-ind pivot)
              tr-ma     (tr-ma-ind trading-range)
              pivot-ma-list (pivot-ma-history pivot-ma) ]
          (tr-ma-buf tr-ma)
          (when (= (count pivot-ma-list) N)
            (let [channel-center (+ open (/ (- (last pivot-ma-list) (first pivot-ma-list)) N))
                  L (/ channel-center (inc (* prev-tr-ma 1/2)))
                  S (* channel-center (inc (* prev-tr-ma 1/2)))
                  SFL (/ channel-center (inc (* prev-tr-ma 2/3)))
                  SFS (* channel-center (inc (* prev-tr-ma 2/3)))]
               (tr-ma-buf tr-ma) (pivot-ma-history pivot-ma)
              (L-buf L) (S-buf S) (SFL-buf SFL) (SFS-buf SFS)
              (log/info L S SFL SFS)))))
      :realtime ;; use this in market time
      (fn [{:keys [date price]}]
        (when-let [L  (L-buf)]
          (let [S    (S-buf)
                SFL  (SFL-buf)
                SFS  (SFS-buf)
                p    price]
            (to-map date p L S SFS SFL))))}))







