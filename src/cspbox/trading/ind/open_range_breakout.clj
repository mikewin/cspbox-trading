(ns cspbox.trading.ind.open-range-breakout
  (:require [cspbox.trading.ind.spec :refer [average-true-range]]
            [cspbox.runtime.store.buf.roll :refer [make-lookback-buffer]]
            [cspbox.trading.order.market :refer [market-closed-p]]
            [cspbox.runtime.sys.utils.macro :refer [to-map]]
            [taoensso.timbre  :as log]))


(defn opening-range-breakout
  ""
  [{:keys [volatility-limit N]}]
  (let [volatility (average-true-range N nil :percent)
        counter-fn (make-lookback-buffer 1)
        K1   1.5 ;;3/2
        K2   3]
    (fn[price timestamp]
      (let [prev-volatility (:value (volatility))
            market-on-close (market-closed-p timestamp)
            counter   (or (counter-fn) 0)
            counter   (if market-on-close 0 (inc counter))
            value   (:value (volatility price))]
        (counter-fn counter)
        (when (and (not market-on-close)
                 (< prev-volatility volatility-limit)
                 (>= value  volatility-limit))
          (let [R1 (* price  (inc (* value  K1)))
                R2 (* price  (inc (* value  K2)))
                S1 (/ price  (inc (* value  K1)))
                S2 (/ price  (inc (* value  K2)))]
            (to-map  R1 R2 S1 S2 market-on-close N counter)))))))
