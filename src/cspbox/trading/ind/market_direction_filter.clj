(ns cspbox.trading.ind.market-direction-filter
  (:require [cspbox.trading.ind.spec :refer [moving-linear-regression channel]]
            [cspbox.trading.ind.base :refer [make-sma-ind]]
            [cspbox.runtime.sys.utils.macro :refer [to-map]]))

(defn market-direction-filter
  [{:keys [period]}]
  (let [direction  (moving-linear-regression period)
        center-indicator  (make-sma-ind 7)
        channel-width-indicator  (make-sma-ind 7)
        width-multiplier 0.02
        envelope   (channel (to-map center-indicator channel-width-indicator width-multiplier))]
    (fn [price]
      (let [value (direction price)
            {:keys [upper-band lower-band]}  (envelope value)]
        (to-map value upper-band lower-band)))))
