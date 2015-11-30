(ns cspbox.trading.ind.envelope-moving-avg-trend-following
  (:require [cspbox.trading.ind.base :refer [make-ema-ind]]
            [cspbox.conv.utils.macro :refer [to-map]]
            [clojure.tools.logging  :as log]))

(defn envelope-moving-avg-trend-following
  [{:keys [N width]}]
  (let[ema-fn (make-ema-ind N)]
    (fn [{:keys [date price]}]
      (let [ema (ema-fn price)
            p  price
            L (* (+ 1 width) ema)
            S (* (- 1 width) ema)] ;ema (or (ema-fn) price)
        (log/info date p S L ema)
        (to-map date p L S)))))
