(ns cspbox.trading.ind.simple
  (:require [cspbox.trading.ind.base :refer [make-sma-ind]]
            [cspbox.conv.utils.macro :refer [to-map]]))

(defn sma-ind
  "wrap sma indicator"
  [{:keys [period]}]
  (let [period  (or period 10)
        sma-buf (make-sma-ind period)]
    (fn [{:keys [date price]}]
      (assert price "need price incoming") ;; will remove
      (let [sma (sma-buf price)
            p   price]
        (to-map date p sma)))))
