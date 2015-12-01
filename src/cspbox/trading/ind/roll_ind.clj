(ns cspbox.trading.ind.roll-ind
  (:require [cspbox.runtime.store.buf.roll :refer [make-lookback-buffer]]))



(defn make-roll-indicator
  [calc period]
  (let [roll-buf  (make-lookback-buffer period :push)]
    (fn [price]
      (-> price
          roll-buf
          calc))))

(defn make-lookback-indicator
  [calc n-lookback] ;; nlb = number of lookback
  (let [roll-buf   (make-lookback-buffer n-lookback)]
    (fn [price]
      (let [old-value (roll-buf)
            value (calc price old-value)]
        (roll-buf value)))))


