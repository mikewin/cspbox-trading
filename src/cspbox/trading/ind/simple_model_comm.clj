(ns cspbox.trading.ind.simple-model-comm
  (:require [cspbox.runtime.store.buf.roll :refer [make-lookback-buffer]]
            [cspbox.trading.ind.base :refer [make-sma-ind]]
            [cspbox.runtime.sys.utils.macro :refer [to-map]]))

(defn simple-model-comm
  ""
  [{:keys [L]}]
  (let [unblock-short-buf (make-lookback-buffer 1)
        unblock-long-buf  (make-lookback-buffer 1)
        sma-ind  (make-sma-ind L)]
    (unblock-short-buf -1)
    (unblock-long-buf   1)
    [(fn [{:keys [state]}]
      (case state
        :long  (do (unblock-long-buf 0) (unblock-short-buf -1))
        :short (do (unblock-long-buf 1) (unblock-short-buf 0))
        :init  (do (unblock-long-buf 0) (unblock-short-buf 0))))
     (fn [price]
       (let [sma (sma-ind price)
             unblock-long (unblock-long-buf)
             unblock-short (unblock-short-buf)]
         (to-map sma unblock-long unblock-short)))]))
