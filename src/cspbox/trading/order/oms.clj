(ns cspbox.trading.order.oms
  (:require [cspbox.trading.stats.base :refer [classify]]
            [cspbox.store.buf.buffer :refer [make-buf remove-item]]
            [cspbox.trading.order.market :refer [simple-slippage adjust-price]]))


(defn simulate-execute
  "Simulate execution of an order"
  [order]
  (let [order-type (:type order)
        size       (:quantity order)
        price      (adjust-price (:price order) simple-slippage size order-type)]
    {:timestamp (:timestamp order)
     :price price
     :quantity size}))

(defn fill-order
  ""
  [trade orders]
  (let [size (:quantity trade)]
    (first (filter #(= (:quantity %) size) orders))))

(defn oms
  "Order Management system "
  [execute]
  (let [orders-buf (make-buf :local)]
    [(fn [order] ;; push
       (orders-buf order))
     (fn [trade] ;; fill the order
       (let [order-list (orders-buf)
             order (fill-order trade order-list)]
         (when order
           (remove-item orders-buf order))))
     (fn [] ;; execute order
       (let [orders  (orders-buf)
             [category-orders non-category-orders] (classify orders [#(= (:algo-type %) :aggressive)
                                                                     #(= (:algo-type %) :passive)])]
         (mapv execute category-orders)))]))


