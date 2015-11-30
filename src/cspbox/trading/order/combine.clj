(ns cspbox.trading.order.combine)

; (defstruct trade
;  timestamp
;  price
;  quantity
;  description)

(defn aggregate-trades
  "combine trades to one"
  [trades-list]
  (let [timestamp  (map :timestamp trades-list)
        quantity   (map :quantity trades-list)
        price      (map :price trades-list)
        agg-quantity  (apply + quantity)
        agg-timestamp (max timestamp)
        weighted-sum  (apply + (map * price quantity))]
    (when agg-quantity
      {:timestamp   agg-timestamp
       :price       (/ weighted-sum agg-quantity)
       :quantity    agg-quantity
       :description "agg-trade"})))

