(ns cspbox.trading.stats.partition
  (:require [cspbox.trading.stats.group :refer [compute-trade-group]]
            [cspbox.runtime.sys.utils.macro :refer[to-map]]))
;; todo
;;copy code from wzrdsappr (translate to clojure) , just for test

(defn proc-trade
  "helper fn for partition"
  [trade-groups coll trade]
  (let [{:keys [new-position current-group]}   coll
        trade-quantity   (:quantity trade)
        old-position     new-position
        new-position     (+ new-position trade-quantity)
        current-group    (cond (< (* old-position new-position) 0)
                                   (do
                                              (swap! trade-groups conj
                                                     (compute-trade-group (conj current-group
                                                               {:timestamp (:timestamp trade)
                                                                   :quantity (- old-position)
                                                                   :price (:trade-price trade)
                                                                   :description "SplitExit"})))
                                              [{:timestamp (:timestamp trade)
                                                            :quantity new-position
                                                            :price     (:price trade)
                                                            :description "SplitEntry"}])
                               (= new-position 0) (do
                                                     (swap! trade-groups conj
                                                       (compute-trade-group (conj current-group trade)))
                                                    nil)
                                :else   (conj current-group trade))]
    (to-map new-position current-group)))

(defn partition-trades
  "partition unprocessed trades"
  [unprocessed-trades last-timestamp last-price]
  (let [trade-groups  (atom nil)
        trades   (reverse unprocessed-trades)
        coll     (reduce (partial proc-trade trade-groups) {:new-position 0} trades)
        {:keys [new-position current-group]}   coll]
    (when (not= new-position 0)
      (swap! trade-groups (conj current-group {
                                                :timestamp last-timestamp
                                                :quantity (- new-position)
                                                :price last-price
                                                :description "DummyExit"})))
    @trade-groups))
