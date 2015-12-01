(ns cspbox.trading.stats.trade
  (:require [cspbox.runtime.tools.date :refer [seconds-per-day current-timestamp]]
            [cspbox.trading.stats.base :refer [epsilon]]
            [cspbox.trading.stats.group :refer [state-trade-group]]))

(comment
"(defstruct trade-stats
  timestamp
  percent-profitable
  win-to-loss
  average-logret
  total-pl
  average-duration
  pos-pl
  neg-pl
  profit-factor)" )

;; todo max drawdown and so on

(defn compute-trade-stats
  "Compute the trade statistics for the given agent."
  [trade-groups & [timestamp]]
  (let [trade-group-count    (count trade-groups)
        timestamp  (or timestamp (current-timestamp))]
    (when (> trade-group-count 0)
      (let [coll (reduce state-trade-group {:profitable-count 0 :total-length 0
                                            :total-pl  0 :total-logret 0 :pos-pl 0 :neg-pl 0}
                         trade-groups)
            {:keys [profitable-count pos-pl neg-pl total-pl total-logret total-length]} coll]
        {
              :timestamp          timestamp
              :percent-profitable (/ profitable-count trade-group-count)
              :win-to-loss        (if (<= neg-pl epsilon) 100 (/ pos-pl neg-pl))
              :average-logret     (/ total-logret trade-group-count)
              :total-pl           total-pl
              :average-duration   (/ total-length trade-group-count)
              :pos-pl             pos-pl
              :neg-pl             neg-pl
              :profit-factor      (if (<= (+ pos-pl neg-pl) epsilon)
                                    0
                                    (/ (- pos-pl neg-pl) (+ pos-pl neg-pl)))}))))

