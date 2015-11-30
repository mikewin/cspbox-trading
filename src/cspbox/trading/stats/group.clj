(ns cspbox.trading.stats.group
  (:require [cspbox.trading.stats.base :refer [epsilon]]
            [cspbox.tools.date :refer [timestamp-difference seconds-per-day]]
            [cspbox.conv.utils.macro :refer[to-map]]))

;;copy code from wzrdsappr (translate to clojure) , just for test
 (comment
 ":type            :mkt       ; market
                   :moo       ; market on open
                   :moc       ; market on close
                   :loo       ; limit on open
                   :loc       ; limit on close
                   :stp       ; stop
                   :lmt       ; limit
                   :stp-lmt   ; stop-limit
(defstruct trade
  timestamp
  price
  quantity
  description)

(defstruct trade-group
  trades
  entry-timestamp
  exit-timestamp
  duration
  pl
  logret) ")

 ;; order -> trade -> trade-group (by entry and exist)


(defn collect-trade-info
  "use for reduce"
  [coll trade]
  (let [{:keys [trade-start trade-end buy-price-sum sell-price-sum
                buy-trades sell-trades trade-pl]}   coll
        timestamp      (:timestamp trade)
        trade-start    (min (or trade-start timestamp) timestamp) ;; todo compare timestamp
        trade-end      (max (or trade-end timestamp) timestamp)
        quantity       (:quantity trade)
        price          (:price  trade)
        buy-price-sum  (if (> quantity 0) (+ buy-price-sum quantity) buy-price-sum)
        sell-price-sum (if (< quantity 0) (+ sell-price-sum quantity) sell-price-sum)
        buy-trades     (if (> quantity 0) (inc buy-trades) buy-trades)
        sell-trades    (if (< quantity 0) (inc sell-trades) sell-trades)
        trade-pl       (+ trade-pl (* price  (- quantity)))]
    (to-map trade-start trade-end buy-price-sum sell-price-sum
                buy-trades sell-trades trade-pl)))

(defn compute-trade-group
  "Create a trade-group instance, computing the relevant stats."
  [trades]
  (let [coll (reduce collect-trade-info
                       {:buy-price-sum 0 :sell-price-sum 0 :buy-trades 0 :sell-trades 0 :trade-pl 0}
                       trades)
        {:keys [trade-start trade-end buy-price-sum sell-price-sum
                buy-trades sell-trades trade-pl]}   coll
        avg-buy-price  (/ buy-price-sum buy-trades)
        avg-sell-price (/ sell-price-sum sell-trades)
        trade-length   (/ (timestamp-difference trade-end trade-start) seconds-per-day)
        trade-logret   (Math/log (if (or (<= avg-buy-price epsilon)
                                       (<= avg-sell-price epsilon))
                                   1
                                   (/ avg-sell-price avg-buy-price)))]
    {:trades (reverse trades)     ;; store trades in reverse chronological order
     :entry-timestamp trade-start
     :exit-timestamp trade-end
     :duration trade-length
     :pl trade-pl
     :logret trade-logret}))



(defn state-trade-group
  "for reduce, add trade-group info to coll, see compute-trade-stats "
  [coll trade-group]
  (let [{:keys [profitable-count pos-pl neg-pl total-pl total-logret total-length]} coll
        trade-group-pl   (:pl trade-group)
        profitable-count (if (>= trade-group-pl 0) (inc profitable-count) profitable-count)
        pos-pl           (+ pos-pl (max trade-group-pl 0))
        neg-pl           (+ neg-pl (max (- trade-group-pl 0)))
        total-pl         (+ total-pl trade-group-pl)
        total-logret     (+ total-logret (:logret trade-group))
        total-length     (+ total-length (:duration trade-group))]
    (to-map profitable-count pos-pl neg-pl total-pl total-logret total-length)))

