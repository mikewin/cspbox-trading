(ns cspbox.trading.fit.ffc
  (:require [cspbox.store.buf.roll :refer [make-lookback-buffer]]
            [cspbox.trading.ind.base :refer [avg make-ema-ind]]
            [cspbox.trading.stats.base :refer [sum-pl]]
            [cspbox.tools.config :refer [select-read-conf]]))


(defn rolling-trade-net-asset-value-fitness
  "Calculation used to judge the fitness of a means-regression type trading strategy. The
  threshold value bumps the moving average up to force the fitness negative when the NAV
  is losing momentum."
  [current-nav nav-ma ffc-threshold-percent]  ;; current-nav from navs of agent
  (- current-nav (* (inc ffc-threshold-percent) nav-ma)))

(defn rolling-profit-loss-fitness
  "Calculation used to judge the fitness of a trend-following or longer term strategy. The
  THRESHOLD-PERCENT band above the moving average to force the fitness negative when the profit/loss
  is losing momentum."
  [current-pl pl-ma ffc-threshold-percent] ;; current-pl from pls of agent
  (- current-pl (* (inc ffc-threshold-percent) pl-ma)))

(defn rolling-profit-factor-fitness
  "Calculate the fitness of a trading agent using a rolling window of the ratio of the system gross profit and loss."
  [gross-p-ma gross-l-ma]
  (/ gross-p-ma gross-l-ma))


(defn path-length
  "Calculate the path length of the values in the trading window."
  [series window-length]
  (let [data1 (drop 1 (take window-length series))
        data2 (take (dec window-length) series)]
    (apply + (map #(Math/abs (- %1 %2)) data2 data1))))


(defn path-length-fitness
  "Measures the deviation of the NAV from a straight line benchmark with the same endpoints
  but also taking account of the average return over the period so low or negative return is
  penalized."
  [data-series window-length] ;(if (= path-type :nav) (navs a) (revalprices a))
  (let  [current-nav (first data-series)
         initial-nav (or (nth data-series (dec window-length)) (last data-series))]
    (* (Math/pow (- (/ current-nav initial-nav) 1) (/ window-length))
       (/ (Math/sqrt (+ (Math/pow window-length 2) (Math/pow (- current-nav initial-nav) 2)))
          (path-length data-series window-length)))))


(defn relative-path-length-fitness
  "Measures the performance of a strategy relative to a long-only or short-only benchmark (index)."
  [window-length navs revalprices]
  (- (path-length-fitness navs window-length))
     (Math/abs (path-length-fitness revalprices window-length)))


(defn compute-fitness-feedback
  [ffc-smoothing-period ffc-live-threshold ffc-offline-threshold
   fitness-measure ffc-threshold-percent ffc-window-length]
  " NAV : net asset value"
  (let [nav-ema (make-ema-ind ffc-smoothing-period)
       pl-ema   (make-ema-ind ffc-smoothing-period)
       gross-profit-ema  (make-ema-ind ffc-smoothing-period)
       gross-loss-ema    (make-ema-ind ffc-smoothing-period)
       ffc-state-buf     (make-lookback-buffer 1)]
    (fn [navs revalprices pls trade-groups]
      (let[cur-nav      (first navs)  ;; newest
           cur-pl       (first pls)
           nav-ma       (nav-ema cur-nav)
           pl-ma        (pl-ema cur-pl) ;; or (pl-ema p)
           [gross-profit gross-loss] (sum-pl trade-groups)
           gross-p-ma   (gross-profit-ema gross-profit)
           gross-l-ma   (gross-loss-ema  gross-loss)
           ffc-state    (or (ffc-state-buf) :offline)
           fitness-level (case fitness-measure
                           :rolling-trade-nav-fitness     (rolling-trade-net-asset-value-fitness
                                                              cur-nav nav-ma ffc-threshold-percent)
                           :rolling-profit-loss-fitness   (rolling-profit-loss-fitness
                                                              cur-pl pl-ma ffc-threshold-percent)
                           :rolling-profit-factor-fitness (rolling-profit-factor-fitness gross-p-ma gross-l-ma)
                           :nav-path-length-fitness       (path-length-fitness navs ffc-window-length)
                           :price-path-length-fitness     (path-length-fitness revalprices ffc-window-length)
                           :relative-path-length-fitness  (relative-path-length-fitness navs revalprices ffc-window-length))
           ffc-state   (cond
                         (and (= ffc-state :offline)
                              (> fitness-level ffc-live-threshold))  :live
                         (and (= ffc-state :live)
                              (<= fitness-level ffc-offline-threshold)) :offline
                         :esel :offline)]
        (ffc-state-buf ffc-state)))))

(defn make-compute-fitness-feedback
  "read config then generate calc function"
  [& [conf]]
  (let [conf  (or conf (select-read-conf "ffc.edn"))
        {:keys [ffc-smoothing-period ffc-live-threshold ffc-offline-threshold
                fitness-measure ffc-threshold-percent ffc-window-length]}  conf]
    (compute-fitness-feedback ffc-smoothing-period ffc-live-threshold ffc-offline-threshold
     fitness-measure ffc-threshold-percent ffc-window-length)))
