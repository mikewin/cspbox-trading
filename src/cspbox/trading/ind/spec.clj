(ns cspbox.trading.ind.spec
  (:require [cspbox.trading.ind.roll-ind :refer [make-roll-indicator make-lookback-indicator]]
            [cspbox.runtime.store.buf.roll :refer [make-lookback-buffer]]
            [cspbox.trading.ind.base :refer [p-max p-min avg make-ema-ind]]
            [cspbox.runtime.sys.utils.macro :refer [to-map]]))

(defn moving-linear-regression
  "http://introcs.cs.princeton.edu/java/97data/LinearRegression.java.html"
  [period]
  (let [P  (make-lookback-buffer period :push)
        start-factor (/ (- period 1) 2)
        factors (range start-factor (- start-factor) -1.0)
        factor-square (map (fn [x] (* x x)) factors)
        factor-divisor (apply + factor-square)]
    (fn [price]
      (let [prices (P price)
            price-sum  (apply + prices)
            weighted-sum (apply + (map #(* %1 %2) factors (reverse prices)))] ;; reverse or not?
        (+ (/ price-sum period)
                 (/ (* (first factors) weighted-sum) ;; optimize start-factor
                        factor-divisor))))))

(defn channel
  "Center and width-based channel. Upper and lower bands are offset
from the center value by 1/2 of the calculated width"
  [{:keys [center-indicator channel-width-indicator width-multiplier]}]
  (fn [price]
    (let [value-width  (channel-width-indicator price)
          value-center (center-indicator price)
          half-width   (/ (* value-width width-multiplier) 2)
          upper-band (+ value-center half-width)
          lower-band (- value-center half-width)]
      (to-map upper-band value-center lower-band))))

(defn centerless-channel
  [upper-band-indicator lower-band-indicator]
  (fn [price]
    (let [upper-band  (upper-band-indicator price)
          lower-band  (lower-band-indicator price)
          value (/ (+ upper-band lower-band) 2)]
     [upper-band value lower-band])))

(defn asymmetric-channel
  [center-indicator upper-band-indicator lower-band-indicator]
  (fn [price]
    (let [value (center-indicator price)
          upper-band  (+ value (upper-band-indicator price))
          lower-band  (- value (lower-band-indicator price))]
      [upper-band value lower-band])))


(defn donchian-channel
  [period & [{:keys [offset price-type]}]] ;
  (let [offset (or offset 1)
        length (+ period offset)
        p-buf  (make-lookback-buffer length :push)
        f-max  (p-max price-type)
        f-min  (p-min price-type)]
    (fn [price]
      (let [prices     (p-buf price)]
        (when (>= (count prices) length)
          (let [cmp-prices (drop-last offset prices)
                upper-band (apply f-max cmp-prices) ;; update indicator
                lower-band (apply f-min cmp-prices)
                value  (/ (+ upper-band lower-band) 2)]
            (to-map value upper-band lower-band)))))))


(defn average-true-range
  [period & [smoothing-type value-type price-type]]
  (let [factor (double (/ 2 (inc period)))
        P      (make-lookback-buffer 1)
        TR     (make-lookback-buffer period :push)
        MA     (if (= smoothing-type :ema) (make-ema-ind period) nil)] ;; smoothing-type is :ema or :sma
    (fn [price]
      (let [prev-price  (P)
            true-range  (if price-type ;; h l o c
                          (if-not (nil? prev-price)
                            (max (- (:high price) (:low price))
                               (Math/abs (- (:high price) (:close prev-price)))
                               (Math/abs (- (:close prev-price) (:low price))))
                            (- (:high price) (:low price)))
                          (if-not (nil?  prev-price)
                            (Math/abs (- prev-price price)) 0))
            true-range  (if (= value-type :percent)  (/ true-range (price prev-price)) true-range)
            tr-buf   (TR true-range)
            value    (if MA (MA true-range) (avg tr-buf))]
        (P price)
       (to-map value)))))

(defn parabolic-sar
  "Parabolic Stop-and-Reverse indicator developed by Welles Wilder."
  [af-step af-step-max]
  (let [P     (make-lookback-buffer 2)
        State (make-lookback-buffer 1)
        Acceleration-factor (make-lookback-buffer 1)
        Extreme  (make-lookback-buffer 1)
        Value    (make-lookback-buffer 1)]
    (fn [price]
      (let [prev-price (P)
            _       (P price)
            state   (State)
            value   (Value)
            extreme (Extreme)
            acceleration-factor (Acceleration-factor)]
        (cond
          (= state :init) (do
                             (State :long)
                             (Acceleration-factor af-step)
                             (Extreme (:high price))
                             (Value (:low price)))
          (= state :short) (if (< value (:high price))
                             (do
                               (State :long)
                               (Acceleration-factor af-step)
                               (Value extreme)
                               (Extreme (:high price)))
                             (do
                               (when (< (:low price) extreme)
                                 (Acceleration-factor (min (+ acceleration-factor af-step) af-step-max))
                                 (Extreme (:low price)))
                               (Value (max (max (:high price) (:high prev-price))
                                  (+ value (* acceleration-factor (- extreme value)))))))
          (= state :long)  (if (> value (:low price))
                             (do  (State :short)
                                  (Acceleration-factor af-step)
                                  (Value extreme)
                                  (Extreme (:low price)))
                             (do
                               (when (> (:high price) extreme)
                                 (Acceleration-factor (min (+ acceleration-factor af-step) af-step-max))
                                 (Extreme (:high price)))
                               (Value (min (min (:low price) (:low prev-price))
                                  (+ value (* acceleration-factor (- extreme value))))))))))))
