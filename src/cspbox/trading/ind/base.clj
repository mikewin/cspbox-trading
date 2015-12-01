(ns cspbox.trading.ind.base
  (:require [cspbox.trading.ind.roll-ind :refer [make-roll-indicator make-lookback-indicator]]
            [cspbox.runtime.store.buf.roll :refer [make-lookback-buffer]]
            [cspbox.runtime.sys.utils.macro :refer [to-map]]))

(defn p-max
  [price-type]
  (if price-type
    (fn [& p] (apply max (map :high p)))
    max))

(defn p-min
  [price-type]
  (if price-type
    (fn [& p] (apply min (map :low p)))
    min))

(defn avg [numbers]
  (float (/ (reduce + numbers)
            (count numbers))))

(defn make-sma-ind
  "return sma function"
  [period]
  (make-roll-indicator avg period))

(defn ema
  "genearte ema function"
  [period]
  (let [factor (/ 2 (inc period))]
    (fn [price & [old-price]]
      (float (+ (* factor price) (* (- 1 factor) (or old-price price)))))))

(defn make-ema-ind
  [period]
  (make-lookback-indicator (ema period) 1))

(defn make-ama-ind
  [{:keys [min-period max-period width-factor snr-factor]}]
  (assert (< min-period max-period))
  (let [pi|2       (/ Math/PI 2)
        min-factor (/ 2 (inc min-period))
        max-factor (/ 2 (inc max-period))
        D+     (make-lookback-buffer 1)
        D-     (make-lookback-buffer 1)
        P      (make-lookback-buffer 1)
        MA     (make-lookback-buffer 1)
        Factor (make-lookback-buffer 1)
        _      (Factor (+ min-factor (/ (- max-factor min-factor) 2)))]
    (fn[price]
      (let [prev-price (or (P) price)
            prev-value (or (MA) price)
            deviation+ (or (D+) 0.0)
            deviation- (or (D-) 0.0)
            factor     (Factor)
            scale-factor- (* width-factor deviation-)
            scale-factor+ (* width-factor deviation+)
            deviation+ (+ (* factor (max (/ (- price prev-price) prev-price) 0))
                          (* (- 1 factor) deviation+))
            deviation- (+ (* factor -1 (min (/ (- price prev-price) prev-price) 0))
                          (* (- 1 factor) deviation-))
            value      (+ (* factor price) (* (- 1 factor) prev-value))
            upper-band (* (+ 1 scale-factor-) value)
            lower-band (* (- 1 scale-factor+) value)
            snr        (cond
                         (and (> price upper-band) (not= scale-factor- 0.0))
                           (/ (- price prev-value) (* prev-value scale-factor-))
                         (and (< price lower-band) (not= scale-factor+ 0.0))
                           (/ (* -1 (- price prev-value)) (* prev-value scale-factor+)) ;; check here later
                         :else 0)
            normalized-atan (/ (+ (Math/atan (* snr-factor snr)) pi|2) (* 2 pi|2))
            factor     (+ max-factor (* (- min-factor max-factor) normalized-atan))]
        (Factor factor)
        (P price)
        (D+ deviation+)
        (D- deviation-)
        (MA value)
        (to-map upper-band value lower-band)))))


(defn make-frama-ind
  "fractal-adaptive-moving-average"
  [{:keys [min-period max-period fractal-period]}]
  (assert (< min-period max-period))
  (assert (even? fractal-period))
  (let [max-factor     (/ 2 (inc max-period))
        log-max-factor (Math/log max-factor)
        half-fractal-period (/ fractal-period 2)
        P      (make-lookback-buffer fractal-period :push)
        MA     (make-lookback-buffer 1)
        find-high (comp (partial apply max) (partial map :high))
        find-low  (comp (partial apply min) (partial map :low))]
    (fn[price]
      (let [prices      (P price)]
        (when (= (count prices) fractal-period)
          (let [[fh-prices bh-prices] (partition half-fractal-period prices)
                [[fh-high  fh-low] [bh-high bh-low] [high low]]
                              (mapv (juxt find-high find-low) [fh-prices bh-prices prices])
                n1   (/ (- fh-high fh-low) half-fractal-period)
                n2   (/ (- bh-high bh-low) half-fractal-period)
                n3   (/ (- high low) fractal-period)
                dimension       (max (min (/ (- (Math/log (+ n1 n2)) (Math/log n3)) (Math/log 2)) 2) 1) ;realpart
                unscaled-factor (Math/exp (* log-max-factor (- dimension 1)))
                unscaled-period (/ (- 2 unscaled-factor) unscaled-factor)
                factor          (min (max (/ 2 (inc (+ (* (- max-period min-period)
                                          (/ (dec unscaled-period) (dec max-period)))
                                               min-period)))  max-factor) 1)
                value (or (MA) (:price price))
                value (MA (+ (* factor (:price price)) (* (- 1 factor) value)))]
            (to-map value)))))))
