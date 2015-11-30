(ns cspbox.trading.ind.extend
  (:require [cspbox.trading.ind.roll-ind :refer [make-roll-indicator make-lookback-indicator]]
            [cspbox.store.buf.roll :refer [make-lookback-buffer]]
            [cspbox.trading.ind.base :refer [p-max p-min avg make-ema-ind]]))

(defn bollinger-band
  "Upper Band: K times an N-period standard deviation above the moving average (MA + Kσ)
  Lower Band: K times an N-period standard deviation below the moving average (MA − Kσ)
  K: number of standard deviations
  N: period"
  [period & [k]]
  (let [k      (or k 2)
        P      (make-lookback-buffer period :push)
        ema    (make-ema-ind period)]
    (fn [price]
      (let [prices   (P price)
            ma       (ema price)
            mean     (avg prices)
            sq-diff  (map (fn [x] (let [diff (- mean x)]
                                    (* diff diff))) prices)
            variance (avg sq-diff)
            standard-deviation (. Math sqrt variance)]
        [(+ ma (* k standard-deviation))
         (- ma (* k standard-deviation))]))))

(defn relative-strength-index
  "The Relative Strength Index (RSI) is a momentum oscillator that measures the speed and change of price movements.
   It oscillates between zero and 100.
   If no 'period' is given, it defaults to 14"
  [&[period]]
  (let [period  (or period 14)
        P       (make-lookback-buffer period :push)
        find-up (comp (partial remove nil?) (partial map #(if (> %1 %2) %1 nil)))
        find-dn (comp (partial remove nil?) (partial map #(if (< %1 %2) %1 nil)))]
    (fn [price]
      (let [old-prices (P)
            prices     (P price)
            up-list    (find-up prices old-prices)
            dn-list    (find-dn prices old-prices)
            avg-up     (/ (apply + up-list) period)
            avg-dn     (/ (apply + dn-list) period)
            rs         (if-not (= 0 avg-dn) (/ avg-up avg-dn) 0)
            rsi        (- 100.0 (/ 100 (+ 1 rs)))]
        rsi))))

(defn moving-average-convergence-divergence
  "The MACD 'oscillator' or 'indicator' is a collection of three signals (or computed data-series), calculated from historical price data. These three signal lines are:

    i) the MACD line: difference between the 12 and 26 days EMAs
      MACD = EMA[stockPrices,12] – EMA[stockPrices,26]

    ii) the signal line (or average line): 9 EMA of the MACD line
      signal = EMA[MACD,9]

    iii) and the difference (or divergence): difference between the blue and red lines
      histogram = MACD – signal

    Options are:
      :macd-window-fast (default is 12)
      :macd-window-slow (default is 26)
      :signal-window (default is 9)"
  [& [fast-period slow-period signal-period]]
  (let [fast-ema    (make-ema-ind (or fast-period 12))
        slow-ema    (make-ema-ind (or slow-period 26))
        signal-ema  (make-ema-ind (or signal-period 9))]
    (fn [price]
      (let [fast   (fast-ema price)
            slow   (slow-ema price)
            macd   (- fast slow)   ;; DIF
            signal (signal-ema macd) ;; DEA
            histogram (- macd signal)]  ;; macd = 2 * histogram
        [macd signal histogram]))))

(defn stochastic-oscillator
  "The stochastic oscillator is a momentum indicator. According to George C. Lane (the inventor),
   it 'doesn't follow price, it doesn't follow volume or anything like that. It follows the speed
  or the momentum of price. As a rule, the momentum changes direction before price'. A 3-line Stochastics
  will give an anticipatory signal in %K, a signal in the turnaround of %D at or before a bottom,
  and a confirmation of the turnaround in %D-Slow. Smoothing the indicator over 3 periods is standard.

     i) last-price:
       the last closing price

     ii) %K:
       (last-price - low-price / high-price - low-price) * 100

     iii) %D:
       3-period exponential moving average of %K

     iv)  %D-Slow
       3-period exponential moving average of %D

     v) low-price:
       the lowest price over the last N periods

     vi) high-price:
       the highest price over the last N periods"
  [period]
  (let [P       (make-lookback-buffer period :push)
        ema-k   (make-ema-ind 3)
        ema-d   (make-ema-ind 3)]
    (fn [price]
      (let [prices   (P price)
            high     (apply max prices)
            low      (apply min prices)
            %K       (if (= high low) 0 (/ (- price low) (- high low)))
            %D       (ema-k %K)
            %D-Slow  (ema-d %D)]
        [%K %D %D-Slow]))))

(defn KDJ
  [period & [price-type k-period d-period ]]
  (let [P (make-lookback-buffer period :push)
        ema-k   (make-ema-ind (or k-period 3))
        ema-d   (make-ema-ind (or d-period 3))
        ;price-type (or price-type :hloc)
        f-max   (p-max price-type)
        f-min   (p-min price-type)
        cur-p   (if price-type #(:close %1) identity)]
    (fn [price]
      (let [prices (P price)
            high   (apply f-max prices)
            low    (apply f-min prices)
            rsv    (if-not (= high low)
                     (* (/ (- (cur-p price) low) (- high low)) 100.0)
                     50.0)
             K     (ema-k rsv)
             D     (ema-d K)
             J     (- (* 3.0 D) (* 2.0 K))]
        [K D J]))))


(defn on-balance-volume
  "On Balance Volume (OBV) measures buying and selling pressure as a cumulative indicator that
  i) adds volume on up days and ii) subtracts volume on down days. We'll look for divergences
  between OBV and price to predict price movements or use OBV to confirm price trends.

   The On Balance Volume (OBV) line is a running total of positive and negative volume.
   i) A tick's volume is positive when the close is above the prior close. Or
  ii) a tick's volume is negative when the close is below the prior close.

    If closing price is above prior:
      Current OBV = Previous OBV + Current Volume

    If closing price is below prior:
      Current OBV = Previous OBV  -  Current Volume

    If closing price equals prior:
      Current OBV = Previous OBV (no change)"
  []
  (let [P   (make-lookback-buffer 1)
        OBV (make-lookback-buffer 1)]
    (fn [price]
      (let [[cur-close cur-vol] ((juxt :close :vol) price)
            prev-close (or (P) cur-close)
            _          (P price)
            prev-obv   (OBV)]
        (if prev-obv
          (let [obv (cond
                      (cur-close > prev-close) (+ prev-obv cur-vol)
                      (cur-close < prev-close) (- prev-obv cur-vol)
                      :else  prev-obv)]
            (OBV obv))
          (OBV cur-vol))))))


