(ns cspbox.trading.ind.swing-mean-reverse
  (:require [cspbox.store.buf.roll :refer [make-lookback-buffer]]
            [cspbox.conv.utils.macro :refer [to-map]]
            [taoensso.timbre  :as log]))


(defn  swing-mean-reverse
  ""
  [{:keys [event-count expected-width max-allowed-breakout]}]
  (let [counter-buf    (make-lookback-buffer 1)
        volatility-buf (make-lookback-buffer 1)
        min-price-buf  (make-lookback-buffer 1)
        max-price-buf  (make-lookback-buffer 1)
        L-buf          (make-lookback-buffer 1)
        S-buf          (make-lookback-buffer 1)
        SFL-buf        (make-lookback-buffer 1)
        SFS-buf        (make-lookback-buffer 1)
        state-buf      (make-lookback-buffer 1)
        revalprice-buf (make-lookback-buffer 1)
        scale-factor   (/ 2 (inc event-count))]
    (fn [{:keys [date price]} state]
      (let [counter  (or (counter-buf) 0)
            counter  (inc counter)
            p          price
            prev-p     (revalprice-buf)
            prev-state (state-buf)]
        (state-buf state)
        (revalprice-buf price)
        (counter-buf counter)
        (when (> counter 1)
          (log/info counter state)
          (let [volatility (or (volatility-buf) 0)
                volatility (+ (* scale-factor (Math/abs (/ (- price prev-p) prev-p)))
                              (* (- 1 scale-factor) volatility))]
            (volatility-buf volatility)
            (when (>= counter event-count)
              (when (or (= counter event-count)
                        (and (contains? #{:init :stop-from-long :stop-from-short} state)
                             (not=  state prev-state)))
                (let [L (/ price (+ 1 (* volatility expected-width 0.5)))
                      S (* price (+ 1 (* volatility expected-width 0.5)))
                      ;; Calculate SFL and SFS here to account for gaps
                      min-price (min-price-buf price)
                      max-price (max-price-buf price)
                      SFL (/ price (+ 1 (* volatility max-allowed-breakout)))
                      SFS (* price (+ 1 (* volatility max-allowed-breakout)))]
                  (L-buf L) (S-buf S) (SFL-buf SFL) (SFS-buf SFS)))
              (when (contains? #{:long :short} state)
                (when (not=  state  prev-state)
                  (let [SFL (/ price  (+ 1 (* volatility max-allowed-breakout)))
                        SFS (* price  (+ 1 (* volatility max-allowed-breakout)))]
                    (SFL-buf SFL) (SFS-buf SFS)))
                (let [max-price (max (max-price-buf) price)
                      min-price (min (min-price-buf) price)
                      L (/ max-price (+ 1 (* volatility expected-width)))
                      S (* min-price (+ 1 (* volatility expected-width)))]
                  (L-buf L) (S-buf S) (max-price-buf max-price) (min-price-buf min-price)))
              (when (> counter event-count)
                (let [L   (L-buf)
                      S   (S-buf)
                      SFL (SFL-buf)
                      SFS (SFS-buf)]
                  (log/debug p S L SFS SFL )
                  (to-map date p L S SFL SFS ))))))))))
