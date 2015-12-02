(ns cspbox.trading.ind.adaptive-moving-avg-trend-following
  (:require [cspbox.trading.ind.spec :refer [average-true-range]]
            [cspbox.trading.ind.base :refer [make-ama-ind]]
            [cspbox.runtime.store.buf.roll :refer [make-lookback-buffer]]
            [cspbox.runtime.sys.utils.macro :refer [to-map]]
            [taoensso.timbre  :as log]))


(defn adaptive-moving-avg-trend-following
  [config]
  (let [ama-fn  (make-ama-ind config)
        atr-fn  (average-true-range 20) ;; period
        SFL-buf (make-lookback-buffer 1)
        SFS-buf (make-lookback-buffer 1)
        PFL-buf (make-lookback-buffer 1)
        PFS-buf (make-lookback-buffer 1)
        p-buf   (make-lookback-buffer 1)
        width-factor (:width-factor config)]
    (fn [{:keys [date price]} state]
      (let [SFL (or (SFL-buf) price)
            SFS (or (SFS-buf) price)
            PFL (or (PFL-buf) price)
            PFS (or (PFS-buf) price)
            prev-p (or (p-buf) price)
            p   price
            ama (ama-fn price)
            atr (atr-fn price)
            value (:value ama)
            atr-value (:value atr)
            upper-band (:upper-band ama)
            lower-band (:lower-band ama)
            SFL (if (< prev-p SFL)
                  (- value (* atr-value width-factor))
                  (max SFL (- value (* atr-value width-factor))))
            SFS (if (> prev-p SFS)
                  (+ value (* atr-value width-factor))
                  (min SFS (+ value (* atr-value width-factor))))
            PFL (if-not (= state :long) (+ upper-band  (* 1.2 (* atr-value width-factor))) PFL)
            PFS (if-not (= state :short) (- lower-band  (* 1.2 (* atr-value width-factor))) PFS)]
        (SFL-buf SFL) (SFS-buf SFS) (PFL-buf PFL) (PFS-buf PFS) (p-buf price)
        (log/debug p upper-band lower-band PFL PFS) ;; for debug
        (to-map date p upper-band lower-band SFL SFS PFL PFS)))))


