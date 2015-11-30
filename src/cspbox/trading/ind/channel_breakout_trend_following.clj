(ns cspbox.trading.ind.channel-breakout-trend-following
  (:require [cspbox.store.buf.roll :refer [make-lookback-buffer]]
            [cspbox.trading.ind.spec :refer [donchian-channel]]
            [cspbox.conv.utils.macro :refer [to-map]]
            [clojure.tools.logging  :as log]))



(defn channel-breakout-trend-following
  "strategy"
  [{:keys [fast-period slow-period offset]}]
  (let [fast-channel-ind (donchian-channel fast-period offset)
        slow-channel-ind (donchian-channel slow-period offset)
        PFL-buf (make-lookback-buffer 1)
        PFS-buf (make-lookback-buffer 1)]
    (fn [{:keys [date price]} state]
      (let [slow-channel  (slow-channel-ind price)
            fast-channel  (fast-channel-ind price)]
        (when (and slow-channel fast-channel)
          (let [p             price
                L             (:upper-band slow-channel)
                S             (:lower-band slow-channel)
                SFS           (:upper-band fast-channel)
                SFL           (:lower-band fast-channel)
                PFL  (if (contains? #{:long :profit-from-long} state) (max (PFL-buf) L) (+ L (- L SFL)))
                PFS  (if (contains? #{:short :profit-from-short} state) (min (PFS-buf) S) (+ S (- S SFS)))]
            ;(log/info  p   S L " -- " SFL SFS " -- " PFS PFL" " state)
            (PFL-buf PFL) (PFS-buf PFS)
            (to-map date p L S SFS SFL PFL PFS)))))))
