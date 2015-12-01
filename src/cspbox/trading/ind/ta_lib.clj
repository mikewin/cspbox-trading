#_(ns cspbox.trading.ind.ta-lib
  (:require [clj-ta-lib.core :as ta]))

#_(defmacro wrap-talib
  [ind & param]
  (let [sname  (name ind)
        fname  (symbol sname)]
    `(defn ~fname [~@param data#]
       (let [res#  (ta/ta ~sname [(double-array data#)] ~@param)]
         (when res#
           (vec (first res#)))))))

;(macroexpand-1 '(wrap-talib :macd fast-period slow-period signal-period))
;(wrap-talib :sma period)
;(wrap-talib :rsi period)
