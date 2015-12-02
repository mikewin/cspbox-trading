(ns cspbox.trading.proc.post
  (:require [cspbox.runtime.store.buf.buffer :refer [make-buf]]
            [cspbox.runtime.store.buf.roll :refer [make-lookback-buffer]]
            [cspbox.trading.order.market :refer [send-order ]]
            [cspbox.runtime.tools.date :refer [date-timestamp]]
            [cspbox.runtime.tools.config :refer [select-read-conf]]
            [cspbox.runtime.tools.write :refer [delete-file]]
            [cspbox.trading.order.record :refer [record-order]]
            [cspbox.trading.fit.ffc :refer [make-compute-fitness-feedback]]
            [taoensso.timbre  :as log]))


(defn make-post-proc
  "generate a function process strategy output"
  [& [{:keys [location ffc record]}]]
  (let [location      (or location :local)
        record-file   (when record
                        (let [file (str (:base-order-path (select-read-conf "path.edn")) "order.csv")]
                          (delete-file file) file))
        position-buf  (make-buf location)
        price-buf     (make-buf location)
        pl-buf        (make-buf location)
        nav-buf       (make-buf location)
        trade-group-buf  (make-buf location)
        calc-ffc (when ffc (make-compute-fitness-feedback))]
    {:proc-postion
      (fn [{:keys [date p position id]}]
        (let [timestamp     (date-timestamp date)
              prev-position (or (first (position-buf)) 0)
              prev-price    (or (first (price-buf)) 0)
              positions    (position-buf position)
              revalprices  (price-buf p)
              pl           (or (* prev-position (- p prev-price)) 0)
              pls          (pl-buf pl)
              nav          (or (first (nav-buf)) 0)
              navs         (nav-buf (+ nav pl))
              trade-groups (trade-group-buf)
              ffc-state    (if ffc (calc-ffc navs revalprices pls trade-groups) :online)] ;gross-profit gross-loss
          ;(log/trace date p position id) ;; for debug
          (when (= ffc-state :online)
            (let [trade-quantity (- position prev-position)]
              (when (not= trade-quantity 0)
                (let [order {:id id, :opc p, :oqt trade-quantity, :timestamp timestamp
                             :otp :stp, :oid :pos-chg}]
                  (when record (record-order order {:file record-file}))
                  (send-order order)))))))
     :recv-trade
       (fn [trade]
          (trade-group-buf trade))}))

