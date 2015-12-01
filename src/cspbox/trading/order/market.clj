(ns cspbox.trading.order.market
  (:require [clj-time.core :as t]
            [clj-time.coerce :as c]
            [cspbox.runtime.tools.date :as d]))

(def market-open  "9:30")
(def market-close "16:00") ;; todo use config for different market

(defn market-hours
  "market open time start-end" ;todo read from config
  []
  (let [start (t/today-at 9 30)
        end   (t/today-at 16 00)]
    (mapv c/to-long [start end])))


(defn market-closed-p
  "Predicate to determine if the market is in after-hours trading for the given event.
  The market is indicated as closed 15 minutes before the end of the trading session to give the
  agent time to close any open positions."
  [timestamp]
  (let [[start end ] (market-hours)]
    (or (contains? #{6 7} (d/timestamp-day-of-week timestamp))
        (not (<= start timestamp end)))))


(defn send-order
  "Create an order.
  SEND-ORDER - Three places in the event consumption cycle by the agent where orders can
  be emitted:
 1. In the FSM transitions ACTUATOR functions. Passive orders should be emitted
    when transitions happen.
 2. In the UPDATE :AFTER method. The immediate change in the agent's desired market position
    is being generated at the level of the FSM transition, and the resulting aggressive order
    can be dealt with after the FSM is processed and before post-processing.
 3. In the aggregator.  When several agents are trading in the same security it is also possible
    to route all the agents' positions into an aggregator so that the slippage is minimized. "
  [{:keys [timestamp oid security otp oqt opc]}]
   {:timestamp timestamp
    :value oid
    :security security
    :type otp
    :quantity oqt
    :price opc
    :algo-type (case otp
                 (:stp :ioc :moc :moo) :aggressive
                 :lmt :passive)})

(defn change-order
  [{:keys [new-opc new-oqt new-otp old-oid]}]
  ;; todo
  (assert nil "no implment now"))

(defn cancel-order
  [old-oid]
  ;; todo
  (assert nil "no implment now"))

(defn signum
  [n]
  (cond (> n 0) 1
        (< n 0) -1
        :else  0))


(defn simple-slippage
  "use fix rate"
  [size ordertype]
  (* 0.0015 (signum size)))


(defn adjust-price
  "adjust price with slippage function and a percent increasement"
  [p slippagefunc size ordertype]
  (* p (inc (if slippagefunc
                (slippagefunc  size ordertype)
                0))))


