(ns cspbox.conv.emit.inout.inject
  (:require [cspbox.conv.emit.inout.chan :refer [get-or-add]]
            [cspbox.conv.utils.util :refer [pickup  get-fn-var]]
            [cspbox.tools.config :refer [select-read-conf]]
            [clojure.core.async :as async]))

;; helper
(defn rt-add-mailbox
  "when runtime, add mailbox fn to globe mailbox map"
  [g-mail f-mail comp-id mailgroup-id]
  (when f-mail
    (swap! g-mail assoc comp-id f-mail)
        (when mailgroup-id
          (swap! g-mail update mailgroup-id conj f-mail))))

(defn handle-multi-in
  "if mult in, generate merge chan or use alt! for multi in-chan"
  [channels in-uid in]
  (let [chans (mapv (partial get-or-add channels) in-uid)]
    (if (= (count in) 1)
      (let [merge-chan  (async/merge chans)] ;;merge all in-chan
        merge-chan)
      (let [chan-key (zipmap chans in)]
        [chans chan-key]))))

(defn call-fn
  "helper function: invoke fn already generate,in out should be one or coll base on count"
  [channels state g-mail [f in-uid out-uid in out id fn-type fn-id]]
  (let [in-chan  (cond (keyword? in-uid) (get-or-add channels in-uid)
                       (coll? in-uid)  (when-not (empty? in) (handle-multi-in channels in-uid in))
                       :else nil)
        out-chan (cond (keyword? out-uid) (get-or-add channels out-uid out)
                       (coll? out-uid) (when-not (empty? out) (mapv (partial get-or-add channels) out-uid out))
                       :else  nil)]
    ;(log/info "out-chan:" out-chan)
     (case fn-type
       :mailbox (rt-add-mailbox g-mail (f channels state) id fn-id)
       :stra    (f channels state out-chan) ;; need embedded out-chan for fsm output
       (f in-chan out-chan state))))

(defn mult-send
  "helper for multi out-chan"
  [out-chan value]
  (when (and value out-chan)
    (async/>! out-chan value)))

;; replace the fn by config
(defn replace-fn
  [info]
  (try
    (let [{:keys [from-ns from-fn to-ns to-fn]}  info
          from-var  (get-fn-var from-ns from-fn)
          to-var    (get-fn-var to-ns to-fn)]
      (alter-var-root  from-var (fn[f] to-var)))
    (catch Exception e (str "replace fn exception: " (.getMessage e)))))

(defn replace-by-config
  "replace current fn by new one, name read from config "
  [config-filename]
  (let [conf (select-read-conf config-filename)]
    (when (coll? conf)
      (mapv replace-fn conf))))


