(ns cspbox.conv.emit.sys.use)

(defn lookup-state [state-fn-map the-state]
  (if-let [a-state-fn (get state-fn-map the-state)]
    a-state-fn
    (throw (RuntimeException. (str "Could not find the state \"" the-state)) "\"")))

(defn fsm-run
  "process a single event with an incremental finite state machine Returns a map with the following keys:"
  [fsm out-chan data]
  {:pre [(map? fsm) (contains? fsm :fsm)]} ;; only valid for incremental fsms
  ((:fsm fsm) out-chan data))

;; todo support multi-id , by-now  only one support
(defn state-get
  "get value from state by id"
  [state id]
  (when state
    (if (coll? id)
      ((first id) @state)
      (id @state))))

(defn state-set
  "set value to state by id, return value"
  [state id value]
  (when state
    (if (coll? id)
      (swap! state assoc (first id) value)
      (swap! state assoc id value)))
    value)
