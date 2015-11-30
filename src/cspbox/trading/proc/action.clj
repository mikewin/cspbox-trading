(ns cspbox.trading.proc.action)

(defn pos
  "set position, used in fsm do action, should be last one"
  [size]
  {:position size})

(defmacro feedback
  "send strategy state to indicator"
  [[agent-id value]]
  `(~'mail ~agent-id :set :state ~value))
