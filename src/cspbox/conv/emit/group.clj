(ns cspbox.conv.emit.group
  (:require [cspbox.conv.utils.util :refer [empty2nil comp-func* pickup remove-items]]
            [clojure.set :refer [difference]]
            [cspbox.conv.emit.sys.use :refer [state-get state-set]]))
            ;[clojure.tools.logging :as log]


;; todo f[] -> f[arg]
;; concovert proc group to fn
;; proc -> f [f1 f2 f3] [f1 {f2 [skey pkey flag]}... fn] [f1 f2 f3 #{f4 f5}] [f1 f2 f3 #{[f4 f5] f6}]

(defn gen-proc-fn
  "generate f from proc with skey pkey flag"
  [proc]
  (if (vector? proc)
    (mapv gen-proc-fn proc)
    (let [[f in out new-in new-out] ((juxt :entry :in :out :new-in :new-out) proc)
          ;_  (log/info "f:" f "in:" in "new-in" new-in "out:" out "new-out" new-out)
          skey    (remove-items out new-out)
          pkey    (remove-items in new-in)]
      ;(log/info "skey:" skey "pkey:" pkey)
      (if (or skey pkey)
        (let [skey (pickup skey)
              pkey (pickup pkey)
              flag (if (= (first in) pkey) true false)] ;; pkey in first place or nots
          {f [skey pkey flag]})
        f))))

(defn dispatch-proc-fn
  "generte fn struct from dispatch proc group"
  [dispatch-proc]
  (let [[head tail] dispatch-proc
        f-head  (if (vector? head)
                  (mapv gen-proc-fn head)
                  (gen-proc-fn head))
        f-tail  (mapv gen-proc-fn tail)]
    [f-head f-tail]))

;;helper functions
;; todo move to util.clj
(defn comp-state
  "comp function with add a parameter -> state"
  ([] identity)
  ([f] f)
  ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y) y))
       ([x y z] (f (g x y z) z))
       ([x y z & args] (f (apply g x y z args) (last args)))))
  ([f g & fs]
     (reduce comp-state (list* f g fs))))

(defn partial-get
  "generate function with get parameter from state"
  [f s-get get-key & [flag]]
  (if flag ;; true mean first arg
    (fn [value state]
      (f (s-get state get-key) value))
    (fn [value state]
      (f value (s-get state get-key)))))

(defn partial-set
  "generate function with set value to state"
  [f s-set set-key]
  (fn [in-value state]
    (s-set state set-key (f in-value state))))

(defn partial-state
  "generate function with state in and out"
  [f]
  (if (vector? f)
    (fn [value state]
      ((comp-func* f) value)))
    (fn [value state]
      (f value)))

(defn comb-fn-state
  "generate new fn with pkey or skey inside"
  [f & [skey pkey flag]]
  (let [f1  (if pkey (partial-get f state-get pkey flag) (partial-state f))]
    (if skey
      (partial-set f1 state-set skey)
      f1)))

(defn conv-fn-item
  "convert f to (f v state), {f []} to (f v state-get) or (state-set (f v))"
  [item]
  (if (map? item)
    (let [[k v] (first item) ]
      ;(log/info "item:" item "k:" k "v:" v)
      `(apply ~'comb-fn-state ~k ~v))
    `(~'partial-state ~item)))

(defn comp-pipe
  "convert [f1 f2 {f5 [skey pkey flag]} [f3 f4]] to one function"
  [f-coll]
  (let [pipe      (flatten f-coll)]
    `(~'comp-state ~@(reverse (mapv conv-fn-item pipe)))))

 (defn run-dispatch
   "generate a function to run/invoke dispatch fn"
   [f-head f-tail]
   (fn [value state]
       (let [intern-val (f-head value state)
             c-fn       (fn [f] (f intern-val state))]
           (pmap c-fn f-tail))))

(defn comp-dispatch
  "convert [f [f1 ... f2]] to a function"
  [f-coll]
  (let [[head tail]  f-coll
        ;_  (log/info "head:" head "tail:" tail)
        conv-fn  (fn [x] (if (vector? x) (comp-pipe x) (conv-fn-item x)))
        f-head   (conv-fn head)
        f-tail   (mapv conv-fn tail)]
    `(~'run-dispatch ~f-head ~f-tail)))




