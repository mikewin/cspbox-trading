(ns cspbox.conv.utils.util
  (:require [clojure.set :as set]
            [clojure.tools.logging :as log]))

(defn conv2pair
  "convert content to pair"
  [content]
  (let [item (first content)]
    (cond
      (keyword? item) (apply sorted-map content)
      (coll? item) (into {} content)
      :else nil)))

(defn concat-two
  "concat a b,support  a or b not coll "
  [a b]
  (let [new-a (if (coll? a ) a (vector a))
        new-b (if (coll? b ) b (vector b))]
    (concat new-a new-b)))

(defn reduce-merge
  "'({:a :b} {:a :d}) -> {:a [:b :d}}"
  ([coll] (reduce-merge (comp vec concat-two) coll))
  ([f coll] (reduce #(merge-with f %1 %2) {} coll)))

(defn reduce-mergev
  "'([:a :b] [:a :d]) -> {:a [:b :d}}"
  [coll]
  (reduce-merge (map #(apply sorted-map %) coll)))

(defn merge-pair
  "'(:a :b :a :d ...) -> {:a [:b :d]..}"
  [content]
  (let [p  (partition 2 content)]
    (reduce-mergev p)))

(defn conv2pair-reduce
  "convert content to pair with combine"
  [content]
  (let [item (first content)]
    (cond
      (keyword? item) (merge-pair content)
      (vector? item)  (reduce-mergev content)
      (map? item)     (reduce-merge content)
      :else nil)))

(defn reduce-by-key
  "({:in :a} {:in :b} {:out :c}) -> (:a :b)"
  [k coll]
  (let [f   (fn [res contain]
              (let [item (k contain)]
                (if (coll? item)
                  (into res item)
                  (conj res item))))]
    (->> coll
       (reduce f #{})
       (remove nil?))))

(defn double-remove
  "remove #set1 #set2 coll"
  [filter1 filter2 coll]
  (let [pred (set/union filter1 filter2)]
    (remove pred coll)))


(defn comp-func*
  [f]
  (if (coll? f)
    (apply comp f)
    f))

(defmacro comp-func
  "combine function"
  [f]
  (if (coll? f)
    `(apply comp ~f)
    f))

(defn empty2nil
  "if empty return nil"
  [item]
  (if (and (coll? item) (empty? item))
    nil
    item))

(defn no-same?
  "if two coll have same item?"
  [coll1 coll2]
  (if (or coll1 coll2)
    (let [t1 (coll? coll1)
          t2 (coll? coll2)]
      (cond
        (and t1 t2)  (empty? (set/intersection (set coll1) (set coll2)))
        (not (or  t1 t2))  (not= coll1 coll2)
        (and (not t1) t2) (empty? (set/intersection (set [coll1]) (set coll2)))
        (and (not t2) t1) (empty? (set/intersection (set coll1) (set [coll2])))))
    true))

(defmacro dofitv
  "do f by the way fit the item type"
  [f item]
  (cond
   (nil? item) nil
   (coll? item) `(mapv ~f ~item)
   :else   `(~f ~item)))

(defn dofitv*
  "do f by the way fit the item type"
  [f item]
  (cond
   (nil? item) nil
   (coll? item) (mapv f item)
   :else   (f item)))

;(macroexpand-1 '(dofitv find-box '(:gen-price :sum-price :output)))
;(macroexpand-1 '(dofitv find-box :output))

(defn conv2set
  "convert item to set"
  [item]
  (when item
    (let [new-item (if (keyword? item) [item] item)]
     (set new-item))))

(defn conv2vec
  "convert item to vector"
  [item]
  (when item
    (if (coll? item)
      (vec item)
      [item])))

(defn fpartial
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with additional args + args ."
  ([f] f)
  ([f arg1]
   (fn
     ([] (f arg1))
     ([x] (f x arg1 ))
     ([x y] (f x y arg1))
     ([x y z] (f x y z arg1))
     ([x y z & args] (apply f x y z (concat args [arg1])))))
  ([f arg1 arg2]
   (fn
     ([] (f arg1 arg2))
     ([x] (f x arg1 arg2))
     ([x y] (f x y arg1 arg2))
     ([x y z] (f x y z arg1 arg2))
     ([x y z & args] (apply f x y z (concat args [arg1 arg2])))))
  ([f arg1 arg2 arg3]
   (fn
     ([] (f arg1 arg2 arg3))
     ([x] (f x arg1 arg2 arg3))
     ([x y] (f x y arg1 arg2 arg3))
     ([x y z] (f x y z arg1 arg2 arg3))
     ([x y z & args] (apply f x y z (concat args [arg1 arg2 arg3])))))
  ([f arg1 arg2 arg3 & more]
   (fn [& args] (apply f (concat more (concat [arg1 arg2 arg3] args))))))

(defn pickup
  "if coll only have one item , get it out"
  [coll]
  (if (and (coll? coll) (< (count coll) 2))
    (first coll)
    coll))

(defn remove-items
  "remove items from coll, items should be set, return a set or nil"
  [coll items]
  (cond
    (coll? coll) (empty2nil (set/difference (set coll) items))
    (keyword? coll)  (if (get items coll) nil #{coll})
     :else   (do (when coll (log/info "coll:" coll "type:" (type coll))) nil)))

(defn reverse-map
  "reverse map from {k2 v2,k1 v1} -> {v2 k2 , v1 k1}"
  [coll]
   (apply merge (map (fn[x]
                       (let [[k v] x]
                         {v k}))
                     coll)))
(defn filter-vals
  "filter a map with pred on value only"
  [pred coll]
  (let [new-pred (fn [res item] (let [[k v] item]
                                  (if (pred v)
                                    (conj res item)
                                    res)))]
    (reduce new-pred {} coll)))


(defn get-fn-var
  "get var (bind with fn) by name-space and fn name, when aot, resolve not work"
  [file fn-name]
    (let [n  (str "autorun.trade." file)]
      (require (symbol n))
    (clojure.lang.RT/var n fn-name)))
