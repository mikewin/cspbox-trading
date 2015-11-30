(ns cspbox.trading.ind.dsl
  (:refer-clojure :exclude [+ - * /])
  (:require [clojure.walk :refer [postwalk]]))
            ;[ka.base.indicator.lib.deploy :refer [add-ind* get-ind-set]]
            ;[ka.base.indicator.lib.partial :refer [fpartial]]
            ;[cinfix.core :refer [$= +lose-math-name+]]


(defn +
  "`+` sums vectors"
  ([a b] (cond (and (vector? a) (vector? b))
                 (vec (map clojure.core/+ a b))
               (and (vector? a) (number? b))
                 (vec (map #(clojure.core/+ % b) a))
               (and (vector? b) (number? a))
                 (vec (map #(clojure.core/+ % a) b))
               :else     (clojure.core/+ a b)))
  ([a b & as] (reduce + a (cons b as))))

(defn -
  "`-` sub vectors"
  ([a b] (cond (and (vector? a) (vector? b))
                 (vec (map clojure.core/- a b))
               (and (vector? a) (number? b))
                 (vec (map #(clojure.core/- % b) a))
               (and (vector? b) (number? a))
                 (vec (map #(clojure.core/- % a) b))
               :else     (clojure.core/- a b)))
  ([a b & as] (reduce - a (cons b as))))

(defn *
  "`*` mul vectors"
  ([a b] (cond (and (vector? a) (vector? b))
                 (vec (map clojure.core/* a b))
               (and (vector? a) (number? b))
                 (vec (map #(clojure.core/* % b) a))
               (and (vector? b) (number? a))
                 (vec (map #(clojure.core/* % a) b))
               :else     (clojure.core/* a b)))
  ([a b & as] (reduce * a (cons b as))))

(defn /
  "`/` div vectors"
  ([a b] (cond (and (vector? a) (vector? b))
                 (vec (map clojure.core// a b))
               (and (vector? a) (number? b))
                 (vec (map #(clojure.core// % b) a))
               :else     (clojure.core// a b)))
  ([a b & as] (reduce / a (cons b as))))


(defn check-it
  "check and deploy indicator :
  1. return should be a var
  2. indicator if not invoke , invoke it (use eval)"
  [exprs return]
  (let [exlist  (map (fn [item]  ;; partition from [var op expr] to [var expr]
                       (let [[a b c] item]
                         [(last a) (drop-last c)]))
                         exprs)
        vars    (reduce (fn [c t]   ;; find all var in [var expr]
                          (let [[v ex] t]
                            (conj c v))) #{} exlist)
        cont    (atom #{})
        onlyfn  (fn [item]             ;; find all symbol
                  (when-let [s (symbol? item)]
                    (when (Character/isLetter (first (name item)))
                      (swap! cont conj item))))
        symbols (reduce (fn [c t]   ;;todo
                          (let [[v ex] t]
                            (into c (postwalk onlyfn ex)))) #{} exlist)]
    (if (vector? return)
      (= return (map vars return))
      (vars return))))

;;inds     (clojure.set/intersection (get-ind-set) symbols)
;;        _        (map #(add-ind*  %) inds)
;(I> name [a b]
;   Y = (L + H)/2
;   R = Y + 3
;   M <- SMA a
;   N = M (R)
;   K = min ((R) , (N))
;   Q = (K - Y ) / b
;   J = N / b
;    [Q J])
;;todo param1 and cont if not have []
(defmacro I>
  "define indicator, return a fn "
  [name & form]
  (assert (> (count form) 0))
  (let [param1    (first form)
        bindings  (if (vector? param1) param1 nil)]
    (when-let [conts (next form)]
      (let [lastv     (last conts)
            end?      (vector? lastv)
            part-fn   (comp (partial partition  3 2) (partial partition-by #(or (= % '=) (= % '<-))))
            expr1     (part-fn conts)
            return    (if end?   ;; find the return var
                        (if (= (count lastv) 1) (first lastv) lastv)
                        (last (first (last expr1))))
            exprs     (if end? expr1   ;;else add end
                          (part-fn (conj (vec conts) return)))]
         (when (check-it exprs return)
           (let [parse (fn [item]
                         (let [[a b c] item
                               var  (last a)
                               op   (first b)
                               lst  (drop-last c)
                               ex   (if (= op '<-)
                                      `(fpartial ~@lst)
                                      `($= ~@lst))]
                       [var ex]))] ;; if return not= var , append to last expr and return last var
        `(defn ~name ~bindings
           (let [~@(mapcat parse exprs)]
             ~return))))))))
