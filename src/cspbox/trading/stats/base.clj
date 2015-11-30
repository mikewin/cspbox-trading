(ns cspbox.trading.stats.base)

(def epsilon 0.000001)

(defn sum-pl
  "helper for calc gross pl"
  [trade-groups]
  (let [trade-group-pl  (map :pl trade-groups)
        gross-list   (filter pos? trade-group-pl)
        loss-list    (filter neg? trade-group-pl)]
    (mapv (partial apply +) [gross-list loss-list])))


(defn classify-all [objects predicates]
  "Iterate of a set of predicates and a set of objects"
  (let [length   (count predicates)
        pred-fn  (fn [obj]
                   (= (count (filter (fn [p-fn] (p-fn obj)) predicates)) length))]
    (reduce (fn [res obj]
              (if (pred-fn obj)
                (conj res obj)
                res)) [] objects)))

;(classify-all [1 2 3 5 6 7 8] [even? #(> %1 3)])

(defn classify [objects predicates]
  "Iterate of a set of predicates and a set of objects and create a set of bins
  containing the objects specified by the predicates."
  (reduce (fn [res pred]
            (let [obj  (filter pred objects)]
              (conj res obj))) [] predicates))

;(classify [1 2 3 5 6 7 8] [even? #(> %1 4)])
