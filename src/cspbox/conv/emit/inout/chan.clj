(ns cspbox.conv.emit.inout.chan
  (:require [clojure.core.async :as async]))

(defn add-chan
  "add chan to component"
  [channels id chan & [mult-chan tap]]
  (swap! channels assoc id {:org chan :mult mult-chan :tap (or tap [])}))

(defn add-mult
  "add or generate mult chan for component"
  [channels id & [mult-chan]]
  (let [chan      (get-in @channels [id :org] (async/chan))
        mult-chan (or mult-chan (async/mult chan))]
    (add-chan channels id mult-chan nil)))

(defn add-tap
  "add tap pair to chan (id)"
  [channels id & [tap-ch]]
  (let [mult-ch  (get-in @channels [id :mult])
        tap-ch   (or tap-ch (async/chan))]
    (async/tap mult-ch tap-ch)
    (swap! channels update-in [id :tap] conj  tap-ch)
    tap-ch))

(defn get-chan
  "get chan if mult tap it else return org"
  [channels id]
  (assert (keyword? id) (str "get-or-add need a keyword id" id))
  (if-let [mult-chan (get-in @channels [:id :mult])]
    (add-tap channels id)
    (get-in @channels [id :org])))

(defn get-or-add
  "get chan by id, if not exist add it"
  [channels id & [org-id]]
  (when id
    (assert (keyword? id) (str "get-or-add need a keyword id" id))
    (when org-id
      (let [mult-outs (:mult-out-chan @channels)]
        (if (contains? mult-outs org-id)
          (add-mult channels id))))
    (let [chan (get-chan channels id)]
      (if chan
        chan
        (let [chan (async/chan)]
          (do
            (add-chan channels id chan)
            chan))))))
