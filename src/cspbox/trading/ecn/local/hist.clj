(ns cspbox.trading.ecn.local.hist
  (:require [cspbox.runtime.tools.csv :as csv]
            [cspbox.runtime.tools.config :refer [select-read-conf]]))

;; read data from csv file and emit as data source

;; helper function
(defn combine-time
  [data]
  (let [[date bar-time] ((juxt :date :barTime) data)]
    (assoc data :date (str date "T"bar-time))))


(defn symobl-to-filename
  "symbol-id is six digital number"
  [conf symbol-id & [kind]]
  (if (= kind :min)
    (let [dir  (:history-min-directory conf)]
      (str dir symbol-id (:file-postfix conf) ".csv"))
    (let [history-data-directory (:history-data-directory conf)
          file-name-prefix  (:file-name-prefix conf)]
      (str history-data-directory file-name-prefix symbol-id ".csv"))))


(defn local-data-config
  "load config file for local data"
  [&[config]]
  (let [config (or config (select-read-conf "local_data.edn"))]
    config))


(defn read-local-hist-day
  "read local history day data cvs file"
  [symbol-id & [{:keys [parse number]}]]
  (let [conf  (local-data-config)
        file (symobl-to-filename conf symbol-id)]
    (if parse
      (csv/read-parse-cvs file number)
      (csv/read-local-csv file))))


(defn read-local-hist-min
  "read local history minute data cvs file"
  [symbol-id numbers]
  (let [conf (local-data-config)
        file (symobl-to-filename conf symbol-id :min)]
    (->> (csv/read-parse-cvs file numbers {:dataDate :date :openPrice :open, :closePrice :close,
                                      :highPrice :high, :lowPrice :low, :totalVolume :volume})
         (map #(combine-time %)))))


;; test

(comment
(def data (read-local-hist "000301"))
(read-local-hist "000301" {:parse true :number 20})
(def pick (select-bar-data (:data-format (local-data-config))))
(read-local-hist-min "00700.XHKG" 300)
(def out  (->> data
              (take 101)
              next
              reverse
              (map pick)))

)

