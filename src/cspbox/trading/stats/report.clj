(ns cspbox.trading.stats.report
  (:require [cspbox.runtime.tools.config :refer [select-read-conf]]
            [cspbox.runtime.tools.markdown :refer [md-header md-list md-table]]
            [cspbox.runtime.tools.write :refer [write-file]]))

;; write to md file then render to html or pdf
;; later support write to pdf


(defn report-header
  "generate Title of report"
  [content]
  (md-header content))

(defn report-list
  "generate a list"
  [list-cont]
  (md-list list-cont))

(defn report-table
  "generate a data table"
  [table-head table-body]
  (md-table table-head table-body))

(defn write-report
  "write report to file"
  [name header body]
  (let [conf (select-read-conf "path.edn")
        base-report-path (or (:base-report-path conf) "./")
        file-name (str base-report-path name)]
    (write-file file-name header body)))

(defn trade-stats-report
  "generate report from trade-stats"
  [trade-stats]
  (assert (map? trade-stats) "only accept map for report")
  (let [table-head ["item" "value"]
        k  (keys trade-stats)
        new-k  (mapv name k)
        new-map  (zipmap new-k (vals trade-stats))
        table-body (vec new-map)]
    (report-table table-head table-body)))


;; test
(comment
(def t     {
              :timestamp          10
              :percent-profitable 20
              :win-to-loss        30
              :average-logret     40
              :total-pl           50
              :average-duration   60
              :pos-pl             70
              :neg-pl             80
              :profit-factor    9 })


(let [header "Trade statistics"]
  (write-report "test.md" (report-header header) (trade-stats-report t)))
)
