(ns cspbox.tools.date
  (:require [clj-time.core :as t]
            [clj-time.coerce :as c]
            [clj-time.format :as f]
            [clj-time.periodic :as p]
            [clj-time.predicates :as pr]))

(def seconds-per-hour 3600)

(def seconds-per-day (* 24 60 60))

(defn current-timestamp
  "use today now as timestamp"
  []
  (-> (t/now)
      c/to-long))

(defn date-timestamp
  "convert date from string to timestamp"
  [date & [date-format]]
  (let [d (if (string? date)
            (let [date-format (or date-format "YYYY-MM-dd")]
              (f/parse (f/formatter date-format) date))
            date)]
    (c/to-long d)))

(defn timestamp-date
  "convert timestamp to date"
  [timestamp & [date-format]]
  (let [date-format (or date-format "YYYY-MM-dd")]
    (f/unparse (f/formatter date-format) (c/from-long timestamp))))


(defn timestamp-difference
  [end start & [ms?]]
  (if ms?
    (- end start)
    (/ (- end start) 1000)))


(defn sec-of
  [timestamp]
  (let [date (c/from-long timestamp)
        start (t/today-at 9 00)]
    (t/in-seconds (t/interval start date))))

(defn timestamp-day-of-week
  [timestamp]
  (let [date (c/from-long timestamp)]
    (t/day-of-week date)))

(defn parse-date
  "parse 2015-11-27T09:31:17 to date object. if have ms use :date-hour-minute-second-ms "
  [date &[formatters]]
  (let [custom-formatter (f/formatters (or formatters :date-hour-minute-second))]
    (f/parse custom-formatter date)))

(defn prev-days
  "get date from today"
  [num-days & [formatter]]
  (let [custom-formatter (f/formatter (or formatter "yyyyMMdd"))
        days  (take num-days (p/periodic-seq (t/now) (t/days -1)))]
    (mapv #(f/unparse custom-formatter %)  days)))


;; test
(comment
(parse-date "2015-11-27T11:35:54")
  (prev-days 5)
  )

