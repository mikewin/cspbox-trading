(ns cspbox.trading.ecn.web.wmcloud.data
  (:require [clojure.data.csv :as csv]
            [cspbox.tools.csv :refer  [write-csv-file write-vector-csv]]
            [semantic-csv.core :as sc :refer [->float ->long ->int]]
            [cspbox.tools.config :refer [select-read-conf]]
            [clj-http.client :as client]
            [cspbox.tools.date :refer [prev-days]]
            [clojure.tools.logging :as log]))

;; helper function
(defn send-request
  "send request to wmcloud with auth token"
  [conf req]
  (let [base   (:base conf)
        token  (:token conf)
        _      (assert (> (count token) 10) "please add token to config file -- tonglian.edn")
        url    (str base req)
        header ((client/wrap-oauth identity) {:oauth-token token})]
    (client/get url (merge {:as :auto} header)))) ;;:debug true


(defn get-data
  "require data by post url, then parse the result"
  [conf url & [return-count]]
  (let [res       (send-request conf url)
        status    (:status res)
        data      (:body res)]
    (when (and (= status 200) (not (and (< (count data) 54) (or (re-find #"-403" data) (re-find #"404" data)
                                                              (re-find #"-1" data)))))
      (let [csv-data  (csv/read-csv data)]
        (if return-count
          (take return-count csv-data)
          csv-data)))))
      ;(log/info "error?" res)

(defn proc-data
  "get and save data"
  [conf url & [file-name]]
  (if file-name
    (when-let [data (get-data conf url)]
      (let [base-path (:base-path conf)
            file   (str base-path file-name)]
        (write-csv-file data file)))
    (get-data conf url)))

;; wrap api

(defn get-sec-id
  "see wmcloud api"
  [conf id & [{:keys [todo]}]]
  (let [sec-id (:sec-id conf)
        url (apply format sec-id (concat [id] (repeat 4 "")))] ;; todo add more parameters
    (get-data conf url)))


(defn get-hk-id
  ""
  [conf id [{:keys [todo]}]]
  (let [hk-equ (:hk-equ conf)
        url (apply format hk-equ (concat [id] (repeat 3 "")))] ;; todo add more parameters
    (get-data conf url)))


(defn get-history-day
  "get history day data SH. or SZ"
  [conf id & [{:keys [file]}]]
  (let [mktequd (:mktequd conf)
        url (apply format mktequd (concat [id] (repeat 5 "")))] ;; todo add more parameters
    (proc-data conf url file)))


(defn get-hk-history-day
  "get HK history day data"
  [conf id & [{:keys [file]}]]
  (let [mkthkequd (:mkthkequd conf)
        url (apply format mkthkequd (concat [id] (repeat 5 "")))] ;; todo add more parameters
    (proc-data conf url file)))


(defn get-rt-bar
  "get readtime bar data"
  [conf sec-id & [{:keys [file start]}]]
  (let [bar-rt-intra-day (:bar-rt-intra-day conf)
        start (or start "")
        url   (apply format bar-rt-intra-day (concat [sec-id start] (repeat 2 ""))) ;; todo add more parameters
        data  (proc-data conf url file)]
    (->> data
         sc/mappify
         (sc/cast-with {:openPrice ->float :closePrice ->float :highPrice ->float :lowPrice ->float :totalVolume ->long})
        doall)))


(defn get-hist-min-bar
  "get history minute bar "
  [conf sec-id date &[file]]
  (let [bar-hist-one-day (:bar-hist-one-day conf)
        url (apply format bar-hist-one-day (concat [sec-id date] (repeat 3 "")))]
    (proc-data conf url file)))


(defn read-config
  "read config from file or input"
  [&[conf]]
  (let [conf (or conf (select-read-conf "tonglian.edn"))]
    conf))


(defn get-days-min-bar
  "save many days minute in file"
  [sec-id num-days & [conf file]]
  (when (and sec-id num-days)
    (let [conf  (or conf (read-config))
          days  (prev-days num-days)
          file  (or file (str (:base-path conf) sec-id ".min.csv"))]
      (doseq [day days]
        (let [data (get-hist-min-bar conf sec-id day)]
          (when data
            (->> data
                 sc/mappify
                 reverse
                 (write-vector-csv {:file file :append true :header [:dataDate :barTime :openPrice
                                     :closePrice :highPrice :lowPrice :totalVolume :totalValue]})
                 doall)))))))


;; test
(comment
  (get-rt-bar (read-config) "00700.XHKG" {:start "14:51"})
  (get-hist-min-bar (read-config) "00700.XHKG" "20150623")
  (get-days-min-bar "00700.XHKG" 200)
(get-hist-min-bar (read-config) "00700.XHKG" "20140616")
  )
