(ns cspbox.trading.ecn.web.grab.data
  (:require [clj-http.client :as client]
            [clojure.string :as str]
            [taoensso.timbre :refer [error info]]
            [clojure.edn :as edn]))

(defn parse-number
  "Reads number from a string. Returns nil if not a number."
  [s]
  (if (re-find #"^-?\d+\.?\d*$" s)
    (edn/read-string s)))

(defn gen-rt-url
  "build realtime stock data url take care hk"
  [base id]
  (let [new-id   (if (re-find #"hk" id) (str "rt_" id) id)]
    (str base new-id)))

(defn gen-multi-url
  "build url with multi symbols"
  [base symbols]
  (let [hk-id  (filter #(re-find #"hk" %) symbols)
        cn-id  (filter #(nil? (re-find #"hk" %)) symbols)
        hk-url (if (empty? hk-id) nil (str base (apply str (interpose "," (map #(str "rt_" %) hk-id)))))
        cn-url (if (empty? cn-id) nil (str base (apply str (interpose "," cn-id))))]
    ;(info "hk:" hk-url " cn:" cn-url)
    [hk-url cn-url]))

(defn parse-hk-rt
  "parse and filter hk realtime data from web site"
  [content last-time & [full?]]
  (let [s      (str/split content #",")
        [d t1] (take-last 2 s)
        t      (first (str/split t1 #"\""))
        id     (re-find #"hk\d+" (first s))
        lt     (get last-time id)]
        ;(info "id:" id " lt=" lt " t=" t " lastt:" last-time)
    (when-not (= lt t)
      (let [tick  (remove nil? (map parse-number (nnext s)))
            [open close high low cur change rate _ _ account vol] tick
            ratio (/ (Math/round (* (/ rate 100) 10000)) 10000.0)]
        (if full?
          [id open close high low cur change ratio account vol d t]
          [id cur change ratio vol d t])))))

(defn parse-cn-rt
  "parse and filter china realtime data from web site"
  [content last-time & [full?]]
  (let [s    (str/split content #",")
        [d t _] (take-last 3 s)
        id     (re-find #"s[a-z]\d+" (first s))
        lt     (get last-time id)]
    ;(prn "s=" s " d=" d " t=" t )
    (when-not (= lt t)
      (let [tick (remove nil? (map parse-number (next s)))
            [open close cur high low _ _ vol account]  tick
            change  (/ (Math/round (*  (- cur close) 100)) 100.0)
            rate    (/ (Math/round (* (/ change close) 10000)) 10000.0)]
        (if full?
          [id open close high low cur change rate account vol d t]
          [id cur change rate vol d t])))))

(defn parse-multi-rt
  "parse and filter hk realtime data from web site"
  [parse-fn content  last-time & [full?]]
  (let [items (str/split content #";")
        conts (filter #(> (count %) 8) items)
        res   (map #(parse-fn % last-time full?) conts)]
    (remove nil? res)))

(defn config-sina-rt-grab
  "fill the datasource state"
  [& [options]]
  (let [{:keys [base interval]} options
        interval (or interval 5000)   ; ms  if 0 -> not loop/schedule
        gtime    last
        getid    first
        base     (or base "http://hq.sinajs.cn/list=")
        acquire  client/get
        url      (partial gen-multi-url base) ;; todo support multi-symbols
        parse-fn {:hk (partial parse-multi-rt parse-hk-rt) :cn (partial parse-multi-rt parse-cn-rt)}]
    {:interval interval :acquire acquire :url url :parse-fn parse-fn :getid getid :gtime gtime}))

(defn grab-tick-data
  "generate function to get realtime tick data"
  [symbols &[options]]
  (let [{:keys [interval acquire url parse-fn getid gtime]} (config-sina-rt-grab options)
        [hk-url cn-url] (url symbols)]
    (fn []
      (let [[hk-parse cn-parse] ((juxt :hk :cn) parse-fn)
            res-hk (if hk-url
                      (-> hk-url acquire :body (hk-parse nil)) nil)
            res-cn (if cn-url
                      (-> cn-url acquire :body (cn-parse nil)) nil)]
        {:hk res-hk :cn res-cn}))))

(defn sina-realtime-tick-data
  "generate function to get realtime tick data for sys DSL"
  [symbols &[options]]
  (let [{:keys [interval acquire url parse-fn getid gtime]} (config-sina-rt-grab options)
        [hk-url cn-url]     (url symbols)
        [hk-parse cn-parse] ((juxt :hk :cn) parse-fn)
        web-url             (or hk-url cn-url)
        parse               (if hk-url hk-parse  cn-parse)]
    (fn []
      (let [[security price change percent volume date1 date2]  (first (-> web-url acquire :body (parse nil)))]
        {:date (str date1 "T" date2) :price price :security security :volume volume}))))

(def grab-3 (sina-realtime-tick-data ["hk03968"]))
(grab-3)
(def grab-1 (sina-realtime-tick-data ["sz002202"]))
(grab-1)

;; test
(comment
(def grab-one (grab-tick-data ["hk00700"]))
(def grab-2 (grab-tick-data ["sh600480"]))
  ;{:hk nil, :cn (["sh600480" 20.28 -0.14 -0.0069 8228519 "2015-11-27" "10:48:38"])}
(grab-2)
(grab-one)
  ;{:hk (["hk00700" 154.7 -1.0 -0.0064 2765648 "2015/11/27" "10:48:52"]), :cn nil}
  )
