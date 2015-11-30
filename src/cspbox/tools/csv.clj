(ns cspbox.tools.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [semantic-csv.core :as sc :refer [->float ->long]]
            [cspbox.tools.markdown :as md]
            [cspbox.tools.write :refer [write-file]]))

;;function for csv

(defn take-numbers
  "wrap take"
  [number coll]
  (if number
    (take number coll)
    coll))

(defn change-keyword
  "change the column name"
  [conv-map]
  (if (and conv-map (map? conv-map))
    (fn [data]
      (map #(clojure.set/rename-keys % conv-map) data))
    (fn [data] data)))

(defn read-parse-cvs
  "use semantic-csv to convert cvs data to table"
  [file number & [conv-map]]
  (let [conv-fn (change-keyword conv-map)]
    (with-open [in-file (io/reader file)]
      (->> (csv/read-csv in-file)
           (take-numbers number)
           sc/mappify
           conv-fn
           (sc/cast-with {:open ->float :close ->float :high ->float :low ->float :volume ->long})
           doall))))


(defn read-local-csv
  "read csv file"
  [file-name]
  (when file-name
    (with-open [in-file (io/reader file-name)]
      (doall
        (csv/read-csv in-file)))))


(defn write-csv-file
  "write date to csv file"
  [data & [file]]
  (let [file (or file "out-file.csv")]
    (with-open [out-file (io/writer file)]
      (csv/write-csv out-file data))))


(defn write-vector-csv
  "use semantic-cvs to write a vector map to cvs"
  [{:keys [file append header]} data]
  (when file
    (let [append? (atom (or append false))
          csv-file  (io/file file)]
      (when-not (.exists csv-file)
        (.. csv-file getParentFile mkdirs)
        (. csv-file createNewFile)
        (reset! append? false))
      (with-open [out-file (io/writer csv-file :append @append?)]
        (->> data
             ;(cast-with {:this #(-> % float str)})
             (sc/vectorize {:header header :prepend-header (not @append?)})
             (csv/write-csv out-file))))))


(defn conv-csv-md
  "convert a csv file to markdown format"
  [csv-file & [md-file]]
  (let [md-file   (or md-file (str csv-file ".md"))
        csv-data  (read-local-csv csv-file)
        header    (first csv-data)
        body      (next csv-data)
        md-data  (md/md-table header body)]
    (write-file md-file nil md-data)))


;;test
(comment
  (let [file "e:/work/autorun/cspbox/draft/log/test.csv"
       data [{:date "2001"  :this "a" :that "b" }
             {:date "2002" :this "x" :that "y"}]]
   (write-vector-csv {:file file :append false :header nil}  data))

 (let [file "e:/work/autorun/cspbox/draft/log/test.csv"
       data [{:date "2003"  :this "a" :that "b" :to 12}
             {:date "2004" :this "e" :that "y"}]]
   (write-vector-csv {:file file :append true :header [:date :this :that]}  data))
(let [file "e:/work/autorun/cspbox/draft/log/test.csv"
       data [{:date "2001"  :this "a" :that "b" }
             {:date "2002" :this "x" :that "y"}]]
   (write-vector-csv data {:file file :append true :header  [:date :this :that]} data))

 (let [file "e:/work/autorun/cspbox/draft/log/test.csv"
       data [{:date "2003"  :this "a" :that "b" :to 12}
             {:date "2004" :this "e" :that "y"}]]
   (write-vector-csv data {:file file :append true :header [:date :this :that]} data))
(let [data [{:this "a" :that "b"}
              {:this "x" :that "y"}]]
          (sc/vectorize {
                        :header nil ;[:that :this]
                      :prepend-header  true}
                     data))
  (conv-csv-md "e:/work/autorun/cspbox/draft/log/test.csv")
  )
