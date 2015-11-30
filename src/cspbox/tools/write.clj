(ns cspbox.tools.write
  (:require [clojure.java.io :as io]))

;;doc :  https://docs.oracle.com/javase/7/docs/api/java/io/File.htm
;;  http://docs.oracle.com/javase/7/docs/api/java/io/File.html

(defn write-item
  [w writer item]
  (when item
    (cond (coll? item)   (mapv writer item)
          (string? item) (.write w item)
      :else (writer item))))

(defn write-file
  "create and write header and content to file"
  [file header content & [{:keys [append output-fn]}]]
  (when file
    (let [out-file  (io/file file)
          output-fn (or output-fn (fn [w content] (.write w content)))
          append    (or append false)]
      (when-not (.exists out-file)
        (.. out-file getParentFile mkdirs)
        (. out-file createNewFile))
      (with-open [w (io/writer out-file :append append)]
        (let [writer (partial output-fn w)]
          (mapv #(write-item w writer %) [header content]))))))

(defn delete-file
  "delete a file"
  [file]
  (when file
    (let [out-file (io/file file)]
      (.delete out-file))))
