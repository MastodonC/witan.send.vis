(ns witan.send.vis.ingest
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))


(defn ->int [x]
  (cond (int? x)
        x
        (double? x)
        (int x)
        (string? x)
        (int (Double/valueOf x))
        :else
        (throw (ex-info (format "Failed to parse supplied value '%s'" x)
                        {:value x}))))

(defn ->double [x]
  (cond (double? x)
        x
        (int? x)
        (double x)
        (string? x)
        (Double/valueOf x)
        :else
        (throw (ex-info (format "Failed to parse supplied value '%s'" x)
                        {:value x}))))

(defn csv->
  "Single arity assumes a header and zipmaps the header to the
  records. 2-arity assumes a header, zipmaps the header to the data,
  and allows the maps to be processed by the xf"
  ([file-name]
   (with-open [r (io/reader file-name)]
     (let [[header & data] (csv/read-csv r)
           header (into [] (map keyword) header)]
       (into []
             (map #(zipmap header %))
             data))))
  ([file-name xf]
   (with-open [r (io/reader file-name)]
     (let [[header & data] (csv/read-csv r)
           header (into [] (map keyword) header)]
       (into []
             (comp
              (map #(zipmap header %))
              xf)
             data)))))
