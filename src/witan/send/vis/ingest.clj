(ns witan.send.vis.ingest
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))


(defn ->number [x]
  (try
    (.parse (java.text.NumberFormat/getInstance) x)
    (catch Exception e
      (throw (ex-info (format "Failed to parse supplied value '%s'" x)
                      {:value x}
                      e)))))

(defn ->int [x]
  (int (->number x)))

(defn ->double [x]
  (double (->number x)))

(defn maybe-number [x]
  (try
    (->number x)
    (catch Exception _ x)))

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
