(ns witan.send.vis.output-count
  (:require [witan.send.chart :as wsc]
            [witan.send.series :as wss]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

(defn output-count [output-count-file]
  (csv-> output-count-file
         (map #(-> %
                   (update :calendar-year ->int)
                   (update :mean ->double)
                   (update :std-dev ->double)
                   (update :iqr ->double)
                   (update :min ->double)
                   (update :low-95pc-bound ->double)
                   (update :q1 ->double)
                   (update :median ->double)
                   (update :q3 ->double)
                   (update :high-95pc-bound ->double)
                   (update :max ->double)
                   (update :low-ci ->double)
                   (update :high-ci ->double)))))

(defn send-populations [title serie-specs]
  (transduce
   (mapcat wss/serie-and-legend-spec)
   (wsc/chart-spec-rf
    (wsc/base-chart-spec {:title title :legend "Data Sets"}))
   serie-specs))
