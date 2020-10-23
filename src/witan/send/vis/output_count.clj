(ns witan.send.vis.output-count
  (:require [witan.send.chart :as wsc]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

(def output-count-file "Output_Count.csv")

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

(def base-count-comparison-chart-def
  {:legend-label "Data Sets"
   :domain-key :population
   :x-axis-label "Calendar Year"
   :y-axis-label "Population"})

(def base-count-comparison-serie-def
  {:historical-y-key :population})

(defn chart [title historical-data projection-data]
  (let [chart-base base-count-comparison-chart-def
        serie-base base-count-comparison-serie-def]
    [(wsc/comparison-chart-and-table
      (assoc
       chart-base
       :title title
       :series
       [(assoc serie-base
               :legend-label "Population"
               :color wsc/blue
               :shape \A
               :historical-data historical-data
               :projection-data projection-data)]))]))

(comment

  (require '[cljplot.config :as cfg])
  (require '[witan.send.vis.ingest.transitions :as vit])
  
  (def plot-cfg
    (swap! cfg/configuration (fn [c]
                               (-> c
                                   (assoc-in [:legend :font] "Open Sans Bold")
                                   (assoc-in [:legend :font-size] 24)))))

  
  (def historic (vit/historical "../witan.send/data/demo/data/transitions.csv"))
  (def projection (output-count (str "../witan.send/data/demo/results/" output-count-file)))
  
  (def historic-counts (vit/total-population-per-calendar-year historic))

  (def count-charts (into []
                          (map wsc/comparison-chart-and-table)
                          (chart
                           "SEND Population"
                           historic-counts
                           projection)))

  (run!
   (partial wsc/save-chart-by-title "demo/charts/count-")
   count-charts)
  
  )
