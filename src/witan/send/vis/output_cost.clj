(ns witan.send.vis.output-cost
  (:require [witan.send.chart :as wsc]
            [witan.send.series :as wss]
            [witan.send.vis :as vis]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

(def output-cost-file "Output_Cost.csv")

(defn output-cost [output-cost-file]
  (csv-> output-cost-file
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

(def base-cost-comparison-chart-def
  {:legend-label "Data Sets"
   :domain-key :cost
   :x-axis-label "Calendar Year"
   :y-tick-formatter vis/millions-formatter
   :y-axis-label "Cost (Millions £)"})

(defn chart [title projection-data]
  (let [chart-base base-cost-comparison-chart-def]
    [(assoc
      chart-base
      :title title
      :series
      [{:legend-label "2020 Baseline"
        :color wsc/blue
        :shape \A
        :projection-data projection-data}])]))
