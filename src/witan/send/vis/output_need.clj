(ns witan.send.vis.output-need
  (:require [witan.send.chart :as wsc]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

(def output-need-file "Output_Need.csv")

(defn base-need-chart-def [domain-values-lookup]
  {:legend-label "Needs"
   :domain-key :need
   :domain-values-lookup domain-values-lookup
   :x-axis-label "Calendar Year" :x-tick-formatter int
   :y-axis-label "Population" :y-tick-formatter int
   :chartf wsc/zero-y-index})

(def base-need-serie-def {:historical-y-key :population})

(defn charts
  ([needs-lookup historical-data projection-data titles-and-sets]
   (let [domain-key :need]
     (wsc/domain-charts {:domain-key domain-key
                         :chart-base-def (base-need-chart-def needs-lookup)
                         :serie-base-def base-need-serie-def
                         :colors-and-points (wsc/domain-colors-and-points domain-key projection-data)
                         :historical-data historical-data
                         :projection-data projection-data}
                        titles-and-sets))))

(defn output-need [output-need-file]
  (csv-> output-need-file
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
