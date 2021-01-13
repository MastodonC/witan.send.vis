(ns witan.send.vis.output-setting-cost
  (:require [witan.send.chart :as wsc]
            [witan.send.vis :as vis]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

(def output-setting-cost-file "Output_Setting_Cost.csv")

(defn base-setting-chart-def [domain-values-lookup]
  {:legend-label "Settings"
   :domain-key :setting
   :domain-values-lookup domain-values-lookup
   :x-axis-label "Calendar Year" :x-tick-formatter int
   :y-axis-label "Millions (£)" :y-tick-formatter vis/millions-formatter
   :chartf wsc/zero-y-index})

(def base-setting-serie-def {:historical-y-key :population})

(defn charts
  [projection-data titles-and-sets]
  (let [domain-key :setting]
    (wsc/domain-charts {:domain-key domain-key
                        :chart-base-def (base-setting-chart-def {})
                        :serie-base-def base-setting-serie-def
                        :colors-and-points (wsc/domain-colors-and-points domain-key projection-data)
                        :projection-data projection-data}
                       titles-and-sets)))

(defn output-setting-cost [output-setting-cost-file]
  (csv-> output-setting-cost-file
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
