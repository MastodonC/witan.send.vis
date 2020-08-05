(ns witan.send.vis.output-ay
  (:require [witan.send.domain.academic-years :as ay]
            [witan.send.chart :as wsc]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

(def output-ay-file "Output_AY.csv")

(def base-ay-comparison-serie-def
  {:historical-y-key :population})

(def base-ay-comparison-chart-def
  {:legend-label "Academic Years"
   :domain-key :academic-year
   :domain-values-lookup ay/ay-lookup
   :x-axis-label "Calendar Year" :x-tick-formatter int
   :y-axis-label "Population" :y-tick-formatter int
   :chartf wsc/zero-y-index})

(defn output-ay [output-ay-file]
  (csv-> output-ay-file
         (map #(-> %
                   (update :calendar-year ->int)
                   (update :academic-year ->int)
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

(def ay-titles-and-sets
  [["Early Years" ay/early-years]
   ["Key Stage 1" ay/key-stage-1]
   ["Key Stage 2" ay/key-stage-2]
   ["Key Stage 3" ay/key-stage-3]
   ["Key Stage 4" ay/key-stage-4]
   ["Key Stage 5" ay/key-stage-5]
   ["NCY 15+" ay/ncy-15+]
   ["All NCYs" (concat ay/early-years ay/key-stage-1 ay/key-stage-2 ay/key-stage-3 ay/key-stage-4 ay/key-stage-5 ay/ncy-15+)]])

(defn charts
  ([historical-data projection-data titles-and-sets]
   (let [domain-key :academic-year
         chart-base base-ay-comparison-chart-def
         serie-base base-ay-comparison-serie-def
         colors-and-points (wsc/domain-colors-and-points domain-key projection-data)]
     (into []
           (comp
            (map (fn [[title domain-values]]
                   (assoc
                    chart-base
                    :title title
                    :series
                    (into []
                          (map (fn [domain-value]
                                 (merge serie-base
                                        {:legend-label domain-value
                                         :color (-> domain-value colors-and-points :color)
                                         :shape (-> domain-value colors-and-points :point)
                                         :projection-data (into [] (filter #(= domain-value (domain-key %))) projection-data)
                                         :historical-data (into [] (filter #(= domain-value (domain-key %))) historical-data)})))
                          domain-values))))
            (map wsc/comparison-chart-and-table))
           titles-and-sets)))
  ([historical-data projection-data]
   (charts historical-data
           projection-data
           ay-titles-and-sets)))
