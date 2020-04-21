(ns witan.send.vis.output-ay
  (:require [witan.send.chart :as wsc]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

(def output-ay-file "Output_AY.csv")

(def ay-lookup
  (sorted-map -5 -5
              -4 -4
              -3 -3
              -2 -2
              -1 -1
              0 0
              1 1
              2 2
              3 3
              4 4
              5 5
              6 6
              7 7
              8 8
              9 9
              10 10
              11 11
              12 12
              13 13
              14 14
              15 15
              16 16
              17 17
              18 18
              19 19
              20 20
              21 21
              22 22
              23 23
              24 24
              25 25))

(def base-ay-comparison-serie-def
  {:historical-y-key :population})

(def base-ay-comparison-chart-def
  {:legend-label "Academic Years"
   :domain-key :academic-year
   :domain-values-lookup ay-lookup
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

(defn inclusive-range [beginning end]
  (range beginning (inc end)))

(def early-years
  (into (sorted-set) (inclusive-range -5 0)))

(def key-stage-1
  (into (sorted-set) (inclusive-range 1 2)))

(def key-stage-2
  (into (sorted-set) (inclusive-range 3 6)))

(def key-stage-3
  (into (sorted-set) (inclusive-range 7 9)))

(def key-stage-4
  (into (sorted-set) (inclusive-range 10 11)))

(def key-stage-5
  (into (sorted-set) (inclusive-range 12 14)))

(def ncy-15+
  (into (sorted-set) (inclusive-range 15 25)))

(defn national-curriculum-stage [y]
  (cond
    (early-years y) :early-years
    (key-stage-1 y) :ks-1
    (key-stage-2 y) :ks-2
    (key-stage-3 y) :ks-3
    (key-stage-4 y) :ks-4
    (key-stage-5 y) :ks-5
    (ncy-15+ y) :further-education))

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
           [["Early Years" early-years]
            ["Key Stage 1" key-stage-1]
            ["Key Stage 2" key-stage-2]
            ["Key Stage 3" key-stage-3]
            ["Key Stage 4" key-stage-4]
            ["Key Stage 5" key-stage-5]
            ["NCY 15+" ncy-15+]
            ["All NCYs" (concat early-years key-stage-1 key-stage-2 key-stage-3 key-stage-4 key-stage-5 ncy-15+)]])))


