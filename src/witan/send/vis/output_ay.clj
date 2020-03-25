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
  {:x-axis-label "Calendar Year" :x-axis-formatter int
   :y-axis-label "Population" :y-axis-formatter int
   :domain-key :academic-year
   :domain-values-lookup ay-lookup
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


(defn early-years? [y]
  (<= -5 y 0))

(defn key-stage-1? [y]
  (<= 1 y 2))

(defn key-stage-2? [y]
  (<= 3 y 6))

(defn key-stage-3? [y]
  (<= 7 y 9))

(defn key-stage-4? [y]
  (<= 10 y 11))

(defn further-education? [y]
  (<= 12 y))

(defn national-curriculum-stage [y]
  (cond
    (early-years? y) :early-years
    (key-stage-1? y) :ks-1
    (key-stage-2? y) :ks-2
    (key-stage-3? y) :ks-3
    (key-stage-4? y) :ks-4
    (further-education? y) :further-education))


(defn all-academic-years [{:keys [title-fmt-str domain-lookup serie-specs]}]
  (let [academic-years (into (sorted-set) (map :academic-year) (-> serie-specs first :data))]
    (into []
          (map (fn [academic-year]
                 (transduce
                  (wsc/all-domain-xf :academic-year academic-year)
                  (wsc/chart-spec-rf
                   (wsc/base-chart-spec
                    {:title (format title-fmt-str (get domain-lookup academic-year academic-year))}))
                  serie-specs)))
          academic-years)))

(defn multi-line-and-iqr-with-history [title academic-years-lookup colors-and-points historical-counts output-academic-year]
  (let [academic-years (into (sorted-set) (map :academic-year) output-academic-year)]
    (transduce
     (wsc/multi-line-and-iqr-with-history-xf
      {:domain-key :academic-year
       :domain-values-lookup academic-years-lookup
       :colors-and-points colors-and-points
       :historical-counts historical-counts
       :projected-data output-academic-year})
     (wsc/chart-spec-rf
      (wsc/base-chart-spec {:title title :legend "Academic Years"}))
     academic-years)))
