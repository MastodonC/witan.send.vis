(ns witan.send.vis.output-ay
  (:require [witan.send.chart :as wsc]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

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
