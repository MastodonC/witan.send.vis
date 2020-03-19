(ns witan.send.vis.output-need
  (:require [clojure2d.color :as color]
            [witan.send.chart :as wsc]
            [witan.send.series :as wss]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

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

(defn all-needs [{:keys [title-fmt-str domain-lookup serie-specs]}]
  (let [needs (into (sorted-set) (map :need) (-> serie-specs first :data))]
    (into []
          (map (fn [need]
                 (transduce
                  (wsc/all-domain-xf :need need)
                  (wsc/chart-spec-rf
                   (wsc/base-chart-spec
                    {:title (format title-fmt-str (get domain-lookup need need))}))
                  serie-specs)))
          needs)))

(defn multi-line-and-iqr-with-history [title needs-lookup colors-and-points historical-counts output-need]
  (let [needs (into (sorted-set) (map :need) output-need)]
    (transduce
     (wsc/multi-line-and-iqr-with-history-xf
      {:domain-key :need
       :domain-values-lookup needs-lookup
       :colors-and-points colors-and-points
       :historical-counts historical-counts
       :projected-data output-need})
     (wsc/chart-spec-rf
      (wsc/base-chart-spec {:title title :legend "Needs"}))
     needs)))
