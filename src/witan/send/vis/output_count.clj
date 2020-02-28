(ns witan.send.vis.output-count
  (:require [clojure2d.color :as color]
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

;; ci for each setting: median, q1, q3, high-95pc, low-95pc
(defn ci-series
  "Excpects a seq of maps that have median, high-ci, low-ci, and a key
  passed in for the x-axis"
  [color point x-key data]
  [[:ci
    (vector
     (into []
           (map (fn [m]
                  (vector
                   (get m x-key) (get m :q3))))
           data)
     (into []
           (map (fn [m]
                  (vector
                   (get m x-key) (get m :q1))))
           data))
    {:color (color/color color 50)}]
   [:line (into []
                (map (fn [m]
                       (vector
                        (get m x-key) (get m :median))))
                data)
    {:color (color/color color) :point {:type point} :stroke {:size 2}}]])

(defn single-count-ci-chart-spec
  "Filtered seq of rows containing :median :q1 and :q3"
  [{:keys [color shape title legend-label data]}]
  {:x-axis {:tick-formatter int :label "Calendar Year"}
   :y-axis {:tick-formatter int :label "Population"}
   :legend {:label "Legend"
            :legend-spec [[:line legend-label {:color (color/color color) :shape shape :stroke {:size 2} :font "Open Sans"}]]}
   :title {:label title :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
   :size {:width 1024 :height 768 :background (color/color :white)}
   :series (ci-series color shape :calendar-year data)})

(defn multi-count-ci-chart-spec
  [title count-data-maps]
  {:x-axis {:tick-formatter int :label "Calendar Year"}
   :y-axis {:tick-formatter int :label "Population"}
   :legend {:label "Legend"
            :legend-spec
            (into []
                  (map (fn [{:keys [color shape legend-label]}]
                         [:line legend-label {:color (color/color color) :shape shape :stroke {:size 2} :font "Open Sans"}]))
                  count-data-maps)}
   :title {:label title :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
   :size {:width 1024 :height 768 :background (color/color :white)}
   :series (into []
                 (mapcat (fn [{:keys [color shape data]}]
                           (ci-series color shape :calendar-year data)))
                 count-data-maps)})
