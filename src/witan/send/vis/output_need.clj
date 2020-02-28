(ns witan.send.vis.output-need
  (:require [clojure2d.color :as color]
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

(defn split-rows-by-domain
  "Turn a seq of maps into a map keyed by domain that where each key has
  the value:
  {:color <a color> :point <a point shape> :data <vector of data points to be mapped>}.

  This can then be filtered aftwards to produce a zoomed in cut of the
  data. Selecting the keys after using this function will keep the
  colors and shapes stable across the particular domain."
  [domain-key statistic ay-data]
  (let [rows->map (reduce
                   (fn [acc x]
                     (update acc (domain-key x) (fnil conj []) [(:calendar-year x) (statistic x)]))
                   (sorted-map)
                   ay-data)
        domain-values (into (sorted-set) (map domain-key) ay-data)
        colors-and-points (let [pal (color/palette-presets :tableau-20-2)
                                points [\O \s \o \S \+ \x]]
                            (into (sorted-map)
                                  (map (fn [domain-value color point]
                                         [domain-value {:color color :point point}])
                                       domain-values
                                       (cycle pal)
                                       (cycle points))))]
    (into (sorted-map)
          (map (fn [[k data]]
                 (let [c-n-p (colors-and-points k)]
                   [k {:data data :color (:color c-n-p) :point (:point c-n-p)}])))
          rows->map)))

(defn multi-line-data [ay-data]
  (reduce
   (fn [acc x]
     (update acc (:need x) (fnil conj []) [(:calendar-year x) (:median x)]))
   (sorted-map)
   ay-data))

(defn domain-colors-and-points
  "Generate colours and shapes for each need so we have
  something consistent"
  [ay-data]
  (let [pal (color/palette-presets :tableau-20-2)
        points [\O \s \o \S \+ \x]
        needs (into (sorted-set) (map :setting) ay-data)]
    (into (sorted-map)
          (map (fn [need color point]
                 [need {:color color :point point}])
               needs
               (cycle pal)
               (cycle points)))))

(defn needs-multi-line-chart-spec
  "Multi-line for all needs x: calendar-years, y: median
  population, each line: need"
  ([need-data-by-domain title]
   (let [legend (into []
                      (map (fn [[domain-key {:keys [color point]}]]
                             [:line domain-key {:color color :shape point :stroke {:size 2} :font "Open Sans" :font-size 36}]))
                      need-data-by-domain)
         series (into []
                      (map (fn [[domain-key {:keys [data color point]}]]
                             [:line data {:point {:type point :size 10}
                                          :stroke {:size 2}
                                          :color color}]))
                      need-data-by-domain)]
     {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
      :y-axis {:tick-formatter int :label "Population" :format {:font-size 24 :font "Open Sans"}}
      :legend {:label "Needs" :font "Open Sans" :font-style nil :font-size 50 :legend-spec legend}
      :title {:label title :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
      :size {:width 1024 :height 768 :background (color/color :white)}
      :series series})))

;; ci for each need: median, q1, q3, high-95pc, low-95pc
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

(defn single-need-ci-chart-spec
  "Filtered seq of rows containing :median :q1 and :q3"
  [{:keys [color shape title legend-label data]}]
  {:x-axis {:tick-formatter int :label "Calendar Year"}
   :y-axis {:tick-formatter int :label "Population"}
   :legend {:label "Needs"
            :legend-spec [[:line legend-label {:color (color/color color) :shape shape :stroke {:size 2} :font "Open Sans"}]]}
   :title {:label title :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
   :size {:width 1024 :height 768 :background (color/color :white)}
   :series (ci-series color shape :calendar-year data)}
  )

(defn multi-need-ci-chart-spec
  [title needs-data-maps]
  {:x-axis {:tick-formatter int :label "Calendar Year"}
   :y-axis {:tick-formatter int :label "Population"}
   :legend {:label "Legend"
            :legend-spec
            (into []
                  (map (fn [{:keys [color shape legend-label]}]
                         [:line legend-label {:color (color/color color) :shape shape :stroke {:size 2} :font "Open Sans"}]))
                  needs-data-maps)}
   :title {:label title :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
   :size {:width 1024 :height 768 :background (color/color :white)}
   :series (into []
                 (mapcat (fn [{:keys [color shape data]}]
                           (ci-series color shape :calendar-year data)))
                 needs-data-maps)})
