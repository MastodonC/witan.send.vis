(ns witan.send.vis.output-setting-cost
  (:require [clojure2d.color :as color]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]
            [witan.send.vis :as vis]))

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
     (update acc (:academic-year x) (fnil conj []) [(:calendar-year x) (:median x)]))
   (sorted-map)
   ay-data))

(defn domain-colors-and-points
  "Generate colours and shapes for each academic year so we have
  something consistent"
  [ay-data]
  (let [pal (color/palette-presets :tableau-20-2)
        points [\O \s \o \S \+ \x]
        academic-years (into (sorted-set) (map :academic-year) ay-data)]
    (into (sorted-map)
          (map (fn [academic-year color point]
                 [academic-year {:color color :point point}])
               academic-years
               (cycle pal)
               (cycle points)))))

(defn setting-multi-line-chart-spec
  "Multi-line for all academic-years x: calendar-years, y: median
  population, each line: academic-year"
  ([setting-data-by-domain title]
   (let [legend (into []
                      (map (fn [[domain-key {:keys [color point]}]]
                             [:line domain-key {:color color :shape point :stroke {:size 2} :font "Open Sans" :font-size 36}]))
                      setting-data-by-domain)
         series (into []
                      (map (fn [[domain-key {:keys [data color point]}]]
                             [:line data {:point {:type point :size 10}
                                          :stroke {:size 2}
                                          :color color}]))
                      setting-data-by-domain)]
     {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
      :y-axis {:tick-formatter vis/millions-formatter :label "Cost in Millions (£)" :format {:font-size 24 :font "Open Sans"}}
      :legend {:label "Academic Year" :font "Open Sans" :font-style nil :font-size 50 :legend-spec legend}
      :title {:label title :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
      :size {:width 1024 :height 768 :background (color/color :white)}
      :series series})))

;; ci for each academic-year: median, q1, q3, high-95pc, low-95pc
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

(defn single-setting-ci-chart-spec
  "Filtered seq of rows containing :median :q1 and :q3"
  [{:keys [color shape title legend-label data]}]
  {:x-axis {:tick-formatter int :label "Calendar Year"}
   :y-axis {:tick-formatter vis/millions-formatter :label "Cost in Millions (£)"}
   :legend {:label "Academic Year"
            :legend-spec [[:line legend-label {:color (color/color color) :shape shape :stroke {:size 2} :font "Open Sans"}]]}
   :title {:label title :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
   :size {:width 1024 :height 768 :background (color/color :white)}
   :series (ci-series color shape :calendar-year data)}
  )

(defn multi-setting-ci-chart-spec
  [title setting-data-maps]
  {:x-axis {:tick-formatter int :label "Calendar Year"}
   :y-axis {:tick-formatter vis/millions-formatter :label "Cost in Millions (£)"}
   :legend {:label "Academic Year"
            :legend-spec
            (into []
                  (map (fn [{:keys [color shape legend-label]}]
                         [:line legend-label {:color (color/color color) :shape shape :stroke {:size 2} :font "Open Sans"}]))
                  setting-data-maps)}
   :title {:label title :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
   :size {:width 1024 :height 768 :background (color/color :white)}
   :series (into []
                 (mapcat (fn [{:keys [color shape data]}]
                           (ci-series color shape :calendar-year data)))
                 setting-data-maps)})
