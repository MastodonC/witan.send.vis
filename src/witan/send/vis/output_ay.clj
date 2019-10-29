(ns witan.send.vis.output-ay
  (:require [clojure2d.color :as color]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]
            [cljplot.core :as plot]
            [witan.send.driver.chart :as chart]))

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

(comment

  (def ay-data (output-ay "/home/bld/wip/witan.send/data/demo/results/Output_AY.csv"))

  (def split-data (split-rows-by-domain :academic-year :median ay-data))

  (domain-colors-and-points ay-data)

  (multi-line-data ay-data)

  )

(defn ay-multi-line-chart-spec
  "Multi-line for all academic-years x: calendar-years, y: median
  population, each line: academic-year"
  ([ay-data-by-domain title]
   (let [legend (into []
                      (map (fn [[domain-key {:keys [color point]}]]
                             [:line domain-key {:color color :shape point :stroke {:size 2} :font "Open Sans" :font-size 36}]))
                      ay-data-by-domain)
         series (into []
                      (map (fn [[domain-key {:keys [data color point]}]]
                             [:line data {:point {:type point :size 10}
                                          :stroke {:size 2}
                                          :color color}]))
                      ay-data-by-domain)]
     {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
      :y-axis {:tick-formatter int :label "Population" :format {:font-size 24 :font "Open Sans"}}
      :legend {:label "Academic Year" :font "Open Sans" :font-style nil :font-size 50 :legend-spec legend}
      :title {:label title :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
      :size {:width 1024 :height 768 :background (color/color :white)}
      :series series})))

(comment

  (require '[cljplot.core :as plot])

  (split-rows-by-domain :academic-year :median ay-data)

  (plot/show (witan.send.vis/chart-0-y-index (ay-multi-line-chart-spec
                                              (split-rows-by-domain :academic-year :median ay-data)
                                              "SEND Population by Academic Year")))

  (plot/show (witan.send.vis/chart-0-y-index (ay-multi-line-chart-spec
                                              (into (sorted-map)
                                                    (filter (fn [[k v]] (early-years? k)))
                                                    (split-rows-by-domain :academic-year :median ay-data))
                                              "Early Years Population by Academic Year")))

  (plot/show (witan.send.vis/chart-0-y-index (ay-multi-line-chart-spec
                                              (into (sorted-map)
                                                    (filter (fn [[k v]] (key-stage-1? k)))
                                                    (split-rows-by-domain :academic-year :median ay-data))
                                              "Key Stage 1 Population by Academic Year")))

  (plot/show (witan.send.vis/chart-0-y-index (ay-multi-line-chart-spec
                                              (into (sorted-map)
                                                    (filter (fn [[k v]] (key-stage-2? k)))
                                                    (split-rows-by-domain :academic-year :median ay-data))
                                              "Key Stage 2 Population by Academic Year")))

  (plot/show (witan.send.vis/chart-0-y-index (ay-multi-line-chart-spec
                                              (into (sorted-map)
                                                    (filter (fn [[k v]] (key-stage-3? k)))
                                                    (split-rows-by-domain :academic-year :median ay-data))
                                              "Key Stage 3 Population by Academic Year")))

  (plot/show (witan.send.vis/chart-0-y-index (ay-multi-line-chart-spec
                                              (into (sorted-map)
                                                    (filter (fn [[k v]] (key-stage-4? k)))
                                                    (split-rows-by-domain :academic-year :median ay-data))
                                              "Key Stage 4 Population by Academic Year")))

  (plot/show (witan.send.vis/chart-0-y-index (ay-multi-line-chart-spec
                                              (into (sorted-map)
                                                    (filter (fn [[k v]] (further-education? k)))
                                                    (split-rows-by-domain :academic-year :median ay-data))
                                              "Further Education Population by Academic Year")))

  )

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

(defn single-ay-ci-chart-spec
  "Filtered seq of rows containing :median :q1 and :q3"
  [{:keys [color shape title legend-label data]}]
  {:x-axis {:tick-formatter int :label "Calendar Year"}
   :y-axis {:tick-formatter int :label "Population"}
   :legend {:label "Academic Year"
            :legend-spec [[:line legend-label {:color (color/color color) :shape shape :stroke {:size 2} :font "Open Sans"}]]}
   :title {:label title :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
   :size {:width 1024 :height 768 :background (color/color :white)}
   :series (ci-series color shape :calendar-year data)}
  )

(defn multi-ay-ci-chart-spec
  [title ay-data-maps]
  {:x-axis {:tick-formatter int :label "Calendar Year"}
   :y-axis {:tick-formatter int :label "Population"}
   :legend {:label "Academic Year"
            :legend-spec
            (into []
                  (map (fn [{:keys [color shape legend-label]}]
                         [:line legend-label {:color (color/color color) :shape shape :stroke {:size 2} :font "Open Sans"}]))
                  ay-data-maps)}
   :title {:label title :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
   :size {:width 1024 :height 768 :background (color/color :white)}
   :series (into []
                 (mapcat (fn [{:keys [color shape data]}]
                           (ci-series color shape :calendar-year data)))
                 ay-data-maps)})

(comment


  (plot/show
   (witan.send.vis/chart-0-y-index
    (single-ay-ci-chart-spec
     {:color :green
      :shape \o
      :title "SEND Population AY 10"
      :legend-label 10
      :data (into [] (filter #(= (:academic-year %) 10)) ay-data)})))


  )
