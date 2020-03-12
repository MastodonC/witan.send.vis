(ns witan.send.vis.general-population
  (:require [clojure2d.color :as color]
            [witan.send.chart :as wsc]
            [witan.send.series :as wss]
            [witan.send.vis.ingest :as ingest :refer [->int csv->]]))

(defn general-population [general-population-file]
  (csv-> general-population-file
         (map #(-> %
                   (update :calendar-year ->int)
                   (update :academic-year ->int)
                   (update :population ->int)))))

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

(defn multi-line [title academic-years-lookup colors-and-points general-population]
  (let [academic-years (into (sorted-set) (map :academic-year) general-population)]
    (transduce
     (mapcat
      (fn [academic-year]
        [{:color (-> academic-year colors-and-points :color)
          :shape (-> academic-year colors-and-points :point)
          :legend-label (academic-years-lookup academic-year academic-year)
          :data (wss/maps->line {:x-key :calendar-year
                                 :y-key :population
                                 :color (-> academic-year colors-and-points :color)
                                 :point (-> academic-year colors-and-points :point)}
                                (filter
                                 #(= (:academic-year %) academic-year)
                                 general-population))}]))
     (wsc/chart-spec-rf
      {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
       :y-axis {:tick-formatter int :label "Population" :format {:font-size 24 :font "Open Sans"}}
       :legend {:label "Academic Years"}
       :title  {:label title
                :format {:font-size 24 :font "Open Sans" :margin 36 :font-style nil}}})
     academic-years)))

(defn total-population [total-population]
  {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
   :y-axis {:tick-formatter int :label "Population" :format {:font-size 24 :font "Open Sans"}}
   :legend {:label "Population"
            :legend-spec [[:line "Total"
                           {:color :blue :stroke {:size 4} :shape \^ :font "Open Sans" :font-size 36}]]} ;; flip shape in legend
   :title  {:label "Total General Population"
            :format {:font-size 24 :font "Open Sans" :margin 36 :font-style nil}}
   :series [(wss/maps->line {:x-key :calendar-year
                             :y-key :population
                             :color :blue
                             :point \V}
                            total-population)]})
