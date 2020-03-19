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


(defn multi-line [title academic-years-lookup colors-and-points general-population]
  (let [academic-years (into (sorted-set) (map :academic-year) general-population)]
    (transduce
     (wsc/multi-line-actual-xf
      {:domain-key :academic-year
       :y-key :population
       :domain-values-lookup academic-years-lookup
       :colors-and-points colors-and-points
       :data general-population})
     (wsc/chart-spec-rf
      (wsc/base-chart-spec {:title title :legend "Academic Years"}))
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
                             :color wsc/blue
                             :point \V}
                            total-population)]})
