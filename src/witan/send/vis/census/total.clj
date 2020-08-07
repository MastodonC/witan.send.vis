(ns witan.send.vis.census.total
  (:require [witan.send.chart :as wsc]
            [witan.send.series :as wss]
            [witan.phyrexian.sen2-processing-tool :as sen]))

(defn total-send-population [total-population]
  {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
   :y-axis {:tick-formatter int :label "Population" :format {:font-size 24 :font "Open Sans"}}
   :legend {:label "Population"
            :legend-spec [[:line "Total"
                           {:color wsc/blue :stroke {:size 4} :shape \^ :font "Open Sans" :font-size 36}]]} ;; flip shape in legend
   :title  {:label "Total SEND Population"
            :format {:font-size 24 :font "Open Sans" :margin 36 :font-style nil}}
   :series [(wss/maps->line {:x-key :calendar-year
                             :y-key :population
                             :color wsc/blue
                             :point \V}
                            total-population)]})

(defn chart [title total-send]
  [(wsc/comparison-chart-and-table (assoc total-send :title title))])
