(ns witan.send.vis.census.total
  (:require [witan.send.chart :as wsc]
            [witan.send.series :as wss]
            [witan.phyrexian.sen2-processing-tool :as sen]))

(defn key-to-year [k]
  (read-string (str "20" (-> k key name (clojure.string/split #"-") second))))

(defn sen2 [LA]
  (let [gss (into [] (sen/search-gss LA))
        pop (sen/generate-current-pop gss)]
    (map #(assoc {}
                 :calendar-year (key-to-year %)
                 :population (val %)) pop)))

(defn total-send-population [total-population sen2]
  {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
   :y-axis {:tick-formatter int :label "Population" :format {:font-size 24 :font "Open Sans"}}
   :legend {:label "Population"
            :legend-spec [[:line "SEN2"
                           {:color wsc/orange :stroke {:size 4} :shape \^ :font "Open Sans" :font-size 36}]
                          [:line "Total"
                           {:color wsc/blue :stroke {:size 4} :shape \^ :font "Open Sans" :font-size 36}]]} ;; flip shape in legend
   :title  {:label "Total SEND Population"
            :format {:font-size 24 :font "Open Sans" :margin 36 :font-style nil}}
   :series [(wss/maps->line {:x-key :calendar-year
                             :y-key :population
                             :color wsc/orange
                             :point \V}
                            sen2)
            (wss/maps->line {:x-key :calendar-year
                             :y-key :population
                             :color wsc/blue
                             :point \V}
                            total-population)]})

(defn chart [title total-send]
  [(wsc/comparison-chart-and-table (assoc total-send :title title))])
