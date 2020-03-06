(ns witan.send.vis.output-count
  (:require [clojure2d.color :as color]
            [witan.send.chart :as wsc]
            [witan.send.series :as wss]
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

(defn compare-populations [{:keys [a-title b-title]} historical-transitions-a historical-transitions-b output-count-a output-count-b]
  (transduce
   (map identity)
   (wsc/chart-spec-rf
    {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
     :y-axis {:tick-formatter int :label "Population" :format {:font-size 24 :font "Open Sans"}}
     :legend {:label "Data Sets"
              :legend-spec [[:line "Historical"
                             {:color :black :stroke {:size 2} :font "Open Sans" :font-size 36}]
                            [:line "Projected"
                             {:color :black :stroke {:size 2 :dash [2.0]} :font "Open Sans" :font-size 36}]]}
     :title  {:label (format "Compare %s and %s total populations" a-title b-title)
              :format {:font-size 24 :font "Open Sans" :margin 36 :font-style nil}}})
   (vector {:color :blue
            :shape \s
            :legend-label a-title
            :data (wss/maps->line {:x-key :calendar-year
                                   :y-key :median
                                   :color :blue
                                   :point \s
                                   :dash [2.0]}
                                  output-count-a)}
           {:color :blue
            :legend-label a-title
            :data (wss/maps->ci {:x-key :calendar-year
                                 :hi-y-key :q3
                                 :low-y-key :q1
                                 :color :blue}
                                output-count-a)}
           {:color :blue
            :legend-label a-title
            :data (wss/maps->ci {:x-key :calendar-year
                                 :hi-y-key :high-95pc-bound
                                 :low-y-key :low-95pc-bound
                                 :color :blue
                                 :alpha 25}
                                output-count-a)}
           {:color :orange
            :shape \o
            :legend-label b-title
            :data (wss/maps->line {:x-key :calendar-year
                                   :y-key :median
                                   :color :orange
                                   :point \o
                                   :dash [2.0]}
                                  output-count-b)}
           {:color :orange
            :legend-label b-title
            :data (wss/maps->ci {:x-key :calendar-year
                                 :hi-y-key :q3
                                 :low-y-key :q1
                                 :color :orange}
                                output-count-b)}
           {:color :blue
            :legend-label b-title
            :data (wss/maps->ci {:x-key :calendar-year
                                 :hi-y-key :high-95pc-bound
                                 :low-y-key :low-95pc-bound
                                 :color :orange
                                 :alpha 25}
                                output-count-b)}
           {:color :blue
            :legend-label "Historical Transitions"
            :shape \s
            :hide-legend true
            :data (wss/maps->line {:x-key :calendar-year
                                   :y-key :population
                                   :color :blue
                                   :point \s}
                                  historical-transitions-a)}
           {:color :orange
            :legend-label "Historical Transitions"
            :shape \o
            :hide-legend true
            :data (wss/maps->line {:x-key :calendar-year
                                   :y-key :population
                                   :color :orange
                                   :point \o}
                                  historical-transitions-b)})))
