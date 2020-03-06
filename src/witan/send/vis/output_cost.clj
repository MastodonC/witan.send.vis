(ns witan.send.vis.output-cost
  (:require [witan.send.chart :as wsc]
            [witan.send.series :as wss]
            [witan.send.vis :as vis]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

(defn output-cost [output-cost-file]
  (csv-> output-cost-file
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

(defn compare-costs [{:keys [a-title b-title]} output-cost-a output-cost-b]
  (transduce
   (map identity)
   (wsc/chart-spec-rf
    {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
     :y-axis {:tick-formatter vis/millions-formatter :label "Cost (Millions £)" :format {:font-size 24 :font "Open Sans"}}
     :legend {:label "Data Sets"}
     :title  {:label (format "Compare %s and %s total costs" a-title b-title)
              :format {:font-size 24 :font "Open Sans" :margin 36 :font-style nil}}})
   (vector {:color :blue
            :shape \s
            :legend-label a-title
            :data (wss/maps->line {:x-key :calendar-year
                                   :y-key :median
                                   :color :blue
                                   :point \s
                                   :dash [2.0]}
                                  output-cost-a)}
           {:color :blue
            :legend-label a-title
            :data (wss/maps->ci {:x-key :calendar-year
                                 :hi-y-key :q3
                                 :low-y-key :q1
                                 :color :blue}
                                output-cost-a)}
           {:color :blue
            :legend-label a-title
            :data (wss/maps->ci {:x-key :calendar-year
                                 :hi-y-key :high-95pc-bound
                                 :low-y-key :low-95pc-bound
                                 :color :blue
                                 :alpha 25}
                                output-cost-a)}
           {:color :orange
            :shape \o
            :legend-label b-title
            :data (wss/maps->line {:x-key :calendar-year
                                   :y-key :median
                                   :color :orange
                                   :point \o
                                   :dash [2.0]}
                                  output-cost-b)}
           {:color :orange
            :legend-label b-title
            :data (wss/maps->ci {:x-key :calendar-year
                                 :hi-y-key :q3
                                 :low-y-key :q1
                                 :color :orange}
                                output-cost-b)}
           {:color :blue
            :legend-label b-title
            :data (wss/maps->ci {:x-key :calendar-year
                                 :hi-y-key :high-95pc-bound
                                 :low-y-key :low-95pc-bound
                                 :color :orange
                                 :alpha 25}
                                output-cost-b)})))
