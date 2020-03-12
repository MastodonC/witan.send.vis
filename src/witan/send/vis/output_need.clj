(ns witan.send.vis.output-need
  (:require [clojure2d.color :as color]
            [witan.send.chart :as wsc]
            [witan.send.series :as wss]
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

(defn domain-colors-and-points
  "Generate colours and shapes for each need year so we have
  something consistent"
  [need-data]
  (let [pal (color/palette-presets :tableau-20-2)
        points [\O \s \o \S \+ \x]
        needs (into (sorted-set) (map :need) need-data)]
    (into (sorted-map)
          (map (fn [need color point]
                 [need {:color color :point point}])
               needs
               (cycle pal)
               (cycle points)))))

(defn compare-all-needs [{:keys [a-title b-title]} historical-transitions-a historical-transitions-b output-need-a output-need-b]
  (let [needs (into (sorted-set) (map :need) output-need-a)]
    (into []
          (map (fn [need]
                 (transduce
                  (map identity)
                  (wsc/chart-spec-rf
                   {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
                    :y-axis {:tick-formatter int :label "Population" :format {:font-size 24 :font "Open Sans"}}
                    :legend {:label "Data Sets"
                             :legend-spec [[:line "Historical"
                                            {:color :black :stroke {:size 4} :font "Open Sans" :font-size 36}]
                                           [:line "Projected"
                                            {:color :black :stroke {:size 4 :dash [2.0]} :font "Open Sans" :font-size 36}]]}
                    :title  {:label (format "Compare %s and %s need populations for %s" a-title b-title need)
                             :format {:font-size 24 :font "Open Sans" :margin 36 :font-style nil}}})
                  (vector {:color :blue
                           :shape \s
                           :legend-label a-title
                           :data (wss/maps->line {:x-key :calendar-year
                                                  :y-key :median
                                                  :color :blue
                                                  :point \s
                                                  :dash [2.0]}
                                                 (filter
                                                  #(= (:need %) need)
                                                  output-need-a))}
                          {:color :blue
                           :legend-label a-title
                           :data (wss/maps->ci {:x-key :calendar-year
                                                :hi-y-key :q3
                                                :low-y-key :q1
                                                :color :blue}
                                               (filter
                                                #(= (:need %) need)
                                                output-need-a))}
                          {:color :blue
                           :legend-label a-title
                           :data (wss/maps->ci {:x-key :calendar-year
                                                :hi-y-key :high-95pc-bound
                                                :low-y-key :low-95pc-bound
                                                :color :blue
                                                :alpha 25}
                                               (filter
                                                #(= (:need %) need)
                                                output-need-a))}
                          {:color :orange
                           :shape \o
                           :legend-label b-title
                           :data (wss/maps->line {:x-key :calendar-year
                                                  :y-key :median
                                                  :color :orange
                                                  :point \o
                                                  :dash [2.0]}
                                                 (filter
                                                  #(= (:need %) need)
                                                  output-need-b))}
                          {:color :orange
                           :legend-label b-title
                           :data (wss/maps->ci {:x-key :calendar-year
                                                :hi-y-key :q3
                                                :low-y-key :q1
                                                :color :orange}
                                               (filter
                                                #(= (:need %) need)
                                                output-need-b))}
                          {:color :blue
                           :legend-label b-title
                           :data (wss/maps->ci {:x-key :calendar-year
                                                :hi-y-key :high-95pc-bound
                                                :low-y-key :low-95pc-bound
                                                :color :orange
                                                :alpha 25}
                                               (filter
                                                #(= (:need %) need)
                                                output-need-b))}
                          {:color :blue
                           :legend-label "Historical Transitions"
                           :shape \s
                           :hide-legend true
                           :data (wss/maps->line {:x-key :calendar-year
                                                  :y-key :population
                                                  :color :blue
                                                  :point \s}
                                                 (filter
                                                  #(= (:need %) need)
                                                  historical-transitions-a))}
                          {:color :orange
                           :legend-label "Historical Transitions"
                           :shape \o
                           :hide-legend true
                           :data (wss/maps->line {:x-key :calendar-year
                                                  :y-key :population
                                                  :color :orange
                                                  :point \o}
                                                 (filter
                                                  #(= (:need %) need)
                                                  historical-transitions-b))}))))
          needs)))

(defn multi-line-and-iqr-with-history [title needs-lookup colors-and-points historical-counts output-need]
  (let [needs (into (sorted-set) (map :need) output-need)]
    (transduce
     (mapcat
      (fn [need]
        [{:color (-> need colors-and-points :color)
          :shape (-> need colors-and-points :point)
          :legend-label (needs-lookup need need)
          :data (wss/maps->line {:x-key :calendar-year
                                 :y-key :median
                                 :color (-> need colors-and-points :color)
                                 :point (-> need colors-and-points :point)
                                 :dash [2.0]}
                                (filter
                                 #(= (:need %) need)
                                 output-need))}
         {:color (-> need colors-and-points :color)
          :data (wss/maps->ci {:x-key :calendar-year
                               :hi-y-key :q3
                               :low-y-key :q1
                               :color (-> need colors-and-points :color)}
                              (filter
                               #(= (:need %) need)
                               output-need))}
         {:color (-> need colors-and-points :color)
          :shape (-> need colors-and-points :point)
          :legend-label (str need " Historical")
          :hide-legend true
          :data (wss/maps->line {:x-key :calendar-year
                                 :y-key :population
                                 :color (-> need colors-and-points :color)
                                 :point (-> need colors-and-points :point)}
                                (filter
                                 #(= (:need %) need)
                                 historical-counts))}]))
     (wsc/chart-spec-rf
      {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 36 :font "Open Sans"}}
       :y-axis {:tick-formatter int :label "Population" :format {:font-size 36 :font "Open Sans"}}
       :legend {:label "Needs"
                :legend-spec [[:line "Historical"
                               {:color :black :stroke {:size 4} :font "Open Sans" :font-size 36}]
                              [:line "Projected"
                               {:color :black :stroke {:size 4 :dash [2.0]} :font "Open Sans" :font-size 36}]]}
       :title  {:label title
                :format {:font-size 36 :font "Open Sans Bold" :margin 36 :font-style :bold}}})
     needs)))
