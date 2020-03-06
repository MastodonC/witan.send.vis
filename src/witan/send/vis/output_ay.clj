(ns witan.send.vis.output-ay
  (:require [clojure2d.color :as color]
            [witan.send.chart :as wsc]
            [witan.send.series :as wss]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

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

(defn compare-all-academic-years [{:keys [a-title b-title]} historical-transitions-a historical-transitions-b output-academic-year-a output-academic-year-b]
  (let [academic-years (into (sorted-set) (map :academic-year) output-academic-year-a)]
    (into []
          (map (fn [academic-year]
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
                    :title  {:label (format "Compare %s and %s academic-year populations for %s" a-title b-title academic-year)
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
                                                  #(= (:academic-year %) academic-year)
                                                  output-academic-year-a))}
                          {:color :blue
                           :legend-label a-title
                           :data (wss/maps->ci {:x-key :calendar-year
                                                :hi-y-key :q3
                                                :low-y-key :q1
                                                :color :blue}
                                               (filter
                                                #(= (:academic-year %) academic-year)
                                                output-academic-year-a))}
                          {:color :blue
                           :legend-label a-title
                           :data (wss/maps->ci {:x-key :calendar-year
                                                :hi-y-key :high-95pc-bound
                                                :low-y-key :low-95pc-bound
                                                :color :blue
                                                :alpha 25}
                                               (filter
                                                #(= (:academic-year %) academic-year)
                                                output-academic-year-a))}
                          {:color :orange
                           :shape \o
                           :legend-label b-title
                           :data (wss/maps->line {:x-key :calendar-year
                                                  :y-key :median
                                                  :color :orange
                                                  :point \o
                                                  :dash [2.0]}
                                                 (filter
                                                  #(= (:academic-year %) academic-year)
                                                  output-academic-year-b))}
                          {:color :orange
                           :legend-label b-title
                           :data (wss/maps->ci {:x-key :calendar-year
                                                :hi-y-key :q3
                                                :low-y-key :q1
                                                :color :orange}
                                               (filter
                                                #(= (:academic-year %) academic-year)
                                                output-academic-year-b))}
                          {:color :blue
                           :legend-label b-title
                           :data (wss/maps->ci {:x-key :calendar-year
                                                :hi-y-key :high-95pc-bound
                                                :low-y-key :low-95pc-bound
                                                :color :orange
                                                :alpha 25}
                                               (filter
                                                #(= (:academic-year %) academic-year)
                                                output-academic-year-b))}
                          {:color :blue
                           :legend-label "Historical Transitions"
                           :shape \s
                           :hide-legend true
                           :data (wss/maps->line {:x-key :calendar-year
                                                  :y-key :population
                                                  :color :blue
                                                  :point \s}
                                                 (filter
                                                  #(= (:academic-year %) academic-year)
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
                                                  #(= (:academic-year %) academic-year)
                                                  historical-transitions-b))}))))
          academic-years)))

(defn multi-line-and-iqr-with-history [title academic-years-lookup colors-and-points historical-counts output-academic-year]
  (let [academic-years (into (sorted-set) (map :academic-year) output-academic-year)]
    (transduce
     (mapcat
      (fn [academic-year]
        [{:color (-> academic-year colors-and-points :color)
          :shape (-> academic-year colors-and-points :point)
          :legend-label (academic-years-lookup academic-year academic-year)
          :data (wss/maps->line {:x-key :calendar-year
                                 :y-key :median
                                 :color (-> academic-year colors-and-points :color)
                                 :point (-> academic-year colors-and-points :point)
                                 :dash [2.0]}
                                (filter
                                 #(= (:academic-year %) academic-year)
                                 output-academic-year))}
         {:color (-> academic-year colors-and-points :color)
          :data (wss/maps->ci {:x-key :calendar-year
                               :hi-y-key :q3
                               :low-y-key :q1
                               :color (-> academic-year colors-and-points :color)}
                              (filter
                               #(= (:academic-year %) academic-year)
                               output-academic-year))}
         {:color (-> academic-year colors-and-points :color)
          :shape (-> academic-year colors-and-points :point)
          :legend-label (str academic-year " Historical")
          :hide-legend true
          :data (wss/maps->line {:x-key :calendar-year
                                 :y-key :population
                                 :color (-> academic-year colors-and-points :color)
                                 :point (-> academic-year colors-and-points :point)}
                                (filter
                                 #(= (:academic-year %) academic-year)
                                 historical-counts))}]))
     (wsc/chart-spec-rf
      {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
       :y-axis {:tick-formatter int :label "Population" :format {:font-size 24 :font "Open Sans"}}
       :legend {:label "Academic-Years"
                :legend-spec [[:line "Historical"
                               {:color :black :stroke {:size 2} :font "Open Sans" :font-size 36}]
                              [:line "Projected"
                               {:color :black :stroke {:size 2 :dash [2.0]} :font "Open Sans" :font-size 36}]]}
       :title  {:label title
                :format {:font-size 24 :font "Open Sans" :margin 36 :font-style nil}}})
     academic-years)))
