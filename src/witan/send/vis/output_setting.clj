(ns witan.send.vis.output-setting
  (:require [clojure2d.color :as color]
            [witan.send.chart :as wsc]
            [witan.send.series :as wss]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

(defn output-setting [output-setting-file]
  (csv-> output-setting-file
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
  [domain-key statistic setting-data]
  (let [rows->map (reduce
                   (fn [acc x]
                     (update acc (domain-key x) (fnil conj []) [(:calendar-year x) (statistic x)]))
                   (sorted-map)
                   setting-data)
        domain-values (into (sorted-set) (map domain-key) setting-data)
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

(defn domain-colors-and-points
  "Generate colours and shapes for each setting year so we have
  something consistent"
  [setting-data]
  (let [pal (color/palette-presets :tableau-20-2)
        points [\O \s \o \S \+ \x]
        settings (into (sorted-set) (map :setting) setting-data)]
    (into (sorted-map)
          (map (fn [setting color point]
                 [setting {:color color :point point}])
               settings
               (cycle pal)
               (cycle points)))))

(defn compare-all-settings [{:keys [a-title b-title]} historical-transitions-a historical-transitions-b output-setting-a output-setting-b]
  (let [settings (into (sorted-set) (map :setting) output-setting-a)]
    (into []
          (map (fn [setting]
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
                    :title  {:label (format "Compare %s and %s setting populations for %s" a-title b-title setting)
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
                                                  #(= (:setting %) setting)
                                                  output-setting-a))}
                          {:color :blue
                           :legend-label a-title
                           :data (wss/maps->ci {:x-key :calendar-year
                                                :hi-y-key :q3
                                                :low-y-key :q1
                                                :color :blue}
                                               (filter
                                                #(= (:setting %) setting)
                                                output-setting-a))}
                          {:color :blue
                           :legend-label a-title
                           :data (wss/maps->ci {:x-key :calendar-year
                                                :hi-y-key :high-95pc-bound
                                                :low-y-key :low-95pc-bound
                                                :color :blue
                                                :alpha 25}
                                               (filter
                                                #(= (:setting %) setting)
                                                output-setting-a))}
                          {:color :orange
                           :shape \o
                           :legend-label b-title
                           :data (wss/maps->line {:x-key :calendar-year
                                                  :y-key :median
                                                  :color :orange
                                                  :point \o
                                                  :dash [2.0]}
                                                 (filter
                                                  #(= (:setting %) setting)
                                                  output-setting-b))}
                          {:color :orange
                           :legend-label b-title
                           :data (wss/maps->ci {:x-key :calendar-year
                                                :hi-y-key :q3
                                                :low-y-key :q1
                                                :color :orange}
                                               (filter
                                                #(= (:setting %) setting)
                                                output-setting-b))}
                          {:color :blue
                           :legend-label b-title
                           :data (wss/maps->ci {:x-key :calendar-year
                                                :hi-y-key :high-95pc-bound
                                                :low-y-key :low-95pc-bound
                                                :color :orange
                                                :alpha 25}
                                               (filter
                                                #(= (:setting %) setting)
                                                output-setting-b))}
                          {:color :blue
                           :legend-label "Historical Transitions"
                           :shape \s
                           :hide-legend true
                           :data (wss/maps->line {:x-key :calendar-year
                                                  :y-key :population
                                                  :color :blue
                                                  :point \s}
                                                 (filter
                                                  #(= (:setting %) setting)
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
                                                  #(= (:setting %) setting)
                                                  historical-transitions-b))}))))
          settings)))

;; dashed projection, solid historical, same colours and points, but
;; only a black dashed and solid line to say historical/projection and
;; then colours showing in the legend
;; output driven by what is in output-setting
(defn multi-line-with-history [title colors-and-points historical-counts output-setting]
  (let [settings (into (sorted-set) (map :setting) output-setting)]
    (transduce
     (mapcat
      (fn [setting]
        [{:color (-> setting colors-and-points :color)
          :shape (-> setting colors-and-points :point)
          :legend-label setting
          :data (wss/maps->line {:x-key :calendar-year
                                 :y-key :median
                                 :color (-> setting colors-and-points :color)
                                 :point (-> setting colors-and-points :point)
                                 :dash [2.0]}
                                (filter
                                 #(= (:setting %) setting)
                                 output-setting))}
         {:color (-> setting colors-and-points :color)
          :shape (-> setting colors-and-points :point)
          :legend-label (str setting " Historical")
          :hide-legend true
          :data (wss/maps->line {:x-key :calendar-year
                                 :y-key :population
                                 :color (-> setting colors-and-points :color)
                                 :point (-> setting colors-and-points :point)}
                                (filter
                                 #(= (:setting %) setting)
                                 historical-counts))}]))
     (wsc/chart-spec-rf
      {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
       :y-axis {:tick-formatter int :label "Population" :format {:font-size 24 :font "Open Sans"}}
       :legend {:label "Data Sets"
                :legend-spec [[:line "Historical"
                               {:color :black :stroke {:size 2} :font "Open Sans" :font-size 36}]
                              [:line "Projected"
                               {:color :black :stroke {:size 2 :dash [2.0]} :font "Open Sans" :font-size 36}]]}
       :title  {:label title
                :format {:font-size 24 :font "Open Sans" :margin 36 :font-style nil}}})
     settings)))


(defn multi-line-and-iqr-with-history [title settings-lookup colors-and-points historical-counts output-setting]
  (let [settings (into (sorted-set) (map :setting) output-setting)]
    (transduce
     (mapcat
      (fn [setting]
        [{:color (-> setting colors-and-points :color)
          :shape (-> setting colors-and-points :point)
          :legend-label (settings-lookup setting setting)
          :data (wss/maps->line {:x-key :calendar-year
                                 :y-key :median
                                 :color (-> setting colors-and-points :color)
                                 :point (-> setting colors-and-points :point)
                                 :dash [2.0]}
                                (filter
                                 #(= (:setting %) setting)
                                 output-setting))}
         {:color (-> setting colors-and-points :color)
          :data (wss/maps->ci {:x-key :calendar-year
                               :hi-y-key :q3
                               :low-y-key :q1
                               :color (-> setting colors-and-points :color)}
                              (filter
                               #(= (:setting %) setting)
                               output-setting))}
         {:color (-> setting colors-and-points :color)
          :shape (-> setting colors-and-points :point)
          :legend-label (str setting " Historical")
          :hide-legend true
          :data (wss/maps->line {:x-key :calendar-year
                                 :y-key :population
                                 :color (-> setting colors-and-points :color)
                                 :point (-> setting colors-and-points :point)}
                                (filter
                                 #(= (:setting %) setting)
                                 historical-counts))}]))
     (wsc/chart-spec-rf
      {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
       :y-axis {:tick-formatter int :label "Population" :format {:font-size 24 :font "Open Sans"}}
       :legend {:label "Settings"
                :legend-spec [[:line "Historical"
                               {:color :black :stroke {:size 2} :font "Open Sans" :font-size 36}]
                              [:line "Projected"
                               {:color :black :stroke {:size 2 :dash [2.0]} :font "Open Sans" :font-size 36}]]}
       :title  {:label title
                :format {:font-size 24 :font "Open Sans" :margin 36 :font-style nil}}})
     settings)))
