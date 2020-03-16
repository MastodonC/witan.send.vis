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

(defn filter-serie-data [setting serie-spec]
  (let [orig-data (:data serie-spec)
        filtered-data (filter
                       (fn [d]
                         (= (:setting d) setting))
                       orig-data)]
    (assoc serie-spec :data filtered-data)))


(defn all-settings [title-fmt-str settings-lookup serie-specs]
  ;; FIXME: a bit brittle relying on the first for the settings?
  (let [settings (into (sorted-set) (map :setting) (-> serie-specs first :data))]
    (into []
          (map (fn [setting]
                 (transduce
                  (comp
                   (map (partial filter-serie-data setting))
                   (mapcat wss/serie-and-legend-spec))
                  (wsc/chart-spec-rf
                   {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
                    :y-axis {:tick-formatter int :label "Population" :format {:font-size 24 :font "Open Sans"}}
                    :legend {:label "Data Sets"
                             :legend-spec wsc/histogram-base-legend}
                    :title  {:label (format title-fmt-str (get settings-lookup setting setting))}})
                  serie-specs)))
          settings)))

;; FIXME: Make me work like all-settings
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
                :legend-spec wsc/histogram-base-legend}
       :title  {:label title}})
     settings)))
