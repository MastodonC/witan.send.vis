(ns witan.send.vis.output-setting
  (:require [witan.send.chart :as wsc]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

(def output-setting-file "Output_Setting.csv")

(defn base-setting-chart-def [domain-values-lookup]
  {:legend-label "Settings"
   :domain-key :setting
   :domain-values-lookup domain-values-lookup
   :x-axis-label "Calendar Year" :x-tick-formatter int
   :y-axis-label "Population" :y-tick-formatter int
   :chartf wsc/zero-y-index})

(def base-setting-serie-def {:historical-y-key :population})

(defn charts
  ([settings-lookup historical-data projection-data titles-and-sets]
   (let [domain-key :setting]
     (wsc/domain-charts {:domain-key domain-key
                         :chart-base-def (base-setting-chart-def settings-lookup)
                         :serie-base-def base-setting-serie-def
                         :colors-and-points (wsc/domain-colors-and-points domain-key projection-data)
                         :historical-data historical-data
                         :projection-data projection-data}
                        titles-and-sets))))


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


(comment

  (require '[cljplot.config :as cfg])
  (require '[witan.send.vis.ingest.transitions :as vit])
  
  (def plot-cfg
    (swap! cfg/configuration (fn [c]
                               (-> c
                                   (assoc-in [:legend :font] "Open Sans Bold")
                                   (assoc-in [:legend :font-size] 24)))))

  
  (def output-setting-csv (str "../witan.send/data/demo/results/" output-setting-file))

  (def historic (vit/historical "../witan.send/data/demo/data/transitions.csv"))
  
  (def setting-data (output-setting output-setting-csv))
  (def history-setting-counts-per-cy (vit/settings-counts-per-calendar-year historic))

  (def settings (into (sorted-set)
                      (map :setting)
                      setting-data))

  (def setting-lookup
    (into (sorted-map)
          (map (fn [s] [s s]))
          settings))

  (def settings-charts (charts setting-lookup
                               historic
                               setting-data 
                               [["A-F" (sorted-set "A" "B" "C" "D" "E" "F")]
                                ["G-M" (sorted-set "G" "H" "I" "J" "K" "L" "M")]
                                ["N-S" (sorted-set "N" "O" "P" "Q" "R" "S")]]))

  (run!
   (partial wsc/save-chart-by-title "demo/charts/setting-")
   settings-charts)
  
  )
