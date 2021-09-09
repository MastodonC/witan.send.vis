(ns witan.send.vis.validation-charts.settings
  (:require [clojure.set :as set]
            [witan.send.chart :as wsc]
            [witan.send.driver.transitions :as dt]
            [witan.send.vis.census.settings :as vis.cs]
            [witan.send.vis.ingest.transitions :as vit]))

(defn all-settings [{:keys [colors-and-points titles-and-sets census]}]
  (vis.cs/charts {:colors-and-points colors-and-points
                  :titles-and-sets titles-and-sets}
                 census
                 true))

(defn stayers [{:keys [colors-and-points titles-and-sets transitions]}]
  (vis.cs/charts {:colors-and-points colors-and-points
                  :titles-and-sets (mapv (fn [[title pred]] [(str "Stayers in " title) pred]) titles-and-sets)}
                 (->> transitions
                      (filter dt/stayer?)
                      (vit/->census))
                 true))

(defn movers-in [{:keys [colors-and-points titles-and-sets transitions]}]
  (vis.cs/charts {:colors-and-points colors-and-points
                  :titles-and-sets (mapv (fn [[title pred]] [(str "Movers in " title) pred]) titles-and-sets)}
                 (->> transitions
                      (filter dt/mover?)
                      (vit/->census))
                 true))

(defn joiners [{:keys [colors-and-points titles-and-sets transitions]}]
  (vis.cs/charts {:colors-and-points colors-and-points
                  :titles-and-sets (mapv (fn [[title pred]] [(str "Joiners to " title) pred]) titles-and-sets)}
                 (into []
                       (comp
                        (filter dt/joiner?)
                        (map #(select-keys % [:calendar-year :setting-2 :need-2 :academic-year-2]))
                        (map #(set/rename-keys % {:setting-2 :setting
                                                  :need-2 :need
                                                  :academic-year-2 :academic-year})))
                       transitions)
                 true))


(defn leavers [{:keys [colors-and-points titles-and-sets transitions]}]
  (vis.cs/charts {:colors-and-points colors-and-points
                  :titles-and-sets (mapv (fn [[title pred]] [(str "Leavers from " title) pred]) titles-and-sets)}
                 (into []
                       (comp
                        (filter dt/leaver?)
                        (map #(select-keys % [:calendar-year :setting-1 :need-1 :academic-year-1]))
                        (map #(set/rename-keys % {:setting-1 :setting
                                                  :need-1 :need
                                                  :academic-year-1 :academic-year})))
                       transitions)
                 true))

(defn settings-charts [transitions titles-and-sets]
  (let [census (vit/->census transitions)
        config {:colors-and-points (wsc/domain-colors-and-points :setting census)
                :titles-and-sets titles-and-sets
                :transitions transitions
                :census census}]
    (into []
          (mapcat (fn [f] (f config)))
          [all-settings
           stayers
           joiners
           leavers])))

(defn ->pngs [output-dir charts]
  (run! (partial wsc/save-chart-by-title (str output-dir "charts/settings-")) charts)
  charts)

(defn ->excel [output-dir charts]
  (wsc/save-workbook (str output-dir "charts/census-settings.xlsx") (wsc/->workbook charts))
  charts)
