(ns witan.send.vis.validation-charts.needs
  (:require [clojure.set :as set]
            [witan.send.chart :as wsc]
            [witan.send.driver.transitions :as dt]
            [witan.send.vis.census.needs :as vis.cn]
            [witan.send.vis.ingest.transitions :as vit]))

(defn all-needs [{:keys [colors-and-points titles-and-sets census]}]
  (vis.cn/charts {:colors-and-points colors-and-points
                  :titles-and-sets titles-and-sets}
                 census
                 true))

(defn stayers-in [{:keys [colors-and-points titles-and-sets transitions]}]
  (vis.cn/charts {:colors-and-points colors-and-points
                  :titles-and-sets (mapv (fn [[title pred]] [(str "Stayers in " title) pred]) titles-and-sets)}
                 (->> transitions
                      (filter dt/stayer?)
                      (vit/->census))
                 true))

(defn movers-in [{:keys [colors-and-points titles-and-sets transitions]}]
  (vis.cn/charts {:colors-and-points colors-and-points
                  :titles-and-sets (mapv (fn [[title pred]] [(str "Movers in " title) pred]) titles-and-sets)}
                 (->> transitions
                      (filter dt/mover?)
                      (vit/->census))
                 true))

(defn joiners-to [{:keys [colors-and-points titles-and-sets transitions]}]
  (vis.cn/charts {:colors-and-points colors-and-points
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

(defn leavers-from [{:keys [colors-and-points titles-and-sets transitions]}]
  (vis.cn/charts {:colors-and-points colors-and-points
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

(defn needs-charts [transitions titles-and-sets]
  (let [census (vit/->census transitions)
        config {:colors-and-points (wsc/domain-colors-and-points :need census)
                :titles-and-sets titles-and-sets
                :transitions transitions
                :census census}]
    (into []
          (mapcat (fn [f] (f config)))
          [all-needs
           stayers-in
           movers-in
           joiners-to
           leavers-from])))

(defn ->pngs [output-dir charts]
  (run! (partial wsc/save-chart-by-title (str output-dir "charts/needs-")) charts)
  charts)

(defn ->excel [output-dir charts]
  (wsc/save-workbook (str output-dir "charts/census-needs.xlsx") (wsc/->workbook charts))
  charts)
