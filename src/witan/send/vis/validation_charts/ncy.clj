(ns witan.send.vis.validation-charts.ncy
  (:require [clojure.set :as set]
            [witan.send.chart :as wsc]
            [witan.send.domain.academic-years :as ay]
            [witan.send.driver.transitions :as dt]
            [witan.send.vis.census.academic-years :as vis.cay]
            [witan.send.vis.ingest.transitions :as vit]))

(def all-ages (into [] (concat ay/early-years ay/key-stage-1 ay/key-stage-2 ay/key-stage-3 ay/key-stage-4 ay/key-stage-5 #_ay/ncy-15+)))

(def titles-and-sets
  [["Early Years" ay/early-years]
   ["Key Stage 1" ay/key-stage-1]
   ["Key Stage 2" ay/key-stage-2]
   ["Key Stage 3" ay/key-stage-3]
   ["Key Stage 4" ay/key-stage-4]
   ["Key Stage 5" ay/key-stage-5]
   ["NCY 15+" ay/ncy-15+]
   ["All NCYs" all-ages]])

(defn overall-chart [{:keys [colors-and-points titles-and-sets census]}]
  (vis.cay/charts {:colors-and-points colors-and-points
                   :titles-and-sets titles-and-sets}
                  census
                  true))

(defn ncy-by-setting [{:keys [colors-and-points census settings]}]
  (mapcat
   (fn [setting]
     (vis.cay/charts {:colors-and-points colors-and-points
                      :titles-and-sets   [[(str "NCYs for Setting " setting)
                                           all-ages]]}
                     (into []
                           (filter #(= setting (:setting %)))
                           census)
                     true))
   settings))

(defn ncy-by-need [{:keys [colors-and-points needs census]}]
  (mapcat
   (fn [need]
     (vis.cay/charts {:colors-and-points colors-and-points
                      :titles-and-sets   [[(str "NCYs for Need " need)
                                           all-ages]]}
                     (into []
                           (filter #(= need (:need %)))
                           census)
                     true))
   needs))

(defn stayers [{:keys [colors-and-points transitions titles-and-sets]}]
  (vis.cay/charts {:colors-and-points colors-and-points
                   :titles-and-sets (mapv (fn [[title pred]] [(str "Stayers in " title) pred]) titles-and-sets)}
                  (->> transitions
                       (filter dt/stayer?)
                       (vit/->census))
                  true))

(defn movers-in [{:keys [colors-and-points transitions titles-and-sets]}]
  (vis.cay/charts {:colors-and-points colors-and-points
                   :titles-and-sets (mapv (fn [[title pred]] [(str "Movers in " title) pred]) titles-and-sets)}
                  (->> transitions
                       (filter dt/mover?)
                       (vit/->census))
                  true))

(defn joiners-to [{:keys [colors-and-points titles-and-sets transitions]}]
  (vis.cay/charts {:colors-and-points colors-and-points
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
  (vis.cay/charts {:colors-and-points colors-and-points
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

(defn ncy-charts
  ([transitions override-titles-and-sets]
   (let [census (vit/->census transitions)
         config {:colors-and-points (wsc/domain-colors-and-points :academic-year census)
                 :titles-and-sets override-titles-and-sets
                 :needs (into (sorted-set) (map :need) census)
                 :settings (into (sorted-set) (map :setting) census)
                 :transitions transitions
                 :census census}]
     (into []
           (mapcat (fn [f] (f config)))
           [overall-chart
            ncy-by-setting
            ncy-by-need
            stayers
            movers-in
            joiners-to
            leavers-from])))
  ([transitions]
   (ncy-charts transitions titles-and-sets)))

(defn ->pngs [output-dir charts]
  (run! (partial wsc/save-chart-by-title (str output-dir "charts/ncy-")) charts)
  charts)

(defn ->excel [output-dir charts]
  (wsc/save-workbook (str output-dir "charts/census-ncy.xlsx") (wsc/->workbook charts))
  charts)
