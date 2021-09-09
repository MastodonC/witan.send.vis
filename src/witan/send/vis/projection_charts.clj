(ns witan.send.vis.projection-charts
  (:require [clojure.core.async :as a]
            [witan.send.chart :as wsc]
            [witan.send.main :as wsm]
            [witan.send.vis.general-population :as vis.gp]
            [witan.send.vis.ingest.transitions :as vis.transitions]
            [witan.send.vis.output-ay :as vis.oay]
            [witan.send.vis.output-cost :as vis.ocost]
            [witan.send.vis.output-count :as vis.ocount]
            [witan.send.vis.output-need :as vis.on]
            [witan.send.vis.output-setting :as vis.os]
            [witan.send.vis.output-setting-cost :as vis.osc]))

(defn charts [config-file {:keys [setting-titles-and-sets need-titles-and-sets]}]
  (let [config                 (wsm/read-config config-file)
        output-dir             (str (:project-dir config) "/" (get-in config [:output-parameters :output-dir]))
        historical-transitions (vis.transitions/historical (str (:project-dir config) "/" (get-in config [:file-inputs :transitions])))
        gen-pop-data           (into []
                                     (comp
                                      ;; only a subset so the charts line up
                                      (filter #(<= 2015 (:calendar-year %)))
                                      (filter #(< (:academic-year %) 21)))
                                     (vis.gp/general-population (str (:project-dir config) "/" (get-in config [:file-inputs :population]))))
        census                 (vis.transitions/->census historical-transitions)
        settings-lookup        (into {}
                                     (map (fn [x] [(:setting x) (:setting x)]))
                                     census)
        needs-lookup           (into {}
                                     (map (fn [x] [(:setting x) (:setting x)]))
                                     census)
        output-ay              (future
                                 (vis.oay/charts
                                  (vis.transitions/ay-counts-per-calendar-year historical-transitions)
                                  (into []
                                        (filter #(< (:academic-year %) 21))
                                        (vis.oay/output-ay (str output-dir "/" vis.oay/output-ay-file)))))
        output-count           (future
                                 (vis.ocount/chart
                                  "2020 Baseline"
                                  (vis.transitions/total-population-per-calendar-year historical-transitions)
                                  (vis.ocount/output-count (str output-dir "/" vis.ocount/output-count-file))))
        output-cost            (future
                                 (vis.ocost/chart
                                  "2020 Baseline"
                                  (vis.ocost/output-cost (str output-dir "/" vis.ocost/output-cost-file))))
        output-need            (future
                                 (vis.on/charts
                                  needs-lookup
                                  (vis.transitions/needs-counts-per-calendar-year historical-transitions)
                                  (vis.on/output-need (str output-dir "/" vis.on/output-need-file))
                                  need-titles-and-sets))
        output-setting         (future
                                 (vis.os/charts
                                  settings-lookup
                                  (vis.transitions/settings-counts-per-calendar-year historical-transitions)
                                  (vis.os/output-setting (str output-dir "/" vis.os/output-setting-file))
                                  setting-titles-and-sets))
        output-setting-cost    (vis.osc/charts
                                (vis.osc/output-setting-cost (str output-dir "/" vis.osc/output-setting-cost-file))
                                setting-titles-and-sets)
        general-population     (future (vis.gp/charts gen-pop-data))]

    (a/thread
      ;; (run! (partial wsc/save-chart-by-title (str output-dir "/charts/ay-")) @output-ay)
      (wsc/save-workbook (str output-dir "/charts/output-ay.xlsx") (wsc/->workbook @output-ay)))

    (a/thread
      ;; (run! (partial wsc/save-chart-by-title (str output-dir "/charts/count-")) @output-count)
      (wsc/save-workbook (str output-dir "/charts/output-count.xlsx") (wsc/->workbook @output-count)))

    (a/thread
      ;; (run! (partial wsc/save-chart-by-title (str output-dir "/charts/cost-")) @output-cost)
      (wsc/save-workbook (str output-dir "/charts/output-cost.xlsx") (wsc/->workbook @output-cost)))

    (a/thread
      ;; (run! (partial wsc/save-chart-by-title (str output-dir "/charts/need-")) @output-need)
      (wsc/save-workbook (str output-dir "/charts/output-need.xlsx") (wsc/->workbook @output-need)))

    (a/thread
      ;; (run! (partial wsc/save-chart-by-title (str output-dir "/charts/setting-")) @output-setting)
      (wsc/save-workbook (str output-dir "/charts/output-setting.xlsx") (wsc/->workbook @output-setting)))

    (a/thread
      ;; (run! (partial wsc/save-chart-by-title (str output-dir "/charts/setting-cost-")) output-setting-cost)
      (wsc/save-workbook (str output-dir "/charts/output-setting-cost.xlsx") (wsc/->workbook output-setting-cost)))

    (a/thread
      ;; (run! (partial wsc/save-chart-by-title (str output-dir "/charts/general-population-")) @general-population)
      (wsc/save-workbook (str output-dir "/charts/general-population.xlsx") (wsc/->workbook @general-population)))

    {:output-ay           @output-ay
     :output-count        @output-count
     :output-cost         @output-cost
     :output-need         @output-need
     :output-setting      @output-setting
     :output-setting-cost output-setting-cost
     :general-population  @general-population}))
