(ns witan.send.vis.validation-charts
  (:require [witan.send.vis.validation-charts.ncy :as ncy]
            [witan.send.vis.validation-charts.needs :as needs]
            [witan.send.vis.validation-charts.settings :as settings]))

(defn charts [transitions {:keys [ncy-titles-and-sets
                                  need-titles-and-sets
                                  setting-titles-and-sets
                                  output-dir]
                           :or {ncy-titles-and-sets ncy/titles-and-sets}}]
  (let [ncy-charts (ncy/ncy-charts transitions ncy-titles-and-sets)
        needs-charts (needs/needs-charts transitions need-titles-and-sets)
        settings-charts (settings/settings-charts transitions setting-titles-and-sets)]

    (ncy/->excel output-dir ncy-charts)
    (needs/->excel output-dir needs-charts)
    (settings/->excel output-dir settings-charts)

    {:transitions transitions
     :ncy-charts ncy-charts
     :needs-charts needs-charts
     :settings-charts settings-charts}))
