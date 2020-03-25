(ns witan.send.vis.output-setting
  (:require [witan.send.chart :as wsc]
            [witan.send.vis.ingest :as ingest :refer [->int ->double csv->]]))

(def output-setting-file "Output_Setting.csv")

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

(defn all-settings [{:keys [title-fmt-str domain-lookup serie-specs]}]
  (let [settings (into (sorted-set) (map :setting) (-> serie-specs first :data))]
    (into []
          (map (fn [setting]
                 (transduce
                  (wsc/all-domain-xf :setting setting)
                  (wsc/chart-spec-rf
                   (wsc/base-chart-spec
                    {:title (format title-fmt-str (get domain-lookup setting setting))}))
                  serie-specs)))
          settings)))

(defn multi-line-and-iqr-with-history [title settings-lookup colors-and-points historical-counts output-setting]
  (let [settings (into (sorted-set) (map :setting) output-setting)]
    (transduce
     (wsc/multi-line-and-iqr-with-history-xf
      {:domain-key :setting
       :domain-values-lookup settings-lookup
       :colors-and-points colors-and-points
       :historical-counts historical-counts
       :projected-data output-setting})
     (wsc/chart-spec-rf
      (wsc/base-chart-spec {:title title :legend "Settings"}))
     settings)))
