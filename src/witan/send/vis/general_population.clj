(ns witan.send.vis.general-population
  (:require [clojure2d.color :as color]
            [witan.send.chart :as wsc]
            [witan.send.series :as wss]
            [witan.send.domain.academic-years :as ay]
            [witan.send.vis.ingest :as ingest :refer [->int csv->]]
            [witan.send.vis :as vis]))

(defn general-population [general-population-file]
  (csv-> general-population-file
         (map #(-> %
                   (update :calendar-year ->int)
                   (update :academic-year ->int)
                   (update :population ->int)))))

(def base-gp-serie-def
  {:historical-y-key :population
   :hide-legend false})

(defn base-gp-chart-def [ay-lookup]
  {:legend-label "Academic Years"
   :domain-key :academic-year
   :domain-values-lookup ay-lookup
   ;; :hide-legend true
   :x-axis-label "Calendar Year" :x-tick-formatter int
   :y-axis-label "Population" :y-tick-formatter int
   :legend-spec [] ;; we don't want any of the default legend stuff
   :chartf wsc/zero-y-index})

(def gen-pop-titles-and-sets
  [["General Population Early Years" ay/early-years]
   ["General Population Key Stage 1" ay/key-stage-1]
   ["General Population Key Stage 2" ay/key-stage-2]
   ["General Population Key Stage 3" ay/key-stage-3]
   ["General Population Key Stage 4" ay/key-stage-4]
   ["General Population Key Stage 5" ay/key-stage-5]
   ["General Population NCY 15+" ay/ncy-15+]
   ["General Population All NCYs" (concat ay/early-years ay/key-stage-1 ay/key-stage-2 ay/key-stage-3 ay/key-stage-4 ay/key-stage-5 ay/ncy-15+)]
   ["Total General Population" #{99}]
   ;; FIXME: 99 is used as a placeholder where data structures require a number be present but is
   ;; converted to a more informative string ("Total") when outputting to a chart and spreadsheet
   ])

(defn sum-population [pop-data]
  (->> (group-by :calendar-year pop-data)
       (map
        (fn [[grp-key values]]
          {:calendar-year grp-key
           :academic-year 99
           ;; FIXME: 99 is used as a placeholder where data structures require a number be present but is
           ;; converted to a more informative string ("Total") when outputting to a chart and spreadsheet
           :population (reduce + (map :population values))}))
       (sort-by :calendar-year)))

;; FIXME: Not terribly happy about calling this "historic data" as it is a projection
(defn charts
  ([ay-lookup historical-data titles-and-sets]
   (let [domain-key :academic-year]
     (wsc/domain-charts {:domain-key domain-key
                         :chart-base-def (base-gp-chart-def ay-lookup)
                         :serie-base-def base-gp-serie-def
                         :colors-and-points (merge (wsc/domain-colors-and-points domain-key historical-data)
                                                   {99 {:color [152.0 223.0 138.0 255.0], :point \V}})
                         ;; FIXME: 99 is used as a placeholder where data structures require a number be present but is
                         ;; converted to a more informative string ("Total") when outputting to a chart and spreadsheet
                         :historical-data (concat historical-data
                                                  (sum-population historical-data))}
                        titles-and-sets)))
  ([historical-data]
   (charts ay/ay-lookup
           historical-data
           gen-pop-titles-and-sets)))
