(ns witan.send.vis.census.needs
  (:require [witan.send.chart :as wsc]))

(def base-comparison-serie-def
  {:historical-y-key :population})

(def lookup {})

(def base-comparison-chart-def
  {:legend-label "Needs"
   :hide-legend false
   :domain-key :academic-year
   :domain-values-lookup lookup
   :x-axis-label "Calendar Year" :x-tick-formatter int
   :y-axis-label "Population" :y-tick-formatter int
   :chartf wsc/zero-y-index})

(defn counts-per-calendar-year [census-data]
  (transduce
   (remove #(= (:need %) "NONSEND"))
   (fn
     ([acc]
      (sort-by :calendar-year
               (into []
                     (map (fn [[[calendar-year need] population]] {:calendar-year calendar-year
                                                                   :need need
                                                                   :population population}))
                     acc)))
     ([acc {:keys [calendar-year need]}]
      (update acc [calendar-year need] (fnil inc 0))))
   {}
   census-data))


(defn charts
  ([census-data titles-and-sets]
   (let [counts (counts-per-calendar-year census-data)
         domain-key :need
         chart-base base-comparison-chart-def
         serie-base base-comparison-serie-def
         colors-and-points (wsc/domain-colors-and-points domain-key census-data)]
     (into []
           (comp
            (map (fn [[title domain-values]]
                   (assoc
                    chart-base
                    :title title
                    :series
                    (into []
                          (map (fn [domain-value]
                                 (merge serie-base
                                        {:legend-label domain-value
                                         :hide-legend false
                                         :color (-> domain-value colors-and-points :color)
                                         :shape (-> domain-value colors-and-points :point)
                                         :historical-data (into [] (filter #(= domain-value (domain-key %))) counts)})))
                          domain-values))))
            (map wsc/comparison-chart-and-table))
           titles-and-sets)))
  ([census-data]
   (let [all-needs (into (sorted-set) (map :need census-data))]
     (charts census-data
             [["All Needs" all-needs]]))))




