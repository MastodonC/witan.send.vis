(ns witan.send.vis.census.needs
  (:require [witan.send.chart :as wsc]))

(def base-comparison-serie-def
  {:historical-y-key :population})

(def lookup {})

(def base-comparison-chart-def
  {:legend-label "Needs"
   :hide-legend false
   :legend-spec [] ;; empty base legend
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


;; 1. Communication and interaction
;; 2. Cognition and learning
;; 3. Social, mental and emotional health
;; 4. Sensory and/or physical

(def all-chart-specs
  [["Communication" (sorted-set "ASD" "SLCN" "SEMH")]
   ["Cognition" (sorted-set "ADHD" "MLD" "PMLD" "SLD" "SPLD")]
   ["Physical" (sorted-set "HI" "MSI" "VI" "PD")]
   ["Other" (sorted-set "Pre-Assesment" "Unknown" "OTH")]])


;; FIXME: Create good defaults in the let rather than the function destructuring
(defn charts
  ([config census-data insert-zeros?]
   (let [domain-key :need
         counts (let [raw-counts (counts-per-calendar-year census-data)]
                  (if insert-zeros?
                    (wsc/insert-zero-counts census-data raw-counts domain-key)
                    raw-counts))
         chart-base base-comparison-chart-def
         serie-base base-comparison-serie-def
         all-census-needs (into #{} (map :need) census-data)
         all-default-needs [["All Needs" (into (sorted-set) (mapcat second) all-chart-specs)]]
         additional-census-needs [["Additional Needs"
                                   (into #{} (remove
                                              #(contains? (-> all-default-needs first second) %) all-census-needs))]]
         {:keys [titles-and-sets colors-and-points watermark]
          :or {titles-and-sets (let [t&s (into all-chart-specs all-default-needs)]
                                 (if (-> additional-census-needs first second seq)
                                   (into t&s additional-census-needs)
                                   t&s))
               colors-and-points (wsc/domain-colors-and-points domain-key census-data)
               watermark ""}} config]
     ;; FIXME: Should this into be a function that gets re-used?
     (into []
           (comp
            (map (fn [[title domain-values]]
                   (assoc
                    chart-base
                    :title title
                    :watermark watermark
                    :series
                    (into []
                          (comp
                           (map (fn [domain-value]
                                  (merge serie-base
                                         {:legend-label domain-value
                                          :hide-legend false
                                          :color (-> domain-value colors-and-points :color)
                                          :shape (-> domain-value colors-and-points :point)
                                          :historical-data (into [] (filter #(= domain-value (domain-key %))) counts)})))
                           (filter (fn [m] (seq (:historical-data m)))))
                          domain-values))))
            (filter (fn [m] (seq (:series m))))
            (map wsc/comparison-chart-and-table))
           titles-and-sets)))
  ([config census-data]
   (charts config census-data false))
  ([census-data]
   (let [all-needs (into (sorted-set) (map :need census-data))]
     (charts {:titles-and-sets (concat [["All Needs" all-needs]]
                                       all-chart-specs)}
             census-data))))
