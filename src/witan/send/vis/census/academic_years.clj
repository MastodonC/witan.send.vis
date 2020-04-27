(ns witan.send.vis.census.academic-years
  (:require [witan.send.chart :as wsc]
            [witan.send.vis.output-ay :as oay]))

(def base-ay-comparison-serie-def
  {:historical-y-key :population})

(def base-ay-comparison-chart-def
  {:legend-label "Academic Years"
   :hide-legend false
   :domain-key :academic-year
   :domain-values-lookup oay/ay-lookup
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
                     (map (fn [[[calendar-year academic-year] population]] {:calendar-year calendar-year
                                                                            :academic-year academic-year
                                                                            :population population}))
                     acc)))
     ([acc {:keys [calendar-year academic-year]}]
      (update acc [calendar-year academic-year] (fnil inc 0))))
   {}
   census-data))


(def all-chart-specs
  [["Early Years" oay/early-years]
   ["Key Stage 1" oay/key-stage-1]
   ["Key Stage 2" oay/key-stage-2]
   ["Key Stage 3" oay/key-stage-3]
   ["Key Stage 4" oay/key-stage-4]
   ["Key Stage 5" oay/key-stage-5]
   ["NCY 15+" oay/ncy-15+]
   ["All NCYs" (concat oay/early-years oay/key-stage-1 oay/key-stage-2 oay/key-stage-3 oay/key-stage-4 oay/key-stage-5 oay/ncy-15+)]])

(defn charts
  ([config census-data]
   (let [ay-counts (counts-per-calendar-year census-data)
         domain-key :academic-year
         chart-base base-ay-comparison-chart-def
         serie-base base-ay-comparison-serie-def
         {:keys [titles-and-sets colors-and-points]
          :or {titles-and-sets all-chart-specs
               colors-and-points (wsc/domain-colors-and-points domain-key census-data)}} config]
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
                                         :historical-data (into [] (filter #(= domain-value (domain-key %))) ay-counts)})))
                          domain-values))))
            (map wsc/comparison-chart-and-table))
           titles-and-sets)))
  ([census-data]
   (charts {:titles-and-sets all-chart-specs}
           census-data)))


(comment

  (mapv (fn [[title pred]] [(str "Joiners to " title) pred]) all-chart-specs)
  [["Joiners to Early Years" #{-5 -4 -3 -2 -1 0}] ["Joiners to Key Stage 1" #{1 2}] ["Joiners to Key Stage 2" #{3 4 5 6}] ["Joiners to Key Stage 3" #{7 8 9}] ["Joiners to Key Stage 4" #{10 11}] ["Joiners to Key Stage 5" #{12 13 14}] ["Joiners to NCY 15+" #{15 16 17 18 19 20 21 22 23 24 25}] ["Joiners to All NCYs" (-5 -4 -3 -2 -1 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)]]
  )
