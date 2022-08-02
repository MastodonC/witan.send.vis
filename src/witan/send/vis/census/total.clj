(ns witan.send.vis.census.total
  (:require [witan.send.chart :as wsc]
            [witan.send.series :as wss]
            [witan.sen2.sen2 :as sen]
            [tablecloth.api :as tc]))

(defn sen2 [s la-or-gss census]
  "Requires a local authority or gss code as a string, the key :la or :gss and census data"
  (let [years (into (sorted-set) (map :calendar-year census))
        pop (some-> s
                    (sen/generate-current-pop la-or-gss)
                    (tc/rows :as-maps))]
    (if pop
      (into [] (comp
                (map (fn [year] (assoc {}
                                       :calendar-year year
                                       :population (-> (some
                                                        #(when (= year (:time_period %))
                                                           %) pop)
                                                       :Total_all)))))
            years)
      [])))

(defn total-send-population
  ([total-population la sen2 watermark legend-spec]
   {:x-axis {:tick-formatter int :label "Calendar Year" :format {:font-size 24 :font "Open Sans"}}
    :y-axis {:tick-formatter int :label "Population" :format {:font-size 24 :font "Open Sans"}}
    :legend {:label "Population"
             :legend-spec (if legend-spec
                            legend-spec
                            (if (seq sen2)
                              [[:line "SEN2"
                                {:color wsc/orange :stroke {:size 4} :shape \O :font "Open Sans" :font-size 36}]
                               [:line "Total count of EHCPs"
                                {:color wsc/blue :stroke {:size 4} :shape \^ :font "Open Sans" :font-size 36}]]
                              [[:line "Total count of EHCPs"
                                {:color wsc/blue :stroke {:size 4} :shape \^ :font "Open Sans" :font-size 36}]]))} ;; flip shape in legend
    :title  {:label (str la " Count of EHCPs")
             :format {:font-size 24 :font "Open Sans" :margin 36 :font-style nil}}
    :watermark watermark
    :series [(wss/maps->line {:x-key :calendar-year
                              :y-key :population
                              :color wsc/orange
                              :point \O}
                             sen2)
             (wss/maps->line {:x-key :calendar-year
                              :y-key :population
                              :color wsc/blue
                              :point \V}
                             total-population)]})
  ([total-population la sen2 watermark]
   (total-send-population total-population la sen2 watermark false)))

(defn chart [title total-send]
  [(wsc/comparison-chart-and-table (assoc total-send :title title))])
