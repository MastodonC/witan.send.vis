(ns witan.send.vis
  (:require [cljplot.render :as plotr]
            [cljplot.build :as plotb]
            [cljplot.core :as plot]
            [clojure2d.core :as c2d]
            [clojure2d.color :as color]
            [net.cgrand.xforms :as x]
            [witan.send.vis.ingest :as ingest]
            [witan.send.vis.ingest.transitions :as transitions]))

(defn map-kv [f coll]
  (reduce-kv (fn [m k v] (assoc m k (f v))) (empty coll) coll))

(defn default-domain-map [k data default-value]
  (zipmap
   (x/into (sorted-set) (map k) data)
   (repeat default-value)))

(defn pivot-table
  "Pivot a vector of maps into a map of idx0 -> vector of idx1, rf value
  pairs all sorted by key and reduced according to rf."
  ([idx0 idx1 rf data]
   (x/into (sorted-map)
           (x/by-key idx0
                     (comp
                      (x/by-key idx1 rf)
                      (x/reduce (fn
                                  ([]
                                   (default-domain-map idx1 data 0)) ;; should default-domain-map be passed in?
                                  ([a]
                                   (sort-by first a))
                                  ([m [k v]]
                                   (assoc m k v))))))
           data))
  ([idx0 rf data]
   (x/into (default-domain-map idx0 data 0)
           (x/by-key idx0 rf)
           data)))

(defn pivot-table-vals [data]
  (map-kv #(map second %) data))

(defn sum-key-rf [k]
  (x/reduce (fn
              ([] 0)
              ([a] a)
              ([a x] (+ a (k x))))))

(defn save
  [prefix {:keys [chart file-name] :as chart-spec}]
  (plot/save chart (str prefix file-name))
  chart-spec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn multi-line [data]
  (let [pal (color/palette-presets :tableau-20-2)
        points [\O \s \o \S \+ \x \v \A \V \^]
        chart-spec (into []
                         (map (fn make-c-and-l
                                [[[series-name series-data] color point]]
                                {:legend [:line series-name {:color color :shape point :stroke {:size 4}}]
                                 :chart [:line series-data {:point {:type point :size 10}
                                                            :stroke {:size 4}
                                                            :color color}]}))
                         (sequence
                          (map (fn [data color point] [data color point]))
                          data
                          (cycle pal)
                          (cycle points)))
        legend (into [] (map :legend) chart-spec)
        ]
    (-> (apply plotb/series (into [#_[:grid]] (map :chart) chart-spec))
        (plotb/preprocess-series)
        (plotb/update-scale :x :fmt int)
        (plotb/update-scale :y :fmt int #_"%,.0f")
        (plotb/add-axes :bottom)
        (plotb/add-axes :left)
        (plotb/add-label :bottom "Calendar Year")
        (plotb/add-label :left "Population")
        (plotb/add-legend "Academic Year" legend)
        (plotr/render-lattice {:width 1024 :height 768}))))


(defn chart [{:keys [x-axis y-axis legend title series size]}]
  (let [size (or size {:width 1024 :height 768})
        title-format (or (:format title) {:font-size 24 :font "Open Sans Bold" :margin 36})]
    (-> (apply plotb/series series)
        (plotb/preprocess-series)
        (plotb/update-scale :x :fmt (:tick-formatter x-axis))
        ;; (plotb/update-scale :x :ticks 5) ;; only needed if we have fewer than 10 points so we don't get double on the axis?
        (plotb/update-scale :y :fmt (:tick-formatter y-axis))
        (plotb/add-axes :bottom {:ticks {:font-size 16}})
        (plotb/add-axes :left {:ticks {:font-size 16}})
        (plotb/add-label :bottom (:label x-axis) {:font-size 20 :font "Open Sans" :font-style nil})
        (plotb/add-label :left (:label y-axis) {:font-size 20 :font "Open Sans" :font-style nil})
        (plotb/add-label :top (:label title) title-format)
        (plotb/add-legend (:label legend) (:legend-spec legend))
        (plotr/render-lattice size))))


(defn zero-index-numerical-y-axes [prepped-data]
  (let [[t v] (get-in prepped-data [:extents :y 0])]
    (if (= :numerical t)
      (plotb/update-scale prepped-data :y :domain [0 (second v)])
      prepped-data)))

(defn chart-0-y-index [{:keys [x-axis y-axis legend title series size]}]
  (let [size (or size {:width 1024 :height 768})
        title-format (or (:format title) {:font-size 24 :font "Open Sans Bold" :margin 36})]
    (-> (apply plotb/series series)
        (plotb/preprocess-series)
        (plotb/update-scale :x :fmt (:tick-formatter x-axis))
        ;;(plotb/update-scale :x :ticks 10)
        (plotb/update-scale :y :fmt (:tick-formatter y-axis))
        (zero-index-numerical-y-axes)
        (plotb/add-axes :bottom {:ticks {:font-size 18}})
        (plotb/add-axes :left {:ticks {:font-size 18}})
        (plotb/add-label :bottom (:label x-axis) {:font-size 20 :font "Open Sans" :font-style nil})
        (plotb/add-label :left (:label y-axis) {:font-size 20 :font "Open Sans" :font-style nil})
        (plotb/add-label :top (:label title) title-format)
        (plotb/add-legend (:label legend) (:legend-spec legend))
        (plotr/render-lattice size))))

(comment

  {:x-axis {:tick-formatter int :label "Calendar Year"}
   :y-axis {:tick-formatter int :label "Population"}
   :legend {:label "Academic Year" :legend-spec legend-spec}
   :title {:label "Foo" :format {:font-size 12 :font "Open Sans" :margin 16}}
   :series series
   :size {:width 1024 :height 768}}

  )


(defn ci-series
  "Excpects a seq of maps that have median, high-ci, low-ci, and a key
  passed in for the x-axis"
  [color x-key data]
  [[:ci
    (vector
     (into []
           (map (fn [m]
                  (vector
                   (get m x-key) (get m :q3))))
           data)
     (into []
           (map (fn [m]
                  (vector
                   (get m x-key) (get m :q1))))
           data))
    {:color (color/color color 100)}]
   [:line (into []
                (map (fn [m]
                       (vector
                        (get m x-key) (get m :median))))
                data)
    {:color (color/color color) :stroke {:size 4}}]])

(defn historical-and-projected-line-series [color point x-key historical-y-key projected-y-key historical-data projected-data]
  [;; historical
   [:line
    (into []
          (map (fn [m] (vector
                        (get m x-key) (get m historical-y-key))))
          historical-data)
    {:point {:type point :size 10} :stroke {:size 4} :color color}]
   ;; projected
   [:line
    (into []
          (map (fn [m] (vector
                        (get m x-key) (get m projected-y-key))))
          projected-data)
    {:point {:type point :size 10} :stroke {:size 4 :dash [4.0]} :color color}]])

(comment

  (map (fn [y] [y (national-curriculum-stage y)]) (range -5 26))

  (def output-ay-file "/home/bld/wip/witan.send/data/demo/results_for_checking/Output_AY.csv")

  (def output-ay (ingest/output-ay output-ay-file))

  (def multi-line-data
    (reduce
     (fn [acc x]
       (update acc (:academic-year x) (fnil conj []) [(:calendar-year x) (:median x)]))
     (sorted-map)
     output-ay))


  (-> output-ay first keys)
  ;; (:min :q1 :q3 :low-ci :mean :high-ci :iqr :academic-year :high-95pc-bound :low-95pc-bound :median :max :std-dev :calendar-year)

  (pivot-table :academic-year :calendar-year concat output-ay)

  (plot/show
   (multi-line multi-line-data))

  (def prepped-data
    (let [data (into (sorted-map)
                     (filter (fn [[academic-year projection]] (further-education? academic-year)))
                     multi-line-data)
          pal (color/palette-presets :tableau-20-2)
          points [\O \s \o \S \+ \x]
          series (into []
                       (map (fn make-chart-series
                              [[[series-name series-data] color point]]
                              [:line series-data {:point {:type point :size 10}
                                                  :stroke {:size 4}
                                                  :color color}]))
                       (sequence
                        (map (fn [data color point] [data color point]))
                        data
                        (cycle pal)
                        (cycle points)))
          legend-spec (into []
                            (map (fn make-legend-spec
                                   [[[series-name series-data] color point]]
                                   [:line series-name {:color color :shape point :stroke {:size 4} :font "Open Sans" :font-size 36}]))
                            (sequence
                             (map (fn [data color point] [data color point]))
                             data
                             (cycle pal)
                             (cycle points)))
          spec {:x-axis {:tick-formatter int :label "Calendar Year"}
                :y-axis {:tick-formatter int :label "Population"}
                :legend {:label "Academic Year" :font "Open Sans" :font-style nil :font-size 50 :legend-spec legend-spec}
                :title {:label "SEND Population for Further Education" :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
                :size {:width 1024 :height 768}
                :series series}]
      #_(plot/show (chart-0-y-index spec))
      (plot/show (chart spec))))


  (def ci-prepped-data
    (let [filt (sequence
                (filter #(= (:academic-year %) 0))
                output-ay)
          chart-spec {:x-axis {:tick-formatter int :label "Calendar Year"}
                      :y-axis {:tick-formatter int :label "Population"}
                      :legend {:label ""
                               :legend-spec [[:line "Capacity" {:color (color/color :red) :stroke {:size 4 :dash [4.0]} :font "Open Sans"}]
                                             [:line "Population" {:color (color/color :green) :stroke {:size 4} :font "Open Sans"}]]}
                      :title {:label "NCY 0 Projected Population vs Capacity including CI" :format {:font-size 16 :font "Open Sans Bold" :margin 36}}
                      :size {:width 800 :height 600 :background (color/color :white)}
                      :series [[:ci
                                (vector
                                 (into []
                                       (map (fn [{:keys [calendar-year high-ci]}]
                                              (vector calendar-year high-ci)))
                                       filt)
                                 (into []
                                       (map (fn [{:keys [calendar-year low-ci]}]
                                              (vector calendar-year low-ci)))
                                       filt))
                                {:color (color/color :lightblue 180) :stroke {:size 0}}]
                               [:line
                                (into []
                                      (map (fn [{:keys [calendar-year median]}]
                                             (vector calendar-year median)))
                                      filt)
                                {:color (color/color :green) :stroke {:size 4}}]
                               [:line
                                [[2017 100]
                                 [2018 105]
                                 [2019 105]
                                 [2020 105]]
                                {:color (color/color :red) :stroke {:size 4 :dash [4.0]}}]
                               ]}]
      (plot/show (chart-0-y-index chart-spec))))


  (def ci-prepped-data-2
    (let [filt (into []
                     (filter #(= (:academic-year %) 0))
                     output-ay)
          chart-spec {:x-axis {:tick-formatter int :label "Calendar Year"}
                      :y-axis {:tick-formatter int :label "Population"}
                      :legend {:label ""
                               :legend-spec [[:line "Capacity" {:color (color/color :red) :stroke {:size 4 :dash [4.0]} :font "Open Sans"}]
                                             [:line "Population" {:color (color/color :green) :stroke {:size 4} :font "Open Sans"}]]}
                      :title {:label "NCY 0 Projected Population vs Capacity including CI" :format {:font-size 16 :font "Open Sans Bold" :margin 36}}
                      :size {:width 1024 :height 768 :background (color/color :white)}
                      :series (ci-series :blue :calendar-year filt)
                      #_(conj (ci-series :blue :calendar-year filt)
                              [:line
                               [[2017 100]
                                [2018 105]
                                [2019 105]
                                [2020 105]]
                               {:color (color/color :blue) :stroke {:size 4 :dash [4.0]}}])}]
      (plot/show (chart-0-y-index chart-spec))))

  )




(comment

  ;; setting cost charts

  (def output-setting-cost (ingest/output-setting
                            "/home/bld/wip/witan.send/data/demo/results/Output_Setting_Cost.csv"))

  (def setting-multi-line-data
    (reduce
     (fn [acc x]
       (update acc (:setting x) (fnil conj []) [(:calendar-year x) (:median x)]))
     (sorted-map)
     output-setting-cost))

  (def settings-chart
    (let [data setting-multi-line-data
          pal (color/palette-presets :tableau-20-2)
          points [\O \s \o \S \+ \x]
          series (into []
                       (map (fn make-chart-series
                              [[[series-name series-data] color point]]
                              [:line series-data {:point {:type point :size 10}
                                                  :stroke {:size 4}
                                                  :color color}]))
                       (sequence
                        (map (fn [data color point] [data color point]))
                        data
                        (cycle pal)
                        (cycle points)))
          legend-spec (into []
                            (map (fn make-legend-spec
                                   [[[series-name series-data] color point]]
                                   [:line series-name {:color color :shape point :stroke {:size 4} :font "Open Sans" :font-size 36}]))
                            (sequence
                             (map (fn [data color point] [data color point]))
                             data
                             (cycle pal)
                             (cycle points)))
          spec {:x-axis {:tick-formatter int :label "Calendar Year"}
                :y-axis {:tick-formatter double :label "Cost £ Millions"}
                :legend {:label "Setting" :font "Open Sans" :font-style nil :font-size 50 :legend-spec legend-spec}
                :title {:label "SEND Population for Each Setting" :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
                :size {:width 1024 :height 768}
                :series series}]
      (plot/show (chart-0-y-index spec))))


  )


(comment


  (def output-setting (ingest/output-setting
                       "/home/bld/wip/witan.send/data/demo/results/Output_Setting.csv"))

  (def setting-multi-line-data
    (reduce
     (fn [acc x]
       (update acc (:setting x) (fnil conj []) [(:calendar-year x) (:median x)]))
     (sorted-map)
     output-setting))

  (def settings-chart
    (let [data setting-multi-line-data
          pal (color/palette-presets :tableau-20-2)
          points [\O \s \o \S \+ \x]
          series (into []
                       (map (fn make-chart-series
                              [[[series-name series-data] color point]]
                              [:line series-data {:point {:type point :size 10}
                                                  :stroke {:size 4}
                                                  :color color}]))
                       (sequence
                        (map (fn [data color point] [data color point]))
                        data
                        (cycle pal)
                        (cycle points)))
          legend-spec (into []
                            (map (fn make-legend-spec
                                   [[[series-name series-data] color point]]
                                   [:line series-name {:color color :shape point :stroke {:size 4} :font "Open Sans" :font-size 36}]))
                            (sequence
                             (map (fn [data color point] [data color point]))
                             data
                             (cycle pal)
                             (cycle points)))
          spec {:x-axis {:tick-formatter int :label "Calendar Year"}
                :y-axis {:tick-formatter double :label "Cost £ Millions"}
                :legend {:label "Setting" :font "Open Sans" :font-style nil :font-size 50 :legend-spec legend-spec}
                :title {:label "SEND Population for Each Setting" :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
                :size {:width 1024 :height 768}
                :series series}]
      (plot/show (chart-0-y-index spec))))

  )

(defn millions-formatter [n]
  (format "%.02f" (/ n (* 1e6))))

(defn settings-cost-grouped-bar-chart [data]
  (let [domain (into (sorted-set) (map (fn [d] (:calendar-year d)) data))
        pal (color/palette-presets :tableau-20-2)
        series [[:stack-vertical [:bar (->> data
                                            (group-by :setting)
                                            (map (fn [[setting data]] [setting (mapv (fn [x] (:median x)) data)]))
                                            (into (sorted-map)))
                                  {:palette pal}]]]
        legend-spec (into []
                          (map (fn [setting color] [:rect setting {:color color}])
                               domain pal))
        spec {:x-axis {:tick-fmatter str :label "Settings"}
              :y-axis {:tick-formatter millions-formatter #_"%,.0f" :label "Cost £ Millions"}
              :legend {:label "Calendar Year" :font "Open Sans" :font-style nil :font-size 50 :legend-spec legend-spec}
              :title {:label "SEND Cost for Each Setting" :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
              :size {:width 1024 :height 768}
              :series series}]
    (plot/show (chart spec))))


(defn settings-population-grouped-bar-chart [data]
  (let [domain (into (sorted-set) (map (fn [d] (:calendar-year d)) data))
        pal (color/palette-presets :tableau-20-2)
        series [[:stack-vertical [:bar (->> data
                                            (group-by :setting)
                                            (map (fn [[setting data]] [setting (mapv (fn [x] (:median x)) data)]))
                                            (into (sorted-map)))
                                  {:palette pal}]]]
        legend-spec (into []
                          (map (fn [setting color] [:rect setting {:color color}])
                               domain pal))
        spec {:x-axis {:tick-formatter str :label "Settings"}
              :y-axis {:tick-formatter "%,.0f" :label "Population"}
              :legend {:label "Calendar Year" :font "Open Sans" :font-style nil :font-size 50 :legend-spec legend-spec}
              :title {:label "SEND Population for Each Setting" :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
              :size {:width 1024 :height 768}
              :series series}]
    (plot/show (chart spec))))

(defn complete-row-for-domain [domain domain-key extract-fn data]
  (mapv
   (fn [setting]
     (if-let [d (some (fn [d] (when (= (domain-key d) setting) d)) data)]
       (extract-fn d)
       0))
   domain))

(defn settings-population-stacked-bar-chart [data]
  (let [domain (reverse (into (sorted-set) (map (fn [d] (:setting d)) data)))
        pal (color/palette-presets :tableau-20-2)
        series [[:stack-vertical [:sbar (->> data
                                             (group-by :calendar-year)
                                             (map (fn [[setting data-for-year]]
                                                    [setting
                                                     (complete-row-for-domain
                                                      domain
                                                      :setting
                                                      (fn [rec] (:median rec (:population rec)))
                                                      data-for-year)]))
                                             (into (sorted-map)))
                                  {:palette pal}]]]
        legend-spec (reverse
                     (into []
                           (map (fn [setting color] [:rect setting {:color color}])
                                domain pal)))
        spec {:x-axis {:tick-formatter str :label "Calendar Year"}
              :y-axis {:tick-formatter "%,.0f" :label "Population"}
              :legend {:label "Setting" :font "Open Sans" :font-style nil :font-size 50 :legend-spec legend-spec}
              :title {:label "Projected SEND Counts by Special Setting" :format {:font-size 24 :font "Open Sans Bold" :margin 36}}
              :size {:width 1024 :height 768}
              :series series}]
    (plot/show (chart spec))))


(comment


  ;; EXAMPLE
  (mapv
   (fn [setting]
     (let [data [{:setting "a" :median 1} {:setting "c" :pop 2}]]
       (if-let [d (some (fn [d] (when (= (:setting d) setting) d)) data)]
         (:pop d (:median d))
         0)))
   ["a" "b" "c"])
  [1 0 2]
  [1 0 2]

  (complete-row-for-domain ["a" "b" "c"] :setting (fn [d] (:pop d (:median d))) [{:setting "a" :median 1} {:setting "c" :pop 2}])
  [1 0 2]
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Design
;; Each series function should return a map of {:legend (with a legend spec) :series (with all the series generated}
;; then you can mapcat in a transducer and pull everything into the chart as needed. Just have to be careful about order.
;; it is probably:
(comment

  ;; series bit
  (into [[:grid]]
        (mapcat :series)
        seq-of-maps-of-series-and-legends)
  )
