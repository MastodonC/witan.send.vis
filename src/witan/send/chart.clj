(ns witan.send.chart
  (:require [cljplot.build :as plotb]
            [cljplot.config :as cfg]
            [cljplot.core :as plot]
            [cljplot.render :as plotr]
            [clojure.string :as s]
            [clojure2d.color :as color]
            [dk.ative.docjure.spreadsheet :as xl]
            [witan.send.series :as wss])
  (:import javax.imageio.ImageIO
           java.awt.image.BufferedImage
           java.io.ByteArrayOutputStream
           org.apache.poi.ss.usermodel.Workbook))

(def orange (nth (color/palette-presets :tableau-20) 2))
(def blue (nth (color/palette-presets :tableau-20) 5))
(def green (nth (color/palette-presets :tableau-20) 4))
(def palette (color/palette-presets :tableau-20))
(def points [\V \\
             \^ \|
             \O \/
             \o \A
             \> \x
             \v \S
             \{ \s
             \< \}
             \-])


(defn domain-colors-and-points
  "Generate colours and shapes for each setting year so we have
  something consistent"
  [domain-key data]
  (let [domain (into (sorted-set) (map domain-key) data)]
    (into (sorted-map)
          (map (fn [domain-value color point]
                 [domain-value {:color color :point point}])
               domain
               (cycle palette)
               (cycle points)))))

(defn title->filename [title]
  (s/lower-case (str (s/replace title " " "_") ".png")))

(defn free-y [{:keys [x-axis y-axis legend title series size]}]
  (let [size (or size {:width 1024 :height 768 :background (color/color :white)})
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
  (let [[t [_bottom top]] (get-in prepped-data [:extents :y 0])]
    (if (= :numerical t)
      (plotb/update-scale prepped-data :y :domain [0 (+ (* 0.025 top) top)])
      prepped-data)))

(defn update-chart-y-axis-ticks [chart-spec series]
  (let [[t [_bottom top]] (get-in chart-spec [:extents :y 0])]
    (if (and (= :numerical t) (< top 10))
      (plotb/update-scale chart-spec :y :domain [0 10])
      chart-spec)))

(defn update-chart-x-axis-ticks [chart-spec series]
  (let [x-count (->> series
                     (mapcat (fn [x] (second x)))
                     (map (fn [y] (first y)))
                     (into #{})
                     count)]
    (if (< x-count 10)
      (plotb/update-scale chart-spec :x :ticks x-count)
      chart-spec)))

(defn zero-y-index [{:keys [x-axis y-axis legend title series size]}]
  (let [_config (swap! cfg/configuration
                       (fn [c]
                         (-> c
                             (assoc-in [:legend :font] "Open Sans Bold")
                             (assoc-in [:legend :font-size] 24))))
        size (or size {:width 1539 :height 1037 :background (color/color :white)}) ;; 1539x1037 is almost exactly the right size to go into the slide
        title-format (or (:format title) {:font-size 36 :font "Open Sans Bold" :font-style :bold :margin 36})]
    (-> (apply plotb/series (into [[:grid]] series))
        (plotb/preprocess-series)
        (plotb/update-scale :x :fmt (:tick-formatter x-axis))
        (update-chart-x-axis-ticks series) ;; str x-axes breaks here
        (plotb/update-scale :y :fmt (:tick-formatter y-axis))
        (zero-index-numerical-y-axes)
        (update-chart-y-axis-ticks series)
        (plotb/add-axes :bottom {:ticks {:font-size 24 :font-style nil}}) ;; str x-axes breaks here
        (plotb/add-axes :left {:ticks {:font-size 24 :font-style nil}})
        (plotb/add-label :bottom (:label x-axis) {:font-size 36 :font "Open Sans" :font-style nil})
        (plotb/add-label :left (:label y-axis) {:font-size 36 :font "Open Sans" :font-style nil})
        (plotb/add-label :top (:label title) title-format)
        (plotb/add-legend (:label legend) (:legend-spec legend))
        (plotr/render-lattice size))))

;; FIXME: Get a passed in dash to work here
;; we only add to the legend if it is a line with a point
(defn add-legend [legend-spec {:keys [legend-label color shape hide-legend]}]
  (if (and shape (not hide-legend))
    (conj legend-spec
          [:shape legend-label {:color color :shape shape :size 15 :stroke {:size 4.0}}]
          #_[:line legend-label
             {:color color :shape shape :stroke {:size 4} :font "Open Sans" :font-size 36}])
    legend-spec))

(def histogram-base-legend
  [[:line "Historical"
    {:color :black :stroke {:size 4} :font "Open Sans" :font-size 36}]
   [:line "Projected"
    {:color :black :stroke {:size 4 :dash [2.0]} :font "Open Sans" :font-size 36}]
   [:rect "Interquartile range"
    {:color (color/set-alpha (color/color :black) 50)}]
   [:rect "90% range"
    {:color (color/set-alpha (color/color :black) 25)}]])

(def historical-projected-legend
  [[:line "Historical"
    {:color :black :stroke {:size 4} :font "Open Sans" :font-size 36}]
   [:line "Projected"
    {:color :black :stroke {:size 4 :dash [2.0]} :font "Open Sans" :font-size 36}]])

(defn base-chart-spec
  [{:keys [title chartf x-tick-formatter y-tick-formatter x-axis-label y-axis-label legend-label legend-spec]
    :or {chartf zero-y-index
         x-tick-formatter str
         y-tick-formatter int
         x-axis-label "Calendar Year"
         y-axis-label "Population"
         legend-label "Data Sets"
         legend-spec histogram-base-legend}}]
  {:title  {:label title}
   :x-axis {:tick-formatter x-tick-formatter :label x-axis-label :format {:font-size 24 :font "Open Sans"}}
   :y-axis {:tick-formatter y-tick-formatter :label y-axis-label :format {:font-size 24 :font "Open Sans"}}
   :legend {:label legend-label
            :legend-spec legend-spec}
   :chartf chartf})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Old chart code

(defn filter-serie-data [domain-key domain-value serie-spec]
  (let [orig-data (:data serie-spec)
        filtered-data (filter
                       (fn [d]
                         (= (domain-key d) domain-value))
                       orig-data)]
    (assoc serie-spec :data filtered-data)))


(defn all-domain-xf [domain-key domain-value]
  (comp
   (map (partial filter-serie-data domain-key domain-value))
   (mapcat wss/serie-and-legend-spec)))

(defn multi-line-and-iqr-with-history-xf
  [{:keys [domain-key domain-values-lookup colors-and-points historical-counts projected-data y-key]}]
  (comp
   (mapcat (fn [domain-value]
             (if historical-counts
               [{:color (-> domain-value colors-and-points :color)
                 :legend-label (get domain-values-lookup domain-value domain-value)
                 :shape (-> domain-value colors-and-points :point)
                 :projection true
                 :hide-legend false
                 :data (filter
                        #(= (domain-key %) domain-value)
                        projected-data)}
                {:color (-> domain-value colors-and-points :color)
                 :legend-label (get domain-values-lookup domain-value domain-value)
                 :shape (-> domain-value colors-and-points :point)
                 :projection false
                 :hide-legend true
                 :y-key (or y-key :population)
                 :data (filter
                        #(= (domain-key %) domain-value)
                        historical-counts)}]
               [{:color (-> domain-value colors-and-points :color)
                 :legend-label (get domain-values-lookup domain-value domain-value)
                 :shape (-> domain-value colors-and-points :point)
                 :projection true
                 :hide-legend false
                 :data (filter
                        #(= (domain-key %) domain-value)
                        projected-data)}])))
   (mapcat wss/serie-and-legend-spec)))

(defn multi-line-actual-xf
  [{:keys [domain-key domain-values-lookup colors-and-points data y-key hide-legend]}]
  (comp
   (mapcat (fn [domain-value]
             [{:color (-> domain-value colors-and-points :color)
               :legend-label (get domain-values-lookup domain-value domain-value)
               :shape (-> domain-value colors-and-points :point)
               :projection false
               :hide-legend (or hide-legend false)
               :y-key (or y-key :population)
               :data (filter
                      #(= (domain-key %) domain-value)
                      data)}]))
   (mapcat wss/serie-and-legend-spec)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Refactored Charts below

(defn legend-shape [serie-shape]
  (case serie-shape
    \^ \v
    \A \V
    \v \^
    \V \A
    \\ \/
    \/ \\
    serie-shape))

(defn comparison-series-defs
  "Turn a single comparison def into a seq of series defs. One
  historical line, one projected line, one projected iqr, one
  projected 90%"
  [{:keys [legend-label color shape hide-legend
           projection-data
           historical-data historical-y-key]
    :or {hide-legend true} ;; FIXME This doesn't feel right
    :as comparison-def}]
  (into []
        (concat
         (when (seq projection-data)
           [;; projection median
            {:legend-label legend-label
             :color color
             :shape (legend-shape shape)
             :hide-legend false
             :data (wss/maps->line {:x-key :calendar-year
                                    :y-key :median
                                    :color color
                                    :point shape
                                    :dash [2.0]}
                                   projection-data)}
            ;; projection iqr
            {:legend-label legend-label
             :color color
             :hide-legend true
             :data (wss/maps->ci {:x-key :calendar-year
                                  :hi-y-key :q3
                                  :low-y-key :q1
                                  :color color}
                                 projection-data)}
            ;; projection 95%
            {:legend-label legend-label
             :color color
             :hide-legend true
             :data (wss/maps->ci {:x-key :calendar-year
                                  :hi-y-key :high-95pc-bound
                                  :low-y-key :low-95pc-bound
                                  :color color
                                  :alpha 25}
                                 projection-data)}])
         (when (seq historical-data)
           [;; history
            {:legend-label legend-label
             :color color
             :shape (legend-shape shape)
             :hide-legend hide-legend
             :data (wss/maps->line {:x-key :calendar-year
                                    :y-key historical-y-key
                                    :color color
                                    :point shape}
                                   historical-data)}]))))


(defn chart-spec-rf [{:keys [chartf title] :or {chartf zero-y-index} :as chart-spec}]
  (fn
    ([] chart-spec)
    ([a] {:chart-def a
          :chart-image (chartf a)
          :filename (title->filename (:label title))})
    ([a {:keys [color shape legend-label data] :as series-spec}]
     (-> a
         (update :series conj data)
         (update-in [:legend :legend-spec] add-legend series-spec)))))

(defn comparison-chart [{:keys [series] :as comparison-defs}]
  (transduce
   (mapcat comparison-series-defs)
   (chart-spec-rf
    (base-chart-spec
     (select-keys comparison-defs [:title :chartf
                                   :x-tick-formatter :y-tick-formatter
                                   :x-axis-label :y-axis-label
                                   :legend-label :legend-spec])))
   series))


(defn comparison-table
  "Turn comparison definition data into a vector of vectors for csv or excel output"
  [{:keys [series domain-key y-axis-label x-axis-label] :as comparison-defs}]
  (into [["label"
          (name domain-key)
          x-axis-label
          y-axis-label
          "min" "low 95pc bound" "q1" "median" "q3" "high 95pc bound" "max" "iqr"]]
        (comp
         (map (fn [{:keys [legend-label projection-data historical-data]}]
                {:historical-data (into [] (map (fn [record] (assoc record :label legend-label))) historical-data)
                 :projection-data (into [] (map (fn [record] (assoc record :label legend-label))) projection-data)}))
         (mapcat (juxt :historical-data :projection-data))
         cat
         (map (juxt :label
                    domain-key
                    :calendar-year
                    :population
                    :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max :iqr)))
        series))

(defn comparison-chart-and-table [comparison-defs]
  (try
    (-> comparison-defs
        (assoc :chart (comparison-chart comparison-defs))
        (assoc :table (comparison-table comparison-defs)))
    (catch Exception e
      (throw (ex-info "Unable to create comparison table and charts" {:comparison-defs comparison-defs} e)))))

(defn domain-charts [{:keys [domain-key chart-base-def serie-base-def colors-and-points historical-data projection-data]} titles-and-sets]
  (into []
        (comp
         (map (fn [[title domain-values]]
                (assoc
                 chart-base-def
                 :title title
                 :series
                 (into []
                       (comp
                        (map (fn [domain-value]
                               (merge serie-base-def
                                      {:legend-label (get-in chart-base-def [:domain-values-lookup domain-value] domain-value)
                                       :color (-> domain-value colors-and-points :color)
                                       :shape (-> domain-value colors-and-points :point)
                                       :projection-data (into [] (filter #(= domain-value (domain-key %))) projection-data)
                                       :historical-data (into [] (filter #(= domain-value (domain-key %))) historical-data)})))
                        (remove #(and (empty? (:historical-data %)) (empty? (:projection-data %)))))
                       domain-values))))
         (remove #(empty? (:series %)))
         (map comparison-chart-and-table))
        titles-and-sets))

(defn ->byte-array [^BufferedImage image]
  (with-open [out (java.io.ByteArrayOutputStream.)]
    (ImageIO/write image "png" out)
    (.toByteArray out)))

(defn ->workbook [comparison-defs]
  (let [wb-data (into []
                      (mapcat (fn [cd]
                                [(:title cd)
                                 (:table cd)]))
                      comparison-defs)
        wb-charts (into []
                        (map (fn [cd]
                               [(:title cd)
                                (-> cd :chart :chart-image :buffer ->byte-array)]))
                        comparison-defs)
        wb (apply xl/create-workbook wb-data)]
    (run! (fn [[sheet-name img]]
            (try
              (let [ ;; int pictureIdx = wb.addPicture(bytes, Workbook.PICTURE_TYPE_JPEG);
                    pic-idx (.addPicture wb img Workbook/PICTURE_TYPE_PNG)
                    sheet (xl/select-sheet sheet-name wb)
                    helper (.getCreationHelper wb)
                    drawing (.createDrawingPatriarch sheet)
                    anchor (.createClientAnchor helper)
                    _ (.setCol1 anchor 14)
                    _ (.setRow1 anchor 2)
                    ;; Picture pict = drawing.createPicture(anchor, pictureIdx);
                    ;; pict.resize();
                    pict (.createPicture drawing anchor pic-idx)]
                (.resize pict))
              (catch Exception e
                (throw (ex-info (str "Failed to create sheet. " sheet-name) {:sheet-name sheet-name} e)))))
          wb-charts)
    wb))

(defn save-chart-by-title [prefix comparison-def]
  (plot/save (-> comparison-def :chart :chart-image) (str prefix (title->filename (-> comparison-def :chart :chart-def :title :label))))
  comparison-def)

(defn save-workbook [file-name wb]
  (xl/save-workbook! file-name wb))

(defn insert-zero-counts
  ;; FIXME: This assumes it is for academic year.
  ([census-data counts]
   (into []
         (mapcat (fn [cy]
                   (map
                    (fn [ay] (if-let [ay-map (some (fn [m] (when (#{ay} (:academic-year m)) m)) (val cy))]
                               ay-map
                               {:calendar-year (:calendar-year (-> cy val first))
                                :academic-year ay
                                :population 0}))
                    (into (sorted-set)
                          (map :academic-year)
                          census-data))))
         (group-by :calendar-year counts)))
  ;; FIXME: Below is more righterer
  ([census-data counts domain-key]
   (into []
         (mapcat (fn [cy]
                   (map
                    (fn [domain-item] (if-let [domain-map (some (fn [m] (when (#{domain-item} (domain-key m)) m)) (val cy))]
                                        domain-map
                                        {:calendar-year (:calendar-year (-> cy val first))
                                         domain-key domain-item
                                         :population 0}))
                    (into (sorted-set)
                          (map domain-key)
                          census-data))))
         (group-by :calendar-year counts))))
