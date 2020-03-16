(ns witan.send.chart
  (:require [cljplot.build :as plotb]
            [cljplot.core :as plot]
            [cljplot.render :as plotr]
            [clojure.string :as s]
            [clojure2d.color :as color]))

(def orange (nth (color/palette-presets :tableau-20) 2))
(def blue (nth (color/palette-presets :tableau-20) 5))
(def green (nth (color/palette-presets :tableau-20) 4))
(def palette (color/palette-presets :tableau-20))
(def points [\s \^ \o])


(defn domain-colors-and-points [domain-key data]
  (let [domain (into (sorted-set) (map domain-key) data)]
    (into (sorted-map)
          (map (fn [academic-year color point]
                 [academic-year {:color color :point point}])
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
  (let [[t v] (get-in prepped-data [:extents :y 0])]
    (if (= :numerical t)
      (plotb/update-scale prepped-data :y :domain [0 (second v)])
      prepped-data)))

(defn zero-y-index [{:keys [x-axis y-axis legend title series size]}]
  (let [size (or size {:width 1539 :height 1037 :background (color/color :white)}) ;; 1539x1037 is almost exactly the right size to go into the slide
        title-format (or (:format title) {:font-size 36 :font "Open Sans Bold" :font-style :bold :margin 36})]
    (-> (apply plotb/series series)
        (plotb/preprocess-series)
        (plotb/update-scale :x :fmt (:tick-formatter x-axis))
        ;;(plotb/update-scale :x :ticks 10)
        (plotb/update-scale :y :fmt (:tick-formatter y-axis))
        (zero-index-numerical-y-axes)
        (plotb/add-axes :bottom {:ticks {:font-size 24 :font-style nil}})
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
          [:line legend-label
           {:color color :shape shape :stroke {:size 4} :font "Open Sans" :font-size 36}])
    legend-spec))

(defn chart-spec-rf [chart-spec]
  (fn
    ([] chart-spec)
    ([a] a) ;; put chartingf and title->filename here and return a map with the {:chart foo :filename bar :chart-spec chart-spec} here?
    ([a {:keys [color shape legend-label data] :as series-spec}]
     (-> a
         (update :series conj data)
         (update-in [:legend :legend-spec] add-legend series-spec)))))

(defn save-chart-by-title [prefix chartingf chart-spec]
  (plot/save (chartingf chart-spec) (str prefix (title->filename (-> chart-spec :title :label))))
  chart-spec)

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
