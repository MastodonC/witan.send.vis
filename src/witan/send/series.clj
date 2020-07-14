(ns witan.send.series
  (:require [clojure2d.color :as color]))

(defn data->line-series
  "data should be a vector of pairs of [x-axis-pt y-axis-pt]"
  [{:keys [color shape stroke dash data]}]
  (let [line-spec {:color (color/color color) :point {:type shape :size 15 :stroke {:size (or stroke 4)}} :stroke {:size (or stroke 4)}}
        line-spec (if dash (assoc-in line-spec [:stroke :dash] dash) line-spec)]
    [:line data line-spec]))

(defn legend-shape [serie-shape]
  (case serie-shape
    \^ \v
    \A \V
    \v \^
    \V \A
    \\ \/
    \/ \\
    serie-shape))

(defn data->line-legend [{:keys [color shape label]}]
  [:shape label {:color color :shape (legend-shape shape) :size 15 :stroke {:size 4.0}}])

(defn data->ribbon-series
  "high-data and low-data should be pairs of [x-axis-pt y-axis-pt]"
  [{:keys [color alpha high-data low-data]}]
  [:ci [high-data low-data]] {:color (color/color color (or alpha 50))})

(defn data->ribbon-legend [{:keys [color alpha label]}]
  [:rect label {:color (color/set-alpha (color/color (or color :black)) (or alpha 50))}])

(defn maps->line [{:keys [x-key y-key color point stroke dash]} xs]
  (let [line-spec {:color (color/color color) :point {:type point :size 15 :stroke {:size (or stroke 4)}} :stroke {:size (or stroke 4)}}
        line-spec (if dash (assoc-in line-spec [:stroke :dash] dash) line-spec)]
    [:line (into []
                 (map (fn [m]
                        (vector
                         (get m x-key) (get m y-key))))
                 xs)
     line-spec]))

(defn maps->ci [{:keys [x-key low-y-key hi-y-key color alpha]} xs]
  [:ci
   (vector
    (into []
          (map (fn [m]
                 (vector
                  (get m x-key) (get m hi-y-key))))
          xs)
    (into []
          (map (fn [m]
                 (vector
                  (get m x-key) (get m low-y-key))))
          xs))
   {:color (color/color color (or alpha 50))}])

;; FIXME: Not quite the right abstraction to roll up the underlying records yet
;; (defn maps->grouped-bar
;;   "summaryf is a function that takes: 
;;   group-domain-map [x-domain-key seq-of-maps-from-passed-in-data]
;;   and returns
;;   [x-domain-key vec-of-nums-to-chart]"
;;   [{:keys [x-key x-group-key y-key summaryf palette]} xs]
;;   (let [group-domain (into (sorted-set) (map x-group-key) xs)
;;         group-domain-map (into (sorted-map) (zipmap group-domain (repeat 0)))
;;         x-domain (into (sorted-set) (map x-key) xs)
;;         x-domain-map (into (sorted-map) (zipmap x-domain (repeat [])))
;;         data (transduce
;;               (map identity)
;;               (fn
;;                 ([] x-domain-map)
;;                 ([acc] (into (sorted-map)
;;                              summaryf
;;                              acc))
;;                 ([acc new] (update acc conj new)))
;;               xs)
;;         ]
;;     x-domain-map
;;     ;;[:stack-vertical [:bar data {:palette palette}]]
;;     ))

(defn serie-and-legend-spec [{:keys [color shape projection legend-label hide-legend y-key data] :as serie-spec}]
  (let [case' (case shape
                \^ \v
                \A \V
                \v \^
                \V \A
                \\ \/
                \/ \\
                shape)]
    (if projection
      [{:color color
        ;; fix the legend triangle
        :shape case' ;; (if (= shape \^) \v shape)
        :legend-label legend-label
        :hide-legend hide-legend
        :data (maps->line {:x-key :calendar-year
                           :y-key :median
                           :color color
                           :point shape
                           :dash [2.0]}
                          data)}
       {:color color
        :legend-label legend-label
        :hide-legend hide-legend
        :data (maps->ci {:x-key :calendar-year
                         :hi-y-key :q3
                         :low-y-key :q1
                         :color color}
                        data)}
       {:color color
        :legend-label legend-label
        :hide-legend hide-legend
        :data (maps->ci {:x-key :calendar-year
                         :hi-y-key :high-95pc-bound
                         :low-y-key :low-95pc-bound
                         :color color
                         :alpha 25}
                        data)}]
      [{:color color
        :shape case' ;; fix the legend triangle
        :legend-label legend-label
        :hide-legend hide-legend
        :data (maps->line {:x-key :calendar-year
                           :y-key y-key
                           :color color
                           :point shape}
                          data)}])))
