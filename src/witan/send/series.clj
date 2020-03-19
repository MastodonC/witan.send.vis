(ns witan.send.series
  (:require [clojure2d.color :as color]))

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
