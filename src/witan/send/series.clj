(ns witan.send.series
  (:require [clojure2d.color :as color]))

(defn maps->line [{:keys [x-key y-key color point stroke dash]} xs]
  (let [line-spec {:color (color/color color) :point {:type point :size 8} :stroke {:size (or stroke 4)}}
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
