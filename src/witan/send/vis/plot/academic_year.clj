(ns witan.send.vis.plot.academic-year
  (:require [witan.plot.chart :as chart]
            [witan.plot.colors :as pc]
            [witan.plot.excel :as xl]
            [witan.plot.series :as ps]))

;; This has dummy values as there aren't good defaults and we want to
;; see them when they come through.
(def base-spec
  {::xl/tab-name        "Puddles and Ducks"
   ::ps/series-key      :academic-year
   ::ps/x-key           :calendar-year
   ::ps/actual-key      :actual
   ::ps/series-specs    {"Puddle" {:color pc/blue :shape \O :legend-shape \O}
                         "Duck"   {:color pc/orange :shape \V :legend-shape \A}}
   ;; FIXME: When default 0 charts work
   ;; ::ps/domain-map      (into (sorted-map)
   ;;                            (sequence
   ;;                             (map (fn [y z] [y z]))
   ;;                             (range 2018 2030)
   ;;                             (repeat 0)))
   ::chart/title        {::chart/label "Puddles and Ducks"}
   ::chart/legend-label "Puddles and Ducks"
   ::chart/y-axis       {::chart/tick-formatter int
                         ::chart/label          "Population"}
   ::chart/x-axis       {::chart/tick-formatter int
                         ::chart/label          "Calendar Years"}
   ::xl/xl-header-keys  [:academic-year :calendar-year]
   ::xl/xl-data-keys    [:actual :min :low-95pc-bound :q1 :median :q3 :high-95pc-bound :max]
   ::xl/header          ["NCY" "Calendar Year" "Actual" "Min" "Low 95% Bound" "Q1" "Median" "Q3" "High 95% Bound" "Max"]})

;; ::ps/data expects a map of
;; { :academic-year 17, :calendar-year 2029 } = { :min 1.0, ... }
;; It will plot whatever it is given in data, so filter before hand

(defn chart-spec [spec data]
  (let [spec' (as-> spec cs
                (assoc cs ::ps/data data)
                (merge cs (ps/line-ci-series-missing-is-0 cs)))]
    (when (chart/has-series? spec') ;; FIXME - should this return null?
      (merge spec' (chart/zero-y-index spec')))))
