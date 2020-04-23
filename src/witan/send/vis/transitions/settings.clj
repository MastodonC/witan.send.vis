(ns witan.send.vis.transitions.settings
  (:require [witan.send.chart :as wsc]))

(def base-comparison-serie-def
  {:historical-y-key :population})

(def lookup {})

(def base-comparison-chart-def
  {:legend-label "Settings"
   :hide-legend false
   :domain-key :academic-year
   :domain-values-lookup lookup
   :x-axis-label "Calendar Year" :x-tick-formatter int
   :y-axis-label "Population" :y-tick-formatter int
   :chartf wsc/zero-y-index})



