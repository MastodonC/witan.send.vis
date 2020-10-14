(ns witan.send.vis.ingest.transitions
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [witan.send.vis.ingest :as ingest]))

(defn historical [transitions-file]
  (ingest/csv-> transitions-file
                (map #(-> %
                          (update :calendar-year ingest/->int)
                          (update :academic-year-1 ingest/->int)
                          (update :academic-year-2 ingest/->int)))))

(defn ->csv [file-name transitions]
  (let [header ["calendar-year" "setting-1" "need-1" "academic-year-1" "setting-2" "need-2" "academic-year-2"]
        extraction-fn (apply juxt (map keyword header))]
    (with-open [w (io/writer file-name)]
      (csv/write-csv w (into [header] (mapv extraction-fn transitions))))))

(defn transition-s1->census [{:keys [calendar-year setting-1 need-1 academic-year-1]}]
  {:anon-ref -1
   :calendar-year calendar-year
   :setting setting-1
   :need need-1
   :academic-year academic-year-1})

(defn transition-s2->census [{:keys [calendar-year setting-2 need-2 academic-year-2]}]
  {:anon-ref -1
   :calendar-year (inc calendar-year)
   :setting setting-2
   :need need-2
   :academic-year academic-year-2})

(defn transition->census-like [first-transition-year transition]
  (if (= first-transition-year (:calendar-year transition))
    [(transition-s1->census transition)
     (transition-s2->census transition)]
    [(transition-s2->census transition)]))

(defn ->census
  "Create a census shaped data structure for further charting output. It
  can never be a proper census file as we don't have the pseudo IDs
  that would allow us to trace longitudinally."
  [transitions]
  (let [first-transition-year (first (into (sorted-set) (map :calendar-year transitions)))]
    (into []
          (comp
           (mapcat (partial transition->census-like first-transition-year))
           (remove #(= (:setting %) "NONSEND"))
           (remove #(= (:need %) "NONSEND")))
          transitions)))


(defn total-population-per-calendar-year [transitions]
  (transduce
   (remove #(= (:setting %) "NONSEND"))
   (fn
     ([acc]
      (sort-by :calendar-year
               (into []
                     (map (fn [[calendar-year population]] {:calendar-year calendar-year
                                                            :population population}))
                     acc)))
     ([acc {:keys [calendar-year]}]
      (update acc calendar-year (fnil inc 0))))
   {}
   (->census transitions)))

(defn settings-counts-per-calendar-year [transitions]
  (transduce
   (remove #(= (:setting %) "NONSEND"))
   (fn
     ([acc]
      (sort-by :calendar-year
               (into []
                     (map (fn [[[calendar-year setting] population]] {:calendar-year calendar-year
                                                                      :setting setting
                                                                      :population population}))
                     acc)))
     ([acc {:keys [calendar-year setting]}]
      (update acc [calendar-year setting] (fnil inc 0))))
   {}
   (->census transitions)))

(defn needs-counts-per-calendar-year [transitions]
  (transduce
   (remove #(= (:need %) "NONSEND"))
   (fn
     ([acc]
      (sort-by :calendar-year
               (into []
                     (map (fn [[[calendar-year need] population]] {:calendar-year calendar-year
                                                                   :need need
                                                                   :population population}))
                     acc)))
     ([acc {:keys [calendar-year need]}]
      (update acc [calendar-year need] (fnil inc 0))))
   {}
   (->census transitions)))

(defn ay-counts-per-calendar-year [transitions]
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
   (->census transitions)))

(comment

  (def historical-transitions (historical "/home/bld/wip/witan.send/data/demo/data/transitions.csv"))

  (def settings-counts-cy (settings-counts-per-calendar-year historical-transitions))

  (def series )

  )
