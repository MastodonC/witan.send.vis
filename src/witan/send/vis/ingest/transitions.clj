(ns witan.send.vis.ingest.transitions
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.set :as cs]
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

(defn transition-calendar-years [transitions]
  (into (sorted-set)
        (map :calendar-year)
        transitions))

(defn max-transitions-calendar-year [transitions]
  (apply max (transition-calendar-years transitions)))

(defn max-census-year-from-transitions [transitions]
  (inc (max-transitions-calendar-year transitions)))

(defn ->census
  "This is a one way, destructive process that turns transitions data
  into a yearly census format."
  [transitions]
  (let [max-calendar-year (max-transitions-calendar-year transitions)
        t1 (into []
                 (comp
                  (map #(select-keys % [:calendar-year :setting-1 :need-1 :academic-year-1]))
                  (map #(cs/rename-keys % {:setting-1 :setting :need-1 :need :academic-year-1 :academic-year})))
                 transitions)
        t2 (into []
                 (comp
                  (filter #(= (:calendar-year %) max-calendar-year))
                  (map #(select-keys % [:calendar-year :setting-2 :need-2 :academic-year-2]))
                  (map #(cs/rename-keys % {:setting-2 :setting :need-2 :need :academic-year-2 :academic-year}))
                  (map #(update % :calendar-year inc)))
                 transitions)]
    (->> (into t1 t2)
         (filter #(not= "NONSEND" (:need %))))))

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
