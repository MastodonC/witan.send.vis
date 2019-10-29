(ns witan.send.vis.ingest.transitions
  (:require [witan.send.vis.ingest.transitions :as sut]
            [clojure.test :as t]))

(t/deftest census-test
  (t/testing "Can convert transitions to a census"
    (t/is (= [{ :calendar-year 2013, :setting "NONSEND", :need "NONSEND", :academic-year -3 }
              { :calendar-year 2014, :setting "A", :need "B", :academic-year -2 }
              { :calendar-year 2015, :setting "C", :need "D", :academic-year -1 }]
             (let [t [{:calendar-year 2013 :setting-1 "NONSEND" :need-1 "NONSEND" :academic-year-1 -3 :setting-2 "A" :need-2 "B" :academic-year-2 -2}
                      {:calendar-year 2014 :setting-1 "A" :need-1 "B" :academic-year-1 -2 :setting-2 "C" :need-2 "D" :academic-year-2 -1}]]
               (sut/->census t))))))
