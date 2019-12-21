(ns advent-of-code-2019.day20
  (:require [clojure.string :as s]
            [advent-of-code-2019.util :refer :all]))

(defn graph-portals
  [[nodes portals]]
  (let [graph (graph-maze (mapv first nodes))
        portals (locate-portals (mapv second nodes))]
    {:graph graph
     :entrance (get portals "AA")
     :exit (get portals "ZZ")}))

(defn parse-donut
  [input]
  (->> input
       (parse-map)
       (filter (fn [[k ^Character v]] (cond
                         (Character/isUpperCase v) true
                         (= v \.) true
                         true false)))
       (sort-by (fn [[kv v]] v))
       (partition-by (fn [[k v]] (= v \.)))
       (graph-portals)))

(defn solve
  [input]
  (let [donut (parse-donut input)]
    donut))
