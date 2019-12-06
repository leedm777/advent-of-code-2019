(ns advent-of-code-2019.day06
  (:require [clojure.string :as s]))

(defn build-map
  ([orbits] (build-map orbits {}))
  ([[orbit & orbits] m]
   (if (nil? orbit)
     m
     (let [[center obj] (s/split orbit #"\)")]
       (recur orbits (assoc m obj center))))))

(defn count-orbits
  [m obj]
  (if (= obj "COM")
    0
    (inc (count-orbits m (m obj)))))

(defn total-orbits
  [orbits]
  (let [m (build-map orbits)]
    (->> m
         (keys)
         (map #(count-orbits m %))
         (apply +))))

(defn solve
  [input]
  (->> input
       (s/split-lines)
       ((fn [orbits] { :orbits (total-orbits orbits)}))))
