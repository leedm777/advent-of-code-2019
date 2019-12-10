(ns advent-of-code-2019.day10
  (:require [clojure.string :as s]
            [advent-of-code-2019.int-code :refer :all]))

(defn find-asteroids-in-row
  [row]
  (->> row
       (map-indexed vector)
       (filter (fn [[idx c]] (= c \#)))
       (map (fn [[x]] x))))

(defn find-asteroids
  [star-map]
  (->> star-map
       (map find-asteroids-in-row)
       (map-indexed (fn [idx row] (map vector row (repeat idx))))
       (apply concat)))

(defn count-visible
  [asteroids [x y]]
  (->> asteroids
       (map (fn [[a-x a-y]]
              (let [delta-y (- y a-y)
                    delta-x (- x a-x)]
                [(neg? delta-x)
                 (neg? delta-y)
                 (if (zero? delta-x)
                   :undefined
                   (/ delta-y delta-x))])))
       (set)
       (count)
       ))

(defn best-station
  [star-map]
  (let [asteroids (find-asteroids star-map)
        counter (map (fn [coords] [(count-visible asteroids coords) coords]) asteroids)
        best (apply max (map first counter))]
    (first (filter (fn [[x]] (= x best)) counter))))

(defn solve
  [input]
  (let [star-map (as-> input i
                   (s/trim i)
                   (s/split-lines i))]
    {:part1 (best-station star-map)
     :part2 :TODO}))
