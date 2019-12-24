(ns advent-of-code-2019.day24
  (:require [advent-of-code-2019.util :refer :all]))

(def bug \#)
(def space \.)

(defn count-neighbors
  [locs pos]
  (->> dirs
       (map (partial pos-move pos))
       (map #(get locs % space))
       (filter #(= bug %))
       (count)))

(defn next-bugs
  [locs]
  (into {} (mapv (fn [[pos tile]]
                   (let [num-neighbors (count-neighbors locs pos)]
                     (cond
                       (and (= tile bug) (not= num-neighbors 1)) [pos space]
                       (and (= tile space) (#{1 2} num-neighbors)) [pos bug]
                       :else [pos tile])))
                 locs)))

(defn bugs-seq
  [locs]
  (cons locs (lazy-seq (bugs-seq (next-bugs locs)))))

(defn first-cycle
  [locs]
  (reduce (fn [seen locs]
            (if (seen locs)
              (reduced locs)
              (conj seen locs)))
          #{}
          (bugs-seq locs)))

(defn parse-locs
  [input]
  (->> input
       (parse-map)
       (into {})))

(defn biodiversity
  [locs]
  (->> locs
       (filterv (fn [[loc tile]] (= tile bug)))
       (mapv first)
       (mapv (fn [[x y]] (+ x (* y 5))))
       (mapv pow2)
       (apply +)))

(defn solve
  [input]
  {:biodiversity (->> input
                      (parse-locs)
                      (first-cycle)
                      (biodiversity))})
