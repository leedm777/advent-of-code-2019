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

(defn parse-folded-locs
  [input]
  (->> input
       (parse-map)
       (filter #(= bug (second %)))
       (map (fn [[[x y]]] [[0 x y] bug]))
       (into (sorted-map))))

(def folded-dirs
  [[0 0 -1]
   [0 0 1]
   [0 -1 0]
   [0 1 0]])

(defn warp-edges
  [[level pos-x pos-y] [level x y :as neighbor]]
  (cond
    ;; edges neighbor the level below
    (= y -1) [[(dec level) 2 1]]
    (= y 5) [[(dec level) 2 3]]
    (= x -1) [[(dec level) 1 2]]
    (= x 5) [[(dec level) 3 2]]
    ;; center spots neighbor the level above
    (= [x y] [2 2]) (cond
                      (= pos-x 1) (->> (range 0 5) (mapv #(vector (inc level) 0 %)))
                      (= pos-x 3) (->> (range 0 5) (mapv #(vector (inc level) 4 %)))
                      (= pos-y 1) (->> (range 0 5) (mapv #(vector (inc level) % 0)))
                      (= pos-y 3) (->> (range 0 5) (mapv #(vector (inc level) % 4))))
    ;; everything else is normal
    :else [neighbor]))

(defn folded-neighbors
  [pos]
  ;; [0 *], [4 *], [* 0], [* 4] neighbor (dec level)
  ;; [2 1], [2 3], [1 2], [3 2] neighbor (inc level)
  (->> folded-dirs
       (map #(pos-move pos %))
       (mapcat (partial warp-edges pos))))


(defn solve
  [input]
  {:biodiversity (->> input
                      (parse-locs)
                      (first-cycle)
                      (biodiversity))})
