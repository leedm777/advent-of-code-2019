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

(defn relative-positions
  [asteroids [x y]]
  (->> asteroids
       (map (fn [[x_i y_i]]
              (let [delta-x (- x_i x)
                    delta-y (- y_i y)]
                {:pos [x_i y_i]
                 :delta-pos [delta-x delta-y]
                 ;; TODO - Use (Math/atan2 opp adj) to compute theta instead of
                 ;; quadrant and slope
                 ;;   - opp = (- x_i x)
                 ;;   - adj = (- y y_i)
                 :quadrant (cond
                             (and (>= delta-x 0) (< delta-y 0)) 1
                             (and (>= delta-x 0) (>= delta-y 0)) 2
                             (and (< delta-x 0) (>= delta-y 0)) 3
                             (and (< delta-x 0) (< delta-y 0)) 4)
                 :slope (cond
                          (and (zero? delta-x) (zero? delta-y)) 0
                          (and (zero? delta-x) (pos? delta-y)) Long/MAX_VALUE
                          (and (zero? delta-x) (neg? delta-y)) (inc Long/MIN_VALUE)
                          true (/ delta-y delta-x))
                 :distance (Math/sqrt (+ (* delta-x delta-x) (* delta-y delta-y)))})))))


(defn count-visible
  [asteroids [x y]]
  (->> (relative-positions asteroids [x y])
       (filter #(not (= (:delta-pos %) [0 0])))
       (map #(select-keys % [:slope :quadrant]))
       (set)
       ;;((fn [slopes] (println x y slopes) slopes))
       (count)))


(defn best-station
  [star-map]
  (let [asteroids (find-asteroids star-map)
        counter (map (fn [coords] [(count-visible asteroids coords) coords]) asteroids)
        best (apply max (map first counter))]
    (first (filter (fn [[x]] (= x best)) counter))))

(defn kill-order
  [star-map coords]
  (let [asteroids (find-asteroids star-map)
        rels (relative-positions asteroids coords)
        lines (->> rels
                   (filter #(not (= (:delta-pos %) [0 0])))
                   (group-by #(select-keys % [:slope :quadrant]))
                   (map (fn [[{:keys [quadrant slope]} v]] {:quadrant quadrant
                                                            :order (- slope)
                                                            :asteroids (sort-by :distance v)}))
                   (sort-by :order >)
                   (sort-by :quadrant)
                   (map :asteroids)
                   )]
    (loop [lines lines
           pre []]
      (let [next (map first lines)
            remaining (->> lines
                           (map rest)
                           (filter not-empty))]
        (if (empty? lines)
          pre
          (recur remaining (concat pre next)))))))

(defn solve
  [input]
  (let [star-map (as-> input i
                   (s/trim i)
                   (s/split-lines i))]
    {:part1 (best-station star-map)
     :part2 (let [[x y] (-> (kill-order star-map [20 20])
                            (nth (dec 200))
                            (:pos))]
              (+ (* x 100) y))}))
