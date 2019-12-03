(ns advent-of-code-2019.day03
  (:require [clojure.string :as s]))

(def empty-board { :points #{}, :pos [0 0]})

(defn plot
  ([path-segment]
   (plot empty-board path-segment))

  ([{:keys [points pos]} path-segment]
   (let [dir (first path-segment)
         distance (Integer. (s/join (rest path-segment)))
         [x y] pos
         new-points (case dir
                      \U (->> (range (inc y) (+ y distance 1))
                              (map #(vector x %)))
                      \D (->> (range (dec y) (- y distance 1) -1)
                              (map #(vector x %)))
                      \R (->> (range (inc x) (+ x distance 1))
                              (map #(vector % y)))
                      \L (->> (range (dec x) (- x distance 1) -1)
                              (map #(vector % y))))]
     { :pos (last new-points), :points (into points new-points) })))

(defn plot-all
  [path-segments]
  (reduce plot
          empty-board
          path-segments))

(defn intercepts
  [& turns]
  (->> turns
       (map plot-all)
       (map :points)
       (apply clojure.set/intersection)))

(defn distance-to-closest-intercept
  [& turns]
  (->> (apply intercepts turns)
       (map (fn [[x y]] (+ (Math/abs x) (Math/abs y))))
       (sort)
       (first)))


(defn solve
  [input]
  {:closest  (->> input
                  (s/split-lines)
                  (map #(s/split % #","))
                  (apply distance-to-closest-intercept))})
