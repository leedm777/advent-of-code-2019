(ns advent-of-code-2019.day03
  (:require [clojure.string :as s]
            [clojure.set]))

(def empty-board { :points #{}, :pos [0 0], :ctr 0, :steps {}})

(defn plot
  ([path-segment]
   (plot empty-board path-segment))

  ([{:keys [points pos ctr steps]} path-segment]
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
     {:pos (last new-points)
      :points (into points new-points)
      :ctr (+ ctr distance)
      :steps (into steps (map vector new-points (range (inc ctr) (+ ctr distance 1))))})))

(defn plot-all
  [path-segments]
  (reduce plot
          empty-board
          path-segments))

(defn find-intercepts
  [& boards]
  (->> boards
       (map :points)
       (apply clojure.set/intersection)))

(defn distance-to-closest-intercept
  [& turns]
  (->> turns
       ;; plot all the wirings
       (map plot-all)
       ;; find all the intercepts
       (apply find-intercepts)
       ;; compute Manhattan distance to each intercept
       (map (fn [[x y]] (+ (Math/abs x) (Math/abs y))))
       ;; find the shortest one
       (sort)
       (first)))

(defn steps-to-closest-intercept
  [& turns]
  (let [
        ;; plot all the wirings
        boards (map plot-all turns)
        ;; find all the intercepts
        intercepts (apply find-intercepts boards)
        steps (map :steps boards)
        ;; find the steps to each intercepts
        intercepted-steps (map #(select-keys % intercepts) steps)
        ;; sum all the steps to each intercept
        steps-to-intercepts (vals (apply merge-with + intercepted-steps))
        ]
    ;; find the shortest one
    (first (sort steps-to-intercepts))))

(defn solve
  [input]
  (let [turns (->> input
                   (s/split-lines)
                   (map #(s/split % #",")))]
    {:closest (apply distance-to-closest-intercept turns)
     :fewest-steps (apply steps-to-closest-intercept turns)}))
