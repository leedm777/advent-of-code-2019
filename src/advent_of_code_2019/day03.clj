(ns advent-of-code-2019.day03
  (:require [clojure.string :as s]))

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

(defn steps-to-closest-intercept
  [& turns]
  (let [boards (map plot-all turns)
        intercepts (->> boards
                        (map :points)
                        (apply clojure.set/intersection))
        steps (map :steps boards)
        intercepted-steps (map #(select-keys % intercepts) steps)
        distances (vals (apply merge-with + intercepted-steps))
        ]
    (first (sort distances))))

(defn solve
  [input]
  (let [turns (->> input
                   (s/split-lines)
                   (map #(s/split % #",")))]
    {:closest (apply distance-to-closest-intercept turns)
     :fewest-steps (apply steps-to-closest-intercept turns)}))
