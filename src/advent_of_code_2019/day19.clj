(ns advent-of-code-2019.day19
  (:require [advent-of-code-2019.int-code :refer :all]
            [advent-of-code-2019.util :refer :all]
            [clojure.string :as s]))

(def full-range (vec (for [x (range 0 50)
                           y (range 0 50)]
                       [x y])))

(defn test-coords
  [drone pos]
  (let [drone (int-resume-input drone pos)
        [pulled] (int-read-output drone)]
    pulled))

(defn full-map
  [drone]
  (for [pos full-range]
    [pos (test-coords drone pos)]))

(defn count-pulls
  [m]
  (->> m
       (map second)
       (reduce +)))

(defn solve
  [input]
  (let [program (int-parse input)
        drone (int-code program)
        m (full-map drone)]
    (println (render-map (apply conj {} m) #(case %
                                              0 \.
                                              1 \#)))
    {:full-map (count-pulls m)}))
