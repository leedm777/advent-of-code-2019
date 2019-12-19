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

(defn fits-sleigh?
  [drone pos]
  ;; x  - pos has pull
  ;; r1 - (move pos [99 0]) has pull
  ;; d1 - (move post [0 99]) has pull
  ;; r2 - (move pos [100 0]) has no pull
  ;; d2 - (move post [0 100]) has no pull
  ;; slope of beam is roughly [5 4]


  ;; find spot where x, r2, d2 all have pull
  ;; move left until d1/d2 are good
  ;; move up until r1/r2 are good
  ;; repeat moving left and up until all points are good
  )

(defn solve
  [input]
  (let [program (int-parse input)
        drone (int-code program)
        m (full-map drone)]
    (println (render-map (apply conj {} m) #(case %
                                              0 \.
                                              1 \#)))
    {:full-map (count-pulls m)}))
