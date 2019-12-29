(ns advent-of-code-2019.day19
  (:require [advent-of-code-2019.int-code :refer :all]
            [advent-of-code-2019.util :refer :all]
            [clojure.string :as s]))

(def full-range (vec (for [x (range 0 50)
                           y (range 0 50)]
                       [x y])))

(defn has-pull?
  [drone pos]
  (let [drone (int-resume-input drone pos)
        [pulled] (int-read-output drone)]
    (= 1 pulled)))

(defn full-map
  [drone]
  (for [pos full-range]
    [pos (has-pull? drone pos)]))

(defn count-pulls
  [m]
  (->> m
       (map second)
       (filter #(= true %))
       (count)))

(defn fits-sleigh?
  [drone pos]
  ;; x  - pos has pull
  ;; r1 - (move pos [99 0]) has pull
  ;; d1 - (move post [0 99]) has pull
  ;; r2 - (move pos [100 0]) has no pull
  ;; d2 - (move post [0 100]) has no pull
  (and (has-pull? drone pos)
       (has-pull? drone (pos-move pos [99 0]))
       (has-pull? drone (pos-move pos [0 99]))
       (not (has-pull? drone (pos-move pos [100 0])))
       (not (has-pull? drone (pos-move pos [0 100])))))

(defn find-rough-fit
  ([drone]
   ;;(find-rough-fit drone [49 40])
   [1319 1058])
  ([drone pos]
   (if-not (has-pull? drone pos)
     :lost
     (if (and (has-pull? drone (pos-move pos [99 0]))
              (has-pull? drone (pos-move pos [0 99])))
       pos
       (recur drone (pos-move pos [49 40]))))))

;; slope of beam is roughly [5 4]
;; find spot where x, r2, d2 all have pull
;; move left until d1/d2 are good
;; move up until r1/r2 are good
;; repeat moving left and up until all points are good
(defn find-sleigh
  [drone]
  (let [pos (find-rough-fit drone)]
    (loop [pos pos]
      (println pos)
      (println "  " (has-pull? drone pos))
      (println "  " (has-pull? drone (pos-move pos [100 0])))
      (println "  " (has-pull? drone (pos-move pos [0 100])))
      (println "  " (has-pull? drone (pos-move pos [99 0])))
      (println "  " (has-pull? drone (pos-move pos [0 99])))
      (cond
        (not (has-pull? drone pos)) :lost
        ;; move up
        (has-pull? drone (pos-move pos [100 0])) (recur (pos-move pos [0 -1]))
        ;; move left
        (has-pull? drone (pos-move pos [0 100])) (recur (pos-move pos [-1 0]))
        ;; move down
        (not (has-pull? drone (pos-move pos [99 0]))) (recur (pos-move pos [0 1]))
        ;; move up
        (not (has-pull? drone (pos-move pos [0 99]))) (recur (pos-move pos [1 0]))
        :else pos))))

(defn solve
  [input]
  (let [program (int-parse input)
        drone (int-code program)
        ;;m (full-map drone)
        ]
    ;;(println (render-map (apply conj {} m) #(case %
    ;;                                          0 \.
    ;;                                          1 \#)))
    {
     ;;:full-map    (count-pulls m)
     :find-sleigh (let [[x y] (find-sleigh drone)]
                    (+ y (* 10000 x)))}))
