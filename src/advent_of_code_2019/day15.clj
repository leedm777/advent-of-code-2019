(ns advent-of-code-2019.day15
  (:require [advent-of-code-2019.int-code :refer :all]
            [clojure.string :as s]))

(def north 1)
(def south 2)
(def east 3)
(def west 4)

(def delta-pos
  {north [0 1]
   south [0 -1]
   east [1 0]
   west [-1 0]})

(def hit-wall 0)
(def moved 1)
(def found-oxygen-system 2)

(defn init-robot
  [brain]
  {:brain brain
   :map {[0 0] :robot}
   :pos [0 0]
   :oxygen-system nil})

(defn move
  [pos direction]
  (->> direction
       (delta-pos)
       (map vector pos)
       (map (partial apply +))))

(defn move-robot
  [robot direction]
  (let [{:keys [brain map pos]} robot
        next-pos (move pos direction)
        brain (int-resume-input brain direction)
        [result brain] (int-read-output brain)]
    (case result
      ;; 0 hit wall
      0      (merge robot
                    {:brain brain
                     :map (assoc map next-pos :wall)})
      ;; 1 moved
      1 (merge robot
               {:brain brain
                :pos next-pos
                :map (merge map {pos :blank
                                 next-pos :robot})
                })
      ;; 2 found oxygen system
      2 (merge robot
               {:brain brain
                :pos next-pos
                :map (merge map {pos :blank
                                 next-pos :oxygen-system})
                :oxygen-system next-pos})
      )))

(defn draw-map
  [robot]
  (let [{:keys [map]} robot
        min-x (apply min (mapv first (keys map)))
        max-x (apply max (mapv first (keys map)))
        min-y (apply min (mapv second (keys map)))
        max-y (apply max (mapv second (keys map)))]
    (print (str (char 27) "[2J") ; clear screen
           (str (char 27) "[;H") ; move cursor to the top left corner of the screen
           )
    (println (s/join "\n"
                     (for [y (range max-y (dec min-y) -1)]
                       (s/join (for [x (range min-x (inc max-x))]
                                 (case (get map [x y] :unvisited)
                                   :blank \.
                                   :wall \#
                                   :robot \D
                                   :oxygen-system \O
                                   :unvisited \ ))))))))

(defn find-oxygen-system
  [robot]
  (draw-map robot)
  (if-let [oxygen-system (:oxygen-system robot)]
    oxygen-system
    (recur (move-robot robot (first (shuffle [north south east west]))))))

(defn solve
  [input]
  (let [program (int-parse input)
        brain (int-code program)
        robot (init-robot brain)]
    {:oxygen-system (find-oxygen-system robot)})  )
