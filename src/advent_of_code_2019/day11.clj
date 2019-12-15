(ns advent-of-code-2019.day11
  (:require [clojure.string :as s]
            [advent-of-code-2019.int-code :refer :all]))

(def north [0 1])
(def south [0 -1])
(def east [1 0])
(def west [-1 0])

(def left {north west
           west south
           south east
           east north})

(def right {north east
            east south
            south west
            west north})

(defn init-robot
  [brain]
  {:position [0 0]
   :direction north
   :panels {}
   :brain brain})

(defn turn-left
  [robot]
  (let [direction (left (:direction robot))
        position (vec (map + (:position robot) direction))]
    (merge robot {:direction direction, :position position}))  )

(defn turn-right
  [robot]
  (let [direction (right (:direction robot))
        position (vec (map + (:position robot) direction))]
    (merge robot {:direction direction, :position position})))

(defn paint
  [robot color]
  (assoc-in robot [:panels (:position robot)] color))

(defn run-robot
  [robot]
  (if (int-halted? (:brain robot))
    robot
    (let [color (get-in robot [:panels (:position robot)] 0)
          brain (int-resume-input (:brain robot) [color])
          [color brain] (int-read-output brain)
          [direction brain] (int-read-output brain)
          turn (case direction
                 0 turn-left
                 1 turn-right)]
      (-> robot
          (assoc :brain brain)
          (paint color)
          (turn)
          (recur)))))

(defn draw-panels
  [panels]
  (let [coords (keys panels)
        min-x (apply min (map first coords))
        max-x (apply max (map first coords))
        min-y (apply min (map second coords))
        max-y (apply max (map second coords))]
    (for [y (range max-y (dec min-y) -1)] ;; I bet having [0 1] for north inverted my graph
      (apply str
             (for [x (range min-x (inc max-x))]
               (if (= (get panels [x y] 0) 1)
                 \u2588
                 \ ))))))

(defn solve
  [input]
  (let [program (int-parse input)
        brain (int-code program)
        robot (init-robot brain)
        part1-panels (-> (run-robot robot)
                         (:panels))]
    {
     :part1-reg (draw-panels part1-panels)
     :part1-count-panels (count part1-panels)
     :registration (-> robot
                       (assoc-in [:panels [0 0]] 1)
                       (run-robot)
                       (:panels)
                       (draw-panels))}))
