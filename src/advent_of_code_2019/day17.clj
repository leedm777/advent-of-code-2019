(ns advent-of-code-2019.day17
  (:require [clojure.string :as s]
            [advent-of-code-2019.int-code :refer :all]))

(def up [0 -1])
(def down [0 1])
(def left [-1 0])
(def right [1 0])

(def all-dirs [up down left right])

(def scaffold \#)
(def open \.)
(def bot #{\^ \v \< \>})

(defn move
  [pos dir]
  (mapv + pos dir))

(defn plot-line
  [y line]
  (->> line
       (map-indexed (fn [x ch]
                      (if (s/includes? "#^v<>" (str ch))
                        [[x y] ch])))
       (mapcat identity)))

(defn plot-map
  [screen]
  (->> screen
       (map-indexed plot-line)
       (mapcat identity)
       (filter some?)
       (apply hash-map)))

(defn find-intersections
  [plot]
  (->> plot
       (filter (fn [[pos ch]]
                 (->> all-dirs
                      (mapv #(move pos %))
                      (every? #(contains? plot %)))))))

(defn sum-alignment-params
  [intersections]
  (->> intersections
       (map first)
       (map (fn [[x y]] (* x y)))
       (apply +)))

(defn init-robot
  [program]
  (let [brain (int-code program)
        [screen-ascii brain] (int-read-all-output brain)
        screen (s/split-lines (s/join (map char screen-ascii)))]
    {:brain brain
     :screen screen
     :map (plot-map screen)}))

(defn solve
  [input]
  (let [program (int-parse input)
        robot (init-robot program)]
    { :sum-alignment-params (->> robot
                                 (:map)
                                 (find-intersections)
                                 (sum-alignment-params))}))

(def full-map
  ["....#########......................................"
   "....#.......#......................................"
   "....#.......#......................................"
   "....#.......#......................................"
   "....#.......#......................................"
   "....#.......#......................................"
   "....###########...................................."
   "............#.#...................................."
   "............#.#...................................."
   "............#.#...................................."
   "............#######................................"
   "..............#...#................................"
   "..............#...#.....###########.........#######"
   "..............#...#.....#.........#.........#.....#"
   "..............#...#.....#.........#.........#.....#"
   "..............#...#.....#.........#.........#.....#"
   "..........###########...#.........#.........#.....#"
   "..........#...#...#.#...#.........#.........#.....#"
   "..........#...###########.........#.........#.....#"
   "..........#.......#.#.............#.........#.....#"])
