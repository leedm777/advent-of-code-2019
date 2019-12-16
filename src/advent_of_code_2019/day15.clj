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

(def opposite
  {north south
   south north
   east west
   west east})

(defn init-robot
  [brain]
  {:brain brain
   :map {[0 0] :robot}
   :pos [0 0]
   :last-direction north
   :oxygen-system nil
   :path-home []})

(defn move
  [pos direction]
  (->> direction
       (delta-pos)
       (map vector pos)
       (map (partial apply +))))

(defn pick
  [s]
  (first (shuffle s)))

(defn choose-direction
  [robot]
  (let [neighbors (->> [north south east west]
                       (group-by #(get-in robot [:map (move (:pos robot) %)] :unknown)))
        {:keys [unknown blank]} neighbors]
    (cond
      (some? unknown) (pick unknown)
      (= 1 (count blank)) (first blank)
      true (pick (remove #(= % (opposite (:last-direction robot))) blank))))  )

(defn update-path
  [path pos]
  (conj (vec (take-while #(not= pos %) path)) pos))

(defn move-robot
  [robot direction]
  (let [{:keys [brain map pos path-home oxygen-system]} robot
        next-pos (move pos direction)
        brain (int-resume-input brain direction)
        [result brain] (int-read-output brain)]
    (case result
      ;; 0 hit wall
      0 (merge robot
               {:brain brain
                :map (assoc map next-pos :wall)})
      ;; 1 moved
      1 (merge robot
               {:brain brain
                :last-direction direction
                :pos next-pos
                :path-home (update-path path-home next-pos)
                :map (merge map {pos (case pos
                                       [0 0] :origin
                                       oxygen-system :oxygen-system
                                       :blank)
                                 next-pos :robot})
                })
      ;; 2 found oxygen system
      2 (merge robot
               {:brain brain
                :last-direction direction
                :pos next-pos
                :path-home (update-path path-home next-pos)
                :map (merge map {pos :blank
                                 next-pos :oxygen-system})
                :oxygen-system next-pos}))))

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
                                   :origin \+
                                   :unvisited \ ))))))))

(defn find-oxygen-system
  [robot]
  (draw-map robot)
  (if (:oxygen-system robot)
    (count (:path-home robot))
    (recur (move-robot robot (choose-direction robot)))))

(def turn-left
  {north west
   west south
   south east
   east north})

(defn choose-full-map-direction
  [robot]
  (let [{:keys [last-direction map pos]} robot
        next-directions (mapv #(inc (mod % 4)) (range last-direction (+ last-direction 4)))
        next-directions (filterv #(not= :wall (get map (move pos %))))
        ]
    (first next-directions)))

(defn find-full-map
  [robot]
  (draw-map robot)
  (recur (move-robot robot (choose-direction robot))))

(defn solve
  [input]
  (let [program (int-parse input)
        brain (int-code program)
        robot (init-robot brain)]
    {;;:oxygen-system (find-oxygen-system robot)
     :time-to-refill (find-full-map robot)})  )

(def full-map
  "
 # ##### ####### ########### ######### #
#.#.....#.......#...........#.........#.#
#.#.#.###.#####.#.#####.#####.#####.#.#.#
#.#.#.....#.....#.#...#.......#.....#.#.#
#.#.#######.#####.#.#.#######.#.#####.#.#
#.#.....#...#.....#.#.#.....#.#...#.#...#
#.#####.#.###.#####.#.#.###.#.###.#.###.#
#...#...#...#.#.....#.#.#...#...#.#...#.#
#.###.#####.#.#.#######.###.#####.#.###.#
#.....#...#...#.......#...#.....#.#.....#
#.#######.###########.###.#####.#.#.####
#...#.............#...#...#...#...#.#...#
 ##.###.#.#######.#.#.#.###.#.#####.###.#
#.......#.#.....#.#.#.#.#.#.#.#...#.....#
#.#########.###.#.#.#.#.#.#.#.#.#.#####.#
#...#.......#.#.#...#.#.#...#.#.#...#...#
 ##.#.#######.#.#.#####.#.###.#.#.###.##
#...#.#...#.#...#.#...#.#...#...#...#...#
 ####.#.#.#.#.#####.#.#.#.#########.###.#
#.....#.#...#.#.....#.#.#.#.......#.#...#
#.#####.#####.###.###.#.#.#.#####.#.#.##
#.#.#.......#.#...#+#...#.#.#.....#.#.#.#
#.#.#.#####.#.#.#.#.#######.#.#####.#.#.#
#.#...#...#.....#.#...#.....#...#.#...#.#
#.#####.#.###########.#.#######.#.#.###.#
#.......#.#...........#.#.....#.#.#...#.#
#.#######.#.###########.#.###.#.#.###.#.#
#.......#.#...#...........#...#.#...#.#.#
 ########.###.#############.###.###.#.#.#
#...#...#.#...#.......#.....#...#...#.#.#
#.#.#.#.#.#.###.#####.#.#####.###.###.#.#
#.#...#.#.#.#...#...#...#.#...#.......#.#
#.#####D#.#.#.###.#######.#.###.#######.#
#.#...#.#.#...#.......#.....#...#.......#
#.#.###.#.#########.#.#.#######.#.#####.#
#.#.#.......#.......#.#.......#...#.....#
#.#.#.#######.#.#############.#####.####
#.#.....#...#.#.........#...#...#...#...#
#.#######.#.#.#########.#.#.###.#.###.#.#
#.........#...........#...#.....#.....#.#
 ######### ########### ### ##### ##### # ")
