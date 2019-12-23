(ns advent-of-code-2019.day15
  (:require [advent-of-code-2019.int-code :refer :all]
            [clojure.string :as s]
            [advent-of-code-2019.util :refer :all]))

(def north 1)
(def south 2)
(def east 3)
(def west 4)

(def directions [north east south west])

(def delta-pos
  {north [0 1]
   south [0 -1]
   east  [1 0]
   west  [-1 0]})

(def opposite
  {north south
   south north
   east  west
   west  east})

(defn init-robot
  [brain]
  {:brain          brain
   :map            {[0 0] :robot}
   :pos            [0 0]
   :todo           [[0 0]]
   :last-direction north
   :oxygen-system  nil
   :path-home      []})

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
  ;; if pos != mapping-pos, and mapping-pos has unknown neighbors, move back
  ;; if pos has an unknown neighbor, pick the first and move there
  ;; if pos has no unknown neighbors, move in the direction of a pos with unknown neighbors
  (let [neighbors (->> [north east south west]
                       (group-by #(get-in robot [:map (move (:pos robot) %)] :unknown)))
        {:keys [unknown blank]} neighbors]
    (cond
      (some? unknown) (pick unknown)
      (= 1 (count blank)) (first blank)
      true (pick (remove #(= % (opposite (:last-direction robot))) blank)))))

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
                :moved false
                :map   (assoc map next-pos :wall)})
      ;; 1 moved
      1 (merge robot
               {:brain          brain
                :moved          true
                :last-direction direction
                :pos            next-pos
                :path-home      (update-path path-home next-pos)
                :map            (merge map {pos      (case pos
                                                       [0 0] :origin
                                                       oxygen-system :oxygen-system
                                                       :blank)
                                            next-pos :robot})
                })
      ;; 2 found oxygen system
      2 (merge robot
               {:brain          brain
                :moved          true
                :last-direction direction
                :pos            next-pos
                :path-home      (update-path path-home next-pos)
                :map            (merge map {pos      :blank
                                            next-pos :oxygen-system})
                :oxygen-system  next-pos}))))

(defn draw-map
  [robot]
  (let [{:keys [map]} robot
        min-x (apply min (mapv first (keys map)))
        max-x (apply max (mapv first (keys map)))
        min-y (apply min (mapv second (keys map)))
        max-y (apply max (mapv second (keys map)))]
    (print (str (char 27) "[2J")                            ; clear screen
           (str (char 27) "[;H")                            ; move cursor to the top left corner of the screen
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

(defn move-robot-seq
  ([robot] (move-robot-seq {[0 0] :origin}
                           (apply conj empty-queue (mapv #(vector robot %) directions))))
  ([map next]
   (if (empty? next)
     '()
     (do
       (let [[robot dir] (peek next)
             robot (assoc robot :map map)
             robot (move-robot robot dir)
             {:keys [pos]} robot
             new-dirs (->> directions
                           (filter (fn [d]
                                     (let [d-pos (move pos d)]
                                       (not (contains? (:map robot) d-pos))))))]
         (draw-map robot)
         (println (count next))
         (if (or (empty? new-dirs)
                 (not (:moved robot)))
           (let [m (:map robot)
                 m (assoc m (:pos robot) :blank)]
             (cons robot (lazy-seq (move-robot-seq m (pop next)))))
           (cons robot (lazy-seq (move-robot-seq (:map robot)
                                                 (apply conj (pop next) (mapv #(vector robot %) new-dirs)))))))))))

(defn time-to-refill
  [robot]
  (let [m (:map robot)
        m (into {} (filter (fn [[_ v]] (not= v :wall)) m))
        start-at (:oxygen-system robot)]
    (loop [c 0
           m m
           next [start-at]
           ]
      (if (empty? m)
        c
        (->> next
             (mapcat #(pos-move next )))))))

(defn solve
  [input]
  (let [program (int-parse input)
        brain (int-code program)
        robot (init-robot brain)
        full-path (move-robot-seq robot)]
    {:oxygen-system (->> full-path
                         (filter :oxygen-system)
                         (first)
                         (:path-home)
                         (count))
     :time-to-refill (time-to-refill (last full-path))
     }))

(def full-map
  "
 # ##### ####### ########### ######### #
#.#.....#.......#...........#.........#.#
#.#.#.###.#####.#.#####.#####.#####.#.#.#
#.#.#.....#.....#.#...#.......#.....#.#.#
#.#.#######.#####.#.#.#######.#.#####.#.#
#.#.....#...#.....#.#.#.....#.#...#.#...#
#.#####.#.###.#####.#.#.###.#.###.#.###.#
#...#...#...#.#.....#D#.#...#...#.#...#.#
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
#.#.#.......#.#...#.#...#.#.#.....#.#.#.#
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
#.#####.#.#.#.###.#######.#.###.#######.#
#.#...#.#.#...#.......#.....#...#.......#
#.#.###.#.#########.#.#.#######.#.#####.#
#.#.#.......#.......#.#.......#...#.....#
#.#.#.#######.#.#############.#####.####
#.#.....#...#.#.........#...#...#...#...#
#.#######.#.#.#########.#.#.###.#.###.#.#
#.........#...........#...#.....#.....#.#
 ######### ########### ### ##### ##### #
  ")
