(ns advent-of-code-2019.day15
  (:require [advent-of-code-2019.int-code :refer :all]
            [clojure.string :as s]
            [advent-of-code-2019.util :refer :all]
            [clojure.set]))

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
       (mapv vector pos)
       (mapv (partial apply +))))

(defn pick
  [s]
  (first (shuffle s)))

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

(defn print-map
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
                           nil
                           (apply conj empty-queue (mapv #(vector robot %) directions))))
  ([m oxygen-system next]
   (if (empty? next)
     '()
     (do
       (let [[robot dir] (peek next)
             robot (merge robot {:map m
                                 :oxygen-system oxygen-system})
             robot (move-robot robot dir)
             {:keys [pos]} robot
             new-dirs (->> directions
                           (filter (fn [d]
                                     (let [d-pos (move pos d)]
                                       (not (contains? (:map robot) d-pos))))))]
         (print-map robot)
         (println oxygen-system)
         (let [oxygen-system (or oxygen-system (:oxygen-system robot))]
          (if (or (empty? new-dirs)
                  (not (:moved robot)))
            (let [m (:map robot)
                  m (assoc m (:pos robot) :blank)]
              (cons robot (lazy-seq (move-robot-seq m oxygen-system (pop next)))))
            (cons robot (lazy-seq (move-robot-seq (:map robot)
                                                  oxygen-system
                                                  (apply conj (pop next) (mapv #(vector robot %) new-dirs))))))))))))

(defn time-to-refill
  [robot]
  (let [locs (:map robot)
        locs (into #{} (->> locs
                             (filter (fn [[_ v]] (not= v :wall)))
                             (map (fn [[loc]] loc))))
        start-at (:oxygen-system robot)]
    (loop [ctr 0
           remaining locs
           next (sorted-set start-at)]
      (clojure.pprint/pprint {:remaining (count remaining) :next (count next) :ctr ctr})
      (if (or (empty? remaining) (empty? next))
        ctr
        (let [neighbors (->> next
                             (mapcat (fn [loc] (map #(pos-move loc %) dirs)))
                             (filter locs)
                             (into (sorted-set)))
              remaining (clojure.set/difference remaining neighbors)]
          (recur (inc ctr) remaining neighbors))))))

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
#.#..O#.#.#...#.......#.....#...#.......#
#.#.###.#.#########.#.#.#######.#.#####.#
#.#.#.......#.......#.#.......#...#.....#
#.#.#.#######.#.#############.#####.####
#.#.....#...#.#.........#...#...#...#...#
#.#######.#.#.#########.#.#.###.#.###.#.#
#.........#...........#...#.....#.....#.#
 ######### ########### ### ##### ##### #
  ")
