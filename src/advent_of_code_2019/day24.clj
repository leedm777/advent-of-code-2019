(ns advent-of-code-2019.day24
  (:require [advent-of-code-2019.util :refer :all]
            [clojure.string :as s]))

(def bug \#)
(def space \.)

(defn count-neighbors
  [locs pos]
  (->> dirs
       (map (partial pos-move pos))
       (map #(get locs % space))
       (filter #(= bug %))
       (count)))

(defn next-bugs
  [locs]
  (into {} (mapv (fn [[pos tile]]
                   (let [num-neighbors (count-neighbors locs pos)]
                     (cond
                       (and (= tile bug) (not= num-neighbors 1)) [pos space]
                       (and (= tile space) (#{1 2} num-neighbors)) [pos bug]
                       :else [pos tile])))
                 locs)))

(defn bugs-seq
  [locs]
  (cons locs (lazy-seq (bugs-seq (next-bugs locs)))))

(defn first-cycle
  [locs]
  (reduce (fn [seen locs]
            (if (seen locs)
              (reduced locs)
              (conj seen locs)))
          #{}
          (bugs-seq locs)))

(defn parse-locs
  [input]
  (->> input
       (parse-map)
       (into {})))

(defn biodiversity
  [locs]
  (->> locs
       (filterv (fn [[loc tile]] (= tile bug)))
       (mapv first)
       (mapv (fn [[x y]] (+ x (* y 5))))
       (mapv pow2)
       (apply +)))

(defn parse-folded-locs
  ([input] (parse-folded-locs input 0))
  ([input layer]
   (->> input
        (parse-map)
        (filter #(= bug (second %)))
        (map (fn [[[x y]]] [[layer x y] bug]))
        (into (sorted-map)))))

(def folded-dirs
  [[0 0 -1]
   [0 0 1]
   [0 -1 0]
   [0 1 0]])

(defn warp-edges
  [[level pos-x pos-y] [level x y :as neighbor]]
  (cond
    ;; edges neighbor the level below
    (= y -1) [[(dec level) 2 1]]
    (= y 5) [[(dec level) 2 3]]
    (= x -1) [[(dec level) 1 2]]
    (= x 5) [[(dec level) 3 2]]
    ;; center spots neighbor the level above
    (= [x y] [2 2]) (cond
                      (= pos-x 1) (->> (range 0 5) (mapv #(vector (inc level) 0 %)))
                      (= pos-x 3) (->> (range 0 5) (mapv #(vector (inc level) 4 %)))
                      (= pos-y 1) (->> (range 0 5) (mapv #(vector (inc level) % 0)))
                      (= pos-y 3) (->> (range 0 5) (mapv #(vector (inc level) % 4))))
    ;; everything else is normal
    :else [neighbor]))

(defn folded-neighbors
  [pos]
  ;; [0 *], [4 *], [* 0], [* 4] neighbor (dec level)
  ;; [2 1], [2 3], [1 2], [3 2] neighbor (inc level)
  (->> folded-dirs
       (map #(pos-move pos %))
       (mapcat (partial warp-edges pos))))

(defn next-folded-bugs
  [locs]
  (->> locs
       (keys)
       (mapcat folded-neighbors)
       (group-by identity)
       (map (fn [[pos tiles]] [pos (count tiles)]))
       (map (fn [[pos num-neighbors]]
              (let [tile (get locs pos space)]
                (cond
                  (and (= tile bug) (not= num-neighbors 1)) [pos space]
                  (and (= tile space) (#{1 2} num-neighbors)) [pos bug]
                  :else [pos tile]))))
       (filter (fn [[pos tile]] (= tile bug)))
       (into {})))

(defn draw-folded-space
  [locs]
  (let [layers (map first (keys locs))
        min-layer (apply min layers)
        max-layer (apply max layers)]

    (str
      (s/join "\n\n"
              (for [layer (range min-layer (inc max-layer))]
                (str "Depth " layer "\n"
                     (s/join "\n"
                             (for [y (range 0 5)]
                               (s/join
                                 (for [x (range 0 5)]
                                   (get locs [layer x y] space))))))))
      "\n\n-----\n")))

(defn folded-bugs-seq
  [locs]
  ;;(print (str (char 27) "[2J") ; clear screen
  ;;       (str (char 27) "[;H") ; move cursor to the top left corner of the screen
  ;;       )
  ;;(draw-folded-space locs)
  (cons locs (lazy-seq (folded-bugs-seq (next-folded-bugs locs)))))

(defn solve
  [input]
  {:biodiversity (->> input
                      (parse-locs)
                      (first-cycle)
                      (biodiversity))
   :life-count   (-> input
                     (parse-folded-locs)
                     (folded-bugs-seq)
                     (nth 200)
                     (count))})
