(ns advent-of-code-2019.day13
  (:require [advent-of-code-2019.int-code :refer :all]))

(def tile-empty 0)
(def tile-wall 1)
(def tile-block 2)
(def tile-paddle 3)
(def tile-ball 4)

(defn init-game
  [program]
  (int-code program))

(defn parse-tiles
  [tiles]
  (reduce ()))

(defn play-game
  [game]
  (loop [[tiles game] (int-read-all-output game)]
    (let [board (parse-tiles tiles)]
      ))
  (->> game
       (:output)
       (partition 3)
       (clojure.pprint/pprint)))

(defn solve
  [input]
  (let [game  (-> input
                  (int-parse)
                  (init-game))
        running-game (-> input
                         (int-parse)
                         (assoc 0 2)
                         (init-game))]
    {
     ;; :block-count (->> game
     ;;                   (:output)
     ;;                   (partition 3)
     ;;                   (map #(nth % 2))
     ;;                   (filter #(= tile-block %))
     ;;                   (count))
     :win-game (play-game running-game)}))
