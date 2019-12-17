(ns advent-of-code-2019.day17-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [advent-of-code-2019.day17 :refer :all]))

(def example1
  ["..#.........."
   "..#.........."
   "#######...###"
   "#.#...#...#.#"
   "#############"
   "..#...#...#.."
   "..#####...^.."])

(deftest day17-part1
  (testing "ex1"
    (is (= 76 (->> example1
                   (plot-map)
                   (find-intersections)
                   (sum-alignment-params))))))
