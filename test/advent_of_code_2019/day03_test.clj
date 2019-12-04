(ns advent-of-code-2019.day03-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day03 :refer :all]))

(deftest day03-part1
  (testing "example 1"
    (is (= 6 (distance-to-closest-intercept
              ["R8" "U5" "L5" "D3"]
              ["U7" "R6" "D4" "L4"]))))

  (testing "example 2"
    (is (= 159 (distance-to-closest-intercept
                ["R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72"]
                ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"]))))

  (testing "example 3"
    (is (= 135 (distance-to-closest-intercept
                ["R98" "U47" "R26" "D63" "R33" "U87" "L62" "D20" "R33" "U53" "R51"]
                ["U98" "R91" "D20" "R16" "D67" "R40" "U7" "R15" "U6" "R7"])))))

(deftest day03-part2
  (testing "example 1"
    (is (= 30 (steps-to-closest-intercept
               ["R8" "U5" "L5" "D3"]
               ["U7" "R6" "D4" "L4"]))))

  (testing "example 2"
    (is (= 610 (steps-to-closest-intercept
                ["R75" "D30" "R83" "U83" "L12" "D49" "R71" "U7" "L72"]
                ["U62" "R66" "U55" "R34" "D71" "R55" "D58" "R83"]))))

  (testing "example 3"
    (is (= 410 (steps-to-closest-intercept
                ["R98" "U47" "R26" "D63" "R33" "U87" "L62" "D20" "R33" "U53" "R51"]
                ["U98" "R91" "D20" "R16" "D67" "R40" "U7" "R15" "U6" "R7"])))))
