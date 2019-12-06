(ns advent-of-code-2019.day06-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day06 :refer :all]))

(deftest day06-test-part1
  (testing "example"
    (is (= 42 (total-orbits ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L"]))))  )

(deftest day06-test-part2
  (testing "example"
    (is (= 4 (transfers-to-santa ["COM)B" "B)C" "C)D" "D)E" "E)F" "B)G" "G)H" "D)I" "E)J" "J)K" "K)L" "K)YOU" "I)SAN"])))))
