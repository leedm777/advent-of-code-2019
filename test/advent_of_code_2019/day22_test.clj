(ns advent-of-code-2019.day22-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day22 :refer :all]))

(deftest day22-part1
  (testing "cut"
    (is (= [3 4 5 6 7 8 9 0 1 2] (cut 3 (range 0 10))))
    (is (= [6 7 8 9 0 1 2 3 4 5] (cut -4 (range 0 10)))))
  (testing "deal"
    (is (= [0 7 4 1 8 5 2 9 6 3] (deal 3 (range 0 10))))
    (is (= [0 3 6 9 2 5 8 1 4 7] (deal 7 (range 0 10))))
    )
  )

(deftest day22-part2
  (testing "unshuffle"
    (is (= 7 (unshuffle 10 ["deal into new stack"] 3)))
    (is (= 6 (unshuffle 10 ["cut 3"] 3)))
    (is (= 9 (unshuffle 10 ["cut -4"] 3)))
    (is (= 1 (unshuffle 10 ["deal with increment 3"] 3)))))
