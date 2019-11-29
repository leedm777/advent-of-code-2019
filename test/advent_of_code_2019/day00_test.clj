(ns advent-of-code-2019.day00-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day00 :refer :all]))

(deftest day00-test
  (testing "example 1"
    (is (= 3 (total [+1 -2 +3 +1]))))

  (testing "example 2"
    (is (= 3 (total [+1 +1 +1]))))

  (testing "example 3"
    (is (= 0 (total [+1 +1 -2]))))

  (testing "example 4"
    (is (= -6 (total [-1 -2 -3]))))

  (testing "part two example 1"
    (is (= 0 (find-repeat [+1 -1]))))

  (testing "part two example 2"
    (is (= 10 (find-repeat [+3 +3 +4 -2 -4]))))

  (testing "part two example 3"
    (is (= 5 (find-repeat [-6 +3 +8 +5 -6]))))

  (testing "part two example 4"
    (is (= 14 (find-repeat [+7 +7 -2 -7 -4]))))

  (testing "solve"
    (is (= {:total 1, :first-dupe 14} (solve "+7\n+7\n-2\n-7\n-4\n")))))
