(ns advent-of-code-2019.day04-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day04 :refer :all]))

(deftest day04-part2
  (testing "example 1"
    (is (is-password-1 111111)))

  (testing "example 2"
    (is (not (is-password-1 223450))))

  (testing "example 3"
    (is (not (is-password-1 123789)))))

(deftest day04-part2
  (testing "example 1"
    (is (is-password-2 112233)))

  (testing "example 2"
    (is (not (is-password-2 123444))))

  (testing "example 3"
    (is (is-password-2 111122))))
