(ns advent-of-code-2019.day01-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day01 :refer :all]))

(deftest day01-test
  (testing "example 1"
    (= 2 (calculate-fuel-naive 12)))

  (testing "example 2"
    (= 2 (calculate-fuel-naive 14)))

  (testing "example 3"
    (= 654 (calculate-fuel-naive 1969)))

  (testing "example 4"
    (= 33583 (calculate-fuel-naive 100756)))

  (testing "example 2.1"
    (= 2 (calculate-fuel 12)))

  (testing "example 2.2"
    (= 966 (calculate-fuel 1969)))

  (testing "example 2.3"
    (= 50346 (calculate-fuel 100756))))
