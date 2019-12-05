(ns advent-of-code-2019.day05-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day05 :refer :all]))

;; day 2 tests should pass on today's puzzle
(deftest day02-test
  (testing "example 1"
    (is (= { :mem [3500 9 10 70 2 3 11 0 99 30 40 50] :out [] } (int-code [1 9 10 3 2 3 11 0 99 30 40 50]))))
  (testing "example 2"
    (is (= { :mem [2 0 0 0 99] :out [] } (int-code [1 0 0 0 99]))))
  (testing "example 3"
    (is (= { :mem [2 3 0 6 99] :out [] } (int-code [2 3 0 3 99]))))
  (testing "example 4"
    (is (= { :mem [2 4 4 5 99 9801] :out [] } (int-code [2 4 4 5 99 0]))))
  (testing "example 5"
    (is (= { :mem [30 1 1 4 2 5 6 0 99] :out [] } (int-code [1 1 1 4 99 5 6 0 99])))))

(deftest day05-test
  (testing "example 1"
    (is (= { :mem [:some-input 0 4 0 99] :out [:some-input] } (int-code [3 0 4 0 99] [:some-input])))))
