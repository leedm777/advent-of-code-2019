(ns advent-of-code-2019.day08-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day08 :refer :all]))

(deftest day08-test
  (testing "example 1 layers"
    (is (= [;; layer 1
            [[1 2 3]
             [4 5 6]]
            ;; layer 2
            [[7 8 9]
             [0 1 2]] (str-to-image 3 2 "123456789012")]))
    (is (= 1 (checksum (str-to-image 3 2 "123456789012"))))))

(deftest day08-part2
  (testing "example 1"
    (is (= [[0 1] [1 0]] (render (str-to-image 2 2 "0222112222120000"))))))
