(ns advent-of-code-2019.day09-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day09 :refer :all]
            [clojure.string :as s]))


(deftest day09-part1
  (testing "example 0"
    (is (= 2019 (:relative-base (int-code [109,2000,109,19,99])))))

  (testing "example 0.1"
    (is (= 1985 (:relative-base (int-code [109,2000,109,19,109,-34,99])))))

  (testing "example 0.3"
    (is (= 8675309 ((:memory (int-code [109,2000,109,19,203,-34,99] [8675309])) 1985))))

  (testing "example 1"
    (is (= [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99] (:output (int-code [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])))))

  (testing "example 2"
    (is (= [1219070632396864] (:output (int-code [1102,34915192,34915192,7,4,7,99,0])))))

  (testing "example 3"
    (is (= [1125899906842624] (:output (int-code [104,1125899906842624,99])))))
  )
