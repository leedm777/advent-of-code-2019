(ns advent-of-code-2019.int-code-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.int-code :refer :all]))
(deftest day02-test
  (testing "example 1"
    (is (= [3500 9 10 70 2 3 11 0 99 30 40 50] (:memory (int-code [1 9 10 3 2 3 11 0 99 30 40 50])))))
  (testing "example 2"
    (is (= [2,0,0,0,99] (:memory (int-code [1,0,0,0,99])))))
  (testing "example 3"
    (is (= [2,3,0,6,99] (:memory (int-code [2,3,0,3,99])))))
  (testing "example 4"
    (is (= [2,4,4,5,99,9801] (:memory (int-code [2,4,4,5,99,0])))))
  (testing "example 5"
    (is (= [30,1,1,4,2,5,6,0,99] (:memory (int-code [1,1,1,4,99,5,6,0,99]))))))

(deftest day05-test-part1
  (testing "example 1"
    (is (= [:some-input] (:output (int-code [3 0 4 0 99] [:some-input])))))

  (testing "example 2"
    (is (= [1002,4,3,4,99] (:memory (int-code [1002,4,3,4,33])))))

  (testing "example 3"
    (is (= [1101,100,-1,4,99] (:memory (int-code [1101,100,-1,4,0]))))))

(deftest day05-test-part2
  (testing "example 1"
    (is (= [0] (:output (int-code [3,9,8,9,10,9,4,9,99,-1,8] [7])))))

  (testing "example 1, part 2"
    (is (= [1] (:output (int-code [3,9,8,9,10,9,4,9,99,-1,8] [8])))))

  (testing "example 2"
    (is (= [0] (:output (int-code [3,9,7,9,10,9,4,9,99,-1,8] [8])))))

  (testing "example 2, part 2"
    (is (= [1] (:output (int-code [3,9,7,9,10,9,4,9,99,-1,8] [7])))))

  (testing "example 3"
    (is (= [0] (:output (int-code [3,3,1108,-1,8,3,4,3,99] [7])))))

  (testing "example 3, part 2"
    (is (= [1] (:output (int-code [3,3,1108,-1,8,3,4,3,99] [8])))))

  (testing "example 4"
    (is (= [0] (:output (int-code [3,3,1107,-1,8,3,4,3,99] [8])))))

  (testing "example 4, part 2"
    (is (= [1] (:output (int-code [3,3,1107,-1,8,3,4,3,99] [7])))))

  (testing "example 5"
    (is (= [0] (:output (int-code [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [0])))))

  (testing "example 5, part 2"
    (is (= [1] (:output (int-code [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] [8675309])))))

  (testing "example 6"
    (is (= [0] (:output (int-code [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [0])))))

  (testing "example 6, part 2"
    (is (= [1] (:output (int-code [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] [8675309])))))

  (testing "example 7"
    (is (= [999] (:output (int-code [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                     1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                     999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [7])))))

  (testing "example 7, part 2"
    (is (= [1000] (:output (int-code [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [8])))))

  (testing "example 7, part 3"
    (is (= [1001] (:output (int-code [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                                      1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                                      999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99] [9]))))))

(deftest day07-part2
  (testing "input pausing"
    (let [st (int-code [3 0 99])]
      (is (= true (:paused st)))
      (is (= [3 0 99] (:memory st)))

      (let [st (int-resume-input st [:some-input])]
        (is (int-halted? st))
        (is (= [:some-input 0 99] (:memory st)))))))


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
    (is (= [1125899906842624] (:output (int-code [104,1125899906842624,99]))))))
