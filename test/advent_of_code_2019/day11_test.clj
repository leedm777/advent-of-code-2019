(ns advent-of-code-2019.day11-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day11 :refer :all]
            [advent-of-code-2019.int-code :refer :all]))

(concat (interleave (repeat 104) [1 0
                                  0 0
                                  1 0
                                  1 0
                                  0 1
                                  1 0
                                  1 0]))

(deftest day11-part-1
  (testing "example"
    (let [dummy-brain (int-code [3 100
                                 104 1
                                 104 0

                                 3 101
                                 104 0
                                 104 0

                                 3 102
                                 104 1
                                 104 0

                                 3 103
                                 104 1
                                 104 0

                                 3 104
                                 104 0
                                 104 1

                                 3 105
                                 104 1
                                 104 0

                                 3 106
                                 104 1
                                 104 0

                                 99])
          actual (run-robot (init-robot dummy-brain))]
      (is (= 6 (count (:panels actual)))))))
