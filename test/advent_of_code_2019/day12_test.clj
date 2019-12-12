(ns advent-of-code-2019.day12-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day12 :refer :all]))

(def example1-1
  (init-n-bodies [[-1 0 2]
                  [2 -10 -7]
                  [4 -8 8]
                  [3 5 -1]]))

(def energy-inputs
  [{ :pos [ 2,  1, -3] :vel [-3, -2,  1]}
   { :pos [ 1, -8,  0] :vel [-1,  1,  3]}
   { :pos [ 3, -6,  1] :vel [ 3,  2, -3]}
   { :pos [ 2,  0,  4] :vel [ 1, -1, -1]}])

(deftest day12-part1
  (testing "examples"
    (let [expected [[{ :pos [-1,   0,  2] :vel [ 0,  0,  0]}
                     { :pos [ 2, -10, -7] :vel [ 0,  0,  0]}
                     { :pos [ 4,  -8,  8] :vel [ 0,  0,  0]}
                     { :pos [ 3,   5, -1] :vel [ 0,  0,  0]}]

                    [{ :pos [ 2, -1,  1] :vel [ 3, -1, -1]}
                     { :pos [ 3, -7, -4] :vel [ 1,  3,  3]}
                     { :pos [ 1, -7,  5] :vel [-3,  1, -3]}
                     { :pos [ 2,  2,  0] :vel [-1, -3,  1]}]

                    [{ :pos [ 5, -3, -1] :vel [ 3, -2, -2]}
                     { :pos [ 1, -2,  2] :vel [-2,  5,  6]}
                     { :pos [ 1, -4, -1] :vel [ 0,  3, -6]}
                     { :pos [ 1, -4,  2] :vel [-1, -6,  2]}]

                    [{ :pos [ 5, -6, -1] :vel [ 0, -3,  0]}
                     { :pos [ 0,  0,  6] :vel [-1,  2,  4]}
                     { :pos [ 2,  1, -5] :vel [ 1,  5, -4]}
                     { :pos [ 1, -8,  2] :vel [ 0, -4,  0]}]

                    [{ :pos [ 2, -8,  0] :vel [-3, -2,  1]}
                     { :pos [ 2,  1,  7] :vel [ 2,  1,  1]}
                     { :pos [ 2,  3, -6] :vel [ 0,  2, -1]}
                     { :pos [ 2, -9,  1] :vel [ 1, -1, -1]}]

                    [{ :pos [-1, -9,  2] :vel [-3, -1,  2]}
                     { :pos [ 4,  1,  5] :vel [ 2,  0, -2]}
                     { :pos [ 2,  2, -4] :vel [ 0, -1,  2]}
                     { :pos [ 3, -7, -1] :vel [ 1,  2, -2]}]

                    [{ :pos [-1, -7,  3] :vel [ 0,  2,  1]}
                     { :pos [ 3,  0,  0] :vel [-1, -1, -5]}
                     { :pos [ 3, -2,  1] :vel [ 1, -4,  5]}
                     { :pos [ 3, -4, -2] :vel [ 0,  3, -1]}]

                    [{ :pos [ 2, -2,  1] :vel [ 3,  5, -2]}
                     { :pos [ 1, -4, -4] :vel [-2, -4, -4]}
                     { :pos [ 3, -7,  5] :vel [ 0, -5,  4]}
                     { :pos [ 2,  0,  0] :vel [-1,  4,  2]}]

                    [{ :pos [ 5,  2, -2] :vel [ 3,  4, -3]}
                     { :pos [ 2, -7, -5] :vel [ 1, -3, -1]}
                     { :pos [ 0, -9,  6] :vel [-3, -2,  1]}
                     { :pos [ 1,  1,  3] :vel [-1,  1,  3]}]

                    [{ :pos [ 5,  3, -4] :vel [ 0,  1, -2]}
                     { :pos [ 2, -9, -3] :vel [ 0, -2,  2]}
                     { :pos [ 0, -8,  4] :vel [ 0,  1, -2]}
                     { :pos [ 1,  1,  5] :vel [ 0,  0,  2]}]

                    [{ :pos [ 2,  1, -3] :vel [-3, -2,  1]}
                     { :pos [ 1, -8,  0] :vel [-1,  1,  3]}
                     { :pos [ 3, -6,  1] :vel [ 3,  2, -3]}
                     { :pos [ 2,  0,  4] :vel [ 1, -1, -1]}]]]
      (->> expected
           (map-indexed vector)
           (map (fn [[n e]]
                  (is (= e (simulate example1-1 n)))))
           (doall))))


  (testing "energy"
    (is (= [6 9 10 6] (map potential-energy energy-inputs)))
    (is (= [6 5 8 3] (map kinetic-energy energy-inputs)))
    (is (= [36 45 80 18] (map total-energy energy-inputs)))
    ))
