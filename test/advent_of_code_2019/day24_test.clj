(ns advent-of-code-2019.day24-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day24 :refer :all]
            [advent-of-code-2019.util :refer :all]))

(deftest day24-part1
  (testing "ex1"
    (is (= (parse-locs "#..#.\n####.\n###.#\n##.##\n.##..") (next-bugs (parse-locs "....#\n#..#.\n#..##\n..#..\n#....")))))

  (testing "ex2"
    (is (= (parse-locs "#####\n....#\n....#\n...#.\n#.###") (next-bugs (parse-locs "#..#.\n####.\n###.#\n##.##\n.##..")))))

  (testing "ex3"
    (is (= (parse-locs "#....\n####.\n...##\n#.##.\n.##.#") (next-bugs (parse-locs "#####\n....#\n....#\n...#.\n#.###")))))

  (testing "ex4"
    (is (= (parse-locs "####.\n....#\n##..#\n.....\n##...") (next-bugs (parse-locs "#....\n####.\n...##\n#.##.\n.##.#")))))

  (testing "ex4"
    (is (= (parse-locs ".....\n.....\n.....\n#....\n.#...") (first-cycle (parse-locs "....#\n#..#.\n#..##\n..#..\n#....")))))

  (testing "ex5"
    (is (= 2129920 (biodiversity (parse-locs ".....\n.....\n.....\n#....\n.#..."))))))

(deftest day24-part2
  (testing "neighbors"
    (is (= #{[0 3 2] [0 3 4] [0 2 3] [0 4 3]} (set (folded-neighbors [0 3 3]))))
    (is (= #{[1 1 0] [1 0 1] [1 2 1] [1 1 2]} (set (folded-neighbors [1 1 1]))))
    (is (= #{[1 2 0] [1 4 0] [1 3 1] [0 2 1]} (set (folded-neighbors [1 3 0]))))
    (is (= #{[0 3 3] [0 3 1] [0 4 2] [1 4 0] [1 4 1] [1 4 2] [1 4 3] [1 4 4]} (set (folded-neighbors [0 3 2]))))
    (is (= #{[1 4 2] [1 3 3] [1 3 1] [2 4 0] [2 4 1] [2 4 2] [2 4 3] [2 4 4]} (set (folded-neighbors [1 3 2]))))
    )

  (testing "folding-bug-seq"
    (is (= (merge (parse-folded-locs "..#..\n.#.#.\n..?.#\n.#.#.\n..#.." -5)
                  (parse-folded-locs "...#.\n...##\n..?..\n...##\n...#." -4)
                  (parse-folded-locs "#.#..\n.#...\n..?..\n.#...\n#.#.." -3)
                  (parse-folded-locs ".#.##\n....#\n..?.#\n...##\n.###." -2)
                  (parse-folded-locs "#..##\n...##\n..?..\n...#.\n.####" -1)
                  (parse-folded-locs ".#...\n.#.##\n.#?..\n.....\n....." 0)
                  (parse-folded-locs ".##..\n#..##\n..?.#\n##.##\n#####" 1)
                  (parse-folded-locs "###..\n##.#.\n#.?..\n.#.##\n#.#.." 2)
                  (parse-folded-locs "..###\n.....\n#.?..\n#....\n#...#" 3)
                  (parse-folded-locs ".###.\n#..#.\n#.?..\n##.#.\n....." 4)
                  (parse-folded-locs "####.\n#..#.\n#.?#.\n####.\n....." 5))
           (nth (folded-bugs-seq (parse-folded-locs "....#\n#..#.\n#.?##\n..#..\n#...."))
                10)))))
