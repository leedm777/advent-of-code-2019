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
    (is (= 2129920 (biodiversity (parse-locs ".....\n.....\n.....\n#....\n.#...")))))
  )
