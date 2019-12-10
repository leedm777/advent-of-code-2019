(ns advent-of-code-2019.day10-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day10 :refer :all]
            [advent-of-code-2019.int-code :refer :all]
            [clojure.string :as s]))

(def large-map [".#..##.###...#######"
                "##.############..##."
                ".#.######.########.#"
                ".###.#######.####.#."
                "#####.##.#.##.###.##"
                "..#####..#.#########"
                "####################"
                "#.####....###.#.#.##"
                "##.#################"
                "#####.##.###..####.."
                "..######..##.#######"
                "####.##.####...##..#"
                ".#####..#.######.###"
                "##...#.##########..."
                "#.##########.#######"
                ".####.#.###.###.#.##"
                "....##.##.###..#####"
                ".#.#.###########.###"
                "#.#.#.#####.####.###"
                "###.##.####.##.#..##"])

(deftest day10-part1
  (testing "example 0"
    (is (= [8 [3 4]] (best-station [".#..#"
                                    "....."
                                    "#####"
                                    "....#"
                                    "...##"]))))

  (testing "example 1"
    (is (= [33 [5 8]] (best-station ["......#.#."
                                     "#..#.#...."
                                     "..#######."
                                     ".#.#.###.."
                                     ".#..#....."
                                     "..#....#.#"
                                     "#..#....#."
                                     ".##.#..###"
                                     "##...#..#."
                                     ".#....####"]))))

  (testing "exmaple 2"
    (is (= [35 [1 2]] (best-station ["#.#...#.#."
                                     ".###....#."
                                     ".#....#..."
                                     "##.#.#.#.#"
                                     "....#.#.#."
                                     ".##..###.#"
                                     "..#...##.."
                                     "..##....##"
                                     "......#..."
                                     ".####.###."]))))

  (testing "exmaple 3"
    (is (= [41 [6 3]] (best-station [".#..#..###"
                                     "####.###.#"
                                     "....###.#."
                                     "..###.##.#"
                                     "##.##.#.#."
                                     "....###..#"
                                     "..#.#..#.#"
                                     "#..#.#.###"
                                     ".##...##.#"
                                     ".....#.#.."]))))

  (testing "exmaple 4"
    (is (= [210 [11 13]] (best-station large-map)))))


(defn order [] (kill-order large-map [11 3]))

(deftest day10-part2
  (testing "example 0"
    (is (= [11 12] (get (order) 0)))))
