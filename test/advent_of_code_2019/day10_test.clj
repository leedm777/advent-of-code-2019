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


(defn order [] (kill-order large-map [11 13]))

(deftest day10-part2
  (testing "example 1"
    (is (= [11 12] (:pos (nth (order) (dec 1))))))

  (testing "example 2"
    (is (= [12 1] (:pos (nth (order) (dec 2))))))

  (testing "example 3"
    (is (= [12 2] (:pos (nth (order) (dec 3))))))

  (testing "example 4"
    (is (= [12 8] (:pos (nth (order) (dec 10))))))

  (testing "example "
    (is (= [16 0] (:pos (nth (order) (dec 20))))))

  (testing "example "
    (is (= [16 9] (:pos (nth (order) (dec 50))))))

  (testing "example "
    (is (= [10 16] (:pos (nth (order) (dec 100))))))

  (testing "example "
    (is (= [9 6] (:pos (nth (order) (dec 199))))))

  (testing "example "
    (is (= [8 2] (:pos (nth (order) (dec 200))))))

  (testing "example "
    (is (= [10 9] (:pos (nth (order) (dec 201))))))

  (testing "example "
    (is (= [11 1] (:pos (nth (order) (dec 299)))))))
