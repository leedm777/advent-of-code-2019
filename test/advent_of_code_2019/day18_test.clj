(ns advent-of-code-2019.day18-test
  (:require [clojure.test :refer :all]
            [advent-of-code-2019.day18 :refer :all]
            [advent-of-code-2019.util :refer :all]
            [clojure.string :as s]))

(def ex1
  (s/join "\n"
          ["#########"
           "#b.A.@.a#"
           "#########"]))

(def ex2
  (s/join "\n"
          ["########################"
           "#f.D.E.e.C.b.A.@.a.B.c.#"
           "######################.#"
           "#d.....................#"
           "########################"]))

(def ex3
  (s/join "\n"
          ["########################"
           "#...............b.C.D.f#"
           "#.######################"
           "#.....@.a.B.c.d.A.e.F.g#"
           "########################"]))
(def ex4
  (s/join "\n"
          ["#################"
           "#i.G..c...e..H.p#"
           "########.########"
           "#j.A..b...f..D.o#"
           "########@########"
           "#k.E..a...g..B.n#"
           "########.########"
           "#l.F..d...h..C.m#"
           "#################"]))
(def ex5
  (s/join "\n"
          ["########################"
           "#@..............ac.GI.b#"
           "###d#e#f################"
           "###A#B#C################"
           "###g#h#i################"
           "########################"]))

(deftest day18-part1
  (testing "ex1"
    (is (= 8 (count (solve-maze (plot-tunnels ex1))))))
  (testing "ex2"
    (is (= 86 (count (solve-maze (plot-tunnels ex2))))))
  (testing "ex3"
    (is (= 132 (count (solve-maze (plot-tunnels ex3))))))
  (testing "ex4"
    (is (= 136 (count (solve-maze (plot-tunnels ex4))))))
  (testing "ex5"
    (is (= 81 (count (solve-maze (plot-tunnels ex5))))))
  )

(def ex2-0
  (s/join "\n"
          ["#######"
           "#a.#Cd#"
           "##...##"
           "##.@.##"
           "##...##"
           "#cB#Ab#"
           "#######"]))

(def ex2-1
  (s/join "\n"
          ["#######"
           "#a.#Cd#"
           "##@#@##"
           "#######"
           "##@#@##"
           "#cB#Ab#"
           "#######"
           ]))

(def ex2-2
  (s/join "\n"
          ["#############"
           "#g#f.D#..h#l#"
           "#F###e#E###.#"
           "#dCba@#@BcIJ#"
           "#############"
           "#nK.L@#@G...#"
           "#M###N#H###.#"
           "#o#m..#i#jk.#"
           "#############"]))

(deftest day18-part2
  (testing "split-vaults"
    (is (= (plot-tunnels ex2-1) (plot-tunnels ex2-0 split-vaults))))
  (testing "ex2-1"
    (is (= 8 (count (solve-maze (plot-tunnels ex2-1))))))

  (testing "ex2-2"
    (is (= 72 (count (solve-maze (plot-tunnels ex2-2)))))))
