(ns advent-of-code-2019.day02
  (:require [clojure.string :as s]))

;; TODO

(defn int-code
  "Execute Intcode program"
  [p]
  (loop [program p
         instruction-pointer 0]
    (let [opcode (get program instruction-pointer)
          a (get program (get program (+ instruction-pointer 1)))
          b (get program (get program (+ instruction-pointer 2)))
          dest (get program (+ instruction-pointer 3))]
      (case opcode
        ;; 1 addition
        1 (recur (assoc program dest (+ a b)) (+ instruction-pointer 4))
        ;; 2 multiplication
        2 (recur (assoc program dest (* a b)) (+ instruction-pointer 4))
        ;; 99 halt
        99 program))))

(defn search
  "Search for settings that result in the expected output"
  [program expected]
  (loop [noun 0
         verb 0]
    (let [updated-program (-> program
                              (assoc 1 noun)
                              (assoc 2 verb))
          [output] (int-code updated-program)]
      (if (= output expected)
        (+ (* 100 noun) verb)
        (if (= verb 99)
          (recur (inc noun) 0)
          (recur noun (inc verb)))))))

(defn solve
  [input]
  (let [program (as-> input i
                  (s/trim i)
                  (s/split i #",")
                  (map #(Integer. %) i)
                  (vec i))
        updated-program (-> program
                            (assoc 1 12)
                            (assoc 2 2))
        [updated-program-output] (int-code updated-program)]
    {:updated-program-output updated-program-output
     :new-settings (search program 19690720)}))
