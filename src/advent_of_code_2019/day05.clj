(ns advent-of-code-2019.day05
  (:require [clojure.string :as s]))

(defn read-mem [program val]
  (program val))

(def opcodes
  {1 {;; addition
      :num-params 3
      :func (fn [program input output [a b dest]]
              [(assoc program dest (+ (read-mem program a) (read-mem program b))) input output])}
   2 {;; multiplication
      :num-params 3
      :func (fn [program input output [a b dest]]
              [(assoc program dest (* (read-mem program a) (read-mem program b))) input output])}
   3 {;; input
      :num-params 1
      :func (fn [program input output [dest]]
              [(assoc program dest (first input)) (rest input) output])}
   4 {;; output
      :num-params 1
      :func (fn [program input output [a]]
              (println a (read-mem program a))
              [program input (conj output (read-mem program a))])}})

(defn int-code
  "Execute Intcode program"
  ([p] (int-code p []))
  ([p i]
   (loop [program p
          input i
          output []
          instruction-pointer 0]
     (let [opcode (get program instruction-pointer)]
       (if (= opcode 99)
         { :mem program, :out output }
         (let [{:keys [num-params func]} (opcodes opcode)
               params (subvec program (inc instruction-pointer) (+ instruction-pointer num-params 1))
               [next-program next-input next-output] (func program input output params)]
           (recur next-program next-input next-output (+ instruction-pointer num-params 1))))))))

(defn solve
  [input]
  (let [program (as-> input i
                  (s/trim i)
                  (s/split i #",")
                  (map #(Integer. %) i)
                  (vec i))]
    { :part1  (int-code program [1]) }))
