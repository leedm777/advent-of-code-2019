(ns advent-of-code-2019.day05
  (:require [clojure.string :as s]))

(defn read-mem [program val mode]
  (case mode
    0 (program val)
    1 val))

(def opcodes
  {1 {;; addition
      :num-params 3
      :func (fn [program input output modes [a b dest]]
              [(assoc program
                      dest
                      (+ (read-mem program a (nth modes 0))
                         (read-mem program b (nth modes 1))))
               input
               output])}
   2 {;; multiplication
      :num-params 3
      :func (fn [program input output modes [a b dest]]
              [(assoc program
                      dest
                      (* (read-mem program a (nth modes 0))
                         (read-mem program b (nth modes 1))))
               input
               output])}
   3 {;; input
      :num-params 1
      :func (fn [program input output modes [dest]]
              [(assoc program
                      dest
                      (first input))
               (rest input)
               output])}
   4 {;; output
      :num-params 1
      :func (fn [program input output modes [a]]
              [program
               input
               (conj output (read-mem program a (nth modes 0)))])}})

(defn param-mode
  [digits]
  (cons (mod digits 10) (lazy-seq (param-mode (quot digits 10)))))

(defn int-code
  "Execute Intcode program"
  ([p] (int-code p []))
  ([p i]
   (loop [program p
          input i
          output []
          instruction-pointer 0]
     (let [instruction (get program instruction-pointer)]
       (if (= instruction 99)
         { :mem program, :out output }
         (let [opcode (mod instruction 100)
               modes (param-mode (quot instruction 100))
               {:keys [num-params func]} (opcodes opcode)
               params (subvec program (inc instruction-pointer) (+ instruction-pointer num-params 1))
               [next-program next-input next-output] (func program input output modes params)]
           (recur next-program next-input next-output (+ instruction-pointer num-params 1))))))))

(defn solve
  [input]
  (let [program (as-> input i
                  (s/trim i)
                  (s/split i #",")
                  (map #(Integer. %) i)
                  (vec i))]
    { :part1 (:out (int-code program [1])) }))
