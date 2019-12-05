(ns advent-of-code-2019.day05
  (:require [clojure.string :as s]))

(defn read-mem-old
  [program val mode]
    (case mode
      0 (program val)
      1 val))

(defn read-mem
  [program ip mode]
  (let [val (program ip)]
    (case mode
      0 (program val)
      1 val)))

(def opcodes
  {1 {;; addition
      :num-params 3
      :func (fn [program input output modes ip]
              (let [a (read-mem program (+ ip 1) (nth modes 0))
                    b (read-mem program (+ ip 2) (nth modes 1))
                    dest (program (+ ip 3))]
                [(assoc program dest (+ a b))
                 input
                 output
                 (+ ip 4)]))}
   2 {;; multiplication
      :num-params 3
      :func (fn [program input output modes ip]
              (let [a (read-mem program (+ ip 1) (nth modes 0))
                    b (read-mem program (+ ip 2) (nth modes 1))
                    dest (program (+ ip 3))]
                [(assoc program dest (* a b))
                 input
                 output
                 (+ ip 4)]))}
   3 {;; input
      :num-params 1
      :func (fn [program input output modes ip]
              (let [dest (program (+ ip 1))]
                [(assoc program
                        dest
                        (first input))
                 (rest input)
                 output
                 (+ ip 2)]))}
   4 {;; output
      :num-params 1
      :func (fn [program input output modes ip]
              (let [a (read-mem program (+ ip 1) (nth modes 0))]
                [program
                 input
                 (conj output a)
                 (+ ip 2)]))}
   5 {;; jump-if-true
      }
   6 {;; jump-if-false
      }
   7 {;; less than
      }
   8 {;; equals
      }
   })

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
               [next-program next-input next-output next-ip] (func program input output modes instruction-pointer)]
           (recur next-program next-input next-output next-ip)))))))

(defn solve
  [input]
  (let [program (as-> input i
                  (s/trim i)
                  (s/split i #",")
                  (map #(Integer. %) i)
                  (vec i))]
    { :part1 (:out (int-code program [1])) }))
