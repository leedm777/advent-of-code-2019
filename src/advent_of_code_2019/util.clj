(ns advent-of-code-2019.util
  (:require [clojure.string :as s]))

(defn last-digit
  "Return the last digit of the given number."
  [n]
  (Math/abs (rem n 10)))

(defn parse-map
  "Parses a map into coordinates and characters"
  [m]
  (->> m
       (s/split-lines)
       (map-indexed (fn [y line]
                      (map-indexed (fn [x ch] [[x y] ch])
                                   line)))
       ;; flatten out two levels
       (apply concat)))

(defn pos-move
  "Move a position by the given delta"
  [pos delta]
  (mapv + pos delta))

;; from https://rosettacode.org/wiki/Least_common_multiple#Clojure
(defn gcd
  "Find the greated common denominator of the given numbers."
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  "Find the lowest common multiple of the given numbers."
  [a b]
  (/ (* a b) (gcd a b)))

;; to calculate the lcm for a variable number of arguments
(defn lcmv [& v] (reduce lcm v))
