(ns advent-of-code-2019.util
  (:require [clojure.string :as s]
            [clojure.pprint]))

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defmethod print-method clojure.lang.PersistentQueue [q, w] ; Overload the printer for queues so they look like fish
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

(defn last-digit
  "Return the last digit of the given number."
  [n]
  (Math/abs (rem n 10)))

(defn tap
  [x]
  (clojure.pprint/pprint x)
  x)

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

(defn render-map
  ([m] (render-map m identity))
  ([m f]
   (let [k (keys m)
         min-x (reduce min (mapv first k))
         max-x (reduce max (mapv first k))
         min-y (reduce min (mapv second k))
         max-y (reduce max (mapv second k))
         ]
     (s/join "\n"
             (for [y (range min-y (inc max-y))]
               (s/join (for [x (range min-x (inc max-y))]
                         (f (get m [x y] \ )))))))))

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
