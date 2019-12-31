(ns advent-of-code-2019.util
  (:require [clojure.string :as s]
            [clojure.pprint]
            [clojure.java.io :as io]))

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

(def up [0 -1])
(def down [0 1])
(def left [-1 0])
(def right [1 0])
(def dirs [up down left right])

(defn graph-maze
  "Graphs a maze by the given list of nodes"
  [nodes]
  (let [nodes (set nodes)]
    (reduce (fn [graph node]
              (assoc graph
                node (->> dirs
                          (mapv (partial pos-move node))
                          (filterv (partial nodes)))))
            {}
            nodes)))

(defn connect-nodes
  "Connects two nodes in a graph bidirectionally."
  [graph a b]
  (-> graph
      (assoc-in [a (count (get graph a))] b)
      (assoc-in [b (count (get graph b))] a)))

(defn shortest-path
  [graph start goal]
  (loop [ctr 0
         visited {}
         unvisited (conj empty-queue { :node start :path []})]
    ;;(clojure.pprint/pprint {:ctr ctr :visited (count visited), :unvisited (count unvisited)})
    (let [{:keys [node path]} (peek unvisited)
          path (conj path node)
          unvisited (pop unvisited)
          old-distance (if (contains? visited node)
                         (count (get visited node))
                         Long/MAX_VALUE)]
      (cond
        ;; found the goal; return the path
        (= goal node) path
        ;; goal not found
        (nil? node) nil
        ;; found longer path; ignore
        (<= old-distance (count path)) (recur (inc ctr) visited unvisited)
        ;; found shorter path; learn it and investigate the neighbors
        :else (let [visited (assoc visited node path)
                    neighbors (->> node
                                   (get graph)
                                   (mapv (fn [n] {:node n :path path})))
                    unvisited (apply conj unvisited neighbors)]
                (recur (inc ctr) visited unvisited))))))

(defn neighbors?
  "Returns true if two positions are neighbors"
  [x y]
  (->> (mapv - x y)
       (mapv #(Math/abs %))
       (reduce +)
       (= 1)))

(defn ascii-to-str
  [chs]
  (s/join (map (fn [c]
                 (if (> c 128)
                   c
                   (char c))) chs)))

(defn str-to-ascii
  [st]
  (mapv int st))

(defn input-for
  [day]
  (-> day
      (str ".txt")
      (io/resource)
      (slurp)))

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

(defn pow2
  [n]
  (case n
    0 1
    1 2
    (* 2 (pow2 (dec n)))))
