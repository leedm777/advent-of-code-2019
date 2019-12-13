(ns advent-of-code-2019.day12
  (:require [clojure.string :as s]))

(defn tap
  [x]
  (clojure.pprint/pprint x)
  x)

(defn update-velocities
  [n-bodies]
  (->> n-bodies
       (map (fn [body]
              (reduce (fn [body other-body]
                        (let [{:keys [pos vel]} body
                              other-pos (:pos other-body)
                              delta-v (->> pos
                                           (map vector other-pos)
                                           (map (fn [[p op]] (cond
                                                               (> p op) 1
                                                               (< p op) -1
                                                               true 0))))
                              next-vel (->> vel
                                            (map vector delta-v)
                                            (mapv #(apply + %)))]
                          {:pos pos
                           :vel next-vel}))
                      body
                      n-bodies)))))

(defn update-positions
  [n-bodies]
  (->> n-bodies
       (mapv (fn [body]
               (let [{:keys [pos vel]} body
                     next-pos (->> pos
                                   (map vector vel)
                                   (map #(apply + %)))]
                 {:pos next-pos
                  :vel vel})))))

(defn next-step
  [n-bodies]
  (->> n-bodies
       (update-velocities)
       (update-positions)))

(defn init-n-bodies
  [positions]
  (map (fn [pos] { :pos pos, :vel [0 0 0]}) positions))

(defn simulate
  [n-bodies]
  (iterate next-step n-bodies))

(defn potential-energy
  [moon]
  (->> moon
       (:pos)
       (map #(Math/abs %))
       (apply +)))

(defn kinetic-energy
  [moon]
  (->> moon
       (:vel)
       (map #(Math/abs %))
       (apply +)))

(defn total-energy
  [moon]
  (* (potential-energy moon)
     (kinetic-energy moon)))

(defn split-axis
  [moon]
  (let [{:keys [pos vel]} moon
        pos-vel-pairs (mapv vector pos vel)]
    (->> pos-vel-pairs
         (mapv (fn [[pos vel]] { :pos [pos], :vel [vel]})))))

(defn cycle-time-single-axis
  [n-bodies-single-axis]
  (reduce (fn [seen n-bodies]
            (if (seen n-bodies)
              (reduced (count seen))
              (conj seen n-bodies)))
          #{}
          (simulate n-bodies-single-axis)))

;; from https://rosettacode.org/wiki/Least_common_multiple#Clojure
(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))

(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

;; to calculate the lcm for a variable number of arguments
(defn lcmv [& v] (reduce lcm v))

(defn cycle-time
  [n-bodies]
  (->> n-bodies
       (mapv split-axis)
       (apply map vector)
       (mapv cycle-time-single-axis)
       (apply lcmv)))

(def input (init-n-bodies
            [[14, 2, 8]
             [7, 4, 10]
             [1, 17, 16]
             [-4, -1, 1]]))

(defn solve
  [_]
  {:total-energy (as-> input i
                   (simulate i)
                   (nth i 1000)
                   (map total-energy i)
                   (apply + i))
   :cycle-time (cycle-time input)})
;; {"day12" {:total-energy 9139, :cycle-time 420788524631496}}
