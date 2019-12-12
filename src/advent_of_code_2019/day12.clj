(ns advent-of-code-2019.day12
  (:require [clojure.string :as s]))

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
                                            (map #(apply + %)))]
                          {:pos pos
                           :vel next-vel}))
                      body
                      n-bodies)))))

(defn update-positions
  [n-bodies]
  (->> n-bodies
       (map (fn [body]
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
  (cons n-bodies (lazy-seq (simulate (next-step n-bodies)))))

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

(def input (init-n-bodies
            [[14, 2, 8]
             [7, 4, 10]
             [1, 17, 16]
             [-4, -1, 1]]))

(defn solve
  [_]
  {:part1 (->> input
               (simulate)
               (drop 1000)

               )})
