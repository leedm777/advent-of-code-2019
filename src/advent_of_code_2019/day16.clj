(ns advent-of-code-2019.day16
  (:require [clojure.string :as s]))

(defn base-pattern
  [n]
  (->> [0 1 0 -1]
       (map #(repeat n %))
       (repeat)
       (flatten)
       (drop 1)))

(defn parse-signal
  [input]
  (->> input
       (s/trim)
       (map #(- (int %) (int \0)))))

(defn tap
  [n]
  (println n)
  n)

(defn last-digit
  [n]
  (Math/abs (rem n 10)))

(defn fft
  [signal]
  (let [signal-length (count signal)
        iter (range 1 (inc signal-length))]
    (->> iter
         (mapv (fn [i]
                 (->> (base-pattern i)
                      (mapv vector signal)
                      (mapv #(apply * %))
                      (apply +)
                      (last-digit)))))
    ))

(defn solve
  [input]
  (let [signal (parse-signal input)]
    {:first-fft (time (s/join (take 8 (nth (iterate fft signal)
                                           100))))}))
