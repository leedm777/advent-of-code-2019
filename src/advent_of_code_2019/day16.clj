(ns advent-of-code-2019.day16
  (:require [clojure.string :as s]))

(def base-pattern
  (memoize
   (fn [n]
     (->> [0 1 0 -1]
          ;; repeat each digit n times
          (map #(repeat n %))
          ;; repeat those repeated digits infinitely
          (repeat)
          ;; flatten the list
          (flatten)
          ;; skip the first one
          (drop 1)))))

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
                      (last-digit)))))))

(defn long-fft
  [signal]
  (let [long-signal (flatten (repeat 10000 signal))
        addr (->> signal
                  (take 8)
                  (s/join)
                  (Long/parseLong))
        final-phase (nth (iterate fft long-signal) 100)
        message (subvec final-phase addr (+ addr 8))
        ]))

(defn solve
  [input]
  (let [signal (parse-signal input)]
    {:first-fft (time (s/join (take 8 (nth (iterate fft signal)
                                           100))))}))
