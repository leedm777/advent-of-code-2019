(ns advent-of-code-2019.day16
  (:require [clojure.string :as s]))

(defn parse-signal
  [input]
  (->> input
       (s/trim)
       (map #(- (int %) (int \0)))))

(defn last-digit
  [n]
  (Math/abs (rem n 10)))

(defn fft-n
  "Finds the nth term in the FFT for a signal"
  [signal i]
  ;; skip the initial (dec i) values
  ;; for the next 4i values, we will
  ;;  - add the first partition
  ;;  - skip the second
  ;;  - sub the third
  ;;  - skip the fourth
  (let [signal (drop (dec i) signal)
        ranges (partition i (+ i i) [] signal)
        add-subs (partition 2 2 []  ranges)]
    (comment (if (zero? (rem i 100))
               (println i (new java.util.Date))))
    (last-digit (reduce (fn [acc [add sub]]
                          (apply - (apply + acc add) sub))
                        0
                        add-subs))))

(defn fft
  [signal]
  (let [signal-length (count signal)
        iter (range 1 (inc signal-length))
        fft-n (partial fft-n signal)]
    (println signal-length)
    (->> iter
         (mapv fft-n))))

(defn long-fft
  [signal]
  (let [long-signal (flatten (repeat 10000 signal))
        addr (->> signal
                  (take 7)
                  (s/join)
                  (Long/parseLong))
        final-phase (nth (iterate fft long-signal) 100)
        _ (println addr "/" (count final-phase))
        message (subvec final-phase addr (+ addr 8))]
    message))

(defn solve
  [input]
  (let [signal (parse-signal input)]
    {:first-fft (time (s/join (take 8 (nth (iterate fft signal)
                                           100))))}))
