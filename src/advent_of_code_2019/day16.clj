(ns advent-of-code-2019.day16
  (:require [clojure.string :as s]
            [advent-of-code-2019.util :refer :all]))

(defn parse-signal
  [input]
  (->> input
       (s/trim)
       (map #(- (int %) (int \0)))))

(defn fft-n
  "Finds the nth term in the FFT for a signal"
  [signal n]
  ;; skip the initial (dec i) values
  ;; for the next 4i values, we will
  ;;  - add the first partition
  ;;  - skip the second
  ;;  - sub the third
  ;;  - skip the fourth
  (let [signal (subvec signal n)
        psize (inc n)
        ranges (partition psize (+ psize psize) [] signal)
        add-subs (partition 2 2 []  ranges)]
    (last-digit (reduce (fn [acc [add sub]]
                          (apply - (apply + acc add) sub))
                        0
                        add-subs))))

(defn fft-slow
  [signal]
  (->> (range 0 (count signal))
       (map #(fft-n signal %))
       (vec)))

(defn fft-first-quarter
  [signal]
  (->> (range 0 (quot (count signal) 3))
       (map #(fft-n signal %)))  )

(defn fft-second-quarter
  [signal]
  (->> (range (quot (count signal) 3) (quot (count signal) 2))
       (map #(subvec signal % (+ % % 1)))
       (map #(apply + %))
       (map last-digit)))

(defn fft-last-half
  [signal]
  (let [len (count signal)
        [res] (reduce (fn [[res acc] n]
                        (let [acc (+ acc n)
                              next (last-digit acc)]
                          [(cons next res) acc]))
                      ['() 0]
                      (reverse (subvec signal (quot len 2))))]
    res))

(defn fft
  [signal]
  (comment (let [signal-length (count signal)
                 iter (range (dec signal-length) -1 -1)
                 fft-n (partial fft-n signal)]
             (println signal-length)
             (->> iter
                  (mapv fft-n))))
  ;; 1st 1/4 of the signal has to be computed as above
  ;; 2nd 1/4 is just a sum of a subvec
  ;; final half can be reduced for the whole vector
  (vec
   (concat (fft-first-quarter signal)
           (fft-second-quarter signal)
           (fft-last-half signal))))

;; I thought about using the repeats to optimize, but it didn't
;; really help. And even if it did, the resulting signal doesn't
;; have repeats, so it won't help on the remaining 99 iterations
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
