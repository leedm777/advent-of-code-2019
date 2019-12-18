(ns advent-of-code-2019.day16
  (:require [clojure.string :as s]
            [advent-of-code-2019.util :refer :all]))

(defn parse-signal
  [input]
  (->> input
       (s/trim)
       (map #(- (int %) (int \0)))
       (vec)))

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

(defn fft-reduction
  [partial-signal]
  (let [[res] (reduce (fn [[res acc] n]
                        (let [acc (+ acc n)
                              next (last-digit acc)]
                          [(cons next res) acc]))
                      ['() 0]
                      (reverse (vec partial-signal)))]
    res))

(defn fft-last-half
  [signal]
  (fft-reduction (subvec signal (quot (count signal) 2))))

(defn fft
  [signal]
  ;;   0 to 1/3 of the signal has to be computed as above
  ;; 1/3 to 1/2 is just a sum of a subvec
  ;; 1/2 to end can be reduced for the whole vector
  (vec
   (concat (fft-first-quarter signal)
           (fft-second-quarter signal)
           (fft-last-half signal))))

;; I thought about using the repeats to optimize, but it didn't
;; really help. And even if it did, the resulting signal doesn't
;; have repeats, so it won't help on the remaining 99 iterations
(defn long-fft
  [signal]
  (let [long-signal (vec (flatten (repeat 10000 signal)))
        addr (->> signal
                  (take 7)
                  (s/join)
                  (Long/parseLong))]
    (if (< addr (/ (count long-signal) 2))
      "sub-optimal; giving up"
      (take 8 (nth (iterate fft-reduction (subvec long-signal addr))
                   100)))))

(defn solve
  [input]
  (let [signal (parse-signal input)]
    {:first-fft (time (s/join (take 8 (nth (iterate fft signal)
                                           100))))
     :long-fft (time (s/join (long-fft signal)))}))
