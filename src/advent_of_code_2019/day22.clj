(ns advent-of-code-2019.day22
  (:require [clojure.string :as s]
            [advent-of-code-2019.int-code :refer :all]
            [advent-of-code-2019.util :refer :all]))

(defn deal
  [n cards]
  (let [n-cards (count cards)]
    (loop [cards cards
           next 0
           new-deck (sorted-map)]
      (if (empty? cards)
        (vals new-deck)
        (recur (rest cards)
               (mod (+ next n) n-cards)
               (assoc new-deck next (first cards)))))))

(defn cut
  [n cards]
  (let [n (mod n (count cards))]
    (concat (drop n cards) (take n cards))))

(defn space-shuffle
  [steps deck]
  (if (empty? steps)
    deck
    (let [step (first steps)
          [_ deal-n] (re-matches #"deal with increment (\d*)" step)
          [_ cut-n] (re-matches #"cut (-?\d*)" step)
          [deal-into-new-stack] (re-matches #"deal into new stack" step)
          deck (cond
                 deal-n (deal (Integer/parseInt deal-n) deck)
                 cut-n (cut (Integer/parseInt cut-n) deck)
                 deal-into-new-stack (reverse deck)
                 true (do
                   (println "Unparsed step: " step)
                   deck))]
      (recur (rest steps) deck))))

(defn unshuffle
  [deck-size steps position]
  (loop [steps steps
         position position]
    (if (empty? steps)
      position
      (let [step (first steps)
            [_ deal-n] (re-matches #"deal with increment (\d*)" step)
            [_ cut-n] (re-matches #"cut (-?\d*)" step)
            [deal-into-new-stack] (re-matches #"deal into new stack" step)
            came-from (cond
                        deal-n (mod (* -1 (Integer/parseInt deal-n) position) deck-size)
                        cut-n (mod (+ position (Integer/parseInt cut-n)) deck-size)
                        deal-into-new-stack (- deck-size position))]
        (recur (rest steps) came-from)))))

(defn solve
  [input]
  (let [steps (s/split-lines (s/trim input))
        first-shuffle (space-shuffle steps (range 0 10007))
        second-shuffle (partial unshuffle 119315717514047 (->> steps
                                                               (reverse)
                                                               (repeat 101741582076661)
                                                               (flatten)))
        ]
    {:first-shuffle (->> first-shuffle
                         (map-indexed (fn [idx n] [idx n]))
                         (filter (fn [[_ n]] (= n 2019)))
                         (first)
                         (first))
     :second-shuffle (second-shuffle 2020)
     }))
;; 119,315,717,514,047 cards
;; 101,741,582,076,661 shuffles
