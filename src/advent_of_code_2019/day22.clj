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

(defn solve
  [input]
  (let [steps (s/split-lines (s/trim input))
        first-shuffle (space-shuffle steps (range 0 10007))
        ;;second-shuffle (->> (range 0 119315717514047)
        ;;                    (space-shuffle steps)
        ;;                    (space-shuffle steps))
        ]
    {:first-shuffle (->> first-shuffle
                         (map-indexed (fn [idx n] [idx n]))
                         (filter (fn [[_ n]] (= n 2019)))
                         (first)
                         (first))
     ;;:second-shuffle (nth second-shuffle 2020)
     }))
; 119,315,717,514,047
