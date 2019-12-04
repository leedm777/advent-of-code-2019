(ns advent-of-code-2019.day04
  (:require [clojure.string :as s]))

(defn is-password-1
  [n]
  (let [nstr (str n)]
    (and (= nstr (s/join (sort nstr)))
         (re-matches #".*([0-9])\1.*" nstr)
         true)))

(defn two-but-no-more
  ;; surely there's a regex to do this
  [s]
  (let [[counts] (reduce (fn [[counts last-char] char]
                           (if (= char last-char)
                             ;; increment last item in counts array
                             [(update counts (dec (count counts)) inc) char]
                             ;; add new character count
                             [(conj counts 1) char]))
                         ;; initial list that won't match any number
                         [[0] \x]
                         s)]
    (some #(= 2 %) counts)))

(defn is-password-2
  [n]
  (let [nstr (str n)]
    (and (= nstr (s/join (sort nstr)))
         (two-but-no-more nstr))))

(defn solve
  ([ignored] (solve))
  ([]
   {:candidates (count (filter is-password-1 (range 271973 785961)))
    :better-candidates (count (filter is-password-2  (range 271973 785961)))}))
