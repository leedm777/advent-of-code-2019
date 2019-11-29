(ns advent-of-code-2019.day00
  (:gen-class))

(defn total
  "Finds the totals of the given deltas"
  [deltas]
  (apply + deltas))

(defn find-repeat
  [deltas]
  (->> deltas
       (repeat)
       (flatten)
       (reduce (fn [acc v]
                 (let [t (+ (:total acc) v)
                       first-dupe (or (:first-dupe acc)
                                      (get (:seen acc) t))]
                   (if (nil? first-dupe)
                     {
                      :seen (conj (:seen acc) t)
                      :total t
                      :first-dupe first-dupe
                      }
                     (reduced first-dupe))))
               { :seen #{0} :total 0 :first-dupe nil})))

(defn solve
  "Day 1 of 2018, just to test my infra"
  [input]
  (->> input
       (clojure.string/split-lines)
       (map #(Integer. %))
       ((fn [deltas] { :total (total deltas), :first-dupe (find-repeat deltas)}))))
