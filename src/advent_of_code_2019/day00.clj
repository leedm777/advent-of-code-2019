(ns advent-of-code-2019.day00
  (:gen-class))

(defn total
  "Finds the totals of the given deltas"
  [deltas]
  (apply + deltas))

(defn find-repeat
  "Repeats the list of deltas, looking for a duplicated total."
  [deltas]
  (->> deltas
       (repeat)
       (flatten)
       (reduce (fn [{:keys [seen total]} val]
                 (let [next-total (+ total val)
                       is-repeated (contains? seen next-total)]
                   (if is-repeated
                     (reduced next-total)
                     {
                      :seen (conj seen next-total)
                      :total next-total
                      })))
               { :seen #{0} :total 0})))

(defn solve
  "Day 1 of 2018, just to test my infra"
  [input]
  (->> input
       (clojure.string/split-lines)
       (map #(Integer. %))
       ((fn [deltas] { :total (total deltas), :first-dupe (find-repeat deltas)}))))
