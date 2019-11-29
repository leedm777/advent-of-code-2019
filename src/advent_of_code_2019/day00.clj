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
       (reduce (fn [{:keys [seen total first-dupe]} val]
                 (let [next-total (+ total val)
                       first-dupe (or first-dupe
                                      (get seen next-total))]
                   (if (nil? first-dupe)
                     {
                      :seen (conj seen next-total)
                      :total next-total
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
