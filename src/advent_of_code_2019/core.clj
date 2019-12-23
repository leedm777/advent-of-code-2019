(ns advent-of-code-2019.core
  (:require [advent-of-code-2019.util :refer :all]
            [clojure.pprint :as p])
  (:gen-class))

(defn solve-for-day [day]
  (let [input (input-for day)
        day-ns-name (str "advent-of-code-2019." day)
        _ (require (symbol day-ns-name))
        day-ns (find-ns (symbol day-ns-name))
        solve (ns-resolve day-ns 'solve)]
    [day (solve input)]))

(defn -main
  [& args]
  (->> args
       (map solve-for-day)
       (flatten)
       (apply sorted-map)
       (p/pprint)))
