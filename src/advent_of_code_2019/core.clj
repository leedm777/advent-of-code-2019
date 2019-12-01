(ns advent-of-code-2019.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as p])
  (:gen-class))

(defn solve-for-day [day]
  (let [input-file (str day ".txt")
        input (slurp (io/resource input-file))
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
