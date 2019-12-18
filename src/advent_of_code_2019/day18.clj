(ns advent-of-code-2019.day18
  (:require [advent-of-code-2019.util :refer :all]
            [clojure.string :as s]))

(def up [0 -1])
(def down [0 1])
(def left [-1 0])
(def right [1 0])
(def dirs [up down left right])

(defn find-neighbors
  [passages]
  (let [nodes (:nodes passages)
        neighbors (mapcat (fn [node] [node (->> dirs
                                                (mapv (partial pos-move node))
                                                (filter #(nodes %))
                                                (vec))])
                          nodes)]
    (assoc passages :neighbors (apply hash-map neighbors))))

(defn plot-passages
  [m]
  (->> m
       (parse-map)
       (reduce (fn [acc [pos ch]]
                 (cond
                   (= ch \#)  acc ;; ignore walls
                   (= ch \@) (merge acc {:loc pos
                                         :nodes (conj (:nodes acc) pos)})
                   (= ch \.) (merge acc {:nodes (conj (:nodes acc) pos)})
                   (Character/isLowerCase ch) (merge acc {:keys (assoc (:keys acc) ch pos)
                                                          :nodes (conj (:nodes acc) pos)})
                   (Character/isUpperCase ch) (merge acc {:doors (assoc (:doors acc) ch pos)})))
               {:loc nil
                :keys {}
                :doors {}
                :nodes #{}})
       (find-neighbors)))

(defn unlock-door
  [passages key]
  (let [door (Character/toUpperCase key)
        {:keys [doors keys nodes]} passages
        loc (get doors door)
        ]
    (find-neighbors
     (merge passages
            {:loc loc
             :doors (dissoc doors door)
             :nodes (conj nodes loc)}))))

(defn find-keys
  [g])

(defn solve
  [input]
  {:steps-to-find-keys (plot-passages input)})
