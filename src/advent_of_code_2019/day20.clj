(ns advent-of-code-2019.day20
  (:require [clojure.string :as s]
            [advent-of-code-2019.util :refer :all]))

(defn find-label
  [[x1 y1] [x2 y2] nodes]
  (if (= y1 y2)
    ;; horizontal label
    (if (nodes [(dec x1) y1])
      [(dec x1) y1]
      [(inc x2) y2])
    ;; vertical label
    (if (nodes [x1 (dec y1)])
      [x1 (dec y1)]
      [x2 (inc y2)])))

(defn locate-portals
  [portal-nodes nodes]
  (let [portal-nodes (into {} portal-nodes)
        locs (keys portal-nodes)
        graph (graph-maze locs)]
    (reduce (fn [acc [loc1 [loc2]]]
              (if (or (> (first loc1) (first loc2))
                      (> (second loc1) (second loc2)))
                acc
                (let [ch1 (get portal-nodes loc1)
                      ch2 (get portal-nodes loc2)
                      label (str ch1 ch2)
                      loc (find-label loc1 loc2 nodes)]
                  (assoc acc label (conj (get acc label []) loc)))
                )) {} graph)))

(defn graph-portals
  [[nodes portals]]
  (let [graph (graph-maze (mapv first nodes))
        portals (locate-portals portals (set (keys graph)))
        graph (reduce (fn [g [label [loc1 loc2]]]
                        (if (nil? loc2)
                          g
                          (connect-nodes g loc1 loc2)))
                      graph
                      portals)]
    {:graph    graph
     :portals  portals
     :entrance (first (get portals "AA"))
     :exit     (first (get portals "ZZ"))}))

(defn parse-donut
  [input]
  (->> input
       (parse-map)
       (filter (fn [[k ^Character v]] (cond
                                        (Character/isUpperCase v) true
                                        (= v \.) true
                                        true false)))
       (sort-by (fn [[kv v]] v))
       (partition-by (fn [[k v]] (= v \.)))
       (graph-portals)))

(defn solve
  [input]
  (let [donut (parse-donut input)]
    donut))

;;        outer-left-1 0
;;        outer-left-2 1
;;        outer-right-2 (->> locs
;;                           (mapv first)
;;                           (apply max))
;;        outer-right-1 (dec outer-right-2)
;;
;;        inner-left-1 (->> locs
;;                          (mapv first)
;;                          (filter #(> % 1))
;;                          (apply min))
;;        inner-left-2 (inc inner-left-1)
;;        inner-right-2 (->> locs
;;                           (mapv first)
;;                           (filter #(< % outer-lower-1))
;;                           (apply min))
;;        inner-right-1 (dec inner-right-2)
;;
;;        outer-upper-1 0
;;        outer-upper-2 1
;;        outer-lower-2 (->> locs
;;                           (mapv second)
;;                           (apply max))
;;        outer-lower-1 (dec outer-lower-2)
;;
;;        inner-upper-1 (->> locs
;;                          (mapv second)
;;                          (filter #(> % 1))
;;                          (apply min))
;;        inner-upper-2 (inc inner-upper-1)
;;        inner-lower-2 (->> locs
;;                           (mapv second)
;;                           (filter #(< % outer-lower-1))
;;                           (apply min))
;;        inner-lower-1 (dec inner-lower-2)
