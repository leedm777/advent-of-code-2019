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
        graph (reduce (fn [g [_ [loc1 loc2]]]
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

(defn best-path
  [donut]
  (a* (:graph donut) (:entrance donut) (:exit donut)))

(defn solve
  [input]
  (let [donut (parse-donut input)
        path (best-path donut)]
    (clojure.pprint/pprint path)
    { :path-length (count path)}))
