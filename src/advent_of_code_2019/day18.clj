(ns advent-of-code-2019.day18
  (:require [advent-of-code-2019.util :refer :all]
            [clojure.string :as s]
            [clojure.set]))

(defn update-graph
  [tunnels]
  (let [nodes (:nodes tunnels)
        neighbors (mapcat (fn [node] [node (->> dirs
                                                (mapv (partial pos-move node))
                                                (filter #(nodes %))
                                                (vec))])
                          nodes)]
    (assoc tunnels :graph (apply hash-map neighbors))))

(defn check-key
  [tunnels key-loc]
  (let [{:keys [doors keys nodes]} tunnels]
    (if-let [key (keys key-loc)]
      (let [door (Character/toUpperCase key)
            door-loc (doors door)]
        (-> tunnels
            (merge {:doors (dissoc doors door)
                    :keys (dissoc keys key-loc)
                    :nodes (conj nodes door-loc)})
            (update-graph)))
      ;; no key, just return the tunnels
      tunnels)))

(defn make-node
  ([tunnels]
   (let [{:keys [entry]} tunnels]
     (make-node tunnels entry)))
  ([tunnels loc]
   (let [tunnels (check-key tunnels loc)]
     {:id      [loc (-> tunnels (:keys) (vals) (set))]
      :loc     loc
      :tunnels tunnels})))

(deftype GatesAndKeys [tunnels]
  Maze
  (maze-start [maze] (make-node tunnels))
  (maze-goal? [maze node] (->> node
                               (:tunnels)
                               (:keys)
                               (empty?)))
  (maze-neighbors [maze node]
    (let [{:keys [loc tunnels]} node
          neighbor-locs (get-in tunnels [:graph loc])
          neighbors (mapv (partial make-node tunnels) neighbor-locs)]
      neighbors))
  (maze-id [maze node] (:id node)))

(defn plot-tunnels
  [m]
  (->> m
       (parse-map)
       (reduce (fn [acc [pos ^Character ch]]
                 (cond
                   (= ch \#) acc                            ;; ignore walls
                   (= ch \@) (merge acc {:entry pos
                                         :nodes (conj (:nodes acc) pos)})
                   (= ch \.) (merge acc {:nodes (conj (:nodes acc) pos)})
                   ;; keys are mapped pos -> ch
                   (Character/isLowerCase ch) (merge acc {:keys  (assoc (:keys acc) pos ch)
                                                          :nodes (conj (:nodes acc) pos)})
                   ;; doors are mapped ch -> pos
                   (Character/isUpperCase ch) (merge acc {:doors (assoc (:doors acc) ch pos)})))
               {:entry nil
                :keys  {}
                :doors {}
                :nodes #{}})
       (update-graph)
       (GatesAndKeys.)))

(defn solve
  [input]
  {:steps-to-find-keys (->> input
                            (plot-tunnels)
                            (solve-maze)
                            (count)
                            (dec))})
