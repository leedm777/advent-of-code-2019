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
                    :keys  (dissoc keys key-loc)
                    :nodes (conj nodes door-loc)})
            (update-graph)))
      ;; no key, just return the tunnels
      tunnels)))

(defn make-node
  ([tunnels]
   (let [{:keys [entry]} tunnels]
     (make-node tunnels entry)))
  ([tunnels locs]
   (let [tunnels (reduce (fn [tunnels loc]
                           (check-key tunnels loc))
                         tunnels
                         locs)]
     {:id      [(set locs) (-> tunnels (:keys) (vals) (set))]
      :locs    locs
      :tunnels tunnels})))

(defrecord GatesAndKeys [tunnels]
  Maze
  (maze-start [maze] (make-node tunnels))
  (maze-goal? [maze node] (->> node
                               (:tunnels)
                               (:keys)
                               (empty?)))
  (maze-neighbors [maze node]
    (let [{:keys [locs tunnels]} node
          neighbor-locs (->> locs
                             (map-indexed (fn [idx loc]
                                            (let [neighbors (get-in tunnels [:graph loc])]
                                              (mapv #(assoc locs idx %) neighbors))))
                             (apply concat))
          neighbors (mapv (partial make-node tunnels) neighbor-locs)]
      neighbors))
  (maze-id [maze node] (:id node)))

(defn plot-tunnels
  ([m] (plot-tunnels m identity))
  ([m tunnel-update]
   (->> m
        (parse-map)
        (reduce (fn [tunnels [pos ^Character ch]]
                  (cond
                    (= ch \#) tunnels                       ;; ignore walls
                    (= ch \@) (-> tunnels
                                  (update :entry #(conj % pos))
                                  (update :nodes #(conj % pos)))
                    (= ch \.) (update tunnels :nodes #(conj % pos))
                    ;; keys are mapped pos -> ch
                    (Character/isLowerCase ch) (-> tunnels
                                                   (update :keys #(assoc % pos ch))
                                                   (update :nodes #(conj % pos)))
                    ;; doors are mapped ch -> pos
                    (Character/isUpperCase ch) (update tunnels :doors #(assoc % ch pos))))
                {:entry []
                 :keys  {}
                 :doors {}
                 :nodes #{}})
        (tunnel-update)
        (update-graph)
        (GatesAndKeys.))))

(defn split-vaults
  [tunnels]
  (let [{:keys [nodes entry]} tunnels
        [entry] entry
        nodes (disj nodes
                    entry
                    (pos-move entry [1 0])
                    (pos-move entry [-1 0])
                    (pos-move entry [0 1])
                    (pos-move entry [0 -1]))
        entry [(pos-move entry [-1 -1])
               (pos-move entry [1 -1])
               (pos-move entry [-1 1])
               (pos-move entry [1 1])]]
    (merge tunnels {:entry entry, :nodes nodes})))

(defn solve
  [input]
  {
   ;;:steps-to-find-keys       (-> input
   ;;                              (plot-tunnels)
   ;;                              (solve-maze)
   ;;                              (count))
   :split-steps-to-find-keys (-> input
                                 (plot-tunnels split-vaults)
                                 (solve-maze)
                                 (count))})
