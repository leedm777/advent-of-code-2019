(ns advent-of-code-2019.day18
  (:require [advent-of-code-2019.util :refer :all]
            [clojure.string :as s]
            [clojure.set]))

(def up [0 -1])
(def down [0 1])
(def left [-1 0])
(def right [1 0])
(def dirs [up down left right])

(defn update-neighbors
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
                   (= ch \#) acc ;; ignore walls
                   (= ch \@) (merge acc {:entry pos
                                         :nodes (conj (:nodes acc) pos)})
                   (= ch \.) (merge acc {:nodes (conj (:nodes acc) pos)})
                   ;; keys are mapped pos -> ch
                   (Character/isLowerCase ch) (merge acc {:keys (assoc (:keys acc) pos ch)
                                                          :nodes (conj (:nodes acc) pos)})
                   ;; doors are mapped ch -> pos
                   (Character/isUpperCase ch) (merge acc {:doors (assoc (:doors acc) ch pos)})))
               {:entry nil
                :keys {}
                :doors {}
                :nodes #{}})
       (update-neighbors)))

(defn check-key
  [passages key-loc]
  (let [{:keys [doors keys nodes]} passages]
    (if-let [key (keys key-loc)]
      (let [door (Character/toUpperCase key)
            door-loc (doors door)]
        (-> passages
            (merge {:doors (dissoc doors door)
                    :keys (dissoc keys key-loc)
                    :nodes (conj nodes door-loc)})
            (update-neighbors)))
      ;; no key, just return the passages
      passages))
  )

;; Use something similar to Dijkstra's algorithm
;;  - Consider [pos keys] as the node
;;  - Be sure to prune the tree when you hit a dead end

(defn make-node
  ([passages] (make-node passages (:entry passages) 0))
  ([passages loc distance]
   {:id [loc (:keys passages)]
    :loc loc
    :passages passages
    :distance distance}))

(defn find-new-neighbors
  [node visited]
  (let [{:keys [loc passages distance]} node
        {:keys [keys nodes]} passages]
    (->> dirs
         (mapv (partial pos-move loc))
         (filter #(nodes %))
         (mapv #(make-node (check-key passages %) % (inc distance)))
         (filter (fn [node]
                   (< (inc distance)
                      (get-in visited
                              [(:id node) :distance]
                              Long/MAX_VALUE))))
         (vec))))

(defn map-seq
  ([passages]
   (let [visited {}
         unvisited (conj empty-queue (make-node passages))]
     (map-seq visited unvisited)))
  ([visited unvisited]
   (println (count unvisited))
   (let [next-node (peek unvisited)
         unvisited (pop unvisited)
         new-neighbors (find-new-neighbors next-node visited)
         visited (assoc visited (:id next-node) next-node)
         unvisited (apply conj unvisited new-neighbors)]
     (cons next-node (lazy-seq (map-seq visited unvisited))))))

(defn find-keys
  [passages]
  (->> passages
       (map-seq)
       (drop-while #(not (empty? (get-in % [:passages :keys]))))
       (first)
       (tap)
       (:distance)))

(defn solve
  [input]
  {:steps-to-find-keys (find-keys (parse-map input))})

;; (defn tap
;;   [v]
;;   (println v)
;;   v)

;; (defn reachable-nodes
;;   [{:keys [neighbors loc]}]
;;   (loop [reachable #{}
;;          next #{loc}]
;;     (if (empty? next)
;;       reachable
;;       (let [r (clojure.set/union reachable next)
;;             n (->> next
;;                    (select-keys neighbors)
;;                    (vals)
;;                    (apply concat)
;;                    (set)
;;                    (#(clojure.set/difference % r)))
;;             ]
;;         (recur r n)))))

;; (defn reachable-keys
;;   [passages]
;;   (let [reachable (reachable-nodes passages)]
;;     (filter (fn [[key pos]] (reachable pos)) (:keys passages))))
