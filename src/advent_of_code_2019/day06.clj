(ns advent-of-code-2019.day06
  (:require [clojure.string :as s]))

(def center "COM")
(def you "YOU")
(def santa "SAN")

(defn build-map
  ([orbits] (build-map orbits {}))
  ([[orbit & orbits] orbit-map]
   (if (nil? orbit)
     orbit-map
     (let [[center orbiting-body] (s/split orbit #"\)")]
       (recur orbits (assoc orbit-map orbiting-body center))))))

(defn path-to
  ([orbit-map start end] (path-to orbit-map start end []))
  ([orbit-map start end p]
   (if (= start end)
     p
     (recur orbit-map (orbit-map start) end (conj p start)))))

(defn transfers-to-santa
  [orbit-map]
  (let [you-path (reverse (path-to orbit-map you center))
        santa-path (reverse (path-to orbit-map santa center))]
    ;; drop any common planets from the path to COM,
    ;; and what's left are the shortest paths to a common
    ;; planet
    (loop [[next-you & rest-you] you-path
           [next-santa & rest-santa] santa-path]
      (if (= next-you next-santa)
        (recur rest-you rest-santa)
        (+ (count rest-you) (count rest-santa))))))

(defn count-orbits
  [orbit-map orbiting-body]
  (count (path-to orbit-map orbiting-body center)))

(defn total-orbits
  [orbit-map]
  (->> orbit-map
       (keys)
       (map #(count-orbits orbit-map %))
       (apply +)))

(defn solve
  [input]
  (let [orbit-map (->> input
                       (s/split-lines)
                       (build-map))]
    {:orbits (total-orbits orbit-map)
     :santa-hops (transfers-to-santa orbit-map)}))
