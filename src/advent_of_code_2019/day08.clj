(ns advent-of-code-2019.day08
  (:require [clojure.string :as s]))

(defn str-to-image
  [width height st]
  (->> st
       ;; char to int
       (map #(- (int %) (int \0)))
       ;; group by width to make lines
       (partition width)
       ;; group by height to make layers
       (partition height)))

(defn checksum
  [image]
  (let [counts-by-layer (->> image
                             (map (fn [layer]
                                    (->> layer
                                         (flatten)
                                         (group-by identity)
                                         (reduce-kv (fn [m k v] (assoc m k (count v))) {})))))
        min-zeros (apply min (map #(% 0) counts-by-layer))
        layer-counts (first (filter #(= (% 0) min-zeros) counts-by-layer))]
    (* (layer-counts 1) (layer-counts 2))))

(defn merge-pixels
  [upper lower]
  (if (= upper 2) lower upper))

(defn merge-lines
  [upper lower]
  (->> (map vector upper lower)
       (map #(apply merge-pixels %))))

(defn merge-layers
  [upper lower]
  (->> (map vector upper lower)
       (map #(apply merge-lines %))))

(defn render-line
  [line]
  (s/join (map {1 \., 0 \ } line)))

(defn render
  [image]
  (->> (reduce merge-layers (first image) (rest image))
       (map render-line)))

(defn solve
  [input]
  (let [image (->> input
                   (str-to-image 25 6))]
    {:checksum (checksum image)
     :render (render image)}))
