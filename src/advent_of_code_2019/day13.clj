(ns advent-of-code-2019.day13
  (:require [advent-of-code-2019.int-code :refer :all]))

(def tile-empty 0)
(def tile-wall 1)
(def tile-block 2)
(def tile-paddle 3)
(def tile-ball 4)

(def render-tile
  {tile-empty \ ,
   tile-wall \u2588
   tile-block \=
   tile-paddle \-
   tile-ball \*})

(defn update-tiles
  [game tiles]
  (reduce (fn [game [x y val]]
            (if (and (= x -1) (zero? y))
              (assoc game :score val)
              (-> game
                  (assoc-in [:by-pos [x y]] val)
                  (cond->
                      (= val tile-paddle) (assoc  :paddle-x x)
                      (= val tile-ball) (assoc :ball-x x)))))
          game
          tiles))

(defn init-game
  [program]
  (let [brain (int-code program)
        [output brain] (int-read-all-output brain)
        tiles (partition 3 output)
        max-x (apply max (map first tiles))
        max-y (apply max (map second tiles))
        ]
    (update-tiles {:brain brain
                   :max-x max-x
                   :max-y max-y
                   :by-pos {}
                   :paddle-x nil
                   :ball-x nil}
                  tiles)))

(defn update-game
  [game move]
  (let [{:keys [brain]} game
        brain (int-resume-input brain [move])
        [output brain] (int-read-all-output brain)
        game (assoc game :brain brain)
        tiles (partition 3 output)]
    (update-tiles game tiles)))

(defn render-game
  [game]
  (cons (str "Score: " (:score game))
        (for [y (range 0 (inc (:max-y game)))]
          (apply str
                 (for [x (range 0 (inc (:max-x game)))
                       :let [tile (get-in game [:by-pos [x y]])]]
                   (render-tile tile \?))))))

(defn play-game
  [game]
  (print (str (char 27) "[2J")) ; clear screen
  (print (str (char 27) "[;H")) ; move cursor to the top left corner of the screen
  (clojure.pprint/pprint (render-game game))
  (if (int-halted? (:brain game))
    (:score game)
    (let [{:keys [ball-x paddle-x]} game
          dir (cond
                (< ball-x paddle-x) -1 ;; left
                (> ball-x paddle-x) 1  ;; right
                true 0 ;; don't move
                )]
      (recur (update-game game dir)))))

(defn solve
  [input]
  (let [
        ;; demo-game  (-> input
        ;;           (int-parse)
        ;;           (init-game))
        running-game (-> input
                         (int-parse)
                         (assoc 0 2) ;; insert quarters
                         (init-game))]
    {
     ;; :block-count (->> demo-game
     ;;                   (:output)
     ;;                   (partition 3)
     ;;                   (map #(nth % 2))
     ;;                   (filter #(= tile-block %))
     ;;                   (count))
     :win-game (play-game running-game)}))
