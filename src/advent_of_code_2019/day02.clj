(ns advent-of-code-2019.day02)

;; TODO

(defn int-code
  [p]
  (loop [program p
         pc 0]
    (let [opcode (get program pc)
          a (get program (get program (+ pc 1)))
          b (get program (get program (+ pc 2)))
          dest (get program (+ pc 3))]
      (cond
        (= opcode 1) (recur (assoc program dest (+ a b)) (+ pc 4))
        (= opcode 2) (recur (assoc program dest (* a b)) (+ pc 4))
        (= opcode 99) program))))

(defn search
  [program]
  (loop [noun 0
         verb 0]
    (let [updated-program (-> program
                              (assoc 1 noun)
                              (assoc 2 verb))
          output (int-code updated-program)]
      (if (= (get output 0) 19690720)
        (+ (* 100 noun) verb)
        (if (= verb 99)
          (recur (inc noun) 0)
          (recur noun (inc verb)))))))

(defn solve
  [input]
  (let [program (->> input
                     (clojure.string/trim)
                     (#(clojure.string/split % #","))
                     (map #(Integer. %))
                     (vec))
        updated-program (-> program
                            (assoc 1 12)
                            (assoc 2 2))
        output (int-code updated-program)]
    {:old-pos-0 (get output 0)
     :new-settings (search program)}))
