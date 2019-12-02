(ns advent-of-code-2019.day02)

;; TODO

(defn int-code
  [p]
  (loop [program p
         instruction-pointer 0]
    (let [opcode (get program instruction-pointer)
          a (get program (get program (+ instruction-pointer 1)))
          b (get program (get program (+ instruction-pointer 2)))
          dest (get program (+ instruction-pointer 3))]
      (cond
        ;; 1 addition
        (= opcode 1) (recur (assoc program dest (+ a b)) (+ instruction-pointer 4))
        ;; 2 multiplication
        (= opcode 2) (recur (assoc program dest (* a b)) (+ instruction-pointer 4))
        ;; 99 halt
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
