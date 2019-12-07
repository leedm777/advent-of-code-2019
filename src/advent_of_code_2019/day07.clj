(ns advent-of-code-2019.day07
  (:require [clojure.string :as s]
            ))

(defn initial-state
  "Initializes the int-code computer"
  ([memory] (initial-state memory []))
  ([memory input]
   {:memory memory
    :input input
    :output []
    :ip 0
    :param-mode 0
    :params []
    :ptrs []
    }))

(defn read-instruction
  "Read an opcode/param-mode instruction from memory"
  [st]
  (let [{:keys [memory ip]} st
        instruction (memory ip)]
    (merge st {:ip (inc ip)
               :opcode (mod instruction 100)
               :param-mode (quot instruction 100)
               :ptrs []
               :params []})))

(defn read-param
  "Read a parameter from memory"
  [st]
  (let [{:keys [memory ip ptrs params param-mode]} st
        mode (mod param-mode 10)
        ptr (case mode
              ;; 0 - position mode
              0 (memory ip)
              ;; 1 - immediate mode
              1 ip)
        val (memory ptr)]
    (merge st {:ip (inc ip)
               :params (conj params val)
               :ptrs (conj ptrs ptr)
               :param-mode (quot param-mode 10)})))

(defn read-params
  "Read n parameters from memory"
  [n st]
  (nth (iterate read-param st) n))

(defn int-binary-op
  [f]
  (fn [st]
    (let [st (read-params 3 st)
          [a b] (:params st)
          [_ _ dest] (:ptrs st)]
      (assoc-in st [:memory dest] (f a b)))))

(def int-add (int-binary-op +))

(def int-mult (int-binary-op *))

(defn int-input
  [st]
  (let [st (read-param st)
        [dest] (:ptrs st)
        [val & input] (:input st)]
    (merge st {:memory (assoc (:memory st) dest val)
               :input input})))

(defn int-output
  [st]
  (let [st (read-param st)
        [val] (:params st)
        output (conj (:output st) val)]
    (merge st {:output output})))

(defn int-jump-if-true
  [st]
  (let [st (read-params 2 st)
        [a target] (:params st)]
    (if (zero? a)
      st
      (assoc st :ip target))))

(defn int-jump-if-false
  [st]
  (let [st (read-params 2 st)
        [a target] (:params st)]
    (if-not (zero? a)
      st
      (assoc st :ip target))))

(def int-less-than (int-binary-op (fn [a b] (if (< a b) 1 0))))

(def int-equals  (int-binary-op (fn [a b] (if (= a b) 1 0))))

(def opcodes
  {1 int-add
   2 int-mult
   3 int-input
   4 int-output
   5 int-jump-if-true
   6 int-jump-if-false
   7 int-less-than
   8 int-equals})

(defn int-code
  ([memory] (int-code memory []))
  ([memory input]
   (loop [st (initial-state memory input)]
     (let [st (read-instruction st)
           {:keys [opcode]} st]
       (if (= opcode 99)
         st
         (do
           ;;(println st)
           (recur ((opcodes opcode) st))))))))

(defn amp
  ([program phase-settings] (amp program phase-settings 0))
  ([program [phase-setting & phase-settings] input]
   (let [st (int-code program [phase-setting input])
         [output] (:output st)]
     (if (empty? phase-settings)
       output
       (recur program phase-settings output)))))

;; https://stackoverflow.com/a/26076145/115478
(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

(defn maximize
  [program]
  (->> (range 0 5)
       (permutations)
       (map #(amp program %))
       (apply max)))

(defn solve
  [input]
  (let [program (as-> input i
                  (s/trim i)
                  (s/split i #",")
                  (map #(Integer. %) i)
                  (vec i))]
    {:part1 (maximize program)}))
