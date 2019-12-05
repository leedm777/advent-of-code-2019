(ns advent-of-code-2019.day05
  (:require [clojure.string :as s]))

(defn initial-state
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

(def opcodes
  {;; 1 - addition
   1 (fn [st]
       (let [st (read-params 3 st)
             {:keys [params ptrs]} st
             [a b] params
             [_ _ dest] ptrs]
         (assoc-in st [:memory dest] (+ a b))))
   ;; 1 - multiplication
   2 (fn [st]
       (let [st (read-params 3 st)
             {:keys [params ptrs]} st
             [a b] params
             [_ _ dest] ptrs]
         (assoc-in st [:memory dest] (* a b))))
   ;; 3 - input
   3 (fn [st]
       (let [st (read-param st)
             {:keys [ptrs input memory]} st
             [dest] ptrs
             [val & input] (:input st)]
         (merge st {:memory (assoc memory dest val)
                    :input input})))
   ;; 4 - output
   4 (fn [st]
       (let [st (read-param st)
             {:keys [params output]} st
             [val] params
             output (conj output val)]
         (merge st {:output output})))
   ;; 5 - jump-if-true
   5 (fn [st]
       (let [st (read-params 2 st)
             [a target] (:params st)]
         (if (zero? a)
           st
           (assoc st :ip target))))
   ;; 6 - jump-if-false
   6 (fn [st]
       (let [st (read-params 2 st)
             [a target] (:params st)]
         (if-not (zero? a)
           st
           (assoc st :ip target))))
   ;; 7 - less than
   7 (fn [st]
       (let [st (read-params 3 st)
             [a b] (:params st)
             [_ _ dest] (:ptrs st)
             val (if (< a b) 1 0)]
         (assoc-in st [:memory dest] val)))
   ;; 8 - equals
   8 (fn [st]
       (let [st (read-params 3 st)
             [a b] (:params st)
             [_ _ dest] (:ptrs st)
             val (if (= a b) 1 0)]
         (assoc-in st [:memory dest] val)))})

(defn int-code
  ([memory] (int-code memory []))
  ([memory input]
   (loop [st (initial-state memory input)]
     (let [st (read-instruction st)
           {:keys [opcode]} st]
       (if (= opcode 99)
         st
         (do
           (recur ((opcodes opcode) st))))))))

(defn solve
  [input]
  (let [program (as-> input i
                  (s/trim i)
                  (s/split i #",")
                  (map #(Integer. %) i)
                  (vec i))]
    {:part1 (:output (int-code program [1]))
     :part2 (:output (int-code program [5]))}))
