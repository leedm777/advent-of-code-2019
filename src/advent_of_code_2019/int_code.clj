(ns advent-of-code-2019.int-code
  (:require [clojure.string :as s]))

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defmethod print-method clojure.lang.PersistentQueue [q, w] ; Overload the printer for queues so they look like fish
  (print-method '<- w)
  (print-method (seq q) w)
  (print-method '-< w))

(defn initial-state
  "Initializes the int-code computer"
  ([memory] (initial-state memory empty-queue))
  ([memory input]
   {:memory memory
    :input (apply conj empty-queue input)
    :output empty-queue
    :ip 0
    :param-mode 0
    :params []
    :ptrs []
    :paused false
    :relative-base 0
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
  (let [{:keys [memory ip ptrs params param-mode relative-base]} st
        mode (mod param-mode 10)
        ptr (case mode
              ;; 0 - position mode
              0 (memory ip)
              ;; 1 - immediate mode
              1 ip
              ;; 2 - relative mode
              2 (+ relative-base (memory ip)))
        val (get memory ptr 0)]
    (merge st {:ip (inc ip)
               :params (conj params val)
               :ptrs (conj ptrs ptr)
               :param-mode (quot param-mode 10)})))

(defn read-params
  "Read n parameters from memory"
  [n st]
  (nth (iterate read-param st) n))

(defn extend-memory
  "Extend memory w/ zeros to the given destination"
  [mem dest]
  (vec (concat mem (repeat (inc (- dest (count mem))) 0))))

(defn int-set-memory
  "Sets memory at dest to the given val"
  [st dest val]
  (let [{:keys [memory]} st
        memory (extend-memory memory dest)]
    (assoc st :memory (assoc memory dest val))))

(defn int-binary-op
  "Creates a binary operation with two parameters and a destination ptr"
  [f]
  (fn [st]
    (let [st (read-params 3 st)
          [a b] (:params st)
          [_ _ dest] (:ptrs st)]
      (int-set-memory st dest (f a b)))))

(def int-add (int-binary-op +))

(def int-mult (int-binary-op *))

(defn int-input
  "Reads one value from input"
  [st]
  (if (empty? (:input st))
    (assoc st :paused true)
    (let [st (read-param st)
          [dest] (:ptrs st)
          val (peek (:input st))
          input (pop (:input st))
          st (int-set-memory st dest val)]
      (assoc st :input input))))

(defn int-output
  "Writes one value to output"
  [st]
  (let [st (read-param st)
        [val] (:params st)
        output (conj (:output st) val)]
    (merge st {:output output})))

(defn int-jump-if-true
  "Jumps to the target address if param is non-zero"
  [st]
  (let [st (read-params 2 st)
        [a target] (:params st)]
    (if (zero? a)
      st
      (assoc st :ip target))))

(defn int-jump-if-false
  "Jumps to the target address if param is zero"
  [st]
  (let [st (read-params 2 st)
        [a target] (:params st)]
    (if-not (zero? a)
      st
      (assoc st :ip target))))

(def int-less-than (int-binary-op (fn [a b] (if (< a b) 1 0))))

(def int-equals  (int-binary-op (fn [a b] (if (= a b) 1 0))))

(defn int-halt
  "Halts execution"
  [st]
  (assoc st :paused true))

(defn int-adjust-relative-base
  "Adjusts relative base pointer by adding the given param"
  [st]
  (let [st (read-params 1 st)
        [adj] (:params st)
        relative-base (:relative-base st)]
    (assoc st :relative-base (+ relative-base adj))))

(def opcodes
  {1 int-add
   2 int-mult
   3 int-input
   4 int-output
   5 int-jump-if-true
   6 int-jump-if-false
   7 int-less-than
   8 int-equals
   9 int-adjust-relative-base
   99 int-halt})

(defn int-exec
  "Executes the current int-code opcode."
  [st]
  (let [{:keys [opcode]} st
        op (opcodes opcode)
        next-st (op st)]
    ;; (println next-st) ; debug
    (if (:paused next-st)
      next-st
      (recur (read-instruction next-st)))))

;;
;; public APIS
;;

(defn int-code
  "Executes an int-code program, with optional input.

  The returned state could be a halted program, or simply paused
  waiting for more input (use int-resume-input)."
  ([memory] (int-code memory []))
  ([memory input]
   (let [st (read-instruction (initial-state memory input))]
     (int-exec st))))

(defn int-resume-input
  "Resumes an int-code program with additional input."
  [st inputs]
  (if-not (sequential? inputs) ;; I keep calling it with single inputs...
    (recur st [inputs])
    (let [input (:input st)]
      (int-exec (merge st { :input (apply conj input inputs), :paused false })))))

(defn int-read-output
  "Reads a value from output."
  [st]
  (let [out (peek (:output st))
        st (assoc st :output (pop (:output st)))]
    [out st]))

(defn int-read-all-output
  "Reads all output as a vector"
  [st]
  (let [out (vec (:output st))
        st (assoc st :output empty-queue)]
    [out st]))

(defn int-halted?
  "Returns true if the given program has halted."
  [st]
  (= 99 (:opcode st)))

(defn int-parse
  [input]
  (as-> input i
    (s/trim i)
    (s/split i #",")
    (map #(Long/parseLong %) i)
    (vec i)))
