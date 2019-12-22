(ns advent-of-code-2019.day21
  (:require [advent-of-code-2019.int-code :refer :all]
            [advent-of-code-2019.util :refer :all]
            [clojure.string :as s]))

(def walkcode
  (str-to-ascii
    (s/join "\n"
            ;; at most 15 instructions
            ;; jumps 3 spaces
           [;; if A, B or C are blank, and D is filled, JUMP
            "NOT A J"
            "NOT B T"
            "OR T J"
            "NOT C T"
            "OR T J"
            "AND D J"
            "WALK"
            ""])))

(def runcode
  (str-to-ascii
    (s/join "\n"
            ;; at most 15 instructions
            ;; jumps 4 spaces, but needs A to be villed
            [;; if E is filled, but any of A-D are blank, JUMP
             "NOT A J"
             "NOT B T"
             "OR T J"
             "NOT C T"
             "OR T J"
             "AND D J"
             "AND E J"
             "RUN"
             ""])))
(defn solve
  [input]
  (let [program (int-parse input)
        springbot (int-code program)
        ;; ignore the prompt
        [_ springbot] (int-read-all-output springbot)

        ;; part 1 - send bot out walking
        walkbot (int-resume-input springbot walkcode)
        [walk-result] (int-read-all-output walkbot)

        ;; part 2 - send bot out running
        runbot (int-resume-input springbot runcode)
        [run-result] (int-read-all-output runbot)
        ]

    (if (< (last walk-result) 128)
      (println (ascii-to-str walk-result)))

    (if (< (last run-result) 128)
      (println (ascii-to-str run-result)))
    {:walk (last walk-result)
     :run (last run-result)}))
