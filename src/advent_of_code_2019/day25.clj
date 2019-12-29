(ns advent-of-code-2019.day25
  (:require [advent-of-code-2019.util :refer :all]
            [advent-of-code-2019.int-code :refer :all]
            [clojure.string :as s]
            [clojure.set]))

;;(defn parse-prompt
;;  [prompt]
;;  (let [lines (s/split-lines (s/trim prompt))
;;        [_ name] (re-matches #"== (.*) ==" (first lines))
;;        descr (->> lines
;;                   (drop 1)
;;                   (take-while not-empty)
;;                   (s/join "\n"))
;;        doors (->> lines
;;                   (drop-while #(not= "Doors here lead:" %))
;;                   (drop 1)
;;                   (take-while #(s/starts-with? % "- "))
;;                   (mapv #(subs % 2)))
;;        items (->> lines
;;                   (drop-while #(not= "Items here:" %))
;;                   (drop 1)
;;                   (take-while #(s/starts-with? % "- "))
;;                   (mapv #(subs % 2)))
;;        ]
;;    {:name      name
;;     :descr     descr
;;     :doors     doors
;;     :items     items
;;     :neighbors {}
;;     }))


;;(defn read-prompt
;;  [game]
;;  (let [{:keys [brain]} game
;;        [prompt brain] (int-read-all-output brain)
;;        prompt (s/join (map char prompt))
;;        room (parse-prompt prompt)]
;;    (clojure.pprint/pprint room)
;;    (-> game
;;        (assoc :brain brain)
;;        (assoc :room room)
;;        (assoc-in [:rooms (:name room)] room))))



;; https://codereview.stackexchange.com/a/19269/108394
(defn powerset [ls]
  (if (empty? ls) '(())
                  (let [n (next ls)]
                    (clojure.set/union (powerset (next ls))
                                       (map #(conj % (first ls)) (powerset (next ls)))))))

(defn bot-command
  [brain dir]
  (int-resume-input brain (mapv int (str dir "\n"))))

(defn manual-control
  ([brain] (manual-control brain []))
  ([brain commands]
   (if (int-halted? brain)
     brain
     (let [[prompt brain] (int-read-all-output brain)
           prompt (s/join (map char prompt))
           _ (println "==> " prompt)
           command (read-line)]
       (if-not (= command "GO")
         (recur (bot-command brain command) (conj commands command))
         [brain commands])))))

(defn get-inventory
  [brain]
  (let [brain (bot-command brain "inv")
        [prompt brain] (int-read-all-output brain)
        prompt (s/join (map char prompt))
        lines (s/split-lines prompt)
        items (->> lines
                   (drop-while #(not= "Items in your inventory:" %))
                   (drop 1)
                   (take-while #(s/starts-with? % "- "))
                   (mapv #(subs % 2)))]
    items))

(defn find-password
  ([brain]
   (let [[_ brain] (int-read-all-output brain)              ;; read all old output
         brain (bot-command brain "east")
         [prompt brain] (int-read-all-output brain)
         prompt (s/join (map char prompt))
         ]
     (s/split-lines (s/trim prompt)))))

(def preload (mapv int
                   (s/join "\n"
                           ["west"
                            "west"
                            "west"
                            "take coin"
                            "east"
                            "east"
                            "east"
                            "north"
                            "east"
                            "take cake"
                            "west"
                            "north"
                            "east"
                            "take antenna"
                            "west"
                            "south"
                            "south"
                            "east"
                            "east"
                            "east"
                            "east"
                            "take boulder"
                            "north"
                            ""])))

(defn solve
  [input]
  (let [program (int-parse input)
        brain (int-code program)
        ;; move where we know already
        brain (int-resume-input brain preload)
        ;; let the human gather items and move to security checkpoint
        ;;[brain moves] (manual-control brain)
        ;;_ (clojure.pprint/pprint moves)
        ]
    {:password (find-password brain)}))
