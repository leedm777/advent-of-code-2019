(ns advent-of-code-2019.day14
  (:require [clojure.string :as s]))

(def ORE "ORE")
(def FUEL "FUEL")

(defn parse-qc
  [qc]
  (->> qc
       (re-matcher #"^(\d+) ([A-Z]+)$")
       (re-find)
       ((fn [[_ quantity chemical]]
          [(bigint quantity) chemical]))))

(defn parse-input
  [csv]
  (->> (s/split csv #", ")
       (mapv parse-qc)))

(defn parse-line
  [line]
  (->> line
       (re-matcher #"^(.*) => (.*)$")
       (re-find)
       ((fn [[_ input output]]
          (let [[q c] (parse-qc output)]
            [c { :quantity q :input (parse-input input)}])))))

(defn parse-reactions
  [input]
  (->> input
       (s/split-lines)
       (mapv parse-line)
       (into { ORE { :quantity 1, :input [[1 ORE]]}})))

(defn needed-chemicals
  [balance]
  (select-keys balance
               (for [[k v] balance :when (pos? v)] k)))

(defn unreact
  [reactions balance]
  (reduce (fn [balance [c q]]
            (let [reaction (get reactions c q)
                  q-made (:quantity reaction)
                  num (bigint (Math/ceil (/ q q-made)))
                  balance (merge-with - balance {c (* num q-made)})
                  need (into {} (mapv (fn [[q c]] [c (* num q)]) (:input reaction)))
                  ]
              (comment (clojure.pprint/pprint {:reaction reaction
                                               :num num
                                               :balance balance
                                               :need need}))
              (merge-with + balance need)))
          balance
          balance))

(defn how-much-ore
  [reactions balance]
  (if (= [ORE] (keys (needed-chemicals balance)))
    balance
    (do
      ;; (clojure.pprint/pprint balance)
      (recur reactions (unreact reactions balance)))))

(defn max-fuel
  [reactions q-ore]
  (let [ore-per-fuel (get (how-much-ore reactions {FUEL 1}) ORE)
        ]
    (loop [good-fuel (quot q-ore ore-per-fuel)
           bad-fuel (* 2 good-fuel)]
      (if (= (inc good-fuel) bad-fuel)
        good-fuel
        (let [mid (quot (+ good-fuel bad-fuel) 2)
              mid-ore (get (how-much-ore reactions {FUEL mid}) ORE)]
          (if (< mid-ore q-ore)
            (recur mid bad-fuel)
            (recur good-fuel mid)))))))

(defn solve
  [input]
  (let [reactions (parse-reactions input)
        ore-per-fuel (get (how-much-ore reactions {FUEL 1}) ORE)]
    {:how-much-ore ore-per-fuel
     :how-much-fuel (max-fuel reactions 1000000000000)}))
