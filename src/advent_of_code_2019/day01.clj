(ns advent-of-code-2019.day01)

(defn calculate-fuel-naive
  [mass]
  (let [fuel   (- (int (/ mass 3)) 2)]
    (max fuel 0)))

(defn calculate-fuel
  [mass]
  (if (> mass 0)
    (let [fuel (calculate-fuel-naive mass)
          fuel-for-the-fuel (calculate-fuel fuel)] ; who needs tail recursion?
      (+ fuel fuel-for-the-fuel))
    0))

(defn solve
  [input]
  (let [module-weights (->> input
                            (clojure.string/split-lines)
                            (map #(Integer. %))
                            )]
    {
     :naive (apply + (map calculate-fuel-naive module-weights))
     :less-naive  (apply + (map calculate-fuel module-weights))
     }))
