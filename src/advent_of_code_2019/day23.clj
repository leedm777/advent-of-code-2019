(ns advent-of-code-2019.day23
  (:require [advent-of-code-2019.int-code :refer :all]
            [advent-of-code-2019.util :refer :all]))

(defn init-node
  [brain addr]
  (int-resume-input brain [addr]))

(defn read-packets
  [node]
  (let [{:keys [brain]} node
        [output brain] (int-read-all-output brain)
        packets (partition 3 output)
        packets (mapv (fn [[a x y]] {:addr a :data [x y]}) packets)]
    [packets brain]))

(defn write-one-input
  [network]
  (reduce-kv (fn [network addr node]
               (let [{:keys [brain input]} node
                     next-packet (or (peek input) -1)
                     input (pop input)
                     brain (int-resume-input brain next-packet)]
                 (assoc-in network [:nodes addr] {:input input :brain brain})))
             network
             (:nodes network)))

(defn init-network
  [num-nodes brain]
  (->> (range 0 num-nodes)
       (mapv (partial init-node brain))
       (mapv (fn [node] {:brain node :input empty-queue}))
       (#(hash-map :nodes % :nat nil))))

(defn deliver-packets
  [network packets]
  (if (empty? packets)
    network
    (reduce (fn [network packet]
              (let [{:keys [addr data]} packet]
                (if (= addr 255)
                  (assoc network :nat data)
                  (update-in network [:nodes addr :input] #(conj % data)))))
            network
            packets)))

(defn read-all-outputs
  [network]
  (reduce-kv (fn [network addr node]
               (let [[packets node] (read-packets node)
                     network (assoc-in network [:nodes addr :brain] node)]
                 (deliver-packets network packets)))
             network
             (:nodes network)))

(defn print-network
  [network]
  (clojure.pprint/pprint
    {:nat   (:nat network)
     :nodes (map-indexed (fn [addr node]
                           [addr (:input node) (:output (:brain node))])
                         (:nodes network))}))

(defn network-settled?
  [network]
  (let [{:keys [nodes]} network]
    (and (some? (:nat network))
         (every? #(empty (:input %)) nodes)
         (every? #(empty (:output (:brain %))) nodes))))

(defn run-network
  [network]
  ;; read all outputs
  (if (network-settled? network)
    (do
      (println "SETTLED!!! " (:nat network))
      (-> network
          (deliver-packets [{:addr 0 :data (:nat network)}])
          (recur)))
    (-> network
        (read-all-outputs)
        (write-one-input))))

(defn network-seq
  [network]
  ;;(print-network network)
  (cons network (lazy-seq (network-seq (run-network network)))))

(defn find-dupe-nat
  [nseq]
  (println "ohai")
  (reduce (fn [seen network]
            (print-network network)
            (let [{:keys [nat]} network
                  [_ y] nat]
              (if (seen y)
                (reduced y)
                (do
                  (clojure.pprint/pprint seen)
                  (conj seen y)))))
          #{}
          nseq))

(defn solve
  [input]
  (let [program (int-parse input)
        brain (int-code program)
        nseq (->> brain
                  (init-network 50)
                  (network-seq))]
    {
     :first-255 (->> nseq
                     (drop-while (fn [network]
                                   (empty? (:nat network))))
                     (first)
                     (:nat)
                     (second))
     :first-repeat (find-dupe-nat nseq)
     }))
