(ns advent-of-code-2019.day23
  (:require [advent-of-code-2019.int-code :refer :all]
            [advent-of-code-2019.util :refer :all]))

(defn init-node
  [brain addr]
  (int-resume-input brain addr))

(defn read-packets
  [node]
  (let [{:keys [brain]} node
        [output brain] (int-read-all-output brain)
        packets (partition 3 output)
        packets (mapv (fn [[a x y]] {:addr a :data [x y]}) packets)]
    [packets brain]))

(defn init-network
  [num-nodes brain]
  (let [nodes (->> (range 0 num-nodes)
                   (mapv (partial init-node brain))
                   (mapv (fn [node] {:brain node :input empty-queue})))]
    {:nodes nodes
     :nat nil}))

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

(defn run-network
  [network]
  ;; read all outputs
  (let [network (read-all-outputs network)
        network (write-one-input network)]
    network))

(defn print-network
  [network]
  (clojure.pprint/pprint (mapv (fn [[addr node]] [addr (:input node) (:output (:brain node))]) network)))

(defn network-seq
  [network]
  ;;(print-network network)
  (cons network (lazy-seq (network-seq (run-network network)))))

(defn solve
  [input]
  (let [program (int-parse input)
        brain (int-code program)]
    {:first-255 (->> brain
                     (init-network 50)
                     (network-seq)
                     (drop-while (fn [network]
                                   (empty? (get-in network [:nat]))))
                     (first)
                     (:nat)
                     (second))}))
