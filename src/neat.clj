(ns neat
  "NEAT genome operations: construction and mutation."
  (:require [cppn :as cppn]))

;;;; Genome construction.

(defn make-genome
  "Minimal genome with random 50–100% of possible input→output connections."
  [num-inputs num-outputs]
  (let [in-ids  (range num-inputs)
        out-ids (range num-inputs (+ num-inputs num-outputs))
        nodes (merge (into {} (for [id in-ids]  [id {:type :input  :activation :sigmoid}]))
                     (into {} (for [id out-ids] [id {:type :output :activation :identity}])))
        all-pairs (for [i in-ids o out-ids] [i o])
        n-total (count all-pairs)
        n-min   (max 1 (int (Math/ceil (* 0.5 n-total))))
        n-keep  (+ n-min (rand-int (- (inc n-total) n-min)))
        conns (vec (map (fn [[i o]]
                          {:in i :out o
                           :weight (- (* 2.0 (rand)) 1.0)
                           :enabled true})
                        (take n-keep (shuffle all-pairs))))]
    (assoc (cppn/make-cppn nodes conns) :fitness nil)))

;;;; Mutations.

(defn- rand-gaussian []
  (let [u (rand)]
    (* (Math/sqrt (* -2.0 (Math/log (max u 1e-10))))
       (Math/cos (* 2.0 Math/PI (rand))))))

(defn mutate-weights
  "Perturb weights: 90% gaussian (σ=0.5), 10% full replacement [-2,2]."
  [genome]
  (update genome :connections
          (partial mapv #(if (< (rand) 0.9)
                           (update % :weight + (* 0.5 (rand-gaussian)))
                           (assoc % :weight (- (* 4.0 (rand)) 2.0))))))

(defn- would-cycle?
  "DFS check: would adding from→to create a cycle?"
  [connections from to]
  (let [adj (reduce (fn [a {:keys [in out]}]
                      (update a in (fnil conj #{}) out))
                    {from #{to}}
                    (filter :enabled connections))]
    (loop [stack [to] visited #{}]
      (cond (empty? stack)            false
            (= (peek stack) from)     true
            (visited (peek stack))    (recur (pop stack) visited)
            :else (recur (into (pop stack) (get adj (peek stack) []))
                         (conj visited (peek stack)))))))

(defn add-connection
  "Add a new connection between two previously unconnected, acyclic nodes."
  [genome]
  (let [{:keys [nodes connections]} genome
        existing (set (map (juxt :in :out) connections))
        valid (for [f (keys nodes) t (keys nodes)
                    :when (and (not= f t)
                               (not= :output (:type (nodes f)))
                               (not= :input  (:type (nodes t)))
                               (not (existing [f t]))
                               (not (would-cycle? connections f t)))]
                [f t])]
    (if (empty? valid) genome
      (let [[f t] (rand-nth valid)]
        (cppn/make-cppn nodes (conj connections
                                    {:in f :out t
                                     :weight (- (* 2.0 (rand)) 1.0)
                                     :enabled true}))))))

(defn add-node
  "Split a random enabled connection by inserting a hidden node."
  [genome]
  (let [{:keys [nodes connections]} genome
        enabled (keep-indexed (fn [i c] (when (:enabled c) i)) connections)]
    (if (empty? enabled) genome
      (let [idx (rand-nth enabled)
            c   (connections idx)
            nid (inc (apply max (keys nodes)))]
        (cppn/make-cppn
         (assoc nodes nid {:type :hidden
                           :activation (rand-nth cppn/hidden-activation-types)})
         (-> connections
             (assoc idx (assoc c :enabled false))
             (conj {:in (:in c) :out nid :weight 1.0 :enabled true})
             (conj {:in nid :out (:out c) :weight (:weight c) :enabled true})))))))

(defn mutate [genome config]
  (let [{:keys [weight-mutation-rate add-connection-rate add-node-rate]
         :or {weight-mutation-rate 0.8 add-connection-rate 0.05 add-node-rate 0.03}} config]
    (cond-> genome
      (< (rand) weight-mutation-rate) mutate-weights
      (< (rand) add-connection-rate)  add-connection
      (< (rand) add-node-rate)        add-node)))
