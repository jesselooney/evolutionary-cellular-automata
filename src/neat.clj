(ns neat
  "NEAT genome operations: construction and mutation."
  (:require [clojure.set :as set]
            [cppn :as cppn]))

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
  (let [{:keys [weight-mutation-rate add-connection-rate add-node-rate max-connections]
         :or {weight-mutation-rate 0.8 add-connection-rate 0.05 add-node-rate 0.03}} config
        at-cap? (and max-connections
                     (>= (count (:connections genome)) (int max-connections)))]
    (cond-> genome
      (< (rand) weight-mutation-rate)                    mutate-weights
      (and (not at-cap?) (< (rand) add-connection-rate)) add-connection
      (and (not at-cap?) (< (rand) add-node-rate))       add-node)))

;;;; Connection key (shared by crossover and compatibility distance).
;; Without innovation numbers, connections are identified by their (in, out) pair.

(defn- connection-key [conn] [(:in conn) (:out conn)])

;;;; Crossover.

(defn crossover
  "NEAT-style crossover between two genomes.
   Connections are aligned by (in, out) key. Matching genes are randomly
   inherited from either parent; disjoint genes come from the more fit parent.
   If fitnesses are equal, disjoint genes from both parents are included."
  [genome-a genome-b]
  (let [fit-a (or (:fitness genome-a) 0.0)
        fit-b (or (:fitness genome-b) 0.0)
        ;; Ensure genome-a is the more (or equally) fit parent
        [g1 g2 equal?] (cond
                          (> fit-a fit-b) [genome-a genome-b false]
                          (< fit-a fit-b) [genome-b genome-a false]
                          :else           [genome-a genome-b true])
        map1 (into {} (map (juxt connection-key identity) (:connections g1)))
        map2 (into {} (map (juxt connection-key identity) (:connections g2)))
        keys1 (set (keys map1))
        keys2 (set (keys map2))
        matching (set/intersection keys1 keys2)
        ;; Build child connections
        child-conns
        (vec (concat
              ;; Matching genes: randomly from either parent
              (map (fn [k] (if (< (rand) 0.5) (map1 k) (map2 k))) matching)
              ;; Disjoint genes from the more fit parent (always)
              (map map1 (set/difference keys1 keys2))
              ;; Disjoint genes from the less fit parent (only if equal fitness)
              (when equal?
                (map map2 (set/difference keys2 keys1)))))
        ;; Collect all node IDs referenced by child connections + original I/O nodes
        io-nodes (into {} (filter (fn [[_ v]] (#{:input :output} (:type v)))
                                  (:nodes g1)))
        conn-node-ids (set (mapcat (fn [c] [(:in c) (:out c)]) child-conns))
        ;; Merge hidden nodes from both parents for referenced IDs
        hidden1 (into {} (filter (fn [[id v]] (and (= :hidden (:type v))
                                                    (conn-node-ids id)))
                                 (:nodes g1)))
        hidden2 (into {} (filter (fn [[id v]] (and (= :hidden (:type v))
                                                    (conn-node-ids id)))
                                 (:nodes g2)))
        child-nodes (merge io-nodes hidden2 hidden1)]
    (assoc (cppn/make-cppn child-nodes child-conns) :fitness nil)))

;;;; Compatibility distance (for NEAT speciation).
;; δ = c1 * (disjoint / N) + c3 * avg-weight-diff-of-matching

(defn compatibility-distance
  "Compute the structural + weight distance between two genomes.
   c1: coefficient for disjoint (non-matching) connections.
   c3: coefficient for average weight difference of matching connections.
   Returns a non-negative double."
  [genome-a genome-b c1 c3]
  (let [conns-a (:connections genome-a)
        conns-b (:connections genome-b)
        map-a   (into {} (map (juxt connection-key identity) conns-a))
        map-b   (into {} (map (juxt connection-key identity) conns-b))
        keys-a  (set (keys map-a))
        keys-b  (set (keys map-b))
        matching    (set/intersection keys-a keys-b)
        disjoint    (count (set/union
                            (set/difference keys-a keys-b)
                            (set/difference keys-b keys-a)))
        n           (double (max (count conns-a) (count conns-b) 1))
        avg-w-diff  (if (empty? matching)
                      0.0
                      (/ (reduce + (map (fn [k]
                                          (Math/abs (- (double (:weight (map-a k)))
                                                       (double (:weight (map-b k))))))
                                        matching))
                         (double (count matching))))]
    (+ (* c1 (/ (double disjoint) n))
       (* c3 avg-w-diff))))
