(ns cppn
  "Compositional Pattern Producing Networks (CPPNs)

   A CPPN is a directed acyclic graph of nodes with heterogeneous activation
   functions and weighted connections. Unlike standard ANNs, CPPNs use a variety
   of activation functions (sin, gaussian, etc.) to produce patterns with
   symmetry, repetition, and variation

   A CPPN is represented as a map:
   - `:nodes` — map from integer node IDs to node descriptors
     `{:type (:input | :output | :hidden), :activation keyword}`
   - `:connections` — vector of connection maps
     `{:in node-id, :out node-id, :weight double, :enabled bool}`
   - `:eval-order` — cached topological ordering of node IDs (computed by `make-cppn`)")

;;;; Activation functions

(def activation-fns
  {:sigmoid  (fn [x] (/ 1.0 (+ 1.0 (Math/exp (- (double x))))))
   :tanh     (fn [x] (Math/tanh (double x)))
   :sin      (fn [x] (Math/sin (double x)))
   :gaussian (fn [x] (Math/exp (- (/ (* (double x) (double x)) 2.0))))
   :relu     (fn [x] (max 0.0 (double x)))
   :identity (fn [x] (double x))
   :clamped  (fn [x] (max 0.0 (min 1.0 (double x))))
   :inverse  (fn [x] (let [x (double x)] (if (zero? x) 0.0 (/ 1.0 x))))
   :log      (fn [x] (let [x (double x)] (if (pos? x) (Math/log x) 0.0)))
   :exp      (fn [x] (Math/exp (double x)))
   :abs      (fn [x] (Math/abs (double x)))
   :hat      (fn [x] (let [x (double x)] (if (< (Math/abs x) 1.0) (- 1.0 (Math/abs x)) 0.0)))
   :square   (fn [x] (let [x (double x)] (* x x)))
   :cube     (fn [x] (let [x (double x)] (* x x x)))})

(def hidden-activation-types
  (vec (keys activation-fns)))

;;;; Topological sort

(defn topological-sort
  [nodes connections]
  (let [enabled (filter :enabled connections)
        all-ids (set (keys nodes))

        in-degree (reduce (fn [deg {:keys [out]}]
                            (update deg out (fnil inc 0)))
                          (zipmap all-ids (repeat 0))
                          enabled)
        adj (reduce (fn [a {:keys [in out]}]
                      (update a in (fnil conj []) out))
                    {}
                    enabled)]
    (loop [queue (into clojure.lang.PersistentQueue/EMPTY
                       (filter #(zero? (get in-degree %)) all-ids))
           in-deg in-degree
           order []]
      (if (empty? queue)
        order
        (let [n (peek queue)
              queue (pop queue)
              neighbors (get adj n [])
              [queue' in-deg']
              (reduce (fn [[q d] m]
                        (let [d' (update d m dec)]
                          (if (zero? (get d' m))
                            [(conj q m) d']
                            [q d'])))
                      [queue in-deg]
                      neighbors)]
          (recur queue' in-deg' (conj order n)))))))


(defn make-cppn
  [nodes connections]
  {:nodes nodes
   :connections connections
   :eval-order (topological-sort nodes connections)})

;;;; Forward pass

(defn evaluate-cppn
  [cppn input-values]
  (let [{:keys [nodes connections eval-order]} cppn
        input-ids (sort (map key (filter #(= :input (:type (val %))) nodes)))
        output-ids (sort (map key (filter #(= :output (:type (val %))) nodes)))
        enabled (filter :enabled connections)
        ;; Pre-group incoming connections by target node
        incoming (group-by :out enabled)
        ;; Seed node activations with input values
        init-vals (zipmap input-ids (map double input-values))
        ;; Walk topological order, computing each node's output
        node-vals (reduce
                   (fn [vals node-id]
                     (if (contains? vals node-id)
                       ;; Input node — already seeded; apply its activation
                       (let [act-fn (get activation-fns
                                        (:activation (get nodes node-id))
                                        (:identity activation-fns))]
                         (update vals node-id act-fn))
                       ;; Hidden or output node — sum weighted inputs, then activate
                       (let [in-conns (get incoming node-id [])
                             weighted-sum (reduce (fn [^double s {:keys [in weight]}]
                                                    (+ s (* (double (get vals in 0.0))
                                                            (double weight))))
                                                  0.0
                                                  in-conns)
                             act-fn (get activation-fns
                                        (:activation (get nodes node-id))
                                        (:identity activation-fns))]
                         (assoc vals node-id (act-fn weighted-sum)))))
                   init-vals
                   eval-order)]
    (mapv #(get node-vals % 0.0) output-ids)))


(defn normalize-state
  [state num-states]
  (cond
    (boolean? state) (if state 1.0 -1.0)
    (integer? state) (if (<= num-states 1)
                       0.0
                       (- (* 2.0 (/ (double state) (dec num-states))) 1.0))
    :else (double state)))

(defn decode-output
  [output-values num-states]
  (let [max-idx (first (apply max-key second (map-indexed vector output-values)))]
    (if (= num-states 2)
      (= max-idx 1)
      max-idx)))
