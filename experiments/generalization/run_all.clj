(ns generalization
  (:require [cellular-automata :as ca]
            [experiment-runner :as er]))

(def patterns
  [{:name "checkerboard"
    :target-fn (fn [[x y] _ _] (even? (+ x y)))}
   {:name "vertical-stripes"
    :target-fn (fn [[x _] _ _] (even? x))}
   {:name "sparse-dots"
    :target-fn (fn [[x y] _ _] (and (even? x) (even? y)))}])

(def train-grid [10 10])
(def test-grids [[15 15] [20 20] [25 25]])

(def base-config
  {:grid-limits        train-grid
   :population-size    150
   :generation-limit   50
   :ca-steps           20
   :fitness-window     10
   :elite-count        2
   :n-runs             10
   :fitness-transform  :ca-neat
   :pca-program-min    5
   :pca-program-max    20
   :num-neighbors      5
   :num-inputs         5
   :num-outputs        2
   :weight-mutation-rate 0.6
   :add-connection-rate  0.15
   :add-node-rate        0.10
   :compatibility-threshold 0.9
   :c1                   1.0
   :c3                   0.4
   :crossover-rate       0.75})

;; evaluate a rule on a different grid size for generalization
(defn test-generalization
  [rule-fn target-fn grid-limits ca-steps]
  (let [init-grid      (er/make-init-grid grid-limits)
        target-grid    (er/make-target-grid grid-limits target-fn)
        cell-neighbors (partial ca/von-neumann-neighbors 1 grid-limits)]
    (er/evaluate-rule init-grid cell-neighbors target-grid
                      rule-fn ca-steps)))

;; best PCA program tested on all test grids
(defn generalize-pca-run
  [history target-fn ca-steps]
  (let [best-record (apply min-key :best-error history)
        best-prog   (:best-program best-record)
        rule-fn     (partial er/push-nv best-prog)]
    {:train-error (:best-error best-record)
     :results
     (mapv (fn [grid-size]
             (let [result (test-generalization rule-fn target-fn grid-size ca-steps)]
               {:grid-size grid-size
                :min-error (float (:min-error result))
                :best-step (:best-step result)}))
           test-grids)}))

;; best NCA genome tested on all test grids
(defn generalize-nca-run
  [history target-fn ca-steps]
  (let [best-record (apply min-key :best-error history)
        best-genome (:best-genome best-record)
        rule-fn     (partial er/cppn-nv best-genome)]
    {:train-error (:best-error best-record)
     :results
     (mapv (fn [grid-size]
             (let [result (test-generalization rule-fn target-fn grid-size ca-steps)]
               {:grid-size grid-size
                :min-error (float (:min-error result))
                :best-step (:best-step result)}))
           test-grids)}))

(def results-file
  "experiments/generalization/results/generalization_results.edn")

;; evolve one pattern, then test generalization
(defn run-and-generalize!
  [pattern config n-runs]
  (let [{:keys [name target-fn]} pattern
        ca-steps (:ca-steps config)
        _        (println (str "\n=== Condition: " name " ==="))
        _        (println "  Running pca...")
        pca-runs (mapv #(er/run-pca-crossover config %) (range n-runs))
        pca-gen  (mapv #(generalize-pca-run % target-fn ca-steps) pca-runs)
        _        (println "  Running nca...")
        nca-runs (mapv #(er/run-nca-crossover config %) (range n-runs))
        nca-gen  (mapv #(generalize-nca-run % target-fn ca-steps) nca-runs)]
    {:name    name
     :config  config
     :methods {:pca pca-runs
               :nca nca-runs}
     :generalization {:pca pca-gen
                      :nca nca-gen}}))

;; save results with generalization data, stripping programs/genomes
(defn save-generalization-results!
  [experiment-name base-config conditions filename]
  (let [strip (fn [record] (dissoc record :best-program :best-genome))
        process-runs (fn [runs]
                       (let [all  (apply concat runs)
                             best (apply min-key :best-error all)]
                         {:runs         (mapv #(mapv strip %) runs)
                          :best-overall {:error      (:best-error best)
                                         :generation (:generation best)
                                         :step       (:best-step best)}}))]
    (spit filename
          (pr-str {:experiment   experiment-name
                   :timestamp    (str (java.time.Instant/now))
                   :base-config  (dissoc base-config
                                         :init-grid :target-grid :cell-neighbors)
                   :train-grid   train-grid
                   :test-grids   test-grids
                   :conditions
                   (mapv (fn [{:keys [name methods generalization]}]
                           (merge {:name name}
                                  (into {} (for [[mk runs] methods]
                                             [mk (process-runs runs)]))
                                  {:generalization generalization}))
                         conditions)}))
    (println (str "Results saved to " filename))))

;; full generalization experiment across all patterns
(defn run-experiment!
  ([] (run-experiment! (:n-runs base-config)))
  ([n-runs]
   (loop [remaining  patterns
          completed  []]
     (if (empty? remaining)
       completed
       (let [pattern   (first remaining)
             cfg       (let [grid-limits (:grid-limits base-config)
                             target-grid (er/make-target-grid grid-limits
                                           (:target-fn pattern))]
                         (merge base-config
                                {:init-grid      (er/make-init-grid grid-limits)
                                 :target-grid    target-grid
                                 :cell-neighbors (partial ca/von-neumann-neighbors
                                                          1 grid-limits)}))
             condition  (run-and-generalize! pattern cfg n-runs)
             completed' (conj completed condition)]
         (save-generalization-results! "generalization" base-config completed'
                                       results-file)
         (recur (rest remaining) completed'))))))


(run-experiment!)
