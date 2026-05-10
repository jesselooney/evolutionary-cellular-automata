(ns local-patterns
  (:require [cellular-automata :as ca]
            [experiment-runner :as er]))

(def patterns
  [{:name "checkerboard"
    :target-fn (fn [[x y] _ _] (even? (+ x y)))}

   {:name "vertical-stripes"
    :target-fn (fn [[x _] _ _] (even? x))}

   {:name "horizontal-stripes"
    :target-fn (fn [[_ y] _ _] (even? y))}

   {:name "sparse-dots"
    :target-fn (fn [[x y] _ _] (and (even? x) (even? y)))}

   {:name "dense-dots"
    :target-fn (fn [[x y] _ _] (or (even? x) (even? y)))}

   {:name "wide-stripes"
    :target-fn (fn [[x _] _ _] (< (mod x 4) 2))}

   {:name "diagonal-bands"
    :target-fn (fn [[x y] _ _] (< (mod (+ x y) 4) 2))}])

(def base-config
  {:grid-limits        [10 10]
   :population-size    200
   :generation-limit   50
   :ca-steps           30
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
   :crossover-rate       0.75
   :num-signals          3})

(defn make-condition-config [pattern]
  (let [grid-limits (:grid-limits base-config)
        target-grid (er/make-target-grid grid-limits (:target-fn pattern))]
    (merge base-config
           {:init-grid       (er/make-init-grid grid-limits)
            :target-grid     target-grid
            :cell-neighbors  (partial ca/von-neumann-neighbors 1 grid-limits)})))

(def results-file
  "experiments/local_patterns/results/local_patterns_results.edn")

;; PCA vs NCA vs Hybrid across locally-solvable patterns
(defn run-experiment!
  ([] (run-experiment! (:n-runs base-config)))
  ([n-runs]
   (loop [remaining patterns
          completed []]
     (if (empty? remaining)
       completed
       (let [pattern   (first remaining)
             cfg       (make-condition-config pattern)
             condition (er/run-condition!
                        (:name pattern) cfg
                        {:pca    er/run-pca-crossover
                         :nca    er/run-nca-crossover
                         :hybrid er/run-hybrid-crossover}
                        n-runs)
             completed' (conj completed condition)]
         (er/save-results! "local-patterns" base-config completed'
                           results-file)
         (recur (rest remaining) completed'))))))


(run-experiment!)
