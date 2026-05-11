(ns thue-morse
  (:require [cellular-automata :as ca]
            [experiment-runner :as er]))

;; Thue-Morse 2D pattern: cell [x,y] is alive iff the sum of the
;; popcount (number of 1-bits) of x and y is even.
;; This produces a fractal pattern related to the Prouhet-Thue-Morse sequence,
;; with connections to overlap-free sequences, automatic sequences, and
;; equal-power-sum partitions of integers.
(def patterns
  [{:name "thue-morse"
    :target-fn (fn [[x y] _ _]
                 (even? (+ (Integer/bitCount x)
                           (Integer/bitCount y))))}])

(def base-config
  {:grid-limits        [30 30]
   :population-size    200
   :generation-limit   200
   :ca-steps           80
   :fitness-window     20
   :elite-count        4
   :n-runs             1
   :fitness-transform  :ca-neat
   :pca-program-min    5
   :pca-program-max    30
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
   :num-signals          4})

(defn make-condition-config [pattern]
  (let [grid-limits (:grid-limits base-config)
        target-grid (er/make-target-grid grid-limits (:target-fn pattern))]
    (merge base-config
           {:init-grid       (er/make-init-grid grid-limits)
            :target-grid     target-grid
            :cell-neighbors  (partial ca/von-neumann-neighbors 1 grid-limits)})))

(def results-file
  "experiments/thue_morse/results/thue_morse_results.edn")

;; PCA vs NCA vs Hybrid on Thue-Morse fractal pattern
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
         (er/save-results! "thue-morse" base-config completed'
                           results-file)
         (recur (rest remaining) completed'))))))

(run-experiment!)
