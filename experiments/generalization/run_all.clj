(ns generalization
  (:require [cellular-automata :as ca]
            [experiment-runner :as er]
            [clojure.edn :as edn]))

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
    :target-fn (fn [[x _] _ _] (< (mod x 4) 2))}])

(def test-grids [[15 15] [20 20] [25 25]])

(def local-patterns-file
  "experiments/local_patterns/results/local_patterns_results.edn")

(def results-file
  "experiments/generalization/results/generalization_results.edn")

;; extract best-rules per condition from local patterns results
(defn load-best-rules [filename]
  (let [data (edn/read-string (slurp filename))]
    (into {}
      (for [condition (:conditions data)]
        [(:name condition)
         (into {}
           (for [method [:pca :nca :hybrid]
                 :let [method-data (get condition method)]
                 :when method-data]
             [method (:best-rules method-data)]))]))))

;; evaluate a rule on a different grid size
(defn test-generalization
  [rule-fn target-fn grid-limits ca-steps]
  (let [init-grid      (er/make-init-grid grid-limits)
        target-grid    (er/make-target-grid grid-limits target-fn)
        cell-neighbors (partial ca/von-neumann-neighbors 1 grid-limits)]
    (er/evaluate-rule init-grid cell-neighbors target-grid
                      rule-fn ca-steps)))

;; test one rule on all test grids
(defn- generalize-on-grids [rule-fn target-fn ca-steps train-error]
  {:train-error train-error
   :results
   (mapv (fn [grid-size]
           (let [result (test-generalization rule-fn target-fn grid-size ca-steps)]
             {:grid-size grid-size
              :min-error (float (:min-error result))
              :best-step (:best-step result)}))
         test-grids)})

;; build rule-fn from best-rule record by method type
(defn- make-rule-fn [method best-rule num-signals]
  (case method
    :pca    (partial er/push-nv (:best-program best-rule))
    :nca    (partial er/cppn-nv (:best-genome best-rule))
    :hybrid (let [genome (:best-genome best-rule)]
              (partial er/hybrid-nv (:cppn genome) (:program genome) num-signals))))

;; generalize all runs of one method on one pattern
(defn generalize-method-runs
  [method best-rules target-fn ca-steps num-signals]
  (mapv (fn [best-rule]
          (let [rule-fn (make-rule-fn method best-rule num-signals)]
            (generalize-on-grids rule-fn target-fn ca-steps (:error best-rule))))
        best-rules))

(defn save-generalization-results!
  [source-config conditions filename]
  (spit filename
        (pr-str {:experiment  "generalization"
                 :timestamp   (str (java.time.Instant/now))
                 :source-file local-patterns-file
                 :source-config source-config
                 :test-grids  test-grids
                 :conditions  conditions}))
  (println (str "Results saved to " filename)))

;; run generalization tests using best rules from local_patterns
(defn run-experiment!
  ([] (run-experiment! 3))
  ([num-signals]
   (println "Loading best rules from local_patterns results...")
   (let [all-best-rules (load-best-rules local-patterns-file)
         source-config  (:base-config (edn/read-string (slurp local-patterns-file)))
         ca-steps       (or (:ca-steps source-config) 30)]
     (loop [remaining patterns
            completed []]
       (if (empty? remaining)
         completed
         (let [pattern    (first remaining)
               pat-name   (:name pattern)
               target-fn  (:target-fn pattern)
               pat-rules  (get all-best-rules pat-name)
               _          (println (str "\n=== Condition: " pat-name " ==="))
               gen-results
               (into {}
                 (for [method [:pca :nca :hybrid]
                       :let [rules (get pat-rules method)]
                       :when rules]
                   (do (println (str "  Testing " (name method)
                                     " (" (count rules) " rules)..."))
                       [method (generalize-method-runs
                                 method rules target-fn ca-steps num-signals)])))
               condition  {:name pat-name :generalization gen-results}
               completed' (conj completed condition)]
           (save-generalization-results! source-config completed' results-file)
           (recur (rest remaining) completed')))))))


(run-experiment!)
