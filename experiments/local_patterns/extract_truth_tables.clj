(ns extract-truth-tables
  (:require [experiment-runner :as er]
            [helpers :as h]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; all 2^5 = 32 input combos for 5-cell von Neumann neighborhood
(def all-inputs
  (apply h/cartesian-product (repeat 5 [false true])))

;; build eval fn from best-rule, dispatching on approach type
(defn make-eval-fn
  [approach best-rule num-signals]
  (case approach
    :pca    (partial er/push-nv (:best-program best-rule))
    :nca    (partial er/cppn-nv (:best-genome best-rule))
    :hybrid (let [{:keys [cppn program]} (:best-genome best-rule)]
              (partial er/hybrid-nv cppn program num-signals))))

;; evaluate rule on all 32 inputs -> [N0 N1 N2 N3 N4 output] rows
(defn evaluate-truth-table
  [eval-fn]
  (mapv (fn [input-row]
          (let [input-vec (vec input-row)
                output    (eval-fn input-vec)]
            (conj (mapv h/bool-to-int input-vec)
                  (h/bool-to-int output))))
        all-inputs))

(defn truth-table->csv
  [rows]
  (let [header "N0,N1,N2,N3,N4,output"
        data-lines (map #(str/join "," %) rows)]
    (str header "\n" (str/join "\n" data-lines) "\n")))

(defn write-truth-table!
  [base-dir pattern-name approach-name run-idx rows]
  (let [dir  (io/file base-dir pattern-name approach-name)
        file (io/file dir (str "run_" run-idx ".csv"))]
    (.mkdirs dir)
    (spit file (truth-table->csv rows))
    (println (str "  Wrote " (.getPath file)))))

(def results-file
  "experiments/local_patterns/results/local_patterns_results.edn")

(def output-dir
  "experiments/local_patterns/results/truth_tables")

(defn extract-all! []
  (let [data        (clojure.edn/read-string (slurp results-file))
        num-signals (get-in data [:base-config :num-signals] 3)
        conditions  (:conditions data)]
    (doseq [condition conditions]
      (let [pattern-name (:name condition)]
        (println (str "\n=== " pattern-name " ==="))
        (doseq [approach [:pca :nca :hybrid]]
          (when-let [method-data (get condition approach)]
            (let [best-rules (:best-rules method-data)]
              (println (str "  " (name approach)
                            " (" (count best-rules) " runs)"))
              (doseq [[run-idx rule] (map-indexed vector best-rules)]
                (let [eval-fn (make-eval-fn approach rule num-signals)
                      rows    (evaluate-truth-table eval-fn)]
                  (write-truth-table! output-dir pattern-name
                                      (name approach) run-idx rows))))))))))

(extract-all!)
