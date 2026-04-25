(ns evolve-pca
  (:require [cellular-automata :as ca]
            [helpers :as h]
            [push :as p]))

;;;; Helpers for evolving Push local rules for CAs with desired properties.
;;;; A program is a sequence of terms representing a Push program.
;;;; An individual is a program and its metadata such as error values.
;;;; A population is a collection of individuals.

(defn into-population
  [aggregate-error program-errors programs]
  (pmap
   #(let [errors (program-errors %)]
      {:program %
       :errors errors
       :error (aggregate-error errors)})
   programs))

(defn evolve
  [next-programs program-errors aggregate-error generation-limit init-programs]
  (loop [generation 0
         population (into-population aggregate-error
                                     program-errors
                                     init-programs)]
    (let [best-individual (apply min-key :error population)]
      (println {:generation generation
                :min-err (float (:error best-individual))
                :best-program (:program best-individual)})
      (if (>= generation generation-limit)
        population
        (let [programs (next-programs population)]
          (recur (inc generation)
                 (into-population aggregate-error
                                  program-errors
                                  programs)))))))

;;;; Utilities

;;; Push

(defn neighbor-terms
  [n]
  (for [i (range n)] (symbol (format "N%d" i))))

(defn neighbor-term-parser
  [return-stack neighbor-values]
  (into {} (for [[index value] (map-indexed vector neighbor-values)]
             [(symbol (format "N%d" index))
              (p/make-constant-instruction value return-stack)])))

(defn mean-cells-error
  [init-cells cell-neighbors drop-count
   sample-count cells-error neighborhood-value]
  (let [cell-states (iterate (partial ca/cells-next-value
                                      cell-neighbors
                                      neighborhood-value)
                             init-cells)]
    (h/mean (map cells-error (take sample-count (drop drop-count cell-states))))))

;;; Crossovers

(defn one-point-crossover
  [program1 program2]
  (let [crossover-index (rand-int (inc (min (count program1) (count program2))))]
    (concat (take crossover-index program1)
            (drop crossover-index program2))))

;;; Mutations

(defn umad
  [rand-term program]
  (let [program-with-additions (apply concat (for [term program]
                                (if (< (rand) 1/10)
                                  (shuffle [term (rand-term)])
                                  [term])))
        program-with-deletions (apply concat (for [term program-with-additions]
                                 (if (< (rand) 1/11)
                                   []
                                   [term])))]
    program-with-deletions))

;;; Selections

(defn tournament
  [tournament-size population]
  (apply min-key :error
         (repeatedly tournament-size #(rand-nth population))))

(defn lexicase
  [population]
  (let [errors-count (count (:errors (first population)))
        ordering (shuffle (range errors-count))]
    (first (sort-by #(vec (h/reorder ordering (:errors %)))
                    population))))

;;; Generation transformers

(defn select-mutate
  [select mutate population]
  (repeatedly (count population)
              #(mutate (:program (select population)))))

(defn select-crossover-mutate
  [select crossover mutate population]
  (repeatedly (count population)
              #(mutate (crossover (:program (select population))
                                  (:program (select population))))))

(defn elitist-select-mutate
  [select mutate elite-count population]
  (let [elites (map :program (take elite-count (sort-by :error population)))
        mutants (repeatedly (- (count population) elite-count)
                            #(mutate (:program (select population))))]
  (concat elites mutants)))

(defn elitist-select-crossover-mutate
  [select crossover mutate elite-count population]
  (let [elites (map :program (take elite-count (sort-by :error population)))
        mutants (repeatedly (- (count population) elite-count)
                            #(mutate (crossover (:program (select population))
                                                (:program (select population)))))]
    (concat elites mutants)))