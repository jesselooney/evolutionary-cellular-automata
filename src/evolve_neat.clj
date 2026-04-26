(ns evolve-neat
  "Evolve NEAT genomes. Mirrors the structure of evolve-pca:
   tournament selection, elitism, mutation."
  (:require [neat :as neat]))

;;;; Selection.

(defn tournament
  "Select the fittest of `size` random individuals."
  [size population]
  (apply max-key #(or (:fitness %) 0.0)
         (repeatedly size #(rand-nth population))))

;;;; Generation transformers.

(defn elitist-select-mutate
  "Keep `elite-count` best, fill the rest with selection + mutation.
   Analogous to evolve-pca/elitist-select-mutate."
  [select elite-count config population]
  (let [sorted (sort-by #(- (or (:fitness %) 0.0)) population)
        elites (take elite-count sorted)]
    (concat elites
            (repeatedly (- (count population) elite-count)
                        (fn []
                          (neat/mutate (select population) config))))))

;;;; Evolution loop.

(defn evolve
  "Main evolution loop. Takes a config map with :fitness-fn,
   :num-inputs, :num-outputs, and optional tuning parameters."
  [config]
  (let [{:keys [population-size generation-limit fitness-fn
                num-inputs num-outputs elitism]
         :or {population-size 200 generation-limit 500 elitism 1}} config
        next-gen (partial elitist-select-mutate
                          (partial tournament 2) elitism config)]
    (loop [gen  0
           population (vec (repeatedly population-size
                                       #(neat/make-genome num-inputs num-outputs)))]
      (let [evaluated (vec (pmap #(assoc % :fitness (fitness-fn %)) population))
            best (apply max-key #(or (:fitness %) 0.0) evaluated)
            _ (println {:generation gen
                        :best-fitness (float (or (:fitness best) 0.0))})]
        (if (or (>= gen generation-limit) (== 1.0 (or (:fitness best) 0.0)))
          {:best best :generation gen}
          (recur (inc gen) (vec (next-gen evaluated))))))))
