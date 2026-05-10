(ns evolve-hybrid
  "Hybrid PCA-NCA evolution operators"
  (:require [evolve-neat :as en]
            [evolve-pca :as ev]
            [neat :as neat]))

;; NEAT on CPPN, UMAD on Push program
(defn mutate-hybrid
  [hybrid-genome config mutate-program]
  {:cppn    (neat/mutate (:cppn hybrid-genome) config)
   :program (mutate-program (:program hybrid-genome))})

;; crossover within each component
(defn crossover-component-wise
  [parent-a parent-b]
  {:cppn    (neat/crossover (:cppn parent-a) (:cppn parent-b))
   :program (ev/one-point-crossover (:program parent-a) (:program parent-b))})

;; randomly swap components between parents
(defn crossover-component-swap
  [parent-a parent-b]
  {:cppn    (if (< (rand) 0.5) (:cppn parent-a) (:cppn parent-b))
   :program (if (< (rand) 0.5) (:program parent-a) (:program parent-b))})

;; assign species by CPPN compatibility distance
(defn assign-hybrid-species
  [hybrid-population species-reps threshold c1 c3 next-species-id]
  (loop [remaining  hybrid-population
         assignment {}
         next-id    next-species-id]
    (if (empty? remaining)
      {:species assignment :next-id next-id}
      (let [hybrid   (first remaining)
            cppn     (:cppn hybrid)
            match-id (first (for [[sid rep] species-reps
                                  :when (< (neat/compatibility-distance cppn rep c1 c3)
                                           threshold)]
                              sid))]
        (if match-id
          (recur (rest remaining)
                 (update assignment match-id (fnil conj []) hybrid)
                 next-id)
          (recur (rest remaining)
                 (update assignment next-id (fnil conj []) hybrid)
                 (inc next-id)))))))

;; lexicase selection, mutation only
(defn speciated-reproduce-hybrid
  [select species-map offspring-counts config mutate-program]
  (vec (mapcat
        (fn [[sid members]]
          (let [n-offspring (get offspring-counts sid 0)]
            (if (zero? n-offspring)
              []
              (let [champion? (>= (count members) 5)
                    champion  (when champion?
                                (apply max-key #(or (:fitness %) 0.0) members))
                    n-mutants (if champion? (dec n-offspring) n-offspring)
                    mutants   (repeatedly n-mutants
                                #(mutate-hybrid (select members)
                                                config mutate-program))]
                (if champion?
                  (cons champion mutants)
                  mutants)))))
        species-map)))

;; crossover + mutation
(defn speciated-reproduce-hybrid-with-crossover
  [select species-map offspring-counts config mutate-program]
  (let [crossover-rate (get config :crossover-rate 0.75)]
    (vec (mapcat
          (fn [[sid members]]
            (let [n-offspring (get offspring-counts sid 0)]
              (if (zero? n-offspring)
                []
                (let [champion? (>= (count members) 5)
                      champion  (when champion?
                                  (apply max-key #(or (:fitness %) 0.0) members))
                      n-children (if champion? (dec n-offspring) n-offspring)
                      children (repeatedly n-children
                                 (fn []
                                   (if (and (> (count members) 1)
                                            (< (rand) crossover-rate))
                                     (mutate-hybrid
                                       (crossover-component-wise (select members)
                                                                 (select members))
                                       config mutate-program)
                                     (mutate-hybrid (select members)
                                                    config mutate-program))))]
                  (if champion?
                    (cons champion children)
                    children)))))
          species-map))))

;; one speciated generation for hybrid genomes
(defn hybrid-speciated-generation
  [evaluated species-reps next-species-id config mutate-program reproduce-fn]
  (let [{:keys [population-size compatibility-threshold c1 c3 complexity-penalty]
         :or   {compatibility-threshold 3.0 c1 1.0 c3 0.4 complexity-penalty 0.0}} config
        population (mapv (fn [e]
                           (let [cppn     (:cppn (:genome e))
                                 n-active (count (filter :enabled
                                                         (:connections cppn)))
                                 raw-fit  (- 1.0 (:min-error e))
                                 fit      (max 0.0 (- raw-fit (* complexity-penalty
                                                                 n-active)))]
                             (assoc (:genome e)
                                    :fitness fit
                                    :errors  (:window-errors e))))
                         evaluated)
        {:keys [species next-id]}
        (assign-hybrid-species population species-reps
                               compatibility-threshold c1 c3
                               next-species-id)
        species' (en/species-adjusted-fitness species)
        offspring-counts (en/allocate-offspring species' population-size)
        next-gen (reproduce-fn en/lexicase species' offspring-counts
                               config mutate-program)
        new-reps (into {} (for [[sid members] species'
                                :when (seq members)]
                            [sid (:cppn (rand-nth members))]))]
    {:genomes         (vec next-gen)
     :species-reps    new-reps
     :next-species-id next-id
     :n-species       (count species)}))
