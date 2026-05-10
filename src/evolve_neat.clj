(ns evolve-neat
  "Evolve NEAT genomes. Mirrors the structure of evolve-pca:
   tournament selection, elitism, mutation."
  (:require [helpers :as h]
            [neat :as neat]))

;;;; Selection.

(defn tournament
  "Select the fittest of `size` random individuals."
  [size population]
  (apply max-key #(or (:fitness %) 0.0)
         (repeatedly size #(rand-nth population))))

(defn lexicase
  "Lexicase selection on :errors vectors (lower is better).
   Shuffles error indices and sorts lexicographically."
  [population]
  (let [errors-count (count (:errors (first population)))
        ordering (shuffle (range errors-count))]
    (first (sort-by #(vec (h/reorder ordering (:errors %)))
                    population))))

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

;;;; Speciation (NEAT-style).

(defn assign-species
  "Assign each genome in `population` to a species.
   `species-reps` is a map of species-id -> representative genome.
   Returns a map of species-id -> vector of member genomes.
   Genomes that don't match any existing species start a new one."
  [population species-reps threshold c1 c3 next-species-id]
  (loop [remaining  population
         assignment {}              ;; species-id -> [genome ...]
         next-id    next-species-id]
    (if (empty? remaining)
      {:species assignment :next-id next-id}
      (let [genome   (first remaining)
            match-id (first (for [[sid rep] species-reps
                                  :when (< (neat/compatibility-distance genome rep c1 c3)
                                           threshold)]
                              sid))]
        (if match-id
          (recur (rest remaining)
                 (update assignment match-id (fnil conj []) genome)
                 next-id)
          ;; New species
          (recur (rest remaining)
                 (update assignment next-id (fnil conj []) genome)
                 (inc next-id)))))))

(defn species-adjusted-fitness
  "Apply fitness sharing: divide each member's fitness by species size.
   Returns the species map with :adjusted-fitness set on each member."
  [species-map]
  (into {} (map (fn [[sid members]]
                  (let [n (double (count members))]
                    [sid (mapv #(assoc % :adjusted-fitness (/ (or (:fitness %) 0.0) n))
                               members)]))
                species-map)))

(defn allocate-offspring
  "Compute how many offspring each species gets, proportional to total
   adjusted fitness. Ensures at least 1 offspring per surviving species
   and total = population-size."
  [species-map population-size]
  (let [species-ids  (keys species-map)
        totals       (into {} (for [[sid members] species-map]
                                [sid (reduce + (map #(or (:adjusted-fitness %) 0.0) members))]))
        grand-total  (reduce + (vals totals))
        ;; Proportional allocation (floor), then distribute remainder
        raw-counts   (if (zero? grand-total)
                       (into {} (for [sid species-ids]
                                  [sid (max 1 (quot population-size (count species-ids)))]))
                       (into {} (for [sid species-ids]
                                  [sid (max 1 (int (Math/floor
                                                    (* population-size
                                                       (/ (double (totals sid))
                                                          grand-total)))))])))
        current-total (reduce + (vals raw-counts))
        remainder     (- population-size current-total)]
    ;; Distribute remainder to species with highest adjusted fitness totals
    (if (<= remainder 0)
      raw-counts
      (let [ranked (sort-by #(- (double (totals (key %)))) raw-counts)]
        (into {} (map-indexed (fn [i [sid c]]
                                [sid (if (< i remainder) (inc c) c)])
                              ranked))))))

(defn speciated-reproduce
  "Produce the next generation using within-species lexicase selection.
   Each species gets its champion preserved (elitism=1 per species with >5 members).
   config: NEAT mutation config."
  [select species-map offspring-counts config]
  (vec (mapcat
        (fn [[sid members]]
          (let [n-offspring (get offspring-counts sid 0)]
            (if (zero? n-offspring)
              []
              (let [;; Champion elitism for species with >5 members
                    champion? (>= (count members) 5)
                    champion  (when champion?
                                (apply max-key #(or (:fitness %) 0.0) members))
                    n-mutants (if champion? (dec n-offspring) n-offspring)
                    mutants   (repeatedly n-mutants
                                          #(neat/mutate (select members) config))]
                (if champion?
                  (cons champion mutants)
                  mutants)))))
        species-map)))

(defn speciated-reproduce-with-crossover
  "Produce the next generation using within-species crossover + mutation.
   Follows CA-NEAT: crossover-rate fraction of offspring are produced by
   selecting two parents, crossing over, then mutating. The rest are
   mutation-only. Champion elitism is preserved as in speciated-reproduce.
   config keys: :crossover-rate (default 0.75)."
  [select species-map offspring-counts config]
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
                                     ;; Crossover + mutation
                                     (neat/mutate
                                       (neat/crossover (select members)
                                                       (select members))
                                       config)
                                     ;; Mutation only
                                     (neat/mutate (select members) config))))]
                  (if champion?
                    (cons champion children)
                    children)))))
          species-map))))

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
