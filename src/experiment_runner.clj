(ns experiment-runner
  (:require [cellular-automata :as ca]
            [cppn :as cppn]
            [evolve-pca :as ev]
            [evolve-neat :as en]
            [helpers :as h]
            [neat :as neat]
            [push :as p]))

;; single-seed initial grid, only [0 0] alive
(defn make-init-grid
  [grid-limits]
  (h/map-keys (ca/cell-grid grid-limits)
              (fn [p] (= p [0 0]))))

;; target grid from applying target-fn to each coordinate
(defn make-target-grid
  [grid-limits target-fn]
  (let [[w h] grid-limits]
    (h/map-keys (ca/cell-grid grid-limits)
                (fn [[x y]] (target-fn [x y] w h)))))

;; fraction of cells not matching target (0.0 = perfect)
(defn grid-error
  [target-grid cells]
  (let [total (count cells)
        correct (count (filter (fn [[k v]] (= v (get target-grid k))) cells))]
    (- 1.0 (/ (double correct) total))))

;; nonlinear accuracy transform (CA-NEAT, Nichele et al. 2018, Eq. 2)
(defn ca-neat-accuracy
  [raw-accuracy]
  (let [x (double raw-accuracy)]
    (* x (/ (Math/exp (* 5.0 x)) (Math/exp 5.0)))))

;; per-step error with CA-NEAT nonlinear penalty
(defn grid-error-ca-neat
  [target-grid cells]
  (let [total (count cells)
        correct (count (filter (fn [[k v]] (= v (get target-grid k))) cells))
        raw-accuracy (/ (double correct) total)]
    (- 1.0 (ca-neat-accuracy raw-accuracy))))

;; resolve per-step error fn from config (:ca-neat or linear)
(defn resolve-error-fn
  [config]
  (case (:fitness-transform config)
    :ca-neat grid-error-ca-neat
    grid-error))

;; errors per step -> best error, step index, raw errors
(defn- evaluate-states
  [states target-grid error-fn]
  (let [rest-states (rest states)
        errors      (mapv (partial error-fn target-grid) rest-states)
        raw-errors  (if (= error-fn grid-error)
                      errors
                      (mapv (partial grid-error target-grid) rest-states))
        [best-idx best-err]
        (reduce-kv (fn [[bi be] i e]
                     (if (< (double e) (double be)) [i e] [bi be]))
                   [0 (double (first errors))]
                   errors)]
    {:min-error     (double best-err)
     :best-step     (inc best-idx)
     :errors        errors
     :raw-min-error (double (raw-errors best-idx))
     :raw-errors    raw-errors}))

;; run a neighborhood-value fn for n-steps, return error metrics
(defn evaluate-rule
  ([init-grid cell-neighbors target-grid neighborhood-value n-steps]
   (evaluate-rule init-grid cell-neighbors target-grid neighborhood-value n-steps grid-error))
  ([init-grid cell-neighbors target-grid neighborhood-value n-steps error-fn]
   (let [step-fn (partial ca/cells-next-value cell-neighbors neighborhood-value)
         states  (take (inc n-steps) (iterate step-fn init-grid))]
     (evaluate-states states target-grid error-fn))))

;; run a custom step fn for n-steps, return error metrics
(defn evaluate-rule-with-step-fn
  ([init-grid target-grid step-fn n-steps]
   (evaluate-rule-with-step-fn init-grid target-grid step-fn n-steps grid-error))
  ([init-grid target-grid step-fn n-steps error-fn]
   (let [states (take (inc n-steps) (iterate step-fn init-grid))]
     (evaluate-states states target-grid error-fn))))

(def pca-definitions
  {'and  (p/make-instruction #(and %1 %2) [:bool :bool] :bool)
   'or   (p/make-instruction #(or %1 %2) [:bool :bool] :bool)
   'xor  (p/make-instruction not= [:bool :bool] :bool)
   'nand (p/make-instruction #(not (and %1 %2)) [:bool :bool] :bool)
   'nor  (p/make-instruction #(not (or %1 %2)) [:bool :bool] :bool)
   'not  (p/make-instruction not [:bool :bool] :bool)})

;; Push term set for a given number of neighbors
(defn make-pca-terms
  [n-neighbors]
  (vec (concat (keys pca-definitions)
               (ev/neighbor-terms n-neighbors)
               [true false])))

;; evaluate a Push program as a CA neighborhood rule
(defn push-nv
  [program neighbor-values]
  (let [parser (p/make-simple-parser
                (merge pca-definitions
                       (ev/neighbor-term-parser :bool neighbor-values)))
        final-state (p/execute-state parser {:exec [program] :bool []})]
    (or (p/peek-stack final-state :bool) false)))

;; random program generator for given terms and length range
(defn make-rand-program
  [terms min-len max-len]
  (let [rand-term #(rand-nth terms)]
    (fn []
      (repeatedly (+ min-len (rand-int (- max-len min-len))) rand-term))))

;; local-only CPPN evaluator, neighbor values only
(defn cppn-nv
  [genome neighbor-values]
  (let [inputs (mapv #(cppn/normalize-state % 2) neighbor-values)
        outputs (cppn/evaluate-cppn genome inputs)]
    (cppn/decode-output outputs 2)))

;; normalize a coordinate to [-1, 1]
(defn normalize-pos
  [x limit]
  (if (= limit 1)
    0.0
    (- (* 2.0 (/ (double x) (double (dec limit)))) 1.0)))

;; position-aware CPPN evaluator, neighbors + normalized x, y
(defn cppn-nv-with-pos
  [genome grid-limits [x y] neighbor-values]
  (let [state-inputs (mapv #(cppn/normalize-state % 2) neighbor-values)
        inputs (conj state-inputs
                     (normalize-pos x (first grid-limits))
                     (normalize-pos y (second grid-limits)))
        outputs (cppn/evaluate-cppn genome inputs)]
    (cppn/decode-output outputs 2)))

;; CA step fn passing cell coordinates to a position-aware CPPN
(defn make-position-step-fn
  [genome grid-limits cell-neighbors]
  (fn [grid]
    (h/map-keys grid
      (fn [cell-key]
        (let [nbr-vals (map grid (cell-neighbors cell-key))]
          (cppn-nv-with-pos genome grid-limits cell-key nbr-vals))))))

;; last `window` errors for lexicase selection
(defn window-errors
  [all-errors window]
  (let [n (count all-errors)]
    (if (<= n window)
      all-errors
      (subvec all-errors (- n window)))))

;; summary complexity metrics from a NEAT genome
(defn genome-complexity
  [genome]
  (let [nodes  (:nodes genome)
        conns  (:connections genome)
        active (filter :enabled conns)]
    {:num-hidden-nodes     (count (filter #(= :hidden (:type %)) (vals nodes)))
     :num-connections      (count conns)
     :num-active-connections (count active)}))

;; PCA evolution with lexicase selection on windowed CA step errors
(defn run-pca
  [config run-idx]
  (let [{:keys [init-grid cell-neighbors target-grid ca-steps fitness-window
                population-size generation-limit elite-count
                pca-program-min pca-program-max num-neighbors]} config
        error-fn    (resolve-error-fn config)
        terms       (make-pca-terms num-neighbors)
        rand-term   #(rand-nth terms)
        rand-prog   (make-rand-program terms pca-program-min pca-program-max)
        mutate      (partial ev/umad rand-term)
        eval-prog   (fn [prog]
                      (evaluate-rule init-grid cell-neighbors target-grid
                                     (partial push-nv prog) ca-steps error-fn))]
    (loop [gen      0
           programs (vec (repeatedly population-size rand-prog))
           history  []]
      (let [evaluated (vec (pmap (fn [prog]
                                   (let [result (eval-prog prog)
                                         werrs  (window-errors (:errors result)
                                                               fitness-window)]
                                     (assoc result
                                            :program prog
                                            :window-errors werrs)))
                                 programs))
            best      (apply min-key :min-error evaluated)
            avg-raw   (h/mean (mapv :raw-min-error evaluated))
            record    {:generation      gen
                       :avg-error       (float avg-raw)
                       :best-error      (float (:raw-min-error best))
                       :best-step       (:best-step best)
                       :program-length  (count (:program best))
                       :best-program    (:program best)}
            _         (println (str "[PCA " run-idx "] "
                                    (dissoc record :best-program)))
            history'  (conj history record)]
        (if (or (>= gen generation-limit) (== 0.0 (:min-error best)))
          history'
          (let [population (mapv (fn [e] {:program (:program e)
                                          :error   (:min-error e)
                                          :errors  (:window-errors e)})
                                 evaluated)
                next-progs (ev/elitist-select-mutate
                            ev/lexicase mutate elite-count population)]
            (recur (inc gen) (vec next-progs) history')))))))

;; PCA evolution with lexicase selection, one-point crossover, UMAD mutation
(defn run-pca-crossover
  [config run-idx]
  (let [{:keys [init-grid cell-neighbors target-grid ca-steps fitness-window
                population-size generation-limit elite-count
                pca-program-min pca-program-max num-neighbors]} config
        error-fn    (resolve-error-fn config)
        terms       (make-pca-terms num-neighbors)
        rand-term   #(rand-nth terms)
        rand-prog   (make-rand-program terms pca-program-min pca-program-max)
        mutate      (partial ev/umad rand-term)
        eval-prog   (fn [prog]
                      (evaluate-rule init-grid cell-neighbors target-grid
                                     (partial push-nv prog) ca-steps error-fn))]
    (loop [gen      0
           programs (vec (repeatedly population-size rand-prog))
           history  []]
      (let [evaluated (vec (pmap (fn [prog]
                                   (let [result (eval-prog prog)
                                         werrs  (window-errors (:errors result)
                                                               fitness-window)]
                                     (assoc result
                                            :program prog
                                            :window-errors werrs)))
                                 programs))
            best      (apply min-key :min-error evaluated)
            avg-raw   (h/mean (mapv :raw-min-error evaluated))
            record    {:generation      gen
                       :avg-error       (float avg-raw)
                       :best-error      (float (:raw-min-error best))
                       :best-step       (:best-step best)
                       :program-length  (count (:program best))
                       :best-program    (:program best)}
            _         (println (str "[PCA-X " run-idx "] "
                                    (dissoc record :best-program)))
            history'  (conj history record)]
        (if (or (>= gen generation-limit) (== 0.0 (:min-error best)))
          history'
          (let [population (mapv (fn [e] {:program (:program e)
                                          :error   (:min-error e)
                                          :errors  (:window-errors e)})
                                 evaluated)
                next-progs (ev/elitist-select-crossover-mutate
                            ev/lexicase ev/one-point-crossover
                            mutate elite-count population)]
            (recur (inc gen) (vec next-progs) history')))))))

;; one speciated generation step -> next generation of genomes
(defn- nca-speciated-generation
  [evaluated species-reps next-species-id config]
  (let [{:keys [population-size compatibility-threshold c1 c3 complexity-penalty]
         :or   {compatibility-threshold 3.0 c1 1.0 c3 0.4 complexity-penalty 0.0}} config
        population (mapv (fn [e]
                           (let [n-active (count (filter :enabled
                                                         (:connections (:genome e))))
                                 raw-fit  (- 1.0 (:min-error e))
                                 fit      (max 0.0 (- raw-fit (* complexity-penalty
                                                                 n-active)))]
                             (assoc (:genome e)
                                    :fitness fit
                                    :errors  (:window-errors e))))
                         evaluated)
        {:keys [species next-id]}
        (en/assign-species population species-reps
                           compatibility-threshold c1 c3
                           next-species-id)
        _shared (en/species-adjusted-fitness species)
        offspring-counts (en/allocate-offspring species population-size)
        next-gen (en/speciated-reproduce en/lexicase species offspring-counts config)
        new-reps (into {} (for [[sid members] species
                                :when (seq members)]
                            [sid (rand-nth members)]))]
    {:genomes         (vec next-gen)
     :species-reps    new-reps
     :next-species-id next-id
     :n-species       (count species)}))

;; local-NCA evolution with speciated lexicase selection
(defn run-nca
  [config run-idx]
  (let [{:keys [init-grid cell-neighbors target-grid ca-steps fitness-window
                population-size generation-limit num-inputs num-outputs]} config
        error-fn    (resolve-error-fn config)
        eval-genome (fn [genome]
                      (evaluate-rule init-grid cell-neighbors target-grid
                                     (partial cppn-nv genome) ca-steps error-fn))]
    (loop [gen             0
           genomes         (vec (repeatedly population-size
                                  #(neat/make-genome num-inputs num-outputs)))
           species-reps    {}
           next-species-id 0
           history         []]
      (let [evaluated (vec (pmap (fn [g]
                                   (let [result (eval-genome g)
                                         werrs  (window-errors (:errors result)
                                                               fitness-window)]
                                     (assoc result :genome g :window-errors werrs)))
                                 genomes))
            best      (apply min-key :min-error evaluated)
            avg-raw   (h/mean (mapv :raw-min-error evaluated))
            spec-info (nca-speciated-generation evaluated species-reps
                                                next-species-id config)
            record    (merge {:generation  gen
                              :avg-error   (float avg-raw)
                              :best-error  (float (:raw-min-error best))
                              :best-step   (:best-step best)
                              :n-species   (:n-species spec-info)
                              :best-genome (:genome best)}
                             (genome-complexity (:genome best)))
            _         (println (str "[NCA " run-idx "] "
                                    (dissoc record :best-genome)))
            history'  (conj history record)]
        (if (or (>= gen generation-limit) (== 0.0 (:min-error best)))
          history'
          (recur (inc gen)
                 (:genomes spec-info)
                 (:species-reps spec-info)
                 (:next-species-id spec-info)
                 history'))))))

;; speciated generation with crossover + mutation reproduction
(defn- nca-speciated-generation-with-crossover
  [evaluated species-reps next-species-id config]
  (let [{:keys [population-size compatibility-threshold c1 c3 complexity-penalty]
         :or   {compatibility-threshold 3.0 c1 1.0 c3 0.4 complexity-penalty 0.0}} config
        population (mapv (fn [e]
                           (let [n-active (count (filter :enabled
                                                         (:connections (:genome e))))
                                 raw-fit  (- 1.0 (:min-error e))
                                 fit      (max 0.0 (- raw-fit (* complexity-penalty
                                                                 n-active)))]
                             (assoc (:genome e)
                                    :fitness fit
                                    :errors  (:window-errors e))))
                         evaluated)
        {:keys [species next-id]}
        (en/assign-species population species-reps
                           compatibility-threshold c1 c3
                           next-species-id)
        _shared (en/species-adjusted-fitness species)
        offspring-counts (en/allocate-offspring species population-size)
        next-gen (en/speciated-reproduce-with-crossover
                   en/lexicase species offspring-counts config)
        new-reps (into {} (for [[sid members] species
                                :when (seq members)]
                            [sid (rand-nth members)]))]
    {:genomes         (vec next-gen)
     :species-reps    new-reps
     :next-species-id next-id
     :n-species       (count species)}))

;; local-NCA evolution with speciated lexicase selection and NEAT crossover
(defn run-nca-crossover
  [config run-idx]
  (let [{:keys [init-grid cell-neighbors target-grid ca-steps fitness-window
                population-size generation-limit num-inputs num-outputs]} config
        error-fn    (resolve-error-fn config)
        eval-genome (fn [genome]
                      (evaluate-rule init-grid cell-neighbors target-grid
                                     (partial cppn-nv genome) ca-steps error-fn))]
    (loop [gen             0
           genomes         (vec (repeatedly population-size
                                  #(neat/make-genome num-inputs num-outputs)))
           species-reps    {}
           next-species-id 0
           history         []]
      (let [evaluated (vec (pmap (fn [g]
                                   (let [result (eval-genome g)
                                         werrs  (window-errors (:errors result)
                                                               fitness-window)]
                                     (assoc result :genome g :window-errors werrs)))
                                 genomes))
            best      (apply min-key :min-error evaluated)
            avg-raw   (h/mean (mapv :raw-min-error evaluated))
            spec-info (nca-speciated-generation-with-crossover
                        evaluated species-reps next-species-id config)
            record    (merge {:generation  gen
                              :avg-error   (float avg-raw)
                              :best-error  (float (:raw-min-error best))
                              :best-step   (:best-step best)
                              :n-species   (:n-species spec-info)
                              :best-genome (:genome best)}
                             (genome-complexity (:genome best)))
            _         (println (str "[NCA-X " run-idx "] "
                                    (dissoc record :best-genome)))
            history'  (conj history record)]
        (if (or (>= gen generation-limit) (== 0.0 (:min-error best)))
          history'
          (recur (inc gen)
                 (:genomes spec-info)
                 (:species-reps spec-info)
                 (:next-species-id spec-info)
                 history'))))))

;; position-aware NCA evolution with speciated lexicase selection
(defn run-nca-position
  [config run-idx]
  (let [{:keys [init-grid cell-neighbors target-grid grid-limits ca-steps
                fitness-window population-size generation-limit
                num-outputs]} config
        error-fn       (resolve-error-fn config)
        num-pos-inputs (+ (count (cell-neighbors [0 0])) 2)
        eval-genome    (fn [genome]
                         (let [step-fn (make-position-step-fn genome grid-limits
                                                              cell-neighbors)]
                           (evaluate-rule-with-step-fn init-grid target-grid
                                                       step-fn ca-steps error-fn)))]
    (loop [gen             0
           genomes         (vec (repeatedly population-size
                                  #(neat/make-genome num-pos-inputs num-outputs)))
           species-reps    {}
           next-species-id 0
           history         []]
      (let [evaluated (vec (pmap (fn [g]
                                   (let [result (eval-genome g)
                                         werrs  (window-errors (:errors result)
                                                               fitness-window)]
                                     (assoc result :genome g :window-errors werrs)))
                                 genomes))
            best      (apply min-key :min-error evaluated)
            avg-raw   (h/mean (mapv :raw-min-error evaluated))
            spec-info (nca-speciated-generation evaluated species-reps
                                                next-species-id config)
            record    (merge {:generation  gen
                              :avg-error   (float avg-raw)
                              :best-error  (float (:raw-min-error best))
                              :best-step   (:best-step best)
                              :n-species   (:n-species spec-info)
                              :best-genome (:genome best)}
                             (genome-complexity (:genome best)))
            _         (println (str "[NCA-pos " run-idx "] "
                                    (dissoc record :best-genome)))
            history'  (conj history record)]
        (if (or (>= gen generation-limit) (== 0.0 (:min-error best)))
          history'
          (recur (inc gen)
                 (:genomes spec-info)
                 (:species-reps spec-info)
                 (:next-species-id spec-info)
                 history'))))))

;; position-aware NCA evolution with speciated lexicase and crossover
(defn run-nca-position-crossover
  [config run-idx]
  (let [{:keys [init-grid cell-neighbors target-grid grid-limits ca-steps
                fitness-window population-size generation-limit
                num-outputs]} config
        error-fn       (resolve-error-fn config)
        num-pos-inputs (+ (count (cell-neighbors [0 0])) 2)
        eval-genome    (fn [genome]
                         (let [step-fn (make-position-step-fn genome grid-limits
                                                              cell-neighbors)]
                           (evaluate-rule-with-step-fn init-grid target-grid
                                                       step-fn ca-steps error-fn)))]
    (loop [gen             0
           genomes         (vec (repeatedly population-size
                                  #(neat/make-genome num-pos-inputs num-outputs)))
           species-reps    {}
           next-species-id 0
           history         []]
      (let [evaluated (vec (pmap (fn [g]
                                   (let [result (eval-genome g)
                                         werrs  (window-errors (:errors result)
                                                               fitness-window)]
                                     (assoc result :genome g :window-errors werrs)))
                                 genomes))
            best      (apply min-key :min-error evaluated)
            avg-raw   (h/mean (mapv :raw-min-error evaluated))
            spec-info (nca-speciated-generation-with-crossover
                        evaluated species-reps next-species-id config)
            record    (merge {:generation  gen
                              :avg-error   (float avg-raw)
                              :best-error  (float (:raw-min-error best))
                              :best-step   (:best-step best)
                              :n-species   (:n-species spec-info)
                              :best-genome (:genome best)}
                             (genome-complexity (:genome best)))
            _         (println (str "[NCA-pos-X " run-idx "] "
                                    (dissoc record :best-genome)))
            history'  (conj history record)]
        (if (or (>= gen generation-limit) (== 0.0 (:min-error best)))
          history'
          (recur (inc gen)
                 (:genomes spec-info)
                 (:species-reps spec-info)
                 (:next-species-id spec-info)
                 history'))))))

;; save multi-condition experiment results to EDN
(defn save-results!
  [experiment-name base-config conditions filename]
  (let [strip (fn [record] (dissoc record :best-program :best-genome))
        process-runs (fn [runs]
                       (let [all (apply concat runs)
                             best (apply min-key :best-error all)]
                         {:runs         (mapv #(mapv strip %) runs)
                          :best-overall {:error      (:best-error best)
                                         :generation (:generation best)
                                         :step       (:best-step best)}}))]
    (spit filename
          (pr-str {:experiment experiment-name
                   :timestamp  (str (java.time.Instant/now))
                   :base-config (dissoc base-config
                                        :init-grid :target-grid :cell-neighbors)
                   :conditions
                   (mapv (fn [{:keys [name config methods]}]
                           (merge {:name name}
                                  (when config
                                    {:config-overrides
                                     (dissoc config :init-grid :target-grid
                                             :cell-neighbors)})
                                  (into {} (for [[method-key runs] methods]
                                             [method-key (process-runs runs)]))))
                         conditions)}))
    (println (str "Results saved to " filename))))

;; run a single experimental condition with given method functions
(defn run-condition!
  [condition-name config method-fns n-runs]
  (println (str "\n=== Condition: " condition-name " ==="))
  {:name    condition-name
   :config  config
   :methods (into {}
              (for [[method-key run-fn] method-fns]
                (do (println (str "  Running " (name method-key) "..."))
                    [method-key (mapv #(run-fn config %) (range n-runs))])))})
