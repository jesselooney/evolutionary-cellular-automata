(ns experiment-runner
  (:require [cellular-automata :as ca]
            [cppn :as cppn]
            [evolve-hybrid :as eh]
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

;;;; Hybrid PCA-NCA definitions.

;; Float instructions for hybrid Push programs
(def hybrid-float-definitions
  {'float> (p/make-instruction > [:float :float] :bool)
   'float< (p/make-instruction < [:float :float] :bool)
   'float+ (p/make-instruction + [:float :float] :float)
   'float- (p/make-instruction - [:float :float] :float)
   'float* (p/make-instruction * [:float :float] :float)
   'float-div (p/make-instruction
              (fn [a b] (if (zero? b) 0.0 (/ a b)))
              [:float :float] :float)
   'float>0 (p/make-instruction #(> (double %) 0.0) [:float] :bool)})

;; Combined definitions for hybrid Push programs (boolean + float ops)
(def hybrid-definitions
  (merge pca-definitions hybrid-float-definitions))

;; Float threshold constants available as evolvable terms
(def hybrid-float-literals
  [0.0 0.25 0.5 0.75 1.0 -0.5 -1.0])

;; Term set for hybrid Push programs
(defn make-hybrid-terms
  [n-neighbors num-signals]
  (vec (concat (keys hybrid-definitions)
               (ev/neighbor-terms n-neighbors)
               (for [i (range num-signals)] (symbol (format "S%d" i)))
               [true false]
               hybrid-float-literals)))

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

;;;; Hybrid evaluation functions.

;; Two-stage hybrid evaluator: CPPN produces k signals, Push consumes them.
;; Stage A: CPPN takes normalized neighbor values -> k continuous signals.
;; Stage B: Push program runs with neighbor bools + named signal terms S0..S(k-1).
(defn hybrid-nv
  [cppn program num-signals neighbor-values]
  (let [inputs  (mapv #(cppn/normalize-state % 2) neighbor-values)
        outputs (cppn/evaluate-cppn cppn inputs)
        signals (vec (take num-signals outputs))
        sig-parser (into {} (for [[i v] (map-indexed vector signals)]
                              [(symbol (format "S%d" i))
                               (p/make-constant-instruction v :float)]))
        parser  (p/make-simple-parser
                  (merge hybrid-definitions
                         (ev/neighbor-term-parser :bool neighbor-values)
                         sig-parser))
        init-state {:exec [program] :bool [] :float []}
        final-state (p/execute-state parser init-state)]
    (or (p/peek-stack final-state :bool) false)))

;; Position-aware hybrid: CPPN takes neighbors + normalized (x, y) -> k signals
(defn hybrid-nv-with-pos
  [cppn program num-signals grid-limits [x y] neighbor-values]
  (let [state-inputs (mapv #(cppn/normalize-state % 2) neighbor-values)
        inputs (conj state-inputs
                     (normalize-pos x (first grid-limits))
                     (normalize-pos y (second grid-limits)))
        outputs (cppn/evaluate-cppn cppn inputs)
        signals (vec (take num-signals outputs))
        sig-parser (into {} (for [[i v] (map-indexed vector signals)]
                              [(symbol (format "S%d" i))
                               (p/make-constant-instruction v :float)]))
        parser  (p/make-simple-parser
                  (merge hybrid-definitions
                         (ev/neighbor-term-parser :bool neighbor-values)
                         sig-parser))
        init-state {:exec [program] :bool [] :float []}
        final-state (p/execute-state parser init-state)]
    (or (p/peek-stack final-state :bool) false)))

;; CA step fn for position-aware hybrid
(defn make-hybrid-position-step-fn
  [cppn program num-signals grid-limits cell-neighbors]
  (fn [grid]
    (h/map-keys grid
      (fn [cell-key]
        (let [nbr-vals (map grid (cell-neighbors cell-key))]
          (hybrid-nv-with-pos cppn program num-signals
                              grid-limits cell-key nbr-vals))))))

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

;; complexity metrics for a hybrid genome (CPPN complexity + program length)
(defn hybrid-complexity
  [hybrid-genome]
  (merge (genome-complexity (:cppn hybrid-genome))
         {:program-length (count (:program hybrid-genome))}))

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
        species' (en/species-adjusted-fitness species)
        offspring-counts (en/allocate-offspring species' population-size)
        next-gen (en/speciated-reproduce en/lexicase species' offspring-counts config)
        new-reps (into {} (for [[sid members] species'
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
        species' (en/species-adjusted-fitness species)
        offspring-counts (en/allocate-offspring species' population-size)
        next-gen (en/speciated-reproduce-with-crossover
                   en/lexicase species' offspring-counts config)
        new-reps (into {} (for [[sid members] species'
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

;;;; Hybrid PCA-NCA evolution loops.

;; local hybrid evolution with speciated lexicase selection, no crossover
(defn run-hybrid
  [config run-idx]
  (let [{:keys [init-grid cell-neighbors target-grid ca-steps fitness-window
                population-size generation-limit num-neighbors
                num-signals pca-program-min pca-program-max]} config
        num-signals    (or num-signals 3)
        error-fn       (resolve-error-fn config)
        terms          (make-hybrid-terms num-neighbors num-signals)
        rand-term      #(rand-nth terms)
        rand-prog      (make-rand-program terms pca-program-min pca-program-max)
        mutate-program (partial ev/umad rand-term)
        eval-hybrid    (fn [{:keys [cppn program]}]
                         (evaluate-rule init-grid cell-neighbors target-grid
                                        (partial hybrid-nv cppn program num-signals)
                                        ca-steps error-fn))]
    (loop [gen             0
           genomes         (vec (repeatedly population-size
                              (fn [] {:cppn    (neat/make-genome num-neighbors num-signals)
                                      :program (rand-prog)})))
           species-reps    {}
           next-species-id 0
           history         []]
      (let [evaluated (vec (pmap (fn [g]
                                   (let [result (eval-hybrid g)
                                         werrs  (window-errors (:errors result)
                                                               fitness-window)]
                                     (assoc result :genome g :window-errors werrs)))
                                 genomes))
            best      (apply min-key :min-error evaluated)
            avg-raw   (h/mean (mapv :raw-min-error evaluated))
            spec-info (eh/hybrid-speciated-generation
                        evaluated species-reps next-species-id config
                        mutate-program eh/speciated-reproduce-hybrid)
            record    (merge {:generation  gen
                              :avg-error   (float avg-raw)
                              :best-error  (float (:raw-min-error best))
                              :best-step   (:best-step best)
                              :n-species   (:n-species spec-info)
                              :best-genome (:genome best)}
                             (hybrid-complexity (:genome best)))
            _         (println (str "[HYBRID " run-idx "] "
                                    (dissoc record :best-genome)))
            history'  (conj history record)]
        (if (or (>= gen generation-limit) (== 0.0 (:min-error best)))
          history'
          (recur (inc gen)
                 (:genomes spec-info)
                 (:species-reps spec-info)
                 (:next-species-id spec-info)
                 history'))))))

;; local hybrid evolution with speciated lexicase selection and crossover
(defn run-hybrid-crossover
  [config run-idx]
  (let [{:keys [init-grid cell-neighbors target-grid ca-steps fitness-window
                population-size generation-limit num-neighbors
                num-signals pca-program-min pca-program-max]} config
        num-signals    (or num-signals 3)
        error-fn       (resolve-error-fn config)
        terms          (make-hybrid-terms num-neighbors num-signals)
        rand-term      #(rand-nth terms)
        rand-prog      (make-rand-program terms pca-program-min pca-program-max)
        mutate-program (partial ev/umad rand-term)
        eval-hybrid    (fn [{:keys [cppn program]}]
                         (evaluate-rule init-grid cell-neighbors target-grid
                                        (partial hybrid-nv cppn program num-signals)
                                        ca-steps error-fn))]
    (loop [gen             0
           genomes         (vec (repeatedly population-size
                              (fn [] {:cppn    (neat/make-genome num-neighbors num-signals)
                                      :program (rand-prog)})))
           species-reps    {}
           next-species-id 0
           history         []]
      (let [evaluated (vec (pmap (fn [g]
                                   (let [result (eval-hybrid g)
                                         werrs  (window-errors (:errors result)
                                                               fitness-window)]
                                     (assoc result :genome g :window-errors werrs)))
                                 genomes))
            best      (apply min-key :min-error evaluated)
            avg-raw   (h/mean (mapv :raw-min-error evaluated))
            spec-info (eh/hybrid-speciated-generation
                        evaluated species-reps next-species-id config
                        mutate-program
                        eh/speciated-reproduce-hybrid-with-crossover)
            record    (merge {:generation  gen
                              :avg-error   (float avg-raw)
                              :best-error  (float (:raw-min-error best))
                              :best-step   (:best-step best)
                              :n-species   (:n-species spec-info)
                              :best-genome (:genome best)}
                             (hybrid-complexity (:genome best)))
            _         (println (str "[HYBRID-X " run-idx "] "
                                    (dissoc record :best-genome)))
            history'  (conj history record)]
        (if (or (>= gen generation-limit) (== 0.0 (:min-error best)))
          history'
          (recur (inc gen)
                 (:genomes spec-info)
                 (:species-reps spec-info)
                 (:next-species-id spec-info)
                 history'))))))

;; position-aware hybrid evolution with speciated lexicase selection
(defn run-hybrid-position
  [config run-idx]
  (let [{:keys [init-grid cell-neighbors target-grid grid-limits ca-steps
                fitness-window population-size generation-limit num-neighbors
                num-signals pca-program-min pca-program-max]} config
        num-signals    (or num-signals 3)
        error-fn       (resolve-error-fn config)
        num-cppn-in    (+ num-neighbors 2)
        terms          (make-hybrid-terms num-neighbors num-signals)
        rand-term      #(rand-nth terms)
        rand-prog      (make-rand-program terms pca-program-min pca-program-max)
        mutate-program (partial ev/umad rand-term)
        eval-hybrid    (fn [{:keys [cppn program]}]
                         (let [step-fn (make-hybrid-position-step-fn
                                         cppn program num-signals
                                         grid-limits cell-neighbors)]
                           (evaluate-rule-with-step-fn init-grid target-grid
                                                       step-fn ca-steps error-fn)))]
    (loop [gen             0
           genomes         (vec (repeatedly population-size
                              (fn [] {:cppn    (neat/make-genome num-cppn-in num-signals)
                                      :program (rand-prog)})))
           species-reps    {}
           next-species-id 0
           history         []]
      (let [evaluated (vec (pmap (fn [g]
                                   (let [result (eval-hybrid g)
                                         werrs  (window-errors (:errors result)
                                                               fitness-window)]
                                     (assoc result :genome g :window-errors werrs)))
                                 genomes))
            best      (apply min-key :min-error evaluated)
            avg-raw   (h/mean (mapv :raw-min-error evaluated))
            spec-info (eh/hybrid-speciated-generation
                        evaluated species-reps next-species-id config
                        mutate-program eh/speciated-reproduce-hybrid)
            record    (merge {:generation  gen
                              :avg-error   (float avg-raw)
                              :best-error  (float (:raw-min-error best))
                              :best-step   (:best-step best)
                              :n-species   (:n-species spec-info)
                              :best-genome (:genome best)}
                             (hybrid-complexity (:genome best)))
            _         (println (str "[HYBRID-pos " run-idx "] "
                                    (dissoc record :best-genome)))
            history'  (conj history record)]
        (if (or (>= gen generation-limit) (== 0.0 (:min-error best)))
          history'
          (recur (inc gen)
                 (:genomes spec-info)
                 (:species-reps spec-info)
                 (:next-species-id spec-info)
                 history'))))))

;; position-aware hybrid evolution with speciated lexicase and crossover
(defn run-hybrid-position-crossover
  [config run-idx]
  (let [{:keys [init-grid cell-neighbors target-grid grid-limits ca-steps
                fitness-window population-size generation-limit num-neighbors
                num-signals pca-program-min pca-program-max]} config
        num-signals    (or num-signals 3)
        error-fn       (resolve-error-fn config)
        num-cppn-in    (+ num-neighbors 2)
        terms          (make-hybrid-terms num-neighbors num-signals)
        rand-term      #(rand-nth terms)
        rand-prog      (make-rand-program terms pca-program-min pca-program-max)
        mutate-program (partial ev/umad rand-term)
        eval-hybrid    (fn [{:keys [cppn program]}]
                         (let [step-fn (make-hybrid-position-step-fn
                                         cppn program num-signals
                                         grid-limits cell-neighbors)]
                           (evaluate-rule-with-step-fn init-grid target-grid
                                                       step-fn ca-steps error-fn)))]
    (loop [gen             0
           genomes         (vec (repeatedly population-size
                              (fn [] {:cppn    (neat/make-genome num-cppn-in num-signals)
                                      :program (rand-prog)})))
           species-reps    {}
           next-species-id 0
           history         []]
      (let [evaluated (vec (pmap (fn [g]
                                   (let [result (eval-hybrid g)
                                         werrs  (window-errors (:errors result)
                                                               fitness-window)]
                                     (assoc result :genome g :window-errors werrs)))
                                 genomes))
            best      (apply min-key :min-error evaluated)
            avg-raw   (h/mean (mapv :raw-min-error evaluated))
            spec-info (eh/hybrid-speciated-generation
                        evaluated species-reps next-species-id config
                        mutate-program
                        eh/speciated-reproduce-hybrid-with-crossover)
            record    (merge {:generation  gen
                              :avg-error   (float avg-raw)
                              :best-error  (float (:raw-min-error best))
                              :best-step   (:best-step best)
                              :n-species   (:n-species spec-info)
                              :best-genome (:genome best)}
                             (hybrid-complexity (:genome best)))
            _         (println (str "[HYBRID-pos-X " run-idx "] "
                                    (dissoc record :best-genome)))
            history'  (conj history record)]
        (if (or (>= gen generation-limit) (== 0.0 (:min-error best)))
          history'
          (recur (inc gen)
                 (:genomes spec-info)
                 (:species-reps spec-info)
                 (:next-species-id spec-info)
                 history'))))))

;; extract the best rule from a single run's history
(defn- best-rule-from-run
  [run-history]
  (let [best (apply min-key :best-error run-history)]
    (merge {:error      (:best-error best)
            :generation (:generation best)
            :step       (:best-step best)}
           (when-let [prog (:best-program best)] {:best-program prog})
           (when-let [geno (:best-genome best)]  {:best-genome geno}))))

;; save multi-condition experiment results to EDN
(defn save-results!
  [experiment-name base-config conditions filename]
  (let [strip (fn [record] (dissoc record :best-program :best-genome))
        process-runs (fn [runs]
                       (let [all (apply concat runs)
                             best (apply min-key :best-error all)]
                         {:runs         (mapv #(mapv strip %) runs)
                          :best-rules   (mapv best-rule-from-run runs)
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
