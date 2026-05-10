(ns checkerboard-comparison
  (:require [cellular-automata :as ca]
            [cppn :as cppn]
            [evolve-pca :as ev]
            [evolve-neat :as en]
            [helpers :as h]
            [neat :as neat]
            [push :as p]))

(def config
  {:grid-limits       [10 10]
   :population-size   100
   :generation-limit  100
   :ca-steps          30
   :elite-count       2
   :tournament-size   3
   :n-runs            10
   :pca-program-min   5
   :pca-program-max   15
   :num-inputs        5
   :num-outputs       2
   :weight-mutation-rate 0.8
   :add-connection-rate  0.05
   :add-node-rate        0.03})

(def grid-limits (:grid-limits config))

(def init-grid
  (h/map-keys
   (ca/cell-grid grid-limits)
   (fn [p] (= p [0 0]))))

(def target-grid
  (h/map-keys
   (ca/cell-grid grid-limits)
   (fn [[x y]] (even? (+ x y)))))

(def cell-neighbors
  (partial ca/von-neumann-neighbors 1 grid-limits))

;; fraction of cells not matching target
(defn grid-error
  [cells]
  (let [total (count cells)
        correct (count (filter (fn [[k v]] (= v (get target-grid k))) cells))]
    (- 1.0 (/ (double correct) total))))

;; run neighborhood-value fn for n-steps, return error metrics
(defn evaluate-rule
  [neighborhood-value n-steps]
  (let [step-fn (partial ca/cells-next-value cell-neighbors neighborhood-value)
        states  (take (inc n-steps) (iterate step-fn init-grid))
        errors  (mapv grid-error (rest states))
        [best-idx best-err]
        (reduce-kv (fn [[bi be] i e]
                     (if (< (double e) (double be)) [i e] [bi be]))
                   [0 (double (first errors))]
                   errors)]
    {:min-error  (double best-err)
     :best-step  (inc best-idx)
     :errors     errors}))

(def pca-definitions
  {'and  (p/make-instruction #(and %1 %2) [:bool :bool] :bool)
   'or   (p/make-instruction #(or %1 %2) [:bool :bool] :bool)
   'xor  (p/make-instruction not= [:bool :bool] :bool)
   'nand (p/make-instruction #(not (and %1 %2)) [:bool :bool] :bool)
   'nor  (p/make-instruction #(not (or %1 %2)) [:bool :bool] :bool)
   'not  (p/make-instruction not [:bool :bool] :bool)})

(def pca-terms
  (vec (concat (keys pca-definitions)
               (ev/neighbor-terms 5)
               [true false])))

(defn rand-term [] (rand-nth pca-terms))

(defn rand-program []
  (let [lo (:pca-program-min config)
        hi (:pca-program-max config)]
    (repeatedly (+ lo (rand-int (- hi lo))) rand-term)))

;; Push program as CA neighborhood rule
(defn push-nv
  [program neighbor-values]
  (let [parser (p/make-simple-parser
                (merge pca-definitions
                       (ev/neighbor-term-parser :bool neighbor-values)))
        final-state (p/execute-state parser {:exec [program] :bool []})]
    (or (p/peek-stack final-state :bool) false)))

(defn pca-evaluate [program]
  (evaluate-rule (partial push-nv program) (:ca-steps config)))

;; CPPN genome as CA neighborhood rule
(defn cppn-nv
  [genome neighbor-values]
  (let [inputs (mapv #(cppn/normalize-state % 2) neighbor-values)
        outputs (cppn/evaluate-cppn genome inputs)]
    (cppn/decode-output outputs 2)))

(defn nca-evaluate [genome]
  (evaluate-rule (partial cppn-nv genome) (:ca-steps config)))

;; one PCA evolution run
(defn run-pca
  [run-idx]
  (let [{:keys [population-size generation-limit elite-count tournament-size]} config
        mutate (partial ev/umad rand-term)
        select (partial ev/tournament tournament-size)]
    (loop [gen      0
           programs (vec (repeatedly population-size rand-program))
           history  []]
      (let [evaluated (vec (pmap (fn [prog]
                                   (assoc (pca-evaluate prog) :program prog))
                                 programs))
            best      (apply min-key :min-error evaluated)
            avg-err   (h/mean (mapv :min-error evaluated))
            record    {:generation   gen
                       :avg-error    (float avg-err)
                       :best-error   (float (:min-error best))
                       :best-step    (:best-step best)
                       :best-program (:program best)}
            _         (println (str "[PCA " run-idx "] "
                                    (dissoc record :best-program)))
            history'  (conj history record)]
        (if (or (>= gen generation-limit) (== 0.0 (:min-error best)))
          history'
          (let [population (mapv (fn [e] {:program (:program e)
                                          :error   (:min-error e)})
                                 evaluated)
                next-progs (ev/elitist-select-mutate
                            select mutate elite-count population)]
            (recur (inc gen) (vec next-progs) history')))))))

;; one NCA evolution run
(defn run-nca
  [run-idx]
  (let [{:keys [population-size generation-limit elite-count tournament-size]} config]
    (loop [gen     0
           genomes (vec (repeatedly population-size
                          #(neat/make-genome (:num-inputs config)
                                            (:num-outputs config))))
           history []]
      (let [evaluated (vec (pmap (fn [g]
                                   (assoc (nca-evaluate g) :genome g))
                                 genomes))
            best      (apply min-key :min-error evaluated)
            avg-err   (h/mean (mapv :min-error evaluated))
            record    {:generation  gen
                       :avg-error   (float avg-err)
                       :best-error  (float (:min-error best))
                       :best-step   (:best-step best)
                       :best-genome (:genome best)}
            _         (println (str "[NCA " run-idx "] "
                                    (dissoc record :best-genome)))
            history'  (conj history record)]
        (if (or (>= gen generation-limit) (== 0.0 (:min-error best)))
          history'
          (let [population (mapv (fn [e]
                                   (assoc (:genome e)
                                          :fitness (- 1.0 (:min-error e))))
                                 evaluated)
                next-gens  (en/elitist-select-mutate
                            (partial en/tournament tournament-size)
                            elite-count config population)]
            (recur (inc gen) (vec next-gens) history')))))))

(defn save-results!
  [pca-runs nca-runs filename]
  (let [strip (fn [record] (dissoc record :best-program :best-genome))
        all-pca (apply concat pca-runs)
        best-pca (apply min-key :best-error all-pca)
        all-nca (apply concat nca-runs)
        best-nca (apply min-key :best-error all-nca)
        data {:experiment "checkerboard-comparison"
              :timestamp  (str (java.time.Instant/now))
              :config     config
              :target     "checkerboard-even-x+y"
              :pca {:runs          (mapv #(mapv strip %) pca-runs)
                    :best-overall  {:error      (:best-error best-pca)
                                    :generation (:generation best-pca)
                                    :step       (:best-step best-pca)
                                    :program    (pr-str (:best-program best-pca))}}
              :nca {:runs          (mapv #(mapv strip %) nca-runs)
                    :best-overall  {:error      (:best-error best-nca)
                                    :generation (:generation best-nca)
                                    :step       (:best-step best-nca)
                                    :genome     (dissoc (:best-genome best-nca)
                                                        :eval-order :fitness)}}}]
    (spit filename (pr-str data))
    (println (str "Results saved to " filename))))

(defn run-experiment!
  ([] (run-experiment! (:n-runs config)))
  ([n-runs]
   (println (str "Running " n-runs " PCA evolutions..."))
   (let [pca-runs (mapv run-pca (range n-runs))]
     (println (str "\nRunning " n-runs " NCA evolutions..."))
     (let [nca-runs (mapv run-nca (range n-runs))]
       (save-results! pca-runs nca-runs
                      "experiments/simple/checkerboard_comparison_results.edn")
       {:pca pca-runs :nca nca-runs}))))

(run-experiment!)
