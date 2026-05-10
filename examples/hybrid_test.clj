(ns hybrid-test
  "Quick test of hybrid PCA-NCA on vertical stripes"
  (:require [cellular-automata :as ca]
            [experiment-runner :as er]
            [grid-draw :as gd]
            [helpers :as h]
            [neat :as neat]
            [quil.core :as q]
            [quil.middleware :as m]))

(def evolve-grid-limits [10 10])
(def draw-grid-limits   [30 30])

(def target-fn (fn [[x _] _ _] (even? x)))

(def config
  {:grid-limits          evolve-grid-limits
   :init-grid            (er/make-init-grid evolve-grid-limits)
   :target-grid          (er/make-target-grid evolve-grid-limits target-fn)
   :cell-neighbors       (partial ca/von-neumann-neighbors 1 evolve-grid-limits)
   :population-size      200
   :generation-limit     50
   :ca-steps             30
   :fitness-window       10
   :elite-count          2
   :fitness-transform    :ca-neat
   :pca-program-min      5
   :pca-program-max      20
   :num-neighbors        5
   :num-signals          3
   :weight-mutation-rate  0.6
   :add-connection-rate   0.15
   :add-node-rate         0.10
   :compatibility-threshold 0.9
   :c1                    1.0
   :c3                    0.4
   :crossover-rate        0.75})

(println "--- Evolving hybrid rule for vertical stripes ---")
(def history (er/run-hybrid-crossover config 0))

(def best-record (apply min-key :best-error history))
(println (str "\nBest error: " (:best-error best-record)
              " at generation " (:generation best-record)
              " step " (:best-step best-record)))

(def best-genome (:best-genome best-record))
(println (str "CPPN nodes: " (count (:nodes (:cppn best-genome)))
              ", connections: " (count (:connections (:cppn best-genome)))))
(println (str "Push program length: " (count (:program best-genome))))
(println (str "Push program: " (pr-str (:program best-genome))))

(def viz-init-grid (er/make-init-grid draw-grid-limits))
(def viz-neighbors (partial ca/von-neumann-neighbors 1 draw-grid-limits))
(def num-signals   (:num-signals config))

(defn hybrid-step [grid]
  (h/map-keys grid
    (fn [cell-key]
      (let [nbr-vals (map grid (viz-neighbors cell-key))]
        (er/hybrid-nv (:cppn best-genome)
                      (:program best-genome)
                      num-signals
                      nbr-vals)))))

(q/defsketch hybrid-viz
  :title "Hybrid PCA-NCA: Vertical Stripes"
  :size [500 500]
  :setup (partial gd/grid-setup viz-init-grid)
  :update (fn [grid] (hybrid-step grid))
  :draw (partial gd/grid-draw draw-grid-limits)
  :middleware [m/fun-mode])
