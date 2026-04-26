(ns checkerboard-neat
  (:require [cellular-automata :as ca]
            [cppn :as cppn]
            [evolve-neat :as en]
            [grid-draw :as gd]
            [helpers :as h]
            [neat :as neat]
            [quil.core :as q]
            [quil.middleware :as m]))

;;;; CA setup

(def grid-limits [10 10])

(def init-grid
  (h/map-keys
   (ca/cell-grid grid-limits)
   (fn [p] (= p [0 0]))))

(def cell-neighbors (partial ca/von-neumann-neighbors 1 grid-limits))

(def target-grid
  (h/map-keys
   (ca/cell-grid grid-limits)
   (fn [[x y]] (even? (+ x y)))))

;;;; Position-aware CPPN evaluation
;; The CPPN receives 7 inputs: 5 neighbor values + normalized x + y.


(defn normalize-pos
  [x limit]
  (- (* 2.0 (/ (double x) (double limit))) 1.0))

(defn cppn-nv-with-pos
  [cppn [x y] neighbor-values]
  (let [state-inputs (mapv #(cppn/normalize-state % 2) neighbor-values)
        inputs (conj state-inputs
                     (normalize-pos x (first grid-limits))
                     (normalize-pos y (second grid-limits)))
        outputs (cppn/evaluate-cppn cppn inputs)]
    (cppn/decode-output outputs 2)))

(defn ca-step
  "One CA step using a position-aware CPPN rule."
  [cppn grid]
  (h/map-keys grid
    (fn [cell-key]
      (let [neighbor-vals (map grid (cell-neighbors cell-key))]
        (cppn-nv-with-pos cppn cell-key neighbor-vals)))))

;;;; Fitness

(defn genome-fitness
  [genome]
  (let [step (partial ca-step genome)
        cell-states (take 31 (iterate step init-grid))
        total-cells (count target-grid)
        ratios (map (fn [cells]
                      (let [correct (count (filter (fn [[k v]]
                                                     (= v (get target-grid k)))
                                                   cells))]
                        (/ (double correct) total-cells)))
                    (rest cell-states))
        max-ratio (if (empty? ratios) 0.0 (apply max ratios))
        ;; Penalize quiescent / trivial solutions.
        ;; f(1.0) = 1.0, f(0.5) ≈ 0.04
        penalized (* max-ratio (/ (Math/exp (* 5.0 max-ratio)) (Math/exp 5.0)))]
    penalized))

;;;; NEAT configuration

(def config
  {:population-size 20
   :generation-limit 200
   :fitness-fn genome-fitness
   :num-inputs 7
   :num-outputs 2
   :elitism 1
   :weight-mutation-rate 0.8
   :add-connection-rate 0.05
   :add-node-rate 0.03})

;;;; Run evolution 

#_(def result (en/evolve config))

;;;; Visualize the best genome

#_(let [best-genome (:best result)]
    (q/defsketch checkerboard-neat-sketch
      :size [500 500]
      :setup (partial gd/grid-setup init-grid)
      :update (partial ca-step best-genome)
      :draw (partial gd/grid-draw grid-limits)
      :middleware [m/fun-mode]))
