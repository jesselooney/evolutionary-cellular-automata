(ns checkerboard-neat
  (:require [cellular-automata :as ca]
            [cppn :as cppn]
            [evolve-neat :as en]
            [grid-draw :as gd]
            [helpers :as h]
            [neat :as neat]
            [quil.core :as q]
            [quil.middleware :as m]))

;;;; Grid sizes

(def evolve-grid-limits [10 10])   ; used for evolution / fitness
(def draw-grid-limits [50 50])      ; used for visualization

;;;; Grid constructors

(defn make-init-grid
  [limits]
  (h/map-keys
   (ca/cell-grid limits)
   (fn [p] (= p [0 0]))))

(defn make-target-grid
  [limits]
  (h/map-keys
   (ca/cell-grid limits)
   (fn [[x y]] (even? (+ x y)))))

(defn cell-neighbors
  [limits cell]
  (ca/von-neumann-neighbors 1 limits cell))

;;;; Position-aware CPPN evaluation
;; The CPPN receives 7 inputs: 5 neighbor values + normalized x + y.

(defn normalize-pos
  [x limit]
  (if (= limit 1)
    0.0
    (- (* 2.0 (/ (double x) (double (dec limit)))) 1.0)))

(defn cppn-nv-with-pos
  [cppn limits [x y] neighbor-values]
  (let [state-inputs (mapv #(cppn/normalize-state % 2) neighbor-values)
        inputs (conj state-inputs
                     (normalize-pos x (first limits))
                     (normalize-pos y (second limits)))
        outputs (cppn/evaluate-cppn cppn inputs)]
    (cppn/decode-output outputs 2)))

(defn ca-step
  [cppn limits grid]
  (h/map-keys
   grid
   (fn [cell-key]
     (let [neighbor-vals (map grid (cell-neighbors limits cell-key))]
       (cppn-nv-with-pos cppn limits cell-key neighbor-vals)))))

;;;; Fitness (always on 10x10)

(def evolve-init-grid (make-init-grid evolve-grid-limits))
(def evolve-target-grid (make-target-grid evolve-grid-limits))

(defn genome-fitness
  [genome]
  (let [step (partial ca-step genome evolve-grid-limits)
        cell-states (take 31 (iterate step evolve-init-grid))
        total-cells (count evolve-target-grid)
        ratios (map (fn [cells]
                      (let [correct (count (filter (fn [[k v]]
                                                     (= v (get evolve-target-grid k)))
                                                   cells))]
                        (/ (double correct) total-cells)))
                    (rest cell-states))
        max-ratio (if (empty? ratios) 0.0 (apply max ratios))]
    max-ratio))

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

;;;; Visualize the best genome on a larger grid

#_(let [best-genome (:best result)
        viz-init-grid (make-init-grid draw-grid-limits)]
    (q/defsketch checkerboard-neat-sketch
      :size [500 500]
      :setup (partial gd/grid-setup viz-init-grid)
      :update (partial ca-step best-genome draw-grid-limits)
      :draw (partial gd/grid-draw draw-grid-limits)
      :middleware [m/fun-mode]))