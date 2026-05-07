(ns sierpinski
  (:require [cellular-automata :as ca]
            [evolve-pca :as ev]
            [grid-draw :as gd]
            [helpers :as h]
            [push :as p]
            [quil.core :as q]
            [quil.middleware :as m]))

(def grid-limits [15 15])

(def init-grid
  (h/map-keys
   (ca/cell-grid grid-limits)
   (fn [p]
     (contains? #{[0 0]} p))))

(def cell-neighbors (partial ca/von-neumann-neighbors 1 grid-limits))

(def definitions {'and (p/make-instruction #(and %1 %2) [:bool :bool] :bool)
                  'or (p/make-instruction #(or %1 %2) [:bool :bool] :bool)
                  'xor (p/make-instruction not= [:bool :bool] :bool)
                  'nand (p/make-instruction #(not (and %1 %2)) [:bool :bool] :bool)
                  'nor (p/make-instruction #(not (or %1 %2)) [:bool :bool] :bool)
                  'not (p/make-instruction not [:bool :bool] :bool)})

(def terms (concat (keys definitions)
                   (ev/neighbor-terms 5)
                   [true false]))

(defn rand-term [] (rand-nth terms))

(defn rand-program
  []
  (repeatedly (+ 5 (rand-int 10)) rand-term))

(defn push-nv
  [program neighbor-values]
  (let [parser (p/make-simple-parser
                (merge definitions
                       (ev/neighbor-term-parser :bool neighbor-values)))
        final-state (p/execute-state parser
                                     {:exec [program]
                                      :bool []})]
    (p/peek-stack final-state :bool)))

(defn matches
  [pattern seq]
  (and (= (count pattern) (count seq))
       (every? identity (map #(or (nil? %1) (= %1 %2)) pattern seq))))

(defn cells-error
  [cells]
  (let [cell-criterion (fn [[cell-key _]]
                         (matches [true true false false true]
                                  (map cells (cell-neighbors cell-key))))]
    (- (count (filter cell-criterion cells)))))

(defn program-error
  [program]
  (ev/mean-cells-error init-grid cell-neighbors 20 5 cells-error
                       (partial push-nv program)))

;;;; Run evolution

#_(def result
    (ev/evolve
     (partial ev/elitist-select-mutate
              (partial ev/tournament 2)
              (partial ev/umad rand-term)
              1)
     program-error
     identity
     20
     (repeatedly 128 rand-program)))


;;;; Visualize the best evolved program

#_(let [best-program (:program (apply min-key :error result))]
    (q/defsketch sierpinski-sketch
      :size [500 500]
      :setup (partial gd/grid-setup init-grid)
      :update (partial ca/cells-next-value cell-neighbors
                       (partial push-nv best-program))
      :draw (partial gd/grid-draw grid-limits)
      :middleware [m/fun-mode]))