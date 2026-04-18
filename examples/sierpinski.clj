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

#_(ev/evolve
   (partial ev/elitist-select-mutate
            (partial ev/tournament 2)
            (partial ev/umad rand-term)
            1)
   program-error
   identity
   20
   (repeatedly 128 rand-program))

#_(ev/evolve
   (partial ev/elitist-select-crossover-mutate
            (partial ev/tournament 2)
            ev/one-point-crossover
            (partial ev/umad rand-term)
            4)
   program-error
   identity
   20
   (repeatedly 128 rand-program))

;; Found via evolution.
(def sierpinski-program '(true true N6 and N1 and N1 not N1 or N2 nand nor N1 N8 N2 N1 xor N8 N0 N5 or))

#_(q/defsketch sketch
  :size [500 500]
  :setup (partial gd/grid-setup init-grid)
  :update (partial ca/cells-next-value cell-neighbors
                   (partial push-nv sierpinski-program))
  :draw (partial gd/grid-draw grid-limits)
  :middleware [m/fun-mode])