(ns jesse_pca_square
  (:require [clojure.math :as math]
            [cellular-automata :as ca]
            [evolve-pca :as ev]
            [grid-draw :as gd]
            [helpers :as h]
            [push :as p]
            [quil.core :as q]
            [quil.middleware :as m]))

(def grid-limits [11 11])
(def center [5 5])

(def init-grid
  (h/map-keys
   (ca/cell-grid grid-limits)
   (fn [p]
     (if (= p center) 0 7))))

(def cell-neighbors (partial ca/moore-neighbors 1 grid-limits))

(def bit-count 3)
(def color-range (int (math/pow 2 bit-count)))

(def definitions {'+ (p/make-instruction #(mod (+ %1 %2) color-range) [:int :int] :int)
                  '* (p/make-instruction #(mod (+ %1 %2) color-range) [:int :int] :int)
                  'min (p/make-instruction min [:int :int] :int)
                  'max (p/make-instruction max [:int :int] :int)

                  'and (p/make-instruction #(and %1 %2) [:bool :bool] :bool)
                  'or (p/make-instruction #(or %1 %2) [:bool :bool] :bool)
                  'not (p/make-instruction not [:bool :bool] :bool)

                  '< (p/make-instruction < [:int :int] :bool)
                  '== (p/make-instruction == [:int :int] :bool)
                  'if (p/make-instruction #(if %1 %2 %3) [:bool :int :int] :int)})

(def terms (concat (keys definitions)
                   (ev/neighbor-terms 9)
                   (range color-range)))

(defn rand-term [] (rand-nth terms))

(defn rand-program
  []
  (repeatedly (+ 5 (rand-int 10)) rand-term))

(defn push-nv
  [program neighbor-values]
  (let [parser (p/make-simple-parser
                (merge definitions
                       (ev/neighbor-term-parser :int neighbor-values)))
        final-state (p/execute-state parser
                                     {:exec [program]
                                      :int []
                                      :bool []})]
    (or (p/peek-stack final-state :int) 0)))

(defn count-interior-errors
  [target cells]
  (count (filter (fn [[p v]]
                   (and
                    (contains? (set target) p)
                    (= v 7)))
                 cells)))

(defn count-exterior-errors
  [target cells]
  (count (filter (fn [[p v]]
                   (and
                    (not (contains? (set target) p))
                    (not= v 7)))
                 cells)))

(defn cells-error
  [cells]
  (let [target (ca/moore-neighbors 2 grid-limits center)
        interior-errors (count-interior-errors target cells)
        exterior-errors (count-exterior-errors target cells)]
    (+ (math/pow interior-errors 1) (math/pow exterior-errors 1))))

(defn cells-errors
  [cells]
  (let [target (ca/moore-neighbors 2 grid-limits center)
        interior-errors (count-interior-errors target cells)
        exterior-errors (count-exterior-errors target cells)]
    [interior-errors exterior-errors]))

(defn program-error
  [program]
  (ev/mean-cells-error init-grid cell-neighbors 10 5 cells-error
                       (partial push-nv program)))

(defn program-errors
  [program]
  (let [cell-states (iterate (partial ca/cells-next-value
                                      cell-neighbors
                                      (partial push-nv program)) init-grid)]
    (apply concat (map cells-errors (take 10 (drop 15 cell-states))))))

#_(ev/evolve
   (partial ev/select-mutate
            ev/lexicase
            (partial ev/umad rand-term))
   program-errors
   h/mean
   30
   (repeatedly 256 rand-program))

#_(ev/evolve
   (partial ev/select-mutate
            ev/lexicase
            (partial ev/umad rand-term))
   program-errors
   h/mean
   30
   (concat (repeatedly 255 rand-program) ['(min N0 N0 7 N2 N6 or N1 N3 N7 * and == + == 4 or if)]))


; Cool guys
; '(N4 N0 6 * if 2 N2 min 0 + *)
; '(N3 < N7 7 or N0 0 and N1 == N6 1 min + N0 if * or and)
; '(N7 == 3 < * N1 if N0 min N6 5 7 N5 N0 0 N1 == N6 1 min + N0 if if *)
; '(N7 and min N0 N8 N8 + N5 + 4 + 7 * and *)
; '(N2 2 not + N2 N0 + *)
; '(min N0 N0 7 N2 N6 or N1 N3 N7 * and == + == 4 or if)
; '(N0 4 0 < N2 N1 N6 min N1 N3 N7 * not == + N4 == 4 or not if)

#_(q/defsketch sketch
    :size [500 500]
    :setup (partial gd/grid-setup init-grid)
    :update (partial ca/cells-next-value cell-neighbors
                     (partial push-nv '(N4 N0 6 * if 2 N2 min 0 + *)))
    :draw (partial gd/grid-draw-8color grid-limits)
    :middleware [m/fun-mode])
    

