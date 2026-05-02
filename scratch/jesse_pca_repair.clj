(ns jesse-pca-repair
  (:require [clojure.math :as math]
            [cellular-automata :as ca]
            [evolve-pca :as ev]
            [grid-draw :as gd]
            [helpers :as h]
            [push :as p]
            [quil.core :as q]
            [quil.middleware :as m]))

(def grid-limits [10 10])
(def center [0 0])

(def init-grid
  (h/map-keys
   (ca/cell-grid grid-limits)
   (fn [p]
     (if (= p center) 0 7))))

(def target-grid
  (h/map-keys
   (ca/cell-grid grid-limits)
   (fn [[x y]] (if (even? (+ x y)) 0 7))))

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

(defn grid-errors
  [target cells]
  (for [[p v] cells]
             (if (contains? target p)
                  (abs (- v (target p)))
                  0)))

(defn grid-errors-count
  [target cells]
  (count (filter #(> % 0)
          (grid-errors target cells))))

(defn rand-damage
  [cells]
  (let [center (rand-nth (keys cells))
        victims (set (ca/moore-neighbors 1 grid-limits center))]
    (map (fn [[p v]] (if (contains? victims p) 7 v)) cells)))

(defn program-errors-counts
  [program]
  (let [cell-states (iterate (partial ca/cells-next-value
                                      cell-neighbors
                                      (partial push-nv program)) init-grid)]
    (map (partial grid-errors-count target-grid) (take 10 (drop 10 cell-states)))))

#_(ev/evolve
   (partial ev/select-mutate
            ev/lexicase
            (partial ev/umad rand-term))
   program-errors-counts
   h/mean
   20
   (repeatedly 128 rand-program))

; (N6 == N3 if N3 not + min N0 N1 min *)
; (N6 == N6 1 N8 N1 N0 N3 min min)

#_(q/defsketch sketch
    :size [500 500]
    :setup (partial gd/grid-setup init-grid)
    :update (partial ca/cells-next-value cell-neighbors
                     (partial push-nv '(* N2 1 N4 < 3 N7 if *)))
    :draw (partial gd/grid-draw-8color grid-limits)
    :middleware [m/fun-mode])
    

