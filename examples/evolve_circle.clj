(ns evolve-circle
  (:require [push :as p]
            [gp :as gp]
            [cellular-automata :as ca]
            [helpers :as h]
            [grid-draw :as gd]
            [clojure.set :as set]
            [quil.core :as q]
            [quil.middleware :as m]))

(def instruction-set
  {'add  (p/make-instruction + [:int :int] :int)
   'sub  (p/make-instruction - [:int :int] :int)
   'mod  (p/make-instruction (fn [a b] (if (zero? b) a (mod a b)))
                             [:int :int] :int)
   'gt   (p/make-instruction > [:int :int] :bool)
   'lt   (p/make-instruction < [:int :int] :bool)
   'eq   (p/make-instruction = [:int :int] :bool)
   'and  (p/make-instruction (fn [a b] (boolean (and a b)))
                             [:bool :bool] :bool)
   'or   (p/make-instruction (fn [a b] (boolean (or a b)))
                             [:bool :bool] :bool)
   'not  (p/make-instruction not [:bool] :bool)
   'dup  (fn [state]
           (let [v (p/peek-stack state :int)]
             (if (nil? v) state (p/push-stack state :int v))))
   'swap (fn [state]
           (if (< (count (get state :int)) 2)
             state
             (let [[a s1] (p/pull-stack state :int)
                   [b s2] (p/pull-stack s1 :int)]
               (p/push-stack (p/push-stack s2 :int a) :int b))))})

(def parser (p/make-simple-parser instruction-set))

(def terminals
  (vec (concat (keys instruction-set)
               [0 1 2 3])))

(def max-push-steps 100)

(defn make-neighborhood-value [program]
  (fn [neighbor-values]
    (if (not-any? identity neighbor-values)
      false
      (let [as-ints    (map #(if % 1 0) neighbor-values)
            init-state {:exec (list program)
                        :int  (apply list as-ints)
                        :bool ()}
            final-state (gp/execute-state-limited parser init-state max-push-steps)]
        (boolean (p/peek-stack final-state :bool))))))

(def solid-square
  #{[-1 -1] [0 -1] [1 -1]
    [-1  0] [0  0] [1  0]
    [-1  1] [0  1] [1  1]})

(defn make-target-pattern [grid-limits center offsets]
  (let [alive (set (map (fn [[dx dy]]
                          [(+ (first center) dx)
                           (+ (second center) dy)])
                        offsets))]
    (h/map-keys (ca/cell-grid grid-limits)
                (fn [p] (contains? alive p)))))

(defn make-initial-grid [grid-limits center]
  (h/map-keys (ca/cell-grid grid-limits)
              (fn [p] (= p center))))

(defn iou [grid-a grid-b]
  (let [cells (keys grid-a)
        a-set (set (filter #(get grid-a %) cells))
        b-set (set (filter #(get grid-b %) cells))
        inter (count (set/intersection a-set b-set))
        union (count (set/union a-set b-set))]
    (if (zero? union) 0.0 (/ (double inter) (double union)))))

(def grid-limits [15 15])
(def center [7 7])
(def ca-steps 20)

(def target (make-target-pattern grid-limits center solid-square))
(def init-grid (make-initial-grid grid-limits center))
(def neighbors (partial ca/moore-neighbors 1 grid-limits))

(def cell-keys (vec (keys target)))
(def target-cells (set (filter #(get target %) cell-keys)))
(def target-size (count target-cells))
(def total-cells (count cell-keys))

(defn alive-count [grid]
  (count (filter val grid)))

(defn expansion-error [grid]
  (/ (double (max 0 (- (alive-count grid) target-size)))
     (- total-cells target-size)))

(defn evaluate-program [program]
  (let [neighborhood-value (make-neighborhood-value program)
        step       (partial ca/cells-next-value neighbors neighborhood-value)
        grids      (vec (take (inc ca-steps) (iterate step init-grid)))
        final-grid (peek grids)
        cell-errors (mapv (fn [k] (if (= (boolean (get final-grid k))
                                         (boolean (get target k)))
                                    0 1))
                          cell-keys)
        step-errors (mapv expansion-error (rest grids))
        errors      (into cell-errors step-errors)]
    {:program program
     :errors  errors
     :fitness (iou final-grid target)}))

(defn run-evolution []
  (gp/evolve {:evaluate-fn  evaluate-program
              :terminals    terminals
              :max-length   20
              :pop-size     300
              :generations  50
              :mutation-rate 0.75}))

(defn visualize [program]
  (let [neighborhood-value (make-neighborhood-value program)]
    (q/defsketch evolved-ca
      :size [500 500]
      :setup (partial gd/grid-setup init-grid)
      :update (partial ca/cells-next-value neighbors neighborhood-value)
      :draw (partial gd/grid-draw grid-limits)
      :middleware [m/fun-mode])))

(defn -main []
  (let [best (run-evolution)]
    (println "Best program:" (pr-str (:program best)))
    (println "Best fitness:" (:fitness best))
    (visualize (:program best))))
