(ns life
  "A vizualization of [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life)."
  (:require [helpers :as h]
            [cellular-automata :as ca]
            [grid-draw :as gd]
            [quil.core :as q]
            [quil.middleware :as m]))

(def grid-limits [10 10])

(def init-grid
  (h/map-keys
   (ca/cell-grid grid-limits)
   (fn [p]
     (contains? #{[1 0] [2 1] [0 2] [1 2] [2 2]} p))))

(def neighbors (partial ca/moore-neighbors 1 grid-limits))

(defn neighborhood-value
  [neighbor-values]
  (let [self-value (first neighbor-values)
        live-count (count (filter identity (rest neighbor-values)))]
    (if self-value
      (contains? #{2 3} live-count)
      (== live-count 3))))

(q/defsketch life
  :size [500 500]
  :setup (partial gd/grid-setup init-grid)
  :update (partial ca/cells-next-value neighbors neighborhood-value)
  :draw (partial gd/grid-draw grid-limits)
  :middleware [m/fun-mode])
