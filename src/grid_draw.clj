(ns grid-draw
  "Utilities for drawing cell grids using Quil."
  (:require [helpers :as h]
            [quil.core :as q]))

(defn grid-setup [grid]
  (q/frame-rate 5)
  (q/color-mode :hsb)
  (q/no-stroke)
  grid)

(defn grid-draw [[i-max j-max] cells]
  (let [cell-width (/ (q/width) i-max)
        cell-height (/ (q/height) j-max)]
    (doseq [[[i j] v] cells]
      (let [x (* i cell-width)
            y (* j cell-height)]
        (q/fill (if v 0 255))
        (q/rect x y cell-width cell-height)))))

(defn grid-draw-8color [[i-max j-max] cells]
  (let [cell-width (/ (q/width) i-max)
        cell-height (/ (q/height) j-max)]
    (doseq [[[i j] v] cells]
      (let [x (* i cell-width)
            y (* j cell-height)]
        (q/color-mode :rgb 1)
        (apply q/fill (map h/bool-to-int (take 3 (h/bit-seq v))))
        (q/rect x y cell-width cell-height)))))