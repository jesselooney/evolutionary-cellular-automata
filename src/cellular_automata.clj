(ns cellular-automata
  "Utilities for programming [cellular automata](https://en.wikipedia.org/wiki/Cellular_automaton).
   
   For the purposes of this namespace, a fully-specified CA consists of the
   following components:
   - a map `cells` of cell keys (often coordinate tuples) to their values;
   - a function `cell-neighbors` from cell keys to sequences of cell keys,
   defining the neighborhood of any given cell; and
   - a function `neighborhood-value` from sequences of cell values to cell
   values, defining the next value of a cell given the current values of its
   neighbors.

   The sequence of values passed to the local rule `neighborhood-value` inherits
   its order from the sequence of keys output by `cell-neighbors`.
   "
  (:require [helpers :as h]))

;;;; Running CAs.

(defn cell-next-value
  "Compute the next value of a cell identified by `cell-key`."
  [cell-neighbors neighborhood-value cells cell-key]
    (neighborhood-value (map cells (cell-neighbors cell-key))))

(defn cells-next-value
  "Compute the next state of `cells`."
  [cell-neighbors neighborhood-value cells]
  (h/map-keys cells
              #(cell-next-value cell-neighbors neighborhood-value cells %)))

;;;; Constructing CAs.

(defn cell-grid
  "Generate a map representing a grid with dimensions given by `grid-limits`.
   
   **Arguments**
   - `grid-limits`: A sequence `[x1 x2 ... xn]` of positive integers.
   
   **Returns**
   - A map with keys of the form `[y1 y2 ... yn]`, where each `yi` is a
   nonnegative integer such that `(< yi xi)`. Each key is assigned a value
   of `nil`."
  [grid-limits]
  (into {} (map #(identity [% nil])
                (apply h/cartesian-product (map range grid-limits)))))

;;; Examples.
;; One-dimensional grid of length 2.
(cell-grid [2])
;; Two-dimensional 2-by-3 grid.
(cell-grid [2 3])
;; Three-dimensional grid.
(cell-grid [2 3 4])

(defn modular-interval
  "An interval of radius `r` around `x`, modulo `x-max`."
  [r x-max x]
  (map #(mod % x-max)
       (range (- x r) (inc (+ x r)))))

;;; Examples.
(modular-interval 1 3 1)
(modular-interval 1 3 0)
(modular-interval 2 5 1)

(defn moore-neighbors
  "The [Moore neighborhood](https://en.wikipedia.org/wiki/Moore_neighborhood) of
   radius `r` around a point `p`, modulo the bounds given by `grid-limits`.
   
   This neighborhood always contains the point `p` itself, which is returned as
   the first element of the output sequence."
  [r grid-limits p]
  (let [coord-ranges (map (partial modular-interval r)
                          grid-limits p)
        coords (apply h/cartesian-product coord-ranges)]
    (cons p (remove #(= p %) coords))))

;;; Examples.
;; Radius-2 neighbors of [1] on a length-3 grid.
(moore-neighbors 2 [3] [1])
;; Radius-1 neighbors of [1 1] on a 3-by-3 grid.
(moore-neighbors 1 [3 3] [1 1])