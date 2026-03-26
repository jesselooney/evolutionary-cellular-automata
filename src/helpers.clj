(ns helpers)

;;; Based on https://stackoverflow.com/a/18248031.
(defn cartesian-product
  "The [Cartesian product](https://en.wikipedia.org/wiki/Cartesian_product) of
   the input sequences."
  ([] '(()))
  ([& seqs]
   (for [prod (apply cartesian-product (rest seqs))
         x (first seqs)]
     (cons x prod))))

;;; Examples
(cartesian-product [1 2])
(cartesian-product [1 2] [3 4])

(defn map-keys
  "Update a map `m` by applying `f` to its keys.
   
   The output map has the same keys as `m`, but the value for each key `k` is
   `(f k)`."
  [m f]
  (into {} (for [[k _] m] [k (f k)])))

;;; Examples
(map-keys {0 nil, 1 nil, 2 nil} inc)
