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

;;; Examples.
(cartesian-product [1 2])
(cartesian-product [1 2] [3 4])

(defn map-keys
  "Update a map `m` by applying `f` to its keys.
   
   The output map has the same keys as `m`, but the value for each key `k` is
   `(f k)`."
  [m f]
  (into {} (for [[k _] m] [k (f k)])))

;;; Examples.
(map-keys {0 nil, 1 nil, 2 nil} inc)

(defn taxicab-norm
  [p]
  (reduce + (map abs p)))

;;; Examples.
(taxicab-norm [0])
(taxicab-norm [2 -1])

(defn taxicab-distance
  "The [taxicab distance](https://en.wikipedia.org/wiki/Taxicab_geometry)
   between points `p` and `q`."
  [p q]
  (taxicab-norm (map - p q)))

;;; Examples.
(taxicab-distance [0] [0])
(taxicab-distance [0 1] [-1 2])

(defn mean
  [coll]
  (/ (reduce + coll) (count coll)))

;;; Examples.
(mean [0 10])

(defn modular-vector-sum
  "Element-wise sum of `p` and `q` modulo `moduli`."
  [moduli p q]
  (map (fn [modulus pi qi] (mod (+ pi qi) modulus)) moduli p q))

;;; Examples.
(modular-vector-sum [3 3 5] [1 2 2] [1 2 2])