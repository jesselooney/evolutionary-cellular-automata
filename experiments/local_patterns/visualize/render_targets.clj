(ns render-targets
  (:require [cellular-automata :as ca]
            [experiment-runner :as er])
  (:import [java.awt Color Graphics2D Font RenderingHints]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File]))

(def patterns
  [{:name "checkerboard"
    :target-fn (fn [[x y] _ _] (even? (+ x y)))}
   {:name "vertical-stripes"
    :target-fn (fn [[x _] _ _] (even? x))}
   {:name "horizontal-stripes"
    :target-fn (fn [[_ y] _ _] (even? y))}
   {:name "sparse-dots"
    :target-fn (fn [[x y] _ _] (and (even? x) (even? y)))}
   {:name "dense-dots"
    :target-fn (fn [[x y] _ _] (or (even? x) (even? y)))}
   {:name "wide-stripes"
    :target-fn (fn [[x _] _ _] (< (mod x 4) 2))}])

(def grid-limits [10 10])
(def cell-size 40)
(def grid-line-width 1)

(def out-dir "experiments/local_patterns/visualize/")

;; render boolean grid to BufferedImage
(defn render-grid
  [grid [w h] cell-size & {:keys [title] :or {title nil}}]
  (let [title-height (if title 30 0)
        img-w (* w cell-size)
        img-h (+ (* h cell-size) title-height)
        img   (BufferedImage. img-w img-h BufferedImage/TYPE_INT_RGB)
        g     (.createGraphics img)]
    (.setColor g Color/WHITE)
    (.fillRect g 0 0 img-w img-h)

    (when title
      (.setRenderingHint g RenderingHints/KEY_ANTIALIASING
                         RenderingHints/VALUE_ANTIALIAS_ON)
      (.setColor g Color/BLACK)
      (.setFont g (Font. "SansSerif" Font/BOLD 14))
      (let [fm (.getFontMetrics g)
            tw (.stringWidth fm title)
            tx (/ (- img-w tw) 2)]
        (.drawString g ^String title (int tx) 20)))

    (doseq [x (range w)
            y (range h)]
      (let [alive? (get grid [x y] false)
            px (* x cell-size)
            py (+ (* y cell-size) title-height)]
        (.setColor g (if alive? Color/BLACK Color/WHITE))
        (.fillRect g px py cell-size cell-size)
        (.setColor g (Color. 200 200 200))
        (.drawRect g px py cell-size cell-size)))

    (.dispose g)
    img))

(defn save-image! [^BufferedImage img path]
  (ImageIO/write img "png" (File. ^String path))
  (println (str "Saved: " path)))

;; individual pattern PNGs
(defn render-individual-patterns! []
  (doseq [{:keys [name target-fn]} patterns]
    (let [grid (er/make-target-grid grid-limits target-fn)
          img  (render-grid grid grid-limits cell-size :title name)
          path (str out-dir name ".png")]
      (save-image! img path))))

;; combined grid layout PNG
(defn render-layout! []
  (let [ncols   3
        nrows   (int (Math/ceil (/ (count patterns) ncols)))
        [w h]   grid-limits
        cell-w  (* w cell-size)
        cell-h  (+ (* h cell-size) 30)
        padding 20
        total-w (+ (* ncols cell-w) (* (inc ncols) padding))
        total-h (+ (* nrows cell-h) (* (inc nrows) padding) 40)
        img     (BufferedImage. total-w total-h BufferedImage/TYPE_INT_RGB)
        g       (.createGraphics img)]

    (.setColor g (Color. 245 245 245))
    (.fillRect g 0 0 total-w total-h)

    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING
                       RenderingHints/VALUE_ANTIALIAS_ON)
    (.setColor g Color/BLACK)
    (.setFont g (Font. "SansSerif" Font/BOLD 18))
    (let [fm (.getFontMetrics g)
          title "Target Patterns (10x10, locally solvable)"
          tw (.stringWidth fm title)
          tx (/ (- total-w tw) 2)]
      (.drawString g ^String title (int tx) 28))

    (doseq [[i {:keys [name target-fn]}] (map-indexed vector patterns)]
      (let [col  (mod i ncols)
            row  (quot i ncols)
            grid (er/make-target-grid grid-limits target-fn)
            sub  (render-grid grid grid-limits cell-size :title name)
            dx   (+ padding (* col (+ cell-w padding)))
            dy   (+ 40 padding (* row (+ cell-h padding)))]
        (.drawImage g sub dx dy nil)))

    (.dispose g)
    (save-image! img (str out-dir "all_patterns.png"))))

(render-individual-patterns!)
(render-layout!)
