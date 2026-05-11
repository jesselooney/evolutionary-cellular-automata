(ns thue-morse.visualize-best
  "Load best evolved rules from results, simulate CA growth, and render
   a 3xN comparison grid showing snapshots at key timesteps."
  (:require [cellular-automata :as ca]
            [clojure.edn :as edn]
            [experiment-runner :as er]
            [helpers :as h])
  (:require [clojure.java.shell :refer [sh]])
  (:import [java.awt Color Graphics2D Font RenderingHints BasicStroke]
           [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File]))

;; ─── Data loading ─────────────────────────────────────────────────

(def results
  (edn/read-string (slurp "experiments/thue_morse/results/thue_morse_results.edn")))

(def condition (first (:conditions results)))

(def grid-limits [30 30])
(def init-grid (er/make-init-grid grid-limits))
(def cell-neighbors (partial ca/von-neumann-neighbors 1 grid-limits))

(def num-signals 4)

(def target-fn (fn [[x y] _ _]
                 (even? (+ (Integer/bitCount x)
                           (Integer/bitCount y)))))
(def target-grid (er/make-target-grid grid-limits target-fn))

(def pca-rule    (first (get-in condition [:pca :best-rules])))
(def nca-rule    (first (get-in condition [:nca :best-rules])))
(def hybrid-rule (first (get-in condition [:hybrid :best-rules])))


(defn pca-step [grid]
  (h/map-keys grid
    (fn [cell-key]
      (let [nbr-vals (map grid (cell-neighbors cell-key))]
        (er/push-nv (:best-program pca-rule) nbr-vals)))))

(defn nca-step [grid]
  (h/map-keys grid
    (fn [cell-key]
      (let [nbr-vals (map grid (cell-neighbors cell-key))]
        (er/cppn-nv (:best-genome nca-rule) nbr-vals)))))

(defn hybrid-step [grid]
  (let [genome (:best-genome hybrid-rule)]
    (h/map-keys grid
      (fn [cell-key]
        (let [nbr-vals (map grid (cell-neighbors cell-key))]
          (er/hybrid-nv (:cppn genome) (:program genome)
                        num-signals nbr-vals))))))

;; ─── Snapshot collection ──────────────────────────────────────────

(def snapshot-steps [5 10 20 31 50])

(defn collect-snapshots
  "Run step-fn from init-grid, return grids at each timestep in `steps`."
  [step-fn steps]
  (let [max-step (apply max steps)
        all-states (vec (take (inc max-step) (iterate step-fn init-grid)))]
    (mapv #(nth all-states %) steps)))

;; ─── Rendering ────────────────────────────────────────────────────

(def cell-px 8)
(def pad 8)
(def row-label-w 30)
(def col-label-h 34)
(def err-label-h 20)
(def title-h 44)

(defn render-grid-at
  "Draw a boolean grid at (x0, y0) on Graphics2D context."
  [^Graphics2D g grid [w h] x0 y0]
  (doseq [gx (range w)
          gy (range h)]
    (let [alive? (get grid [gx gy] false)
          px (+ x0 (* gx cell-px))
          py (+ y0 (* gy cell-px))]
      (.setColor g (if alive? Color/BLACK Color/WHITE))
      (.fillRect g px py cell-px cell-px)))
  ;; thin border
  (.setColor g (Color. 160 160 160))
  (.setStroke g (BasicStroke. 1.0))
  (.drawRect g x0 y0 (* w cell-px) (* h cell-px)))

(defn render-error-label
  "Draw error value centered below a grid cell."
  [^Graphics2D g error x0 y0 grid-w]
  (let [err-str (if (< error 0.001) "0.000" (format "%.3f" (double error)))
        color   (cond (< error 0.01)  (Color. 0 130 0)
                      (< error 0.05)  (Color. 140 100 0)
                      :else           (Color. 180 0 0))]
    (.setFont g (Font. "Monospaced" Font/PLAIN 12))
    (.setColor g color)
    (let [fm (.getFontMetrics g)
          tw (.stringWidth fm err-str)
          cx (+ x0 (/ grid-w 2))]
      (.drawString g ^String err-str
                   (int (- cx (/ tw 2)))
                   (int (+ y0 12))))))

(defn render-comparison!
  "Render 3xN comparison grid: rows=methods, cols=timesteps."
  [methods out-dir]
  (let [[w h]     grid-limits
        grid-w    (* w cell-px)
        grid-h    (* h cell-px)
        n-cols    (count snapshot-steps)
        n-rows    (count methods)
        row-block (+ grid-h err-label-h pad)
        total-w   (+ row-label-w pad (* n-cols (+ grid-w pad)))
        total-h   (+ title-h col-label-h (* n-rows row-block) pad)
        img       (BufferedImage. total-w total-h BufferedImage/TYPE_INT_RGB)
        g         (.createGraphics img)]

    ;; background
    (.setColor g (Color. 252 252 252))
    (.fillRect g 0 0 total-w total-h)

    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING
                       RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint g RenderingHints/KEY_TEXT_ANTIALIASING
                       RenderingHints/VALUE_TEXT_ANTIALIAS_ON)

    ;; title
    (.setColor g Color/BLACK)
    (.setFont g (Font. "SansSerif" Font/BOLD 18))
    (let [title "Thue-Morse 2D — CA Growth Comparison (30x30)"
          fm    (.getFontMetrics g)
          tw    (.stringWidth fm title)]
      (.drawString g ^String title (int (/ (- total-w tw) 2)) 30))

    ;; column labels: timesteps
    (.setFont g (Font. "SansSerif" Font/PLAIN 14))
    (.setColor g (Color. 60 60 60))
    (doseq [[i step] (map-indexed vector snapshot-steps)]
      (let [x     (+ row-label-w pad (* i (+ grid-w pad)))
            label (str "t=" step)
            fm    (.getFontMetrics g)
            tw    (.stringWidth fm label)
            cx    (+ x (/ grid-w 2))]
        (.drawString g ^String label
                     (int (- cx (/ tw 2)))
                     (+ title-h 22))))

    ;; method rows
    (doseq [[ri {:keys [label snapshots]}] (map-indexed vector methods)]
      (let [y-base (+ title-h col-label-h (* ri row-block))]

        ;; row label (rotated 90 degrees CCW, flush against first grid)
        (.setFont g (Font. "SansSerif" Font/BOLD 15))
        (.setColor g Color/BLACK)
        (let [fm (.getFontMetrics g)
              tw (.stringWidth fm label)
              tx (int (+ 4 (.getAscent fm)))
              ty (int (+ y-base (/ grid-h 2) (/ tw 2)))
              orig (.getTransform g)]
          (.rotate g (- (/ Math/PI 2)) tx ty)
          (.drawString g ^String label tx ty)
          (.setTransform g orig))

        ;; snapshot grids + error
        (doseq [[ci grid] (map-indexed vector snapshots)]
          (let [x (+ row-label-w pad (* ci (+ grid-w pad)))]
            (render-grid-at g grid grid-limits x y-base)
            (render-error-label g (er/grid-error target-grid grid)
                                x (+ y-base grid-h) grid-w)))))

    (.dispose g)

    (let [png-path (str out-dir "/growth_comparison.png")
          pdf-path (str out-dir "/growth_comparison.pdf")]
      (ImageIO/write img "png" (File. ^String png-path))
      (println (str "\nSaved: " png-path))
      (let [{:keys [exit err]} (sh "python" "-c"
                                   (str "from PIL import Image;"
                                        "Image.open('" png-path "')"
                                        ".save('" pdf-path "','PDF',"
                                        "resolution=150)"))]
        (if (zero? exit)
          (println (str "Saved: " pdf-path))
          (println (str "PDF export failed: " err)))))))

;; ─── Run ──────────────────────────────────────────────────────────

(println "\nCollecting PCA snapshots...")
(def pca-snaps (collect-snapshots pca-step snapshot-steps))
(println "Collecting NCA snapshots...")
(def nca-snaps (collect-snapshots nca-step snapshot-steps))
(println "Collecting HyCA snapshots...")
(def hybrid-snaps (collect-snapshots hybrid-step snapshot-steps))

(println "Rendering comparison grid...")
(render-comparison!
  [{:label "PCA (Push)" :snapshots pca-snaps}
   {:label "NCA (NEAT)" :snapshots nca-snaps}
   {:label "HyCA"       :snapshots hybrid-snaps}]
  "experiments/thue_morse/results")

(println "Done!")
