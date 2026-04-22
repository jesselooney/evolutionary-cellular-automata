(ns living-evolution
  "Artificial life on a cellular automata grid.

   Three species compete for territory. Each living cell carries a Push program
   and a finite energy reserve that depletes each tick. The program's sole
   decision is WHEN to reproduce — spending energy to place an offspring in an
   adjacent empty cell. Cells that never reproduce go extinct; cells that
   reproduce too eagerly burn out before spreading far. Programs that learn to
   time reproduction well dominate the grid.

   Offspring programs are bred from same-species neighbors via crossover and
   mutation. Cell brightness reflects remaining energy — bright cells are
   young, dim cells are near death."
  (:require [cellular-automata :as ca]
            [push :as p]
            [gp :as gp]
            [quil.core :as q]
            [quil.middleware :as m]))

(declare make-initial-state)

;;;; ---- Configuration -------------------------------------------------------

(def grid-limits [100 100])
(def max-push-steps 20)
(def max-program-length 10)
(def max-energy 30)
(def reproduction-cost 10)
(def seed-radius 3)
(def spontaneous-spawn-chance 0.010)

;; Three species: orange, green, blue
(def species-hues [30 115 200])

;;;; ---- Push interpreter setup ----------------------------------------------

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
(def terminals (vec (concat (keys instruction-set) [0 1 2 3 4 5])))
(def neighbors (partial ca/moore-neighbors 1 grid-limits))

;;;; ---- Cell logic ----------------------------------------------------------
;; Cell state: nil (dead) or {:program [...] :species int :energy int}

(defn run-cell-program
  "Execute a cell's Push program to decide whether to reproduce.
   Input on :int stack: energy (top), then self + 8 neighbors as 1/0.
   Output: boolean from :bool stack — true means reproduce this tick."
  [program energy neighbor-values]
  (let [as-ints     (map #(if (some? %) 1 0) neighbor-values)
        init-state  {:exec (list program)
                     :int  (apply list (cons energy as-ints))
                     :bool ()}
        final-state (gp/execute-state-limited parser init-state max-push-steps)]
    (boolean (p/peek-stack final-state :bool))))

(defn reproduce
  "Create a child program from one or more parent programs."
  [parent-programs]
  (if (< (count parent-programs) 2)
    (gp/mutate terminals (first parent-programs) max-program-length)
    (let [p1 (rand-nth parent-programs)
          p2 (rand-nth parent-programs)]
      (if (< (rand) 0.4)
        (gp/mutate terminals p1 max-program-length)
        (gp/crossover p1 p2 max-program-length)))))

;;;; ---- World state ---------------------------------------------------------
;; {:cells {[x y] -> nil | {:program [...] :species int :energy int}}, :tick int}

(defn make-initial-state
  "Seed 3 species clusters in a triangle around center."
  []
  (let [grid    (ca/cell-grid grid-limits)
        [cx cy] (mapv #(quot % 2) grid-limits)
        offsets [[-6 -4] [6 -4] [0 6]]
        centers (mapv (fn [[dx dy]] [(+ cx dx) (+ cy dy)]) offsets)]
    {:cells (reduce
              (fn [g [species-id [ccx ccy]]]
                (let [positions (for [dx (range (- seed-radius) (inc seed-radius))
                                      dy (range (- seed-radius) (inc seed-radius))
                                      :when (<= (+ (* dx dx) (* dy dy))
                                                (* seed-radius seed-radius))]
                                  [(+ ccx dx) (+ ccy dy)])]
                  (reduce (fn [g' pos]
                            (if (contains? g' pos)
                              (assoc g' pos
                                     {:program (gp/random-program terminals
                                                                  max-program-length)
                                      :species species-id
                                      :energy  max-energy})
                              g'))
                          g
                          positions)))
              grid
              (map-indexed vector centers))
     :tick 0}))

(defn update-state
  "Advance the world one tick.

   Pass 1 — process every living cell:
     Energy ticks down by 1.  Zero energy → death.
     The Push program decides whether to reproduce this tick.
     Reproducing costs `reproduction-cost` energy and queues an offspring
     for a random adjacent empty cell.

   Pass 2 — place offspring:
     The birth list is shuffled for fairness; offspring only land in cells
     that are still empty after pass 1."
  [{:keys [cells tick]}]
  (let [;; Pass 1: age cells, run programs, collect birth requests
        [next-cells births]
        (reduce-kv
          (fn [[acc pending] cell-key cell-val]
            (if (nil? cell-val)
              [acc pending]
              (let [energy' (dec (:energy cell-val))]
                (if (<= energy' 0)
                  ;; Old age — die
                  [(assoc acc cell-key nil) pending]
                  ;; Still alive — ask program about reproduction
                  (let [nkeys      (neighbors cell-key)
                        nvals      (map cells nkeys)
                        reproduce? (run-cell-program (:program cell-val)
                                                     energy' nvals)
                        can?       (and reproduce? (>= energy' reproduction-cost))
                        empty-nbs  (when can?
                                     (filterv #(nil? (cells %)) (rest nkeys)))
                        will?      (and can? (seq empty-nbs))
                        target     (when will? (rand-nth empty-nbs))
                        energy''   (if will?
                                     (- energy' reproduction-cost)
                                     energy')
                        ;; Gather same-species neighbors for crossover
                        same-sp    (when will?
                                     (filter #(and (some? %)
                                                   (= (:species %)
                                                      (:species cell-val)))
                                             (map cells (rest nkeys))))]
                    [(assoc acc cell-key (assoc cell-val :energy energy''))
                     (if will?
                       (conj pending {:pos     target
                                      :species (:species cell-val)
                                      :parents (mapv :program
                                                     (cons cell-val same-sp))})
                       pending)])))))
          [cells []]
          cells)

        ;; Pass 2: place offspring into empty cells (shuffled for fairness)
        final-cells
        (reduce (fn [acc {:keys [pos species parents]}]
                  (if (nil? (get acc pos))
                    (assoc acc pos {:program (reproduce parents)
                                    :species species
                                    :energy  max-energy})
                    acc))
                next-cells
                (shuffle births))

        ;; Pass 3: spontaneous spawn — small chance a cluster appears
        final-cells'
        (if (< (rand) spontaneous-spawn-chance)
          (let [all-keys   (keys final-cells)
                empty-ks   (filterv #(nil? (get final-cells %)) all-keys)]
            (if (seq empty-ks)
              (let [center (rand-nth empty-ks)
                    [cx cy] center
                    sp     (rand-int (count species-hues))
                    positions (for [dx (range (- seed-radius) (inc seed-radius))
                                    dy (range (- seed-radius) (inc seed-radius))
                                    :when (<= (+ (* dx dx) (* dy dy))
                                              (* seed-radius seed-radius))]
                                [(+ cx dx) (+ cy dy)])]
                (reduce (fn [g pos]
                          (if (and (contains? g pos) (nil? (get g pos)))
                            (assoc g pos
                                   {:program (gp/random-program terminals
                                                                max-program-length)
                                    :species sp
                                    :energy  max-energy})
                            g))
                        final-cells
                        positions))
              final-cells))
          final-cells)

        alive (count (filter (fn [[_ v]] (some? v)) final-cells'))]
    (if (zero? alive)
      (do (println (str "Extinction at tick " tick " — reseeding"))
          (make-initial-state))
      {:cells final-cells' :tick (inc tick)})))

;;;; ---- Quil visualization --------------------------------------------------

(defn setup []
  (q/frame-rate 600)
  (q/color-mode :hsb 256)
  (q/no-stroke)
  (make-initial-state))

(defn draw [{:keys [cells tick]}]
  ;; Fade trail (disabled — uncomment to enable ghostly afterimages):
  ;; (q/fill 0 0 0 18)
  ;; (q/rect 0 0 (q/width) (q/height))
  (q/background 0 0 10)
  (let [[i-max j-max] grid-limits
        cell-w  (/ (q/width) i-max)
        cell-h  (/ (q/height) j-max)
        living  (filter (fn [[_ v]] (some? v)) cells)
        alive   (count living)
        counts  (frequencies (map (comp :species val) living))]
    ;; Draw living cells — glow halo + bright core
    (q/no-stroke)
    (doseq [[[i j] cell] cells]
      (when (some? cell)
        (let [cx         (+ (* i cell-w) (/ cell-w 2))
              cy         (+ (* j cell-h) (/ cell-h 2))
              hue        (nth species-hues (:species cell))
              energy-pct (/ (:energy cell) max-energy)
              brightness (+ 80 (int (* 175 energy-pct)))]
          ;; Outer glow — larger, translucent
          (q/fill hue 180 brightness 60)
          (q/ellipse cx cy (* cell-w 2.2) (* cell-h 2.2))
          ;; Core — bright and solid
          (q/fill hue 220 brightness)
          (q/ellipse cx cy (* cell-w 0.7) (* cell-h 0.7)))))
    ;; Stats overlay
    (q/fill 0 0 0)
    (q/rect 0 0 350 28)
    (q/fill 0 0 255)
    (q/text-size 14)
    (q/text (str "Tick: " tick "   Living: " alive
                 "   Species: " (pr-str counts))
            10 20)))

(defn -main []
  (q/defsketch living-evolution
    :title "Living Evolution"
    :size [800 800]
    :setup setup
    :update update-state
    :draw draw
    :middleware [m/fun-mode]))
