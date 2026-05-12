(ns shapes
  "Evolutionary pipeline for evolving CA rules to generate a target shape."
  (:require [push :as p]
            [cellular-automata :as ca]
            [helpers :as h]
            [grid-draw :as gd]
            [quil.core :as q]
            [quil.middleware :as m]))


;;; push instruction set up
(def push-and (p/make-instruction
               #(cond
                  (and %1 %2) true
                  :else       false)
               [:bool :bool] :bool))

(def push-or  (p/make-instruction
               #(cond
                  %1    true
                  %2    true
                  :else false)
               [:bool :bool] :bool))

(def push-not (p/make-instruction
               #(cond
                  %1    false
                  :else true)
               [:bool] :bool))

(def push-dup  (p/make-instruction (fn [x] [x x]) [:bool] [:bool :bool]))
(def push-swap (p/make-instruction (fn [x y] [y x]) [:bool :bool] [:bool :bool]))
(def push-pop  (p/make-instruction (fn [x] []) [:bool] []))

;translate instructions into a parser
(def instruction-set {'and  push-and
                      'or   push-or
                      'not  push-not
                      'dup  push-dup
                      'swap push-swap
                      'pop  push-pop})
;genetic maaterial
(def instruction-pool
  ['or 'or 'or 'or 'dup 'dup 'swap 'not 'and 'pop true false false])

(def parser (p/make-simple-parser instruction-set))

(defn make-push-rule
  [program]
  (fn [neighbor-values]
    (let [initial-state {:exec [program]
                         :bool (into [] neighbor-values)}
          final-state (p/execute-state parser initial-state)
          result (p/peek-stack final-state :bool)]
      (if (nil? result) false result))))

;Set up environment
(def grid-limits [10 10])

;initial cell location
(def seed-grid
  (h/map-keys (ca/cell-grid grid-limits) (fn [p] (= p [4 6]))))

;;Different target shapes I tested out
;(def target-triangle
 ; #{[2 9] [3 9] [4 9] [5 9] [6 9] [3 8] [4 8] [5 8] [4 7]})

;(def target
 ;#{[0 8] [1 8] [2 8] [3 8] [4 8] [5 8] [6 8] [7 8] [8 8]
  ;  [1 7] [2 7] [3 7] [4 7] [5 7] [6 7] [7 7]
   ; [2 6] [3 6] [4 6] [5 6] [6 6]
    ;[3 5] [4 5] [5 5]
    ;[4 4]})

;(def target ;now evolving an A
 ; #{[4 3]                         
  ;  [3 4] [5 4]                     
   ; [2 5] [3 5] [4 5] [5 5] [6 5]         
    ;[2 6]             [6 6]               
    ;[2 7]             [6 7]
    ;[2 8]             [6 8]})   

;square
;(def target 
 ; #{[3 5] [4 5] [5 5] [6 5]   
  ;  [3 6] [4 6] [5 6] [6 6]  
   ; [3 7] [4 7] [5 7] [6 7]   
    ;[3 8] [4 8] [5 8] [6 8]})  

;Diamond
(def target
  #{[4 3]  [5 3]               
    [3 4] [4 4] [5 4]        
    [2 5] [3 5] [4 5] [5 5] [6 5] 
    [3 6] [4 6] [5 6]        
    [4 7] [5 7]})               

;; Was largely unsuccesfull in evolving the square and diamond shapes however the triangle was able to be evolved with a few tweaks of the evolve parameters and starting cell

(def target-grid
  (h/map-keys (ca/cell-grid grid-limits) (fn [p] (contains? target p))))

(defn neighbors [coord]
  (ca/moore-neighbors 1 grid-limits coord))


(defn run-ca-steps
  [grid rule steps]
  (loop [current-grid grid
         current-step 0]
    (if (>= current-step steps)
      current-grid
      (let [; Find the coordinates of all currently alive cells
            live-coords (keep (fn [[coord is-alive?]] (when is-alive? coord)) current-grid)

            ; Get the neighbors of those live cells
            active-boundary (mapcat neighbors live-coords)

            ; Combine them into a set 
            cells-to-check (set (concat live-coords active-boundary))
            blank-grid (h/map-keys (ca/cell-grid grid-limits) (constantly false))

            next-grid (reduce (fn [new-grid coord]
                                (let [
                                      neighbor-coords (neighbors coord)
                                      neighbor-vals (map #(get current-grid % false) neighbor-coords)
                                      new-state (rule neighbor-vals)]
                                  ; Update the specific coordinate on the new canvas
                                  (assoc new-grid coord new-state)))
                              blank-grid
                              cells-to-check)]

        (recur next-grid (inc current-step))))))

;fitness function
;scores the grid after 5 steps 
;based on how well it matches the target triangle, 
;with heavy penalties for incorrect cells far from the target
;and failure to grow
(defn evaluate-fitness
  [program target-set seed-coord]
  (let [rule (make-push-rule program)

        actual-seed-grid (h/map-keys (ca/cell-grid grid-limits) (fn [p] (= p seed-coord)))

        final-grid (run-ca-steps actual-seed-grid rule 5)

        live-coords (keep (fn [[k v]] (when v k)) final-grid)
        live-count (count live-coords)
        local-target-grid (h/map-keys (ca/cell-grid grid-limits)
                                      (fn [p] (contains? target-set p)))]
    (reduce (fn [score [coord target-val]]
              (let [actual-val (get final-grid coord)]
                (cond
                  (and target-val actual-val) (+ score 100)
                  (and (not target-val) (not actual-val)) (+ score 1)
                  (and (not target-val) actual-val)
                  (let [[cx cy] coord
                        distances (map (fn [[tx ty]]
                                         (+ (Math/abs (- cx tx)) (Math/abs (- cy ty))))
                                       target-set)
                        min-dist (if (seq distances) (apply min distances) 0)
                        penalty (* 10 min-dist)]
                    (- score penalty))
                  :else score)))
            (cond
             (not (get final-grid seed-coord)) -2000
              (<= live-count 1) -1000
              :else 0)
            local-target-grid)))
;Evolution functions
(defn generate-random-program [length]
  (doall (repeatedly length #(rand-nth instruction-pool))))

;point mutation
(defn mutate [program mutation-rate]
  (doall (map #(if (< (rand) mutation-rate) (rand-nth instruction-pool) %) program)))

;single point crossover
(defn crossover [parent1 parent2]
  (let [split (rand-int (inc (min (count parent1) (count parent2))))]
    (doall (concat (take split parent1) (drop split parent2)))))


(def live-best-rule (atom (make-push-rule '(false))))

;Evolve the population of programs, while also updating the live visualization with the best rule of each generation
(defn evolve-live
  [pop-size generations prog-length mutation-rate current-target current-seed]
  (reset! live-best-rule (make-push-rule '(false)))
  (future
    (try
      (loop [population (doall (repeatedly pop-size #(generate-random-program prog-length)))
             current-gen 0]
        (let [scored-pop (pmap (fn [p]
                                 {:program p
                                  :score (evaluate-fitness p current-target current-seed)})
                               population)
              sorted-pop (sort-by (comp - :score) scored-pop)
              best (first sorted-pop)]

          (reset! live-best-rule (make-push-rule (:program best)))
          (spit "evolution-progress.txt"
                (str "GEN: " current-gen " | BEST SCORE: " (:score best) "\n")
                :append true)

          (if (< current-gen generations)
            (let [parents (map :program (take (/ pop-size 2) sorted-pop))
                  elites (map :program (take 2 sorted-pop))
                  offspring (doall (repeatedly (- pop-size (count elites))
                                               #(mutate (crossover (rand-nth parents) (rand-nth parents))
                                                        mutation-rate)))]
              (recur (concat elites offspring) (inc current-gen)))
            (println "Evolution Complete!"))))
      (catch Exception e
        (.printStackTrace e)))))

;Quil visualization setup
(defn setup-live []
  {:grid seed-grid})

(defn update-live [state]
  (let [rule @live-best-rule
        final-shape (run-ca-steps seed-grid rule 5)]
    {:grid final-shape}))

(defn draw-live [state] (gd/grid-draw grid-limits (:grid state)))

(q/defsketch live-evolution-window
  :title "Live CA Evolution"
  :size [500 500]
  :setup setup-live
  :update update-live
  :draw draw-live
  :middleware [m/fun-mode]
  :settings (fn [] (q/frame-rate 5)))


(evolve-live 500 100 25 0.05 target [4 9])
