(ns gp
  (:require [push :as p]))

(defn execute-state-limited [parser state max-steps]
  (loop [state state
         steps 0]
    (if (or (empty? (get state :exec))
            (>= steps max-steps))
      state
      (let [[program state'] (p/pull-stack state :exec)]
        (if (fn? program)
          (recur (program state') (inc steps))
          (recur (update state' :exec
                         #(apply conj % (reverse (parser program))))
                 steps))))))

(defn random-program [terminals max-length]
  (let [length (inc (rand-int max-length))]
    (vec (repeatedly length #(rand-nth terminals)))))

(defn crossover [prog-a prog-b max-length]
  (let [cut-a (rand-int (count prog-a))
        cut-b (rand-int (count prog-b))
        child (vec (concat (subvec prog-a 0 cut-a)
                           (subvec prog-b cut-b)))]
    (if (> (count child) max-length)
      (subvec child 0 max-length)
      child)))

(defn mutate-replace [terminals program]
  (let [idx (rand-int (count program))]
    (assoc program idx (rand-nth terminals))))

(defn mutate-insert [terminals program max-length]
  (if (>= (count program) max-length)
    (mutate-replace terminals program)
    (let [idx (rand-int (inc (count program)))]
      (vec (concat (subvec program 0 idx)
                   [(rand-nth terminals)]
                   (subvec program idx))))))

(defn mutate-delete [terminals program]
  (if (<= (count program) 1)
    (mutate-replace terminals program)
    (let [idx (rand-int (count program))]
      (vec (concat (subvec program 0 idx)
                   (subvec program (inc idx)))))))

(defn mutate [terminals program max-length]
  (if (empty? program)
    (random-program terminals 5)
    (case (rand-int 3)
      0 (mutate-replace terminals program)
      1 (mutate-insert terminals program max-length)
      2 (mutate-delete terminals program))))

(defn tournament-select [population tournament-size]
  (let [contestants (repeatedly tournament-size #(rand-nth population))]
    (apply max-key :fitness contestants)))

(defn lexicase-select [population]
  (let [num-cases (count (:errors (first population)))]
    (loop [candidates population
           cases (shuffle (range num-cases))]
      (if (or (empty? cases) (<= (count candidates) 1))
        (rand-nth (vec candidates))
        (let [case-idx (first cases)
              best-err (apply min (map #(nth (:errors %) case-idx) candidates))
              survivors (filter #(= (nth (:errors %) case-idx) best-err) candidates)]
          (recur survivors (rest cases)))))))

(defn evolve [{:keys [evaluate-fn select-fn terminals max-length pop-size
                       generations mutation-rate]}]
  (let [terminals (vec terminals)
        select    (or select-fn lexicase-select)]
    (loop [population (mapv (fn [_] (evaluate-fn (random-program terminals max-length)))
                            (range pop-size))
           gen 0
           best-ever nil]
      (let [gen-best (apply max-key :fitness population)
            best     (if (or (nil? best-ever)
                             (> (:fitness gen-best) (:fitness best-ever)))
                       gen-best
                       best-ever)]
        (println (pr-str {:generation gen
                         :best-fitness (:fitness best)
                         :program (:program best)}))
        (if (>= gen generations)
          best
          (let [new-pop
                (into [best]
                      (map (fn [_]
                             (if (< (rand) mutation-rate)
                               (evaluate-fn (mutate terminals
                                                    (:program (select population))
                                                    max-length))
                               (evaluate-fn (crossover
                                             (:program (select population))
                                             (:program (select population))
                                             max-length))))
                           (range (dec pop-size))))]
            (recur new-pop (inc gen) best)))))))
