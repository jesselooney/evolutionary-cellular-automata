(ns push)

;;;; A state is a map from keys to stacks of values, represented as sequences.
;;;; An instruction is a function from states to states.
;;; Examples.
;; A simple state.
(def example-state {:int '(0 1 2)
                     :str '("xyz" "abc")})
;; An instruction that pops off the :int stack.
(defn example-instruction [state] (update state :int pop))
(example-instruction example-state)

;;;; Helper functions for manipulating state.
(defn peek-stack
  "Return the value on top of stack `stack` in state `state`."
  [state stack]
  (peek (get state stack)))

(defn pop-stack
  "Modify state `state` by popping off the top of stack `stack`."
  [state stack]
  (update state stack pop))

(defn pull-stack
  "Modify `state` by popping from `stack` and returning the popped value."
  [state stack]
  [(peek-stack state stack) (pop-stack state stack)])

(defn push-stack
  "Modify state `state` by pushing `value` onto stack `stack`."
  [state stack value]
  (update state stack #(conj % value)))

;;; Examples.
(peek-stack {:int [0 1]} :int)
(pop-stack {:int [0 1]} :int)
(pull-stack {:int [0 1]} :int)
(push-stack {:int [0 1]} :int 2)

(defn pull-stacks
  "Pull the top value from each stack in `stacks`.
   
   Returns a sequence of values resulting from sequentially pulling values off
   of stacks of `state` in the order given by `stacks`, as well as the final
   state of `state` after performing the necessary pops."
  [state stacks]
  (let [reducer (fn [[values state] stack]
                  [(conj values (peek-stack state stack))
                   (pop-stack state stack)])]
    (reduce reducer [[] state] stacks)))

;;; Examples.
(pull-stacks {:int [0 1 2] :str ["a" "b"]} [:int :str :int])

;;;; Helpers for defining Push interpreters.
(defn make-instruction
  "Make a Push instruction based on a function `f` with the given signature.
   
   The instruction modifies the state by pulling from the stacks `arg-stacks`,
   calling `f` on those arguments, and then pushing the result to the stack
   `return-stack`.
   
   If any error occurs while attempting the steps above (e.g. insufficient
   arguments, error in `f` itself), the error will be caught and pushed to the
   `:err` stack, if `state` contains one."
  [f arg-stacks return-stack]
  (fn [state]
    (try (let [[args state'] (pull-stacks state arg-stacks)]
           (push-stack state' return-stack (apply f args)))
         (catch Exception e
           (if (contains? state :err)
             (push-stack state :err e)
             state)))))

;;; Examples.
((make-instruction + [:int :int] :int) {:int [1 2]})
((make-instruction > [:int :int] :bool) {:int [1 2] :bool []})
;; Errors ignored (insufficient arguments).
((make-instruction + [:int :int] :int) {:int []})
;; Errors ignored, but reported.
((make-instruction + [:int :int] :int) {:int [] :err []})

(defn make-constant-instruction
  "Makes a Push instruction out of a constant.
   
   The instruction modifies the state by pushing `value` onto `return-stack`.
   
   This function is a thin wrapper around [make-instruction]."
  [value return-stack]
  (make-instruction (constantly value) [] return-stack))

;;; Examples.
((make-constant-instruction 0 :int) {:int []})
((make-constant-instruction true :bool) {:bool []})

;;;; Executing Push states.

(defn execute-state
  "Execute a Push state by pulling programs from the `:exec` stack.
   
   Recursively pulls from the `:exec` stack until empty and uses the resulting
   program to compute the next state. If the program is an instruction, the
   next state is the result of applying that instruction to the current state.
   Otherwise, if the program is a sequence, the sequence is parsed into an
   instruction sequence using `parser`, then the instructions are pushed in
   reverse order onto the `:exec` stack."
  [parser state]
  (loop [state state]
    (if (empty? (get state :exec))
      state
      (let [[program state'] (pull-stack state :exec)]
        (recur (if (fn? program)
          (program state')
          (update state' :exec 
                  #(apply conj % (reverse (parser program))))))))))

;;;; To make Push programs easier to read and write, we can write parsers that
;;;; transform sequences of values (like constants and symbols) into sequences
;;;; of Push instructions. A simple way is to provide definitions for symbols
;;;; via a map, and then automatically translate constants into constant
;;;; instructions.

(defn make-simple-parser
  [symbol-parser]
  (fn [program]
    (let [term-parser
          (fn [term]
            (cond (symbol? term) (symbol-parser term)
                  (boolean? term) (make-constant-instruction term :bool)
                  (integer? term) (make-constant-instruction term :int)
                  (string? term) (make-constant-instruction term :str)))]
      (map term-parser program))))

;;; Examples.
(let [parser (make-simple-parser {'- (make-instruction - [:int :int] :int)
                                 '+ (make-instruction + [:int :int] :int)})]
  (execute-state parser {:exec [[1 2 3 '- '+]] :int []}))

;; Tests whether (x, y, z) is a Pythagorean triple.
(let [parser (make-simple-parser
              {'* (make-instruction * [:int :int] :int)
               '+ (make-instruction + [:int :int] :int)
               '= (make-instruction = [:int :int] :bool)
               'x (make-constant-instruction 3 :int)
               'y (make-constant-instruction 4 :int)
               'z (make-constant-instruction 5 :int)})]
  (execute-state parser
                 {:exec [['x 'x '* 'y 'y '* '+ 'z 'z '* '=]]
                  :int []
                  :bool []}))
