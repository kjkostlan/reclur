(ns clooj.coder.debugger
  (:require [clooj.coder.grammer :as grammer] [clooj.coder.strmap :as strmap] [clojure.walk :as walk]))
;; (require '[clooj.coder.debugger :as debugger])
;; Functional debugging tool in the spirit of elm debugger.

; Structure of dbstate:
;   :step = how many times we called the _add-frame!! function in this case.
;   :path = our computation path.
;        [1 2 3 ...] = the path to a bit of code that is doing something.
;        []
(def dbstate (atom {})) ; :step and :path

(defn reset-state!! []
 "Resets the debug state."
 (reset! dbstate {:step 0 :path {}}))

(defn _add-frame!! [stack x] ; adds x to the current stack. The stack is our path and is always a numerical vector starting with 0.
  (let [s @dbstate x {:step (:step s) :val (grammer/expanded2qualified x)}
        old-xs (let [q (get (:path s) stack)] (if (nil? q) [] q))] ; make an empty vector if there is nothing there.
    ;(println s stack old-xs x)
    (reset! dbstate {:step (inc (:step s)) :path (assoc (:path s) stack (conj old-xs x))}))) ; Add our result to the stack frame.

;(defn get-history [code-str ix]
;  "Gets the history of whatever is at position ix of code-str, after we already computed things."
;  ((strmap/read-string+ code-str) TODO
;  ))

(defn prt [x] (println x) x) ; print and returns the object unmodified.

; TODO: hotkey for watch.
; TODO: list when and where the variable is.
; TODO: click on a value and it finds out where that is.
; TODO: simplify (let [_C 1] (addframe [...] _C) _C)    to   (do (addframe [] 1) 1)   for symbols.
; TODO: let statements with multi vars of same name cranky (is this a big problem)?

(defn _tail-recur? [c]
  "Does c end in an embedded recur statement? This prohibits getting a value for c."
  (cond
    (= c 'recur) true
    (not (coll? c)) false
    (= (first c) 'recur) true
    ; if statements are special this way: either or both branches are allowed to recur:
    (= (first c) 'if) (or (_tail-recur? (nth c 2)) (if (> (count c) 3) (_tail-recur? (nth c 3)) false))
    (coll? (last c)) (_tail-recur? (last c))
    :else false))
      
; The main debugging tool available:
; stack: a vector of symbols and/or integers showing where we are in the code. 
   ; The stack is mostly integer-based (maps are unwrapped), but symbol declarations shortcircuit
   ; This rule because of special rules of let/let* and loop/loop*. You can use fancy forms, they get macroed down into simple forms so we don't need to worry (RoA).
; code: the code inside said stack. equal to the original code when the stack is empty.
; At each level we store an array of maps from key to value.
; This function maps code => code objects.
(defn _watch [code stack]
  (let [root-stack #(list 'let ['_C %] (list '_add-frame!! stack '_C) '_C) ; adds the root-level variable notice. We typically do this at the end.
        piece-stack (fn [code] (grammer/cmap :flatten #(_watch %1 (conj stack %2)) code (range))) ; stacks all the pieces of the code.
        ; Handles lets and loops, both of which assign special bindings to the code:
        let-loop (fn [code] (let [bindings (second code) n (count bindings) evens (range 0 n 2) odds (range 1 n 2) ; Run recursivly on every odd-numbered entry in the [a 1 b 2 c 3 ...] part:
                                  vars (mapv #(nth bindings %2) bindings evens) ; Symbols a b c etc.
                                  add-sym #(conj stack (list 'quote %)); add a symbol to the stack.
                                  f (fn [c ix] (assoc c (nth odds ix) (_watch (get c (nth odds ix)) (add-sym (get vars ix)))))
                                  bindings1 (reduce f bindings (range (count vars)))] ; Apply recursivly on 1 2 3 with the proper bindings.
                              (apply list (first code) bindings1 (mapv #(_watch %1 (conj stack (+ %2 2))) (rest (rest code)) (range 2)))))
        let-loop? (and (coll? code) (contains? #{'loop 'let 'loop* 'let*} (first code)))
        has-tail-recur? (_tail-recur? code)] ; this bit of code will recur onto some loop.
    (cond let-loop? (root-stack (let-loop code)); lets and loops which make variables. No piece stacking, the pieces do special stuff.
          (and (coll? code) (not let-loop?) has-tail-recur?) (piece-stack code) ; no root-stacking for a statemt that ends in a tail recur, unless it is the entire loop body itself.
          (coll? code) (root-stack (piece-stack code)) ; normal branches of code.
          (and (symbol? code) (grammer/special-form? code)) code ; things like if and loop that can't be converted.
          :else (root-stack code))))

(defmacro watch!! [code]
  "Puts a debugging watch on a piece of code. Run that code to store the state as it changes over time."
  (list `do `(reset-state!!) (_watch (walk/macroexpand-all code) [0])))

(defn _if [c a b] (if c a b)) 

(defn lp [x] 
  (loop [acc 0 ix 0] 
    (if (= ix 1) (+ 1 2) (if (< 1 0) 1 (recur (+ acc x) (inc ix))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn tast [x] (watch!! (* x x)))

(defn tast1 [x] (watch!! (let [a 1] (+ a x))))

(defn tast1_5 [x] (watch!! (if (> x 1) x 0)))

; (do (clojure.core/reset! clooj.coder.debugger/dbstate {}) (let [_C []] (_add-frame!! [0] _C) _C))

(defn tast2_0 [x] (loop [acc 0 ix 0] 
                           (if (= ix x) acc
                               (recur (+ acc x) (inc ix)))))

; Noobish deluxe:
(defn tast2 [x] (watch!! (loop [acc 0 ix 0] 
                           (if (= ix x) acc
                               (recur (+ acc x) (inc ix))))))

;(defn test-debug-string [] 
;  ; Tests reading a string, adding a break-point, and tracking a progress.
;  (let [s "(watch!! (loop [acc 0 ix 0] 
;                           (if (= ix 10) acc
;                               (recur (+ acc 10) (inc ix)))))" ; our example.
;        _ (eval s) ; side-effects created in the atom.
;        h (get-history s TODO)
;        ] 
;    c))
