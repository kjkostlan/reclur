; Code refactoring 101.
; These functions map from strings to strings.

(ns clooj.coder.refactor
  (require [clooj.coder.grammer :as grammer]
           [clooj.coder.repl :as repl]
           [clojure.walk :as walk]
           [clooj.coder.blitcode :as blitcode]
           [clooj.collections :as collections]
           [clooj.coder.debugger :as debugger]))

(def debug (atom nil))

; Make sure that the string -> code -> string exactly reconstructs the string.
; This piece of code in grammer isn't rock-solid yet.
(def err-check? true)

;blitcode/blitify [f-code nms]
;grammer/qualify-in [code nms]

; Convience functions for text -> code -> text conversion.
(defn str2code [s] (grammer/reads-string-blit s))
(defn code2str [c] 
  (let [s (grammer/blit-code-to-str c)] s))
(defn refactor [s mutate-code] 
  "Code that refactors the string belonging to s using the mutate-code function.
   Mutate-code is code for a fn that takes in ONE argument, the code
   and spits out the modified code. mutate-code is built to work with
   the AST but gets converted to work with blitted code via blittify."
  (let [a (gensym 'blitted?)
        ix-a (if (vector? (second mutate-code)) 1 2)
        f-arg (nth mutate-code ix-a) ; the []
        ; blittify compatible:
        f-code (collections/lassoc mutate-code ix-a (conj f-arg a))
        f-codeb (blitcode/blit-translate f-code *ns*)]
    `(let [c# (str2code ~s)
           !# (if (and err-check? (not= ~s (code2str c#))) ; no refactoring done yet. 
               (throw (Exception. "error in clooj.coder.grammer: conversion of string to code: it doesn't convert back into the same string.")))
           c1# (~f-codeb [c#] [true])] ; call the function.   
       (code2str c1#))))
(defmacro mrefactor [s mutate-code]
  "Convience macro, avoid having to write a macro and quote the code.
   But writing the macro is better for debugging..."
  (refactor s mutate-code))

(defmacro add-statement [txt unblitted-statement]
  "Adds a statement, which is not blitted, to the end of the txt file."
  (refactor txt
    `(fn [c#] (conj c# ~unblitted-statement))))

(defmacro ensure-require [txt ns-sym & as]
  "Ensures that the ns is included.
   ns-sym is the (fully-qualified) symbol form.
   as (optional) is the shorthand, if so it will replace.
   Ignores use, etc."
  (refactor txt 
    '(fn [code]
      (let [ns-code (first code)
            n (count ns-code)
            ind-require (first (collections/which #(or (= % 'require) (= % 'clojure.core/require)) ns-code))
            r-statement (if (first as) [ns-sym :as (first as)] [ns-sym])
            re-code (if ind-require (nth ns-code (first ind-require)) '(require)) ; all require statements.
            remove-the-ns-sym (keep #(or (not (vector? %)) (not= (first %) ns-sym)) re-code)
            re-code1 (conj (rest remove-the-ns-sym) r-statement 'require)]
        (collections/lassoc code 0 (if ind-require (collections/lassoc ns-code ind-require re-code1)
                                 (apply list (concat ns-code [re-code1]))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;; Testing this tricky code ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Test functions and cases:
(defn pr0 [x] (repl/previty x))
(defn prp [x] (repl/previty (walk/macroexpand-all x)))
(defn prp0 [x] (println (repl/indent (walk/macroexpand-all x))))

(def s-test0 "(ns foo.bar) (def x 1) (def y 2) (def z 3)")

; Scratchbook below:
(repl/clc)
(def s s-test0)

;(println (str "Conversion test: \n" s "\n" (code2str (str2code s)))) ; using str prevents extra spaces.


;(repl/pr-err (prp '(add-statement s '(+ 1 2))))
(def ns-this *ns*)
(defn test101 [] (binding [*ns* ns-this] (repl/pr-err (prp '(add-statement s '(+ 1 2))))))

(defn test-code-that-is-generated []
(let* 
  [c (str2code s) ! 
    (if 
      (let* [And err-check?] 
        (if And (not= s (code2str c)) And)) (throw 
        (new java.lang.Exception 
          "error in clooj.coder.grammer: conversion of string to code: it doesn't convert back into the same string."))) c1 
    ((fn* 
        ([c11 blitted?] 
          (let* 
            [c11 
              (mapv 
                (fn* [p1] 
                  (if (nth blitted? p1) 
                    (clooj.coder.blitcode/_salt-blit-code (nth c11 p1)) (nth c11 p1))) (range (count blitted?)))]
(pr0 (first c11)) ; look at the blitted code.
              (clooj.coder.blitcode/_unsalt-blit-code 
                ((get clooj.coder.blitcode/fns-manual-overrides 
                    (quote clojure.core/conj)) 
                  c11 (quote (+ 1 2))))))) 
      [c] [true])] (code2str c1))

)
;(repl/pr-err (test-code-that-is-generated))


;(prp '(ensure-require simple-code 'added.ns 'addns))