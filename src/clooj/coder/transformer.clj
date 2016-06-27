; What categories do these belong in?
(ns coder.transformer )

        

; Note: docstrings go byby with macroexpand-all.


  
; FIXED argument # function recursion expansion:
;Should be able to work with multible arity, even where one arity calls another.
  ;However, it will NOT work with any "real" recursion.
; used-up = used-up airties, a map with ints as keys. No negative numbers.
(defn _fn-expand [code fn-name fn-options used-up symbol-check]
  ; Code: what we operate on. fn-name: the name of the function. fn-options: map from arity => code in function.
  (let [; expand children recursivly:
        selfcall (and (seq? code) (> (count code) 0) (= (str (first code)) (str fn-name)))
        airity (if selfcall (dec (count code)) nil)
        used-up (if selfcall (assoc used-up airity true) used-up)
        exp (fn [c] (mapv #(_fn-expand %1 fn-name fn-options used-up (> %2 0)) c (range)))
        code (if (seq? code)
                (seq (exp code)); recursive.
                (if (vector? code)
                    (apply vector (exp code)); another recursive.
                    (if (list? code)
                        (apply list (exp code)); more recursive.
                        (if (map? code)
                            (zipmap (exp (keys code)) (exp (vals code))) ; even more recursive.
                            code))))]
    (if selfcall ; a function call, which is the outer level or a recursive call.
      (let [airity (dec (count code))
            opt (get fn-options airity) ; the one cooresponding to the ariety.
            multi (nil? opt) ; special multi-unpack airity challange
            opt (if (nil? opt) (get fn-options -1) opt) ; -1 for multi-arity, if it exists. 

            ; error checks:
            check-not-used (if (get used-up airity) (throw (Exception. "Function has same-arity recursion, can't expand.")) true)
            check-valid-airity (if (nil? opt) (throw (Exception. "Function calls itself with an invalid airity")) true)
            
            ; The part inside the let statement that we construct for inlining:
            args (first opt) ; arg symbols, has & second to last for functions.
            lets (if multi
                    (into [] (concat (interleave (butlast (butlast args)) (rest code))
                                     [concat [(last args)] (subvec code (- (count args) 2))])) 
                    (into [] (interleave args (rest code))))
            ]
        ; single-unpack: (foo A B) and (fn [a b] ...) =>(let [a A b B] ...)
        ; multi-unpack: (foo A B C D E F) and (fn [a b c & d] ...) => (let [a A b B c C d [D E F]] ...)
        (cons `let (cons lets (rest opt))))
      ; non-self-call
      (if (and symbol-check (symbol? code) (= (str code) (str fn-name))) 
          (throw (Exception. "Use of symbol refering to function inside of said function."))
          code)))) ; nothing special.
(defn fn-def-expand [code]
  "Expands a function definition (non-anomonous only) to inline it's own self-calls, etc."
  (let [fname (fn-name code)
        opts (fn-code-arity code) ; one for each airty.
        used-up {}] ; nothing starts used-up.
    ; acc is the accumilated code.
    (_fn-expand code fname opts used-up false))) ; Note: the last arg does not matter.

(defn fn-inline [fncode call]
  ; TODO: recursive.
 "Inlines a function call (different fn-def-expand which expands a function definition).
  Uses gensym to prevent variable name collisions.
  Example: ( `(fn [x] (+ x 1)) (x 4)) => `(+ 4 1)"
  (let [fncode (fn-def-expand fncode)
        codei (get (fn-code-arity fncode) (nth call 0)) ; this arity call.
        ]
    TODO))





; Higher level tools that work with the code that are abstracted away from the details.
