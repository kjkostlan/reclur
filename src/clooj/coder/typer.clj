; A roll-our-own type system. I wonder how this compares to core.typed?

(ns coder.typer)

; TODO: having unknowns in functions is used for recursive hinting.
   ; when there is a branch and one value is unknown we assume that thats the recursive one.
   ; and assign the known type to the non-recursive one.
   ; Thus the unknown is replaced by a known function.
   ; If you call an external library AND return a different type than.

;TODO: core/meta is a valuable function that extracts metadata.
; (def x `(defn x [^double y ^double z] (* y z)))
; (meta (first (nth x 2)))   ; {:tag clojure.core/double}
; meta extracts the invisible!

; TODO: . notation means java and we can auto-assume side effect.
  ; won't catch far-java side effects. But you shouldn't have those!
  ; Or we can have a java cesspool.

;; NOTE: all functions work on fully-qualified function names unless otherwise specified.
; This avoids namespace collisions.

; NOTE: it IS possible to break the inference with a i.e. loop-recur by changing the type for the recur part. This is bad news.
  ; TODO: check for recur breaking this and recursive functions as well.

; clojure.walk/macroexpand-all is amazing because it can expand macros even though we don't require, etc the namespaces.

(defn get-meta [code key]
  "Convience function"
  (get (meta code) key))
(defn set-meta [code key val]
  "Convience function"
  (with-meta code (assoc (meta code) key val)))
  

(defn recur-type-assert [looptypes local-types]
  "The types in recur must match those in loop."
  (if (not (= (count looptypes) (count local-types))) (throw (Exception. "Inconcistant recur arg count")))
  (doall (mapv #(if (not (= %1 %2)) (throw "Type disagreement for recur")) looptypes local-types)))

(defn non-primative-assert [ty]
  "Throws an error if the type is primative. Use this to make sure you dont make null a primative value."
(TODO))

(defn loop-output-extract [code]
 "Code is a (loop [...] ....) construct. At some point, we must return something from said code."
 (TODO)
)

(defn fn-def-type [code var-map local-types]
  "Gets the type of the function definition (a special type [see builtin] that stores the code for later use).
  Should work on clojures, thus the use of local-types.
  Won't work on most recursive stuff TODO: change this."
  TODO)

(defn var-fn-type [code var-map local-types]
 "Solves cases like:
  (def y (let [x #(inc %)] (x 2))); y is what type?
  We have a special function type that stores the code.
  It may have nested branches! We may have to branch if it also branches." 
  TODO)

; Calculates the type of the code.
  ; The type goes into the metadata of each code as :type.
  ; (This complements the :tag metadata).
; TODO: smarter if statement type branching.
(defn type-code [code var-map local-types looptypes]
  "code: we work on this code. var-map: the global-map of all vars (string => code).
   local-types: type of local variables (see builtin.clj for how types are represented).
   looptypes: assert that recur does not change the type of loop."
  (if (sequential? code) ; Avoids sets and map literals.
    ; SPECIAL if statement case.
    (let [code0 (first code)
          letty (or (= code0 `loop*) (= code0 `let*)) ; do we have a binding pattern?
          is-loop (= code0 `loop*)
          ; Let statements or loops create/rebind local variables.
          ; Also, the code inside each let statement needs to be updated.
          ; Loop types is nil for this step (I think).
          let-analysis (if letty (reduce
            #(let [varname (str (:even %2)) ; the name in the let-statement.
                   loc-types (:loc-ty %1)
                   let-code (:let-code %1)
                   let-ty (:let-ty %1)
                   code+ (inline-type (:odd %2) var-map loc-types nil)
                   out {:loc-ty (assoc loc-types varname (get-meta code+ :type)) ; new or replacement type.
                        :let-code (conj let-code (:even %2) code+)
                        :let-ty (conj let-ty (get-meta code+ :type)))}
                   ] out)
            {:loc-ty local-types :let-code [] :let-ty []} (even-odd (second code)))
          code (if letty (assoc code 1 (:let-code let-analysis)) code) ; the code int he let statements has been replaced.
          local-types (if letty (:loc-types let-analysis) code)
          looptypes (if letty (:let-ty let-analysis) looptypes)
          is-recur (= (first code) `recur)
          ; make sure recurs don't change types on rebinding:
          recur-type-check (if is-recur (recur-type-assert looptypes local-types) true)

          ; Apply recursivly if the code is a collection. Note: this is done AFTER any let statements.
          code (mapt #(inline-type % var-map local-types) code)
          code0 (first code) ; refresh this.

          ; Incase of a def (or defn, etc that are macroexpanded to def). Note: this is done AFTER the recursive part.
          var-map (if (= code0 `def) (assoc var-map (str (second code)) (get-meta (nth code 2) :type)) var-map)          

          type0 (get-meta (first code) :type)
          ]
      ; MAJOR cases: Note that metadata does not change equality.
      (let 
        [ty; the type that the code itself evaluates to.
          (cond 
            (= code0 `if); And/or/cond/etc is a macro so was macroexpanded into ifs and is handled here.
            (if (= (count code) 3)
              ; No branching, may return nil. It better not be a primitave type!
              (let [ty (get-meta (nth code 2) :type)
                    primcheck (non-primative-assert ty)] ty)
              ; Branched statements (count code = 4)
                ; A branched type (if the return types are different) or an unbranched type (if both returns are the same type)
              (builtin/combine-type (get-meta (nth code 2) :type) (get-meta (nth code 3) :type)))

            ; Let statements and do statements evaluate to the type of the last element:
            (or (= code `let*) (= code `do*))
            (get-meta (last code) :type)

            ; Loop statements go on a recursive expedition to find the output:
            (= code `loop*)
            (loop-output-extract code)
            
            ; Keywords extract the type for a cooresponding map.
            (= type0 "clojure.lang.Keyword")
            (if (keyword? code0)
              (builtin/key-extract (get-meta (first code) :type) code0) ; literal keyword
              (builtin/vague-extract (get-meta (first code) :type)))); Becomes a keyword, don't know which at compile time.

            ; Function calls:
            (if (and (symbol? code0) (not (= code0 `defn)) 
              (let [varcode (get var-map (str code0)) ; lookup the fully-qualified name over all vars in all files.
                    loc-ty (get local-types (str code0)) ; Local type lookup.
                    builtin-ty (builtin/type-builtin-call code)
                    ]
                (if (nil? varcode)
                  (if (nil? locty)
                   (if (nil? builtin-ty)
                     (throw (Exception. (str "Can't recognize type for: " (str code0))))
                     builtin-ty) ; recognized as builtin fn.
                    ; code0 is a local varaible that is also a function.
                    ; Figure out which type is returned:
                    (var-fn-type code var-map local-types))
                  ; Calling one of our functions: inline the funciton call, type it, and return the root type.
                    ; TODO: a better way for recursive functions.
                  ; This allows polymorphism because different types going into the function may be different types comming out.
                  (get-meta (type-code (fn-inline varcode code)) :type)))

              ; Function declarations:
              (if (= code0 `defn) (fn-def-type code var-map local-types))
         ]

        ; Using a variable as a function:
        
      )
          )
          (TODO_non_sequential_cases_collection_literals_and_singletons)))

;(defn return-type [code var-map]
;  ; all hail the mighty macroexpand-all!
;  (_return-type (walk/macroexpand-all code) {})))


(defn mapt [f coll]
  "Like map but returns a collection of the SAME type as the first element."
  TODO)  
  
(defn collapse-branch [ty] 
  "Collapses nested branches and branches that evaluate to the same type.
   No harm done except cpu cycles if this is called defensivly.
   Structured types can have subfields that are in turn branches. This will NOT collapse those."
  TODO)

; Extracts the child type from ty. 
(defn key-extract [ty ky]
  "Gets the child type from keyword. Works for branched types or unbranched types."
  TODO
)

(defn vague-extract [ty]
 "Gets the possible child type(s) from keyword. Works for branched types or unbranched types.
  Returns a set of possible types, or a single type if not a set."
  TODO
)

(defn type-builtin-call [almost-typed-code]
  "The type that this function call evaluates to given the function name and input arguments. 
    Each argument (except the first one) must be typed (with :type added to the metadata).
    We then lookup the return type based on the code.
    nil is returned when we don't recognize the input as a core function."
    BIGTODO
)

; Map or vector access will tend to pull out the corresponding child type. But things can go wrong...
(defn map-vector-access [code argtypes] 
  ; This will also work with vectors!
  (if (sequential? code) ; a failsafe so that passing the number 1 here does not fail.
    (let [is-get (= (first code) `get)
          is-keyword (keyword? (first code))
          is-first (= (first code) `first)
          is-second (= (first code) `second)
          is-nth (= (first code) `nth)
          is-aget (= (first code) `aget)]
      (if (or is-get is-keyword is-first is-second is-nth is-aget) ; we ARE a valid map getting.
        (let [typesinmap (:ch (first argtypes)) ; collection type.
              deftype (if (and is-get (= (count code) 4)) (nth argtypes 2) nil) ] ; default type.
          (if (or (map? typesinmap) (vector? typesinmap)) ; the map IS a static struct of types, so we need to know which to pick.
            ; We know the type if the key extracts the 
            (let [third (if (> (count code) 2) (nth code 2) nil)
                  key (if is-get third (first code)) ; both (:bar foo) and (get foo bar) are valid.
                  key (if is-first 0 key) ; array access is also valid.
                  key (if is-second 1 key)
                  key (if (or is-nth is-aget) third key)]
              (if (symbol? key) ;Symbol means we can't find a static key, so are forced to mix types.
                  ; always combine with default type (nil default type means nothingh happens).
                  (combine-type (if (map? typesinmap) (seq (keys typesinmap)) (seq typesinmap)) deftype)
                  (combine-type (get typesinmap key) deftype)
              ))
           typesinmap))); either a mixed type (so we don't know which one goes to which key), or a mono-type (so all the keys go to the same).
    nil))); not sequential code
