; Abstractions of clojures syntax. In any non-lisp language this file would be at least 10 times more.
; Also, these functions are useful for non-code (homoiconicity).
(ns clooj.coder.grammer (:require [clooj.coder.io :as io] [clooj.utils :as utils] [clojure.string :as string])
  (:import (clojure.lang Compiler) (java.util Arrays)))
; TODO: what functions here, if any, belong in collections?
; i.e. ckeys is more of a collection tool than a syntax tool?

(defmacro pn [expr]
  "Prints the result and returns it. Useful for quick debugging, 
    but it's better to use a more powerful tool for serious bugs."
  `(let [out# ~expr]
      (println out#) out#))

(def _lazies 
  #{clojure.lang.Cons clojure.lang.Cycle clojure.lang.Iterate 
    clojure.lang.IteratorSeq clojure.lang.LazilyPersistentVector
    clojure.lang.LazySeq clojure.lang.LongRange
   clojure.lang.Range clojure.lang.RecordIterator
   clojure.lang.SeqEnumeration ; what is this even?
   clojure.lang.SeqIterator clojure.lang.StringSeq
   clojure.lang.TransformerIterator})
(defn lazy? [x]
  "Can x be very long (shallow only)? 
   Long means that init is ~O(1), which means iterating over it can be expensive or infinite.
   This function errs on the defensive side to avoid infinite loops.
   Laziness of & args depends on what was put into the fn.
   Minor inconsistancy: (range 0) is not lazy, but for >0 is.
   Variable args: most of the time it works, but (apply f 1 [1 2]) is lazy even if it's just a vector."
  (and (coll? x)
    (or (not (counted? x))
       (boolean (get _lazies (type x)))
       ; This is an ephermial class created by variable argument functions. TODO: is this nessessary?
       (and (instance? clojure.lang.ArraySeq x)
          (<= (count (.array ^clojure.lang.ArraySeq x)) 1)
          (= (count (take 2 x)) 2)))))


(def token-match
  "use (mapv first (re-seq ...)) with this to find symbols and numerical values. It will find stuff inside a string but not the string itself.
   NOTE: reader macro chars are excluded, they can be made into symbols but this is BAD practice."
  #"([a-z]|[0-9]|[A-Z]|=|\+|-|\*|\/|!|\$|%|&|,|\?|_)+")

(defn clojurestyle2java [jn]
  "Clojure -> java's naming style. Mung loses information, for example if foo_BANG_ and foo! both
   go to foo_BANG_. The main worry is for _foo bieng munged and demunged back into -foo."
  (Compiler/munge jn))

(defn javastyle2clojure [cn]
  "Java -> clojure's naming style."
  (Compiler/demunge cn))

(defn lassoc [l k v & kvs]
  "List assoc: treat lists like vectors. O(n+kvs) but not a big deal for most lists based on very short code datastructures.
   Like assoc it will throw out-of-bounds when we try to assoc and out of bounds thing."
  (apply list (apply assoc (into [] l) k v kvs)))

(defn l-end-cat [l x]
  "Like (concat l [x]) but returns a list not a lasy seq. O(n)"
  (apply list (conj (into [] l) x)))

(defn special-form-symbols [] ; see the doc for interned?
  (into [] (keys (Compiler/specials))))
(defn special-form? [sym]
  "Returns true for symbols that are interned in Compiler.clj, false otherwise.
   These are very slippery symbols: (def a 1) works but def doesn't.
   If you are trying to get a fully qualified namespace."
  (contains? (apply hash-set (special-form-symbols)) sym))

; TODO: refactor to use this function.
(defn kwd2sym [kwd]
  "You can easily make a kwd from a sym but not visa-versia."
  (symbol (apply str (rest (str kwd)))))

(defn class2sym 
  "Converts a java.lang.Class to symbol. Optional: specify to only include the leaf."
  ([cl] (class2sym cl false))
  ([cl leaf?]
    (let [s (subs (str cl) 6)]
      (symbol (if leaf? (last (string/split s #"\.")) s)))))

(defn var2sym 
  "Converts a clojure.lang.Var to symbol. Optional: specify to only include the leaf."
  ([v] (var2sym v false))
  ([v leaf?]
    (let [s (subs (str v) 2)]
      (symbol (if leaf? (last (string/split s #"/")) s)))))

(defn cdissoc [c k]
  "Like dissoc but works on most collections preserving the type. Not lazy.
   WARNING: O(n) for vectors and lists."
  (cond (nil? c) nil
    (vector? c) (into [] (vals (dissoc (zipmap (range (count c)) c) k)))
    (list? c) (apply list (dissoc (zipmap (range (count c)) c) k))
    (map? c) (dissoc c k)
    (set? c) (disj c k)
    (coll? c) (throw (Exception. (str "Unrecognized or unimplemented collection: " (type c))))
    :else (throw (Exception. "Not a clojure collection.")))) 

(defn ckeys [c]
  "Like keys but also works for vectors and prserves laziness.
   Example: (keys [:a :b :c]) fails but (ckeys [:a :b :c]) gives [0 1 2]."
  (cond (nil? c) nil
    ; lazy is always sequential:
    (lazy? c) (if (counted? c) (range (count c)) (map (fn [_ b] b) c (range)))
    (sequential? c) (into [] (range (count c)))
    (map? c) (into [] (keys c))
    (set? c) (into [] c) ; sets can get themselves.
    (coll? c) (throw (Exception. (str "Unrecognized or unimplemented collection: " (type c))))
    :else (throw (Exception. "Not a clojure collection.")))) 

(defn cvals [c]  
  "Like ckeys but for values."
  (cond (nil? c) nil
    (lazy? c) c
    (or (sequential? c) (set? c)) (into [] c)
    (map? c) (into [] (vals c))
    (coll? c) (throw (Exception. (str "Unrecognized or unimplemented collection: " (type c))))
    :else (throw (Exception. "Not a clojure collection.")))) 

(defn to-map [c]
  "Converts c to a map such that (get (to-map c) %) = (get c %). Not lazy."
 (cond (nil? c) nil
   (sequential? c) (let [cv (into [] c)] (zipmap (range (count cv)) cv))
   (map? c) c (set? c) (zipmap c c)
   (coll? c) (throw (Exception. (str "Unrecognized or unimplemented collection: " (type c))))
   :else (throw (Exception. "Not a clojure collection."))))


(defn cmap [map-option f code & args]
  "like map but it (as best as possible) preserves the type of code.
   Duplicates in sets will collapse if mapped to the same thing.
   What we do in for a map? has three options:
      :entry => apply f to each entry of a map (as a 2-long vector) returns a 2-long vector or a 1-key map.
        args are passed as additional two-long vectors instead of single elements.
      :flatten => apply f to keys and then to vals independently. Combine these back into a map.
        args are similarly flattened. Note: for args this is DIFFERENT than how maps are presented.
      :keys => apply f to only the keys (if f creates duplicate keys the map gets shorter as the earlier values are discarded).
      :vals => apply f to only the vals (keys are unchanged). 
        1:1 aligned with each of cvals for each args.
  Code determines the type/laziness no matter what args is."
  (cond (nil? code) nil
     (lazy? code) (apply map f code args) ; lazy seqs map.
     (or (list? code) (seq? code)) (apply list (apply map f code args))
     (vector? code) (apply mapv f code args)
     (set? code) (apply hash-set (apply map f code args)); set will remove duplicated.
     (map? code) 
        ; Flatten maps puts all the keys first and all the values next.
        (cond (= map-option :entry)
          (let [fx (apply map f code args) kf (map #(if (map? %) (first (keys %)) (first %)) fx)
                vf (map #(if (map? %) (first (vals %)) (second %)) fx)] (zipmap kf vf))
          (= map-option :flatten) 
          (zipmap (apply map f (keys code) (mapv ckeys args)) (apply map f (vals code) (mapv cvals args)))
          (= map-option :keys)
          (zipmap (apply map f (keys code) (mapv ckeys args)) (cvals code))
          (= map-option :vals)
          (zipmap (keys code) (apply map f (vals code) (mapv cvals args)))
          :else (throw (Exception. (str "Unrecognized map-option: " map-option))))
     (.isArray (.getClass code)) (into-array (apply mapv f code args)) ; must be type-consistant. WARNING: not high-performance (can this be changed?).
     (coll? code) 
     (throw (Exception. (str "Coll-type not implemented: " (type code))))
     :else 
     (throw (Exception. (str "Code type is not a collection: " (str code))))))

(defn meta-expand [m]
  "Expands m if it is a meta-data shorthand, otherwise returns m."
  (if (map? m) m {:tag m}))

(defn _expanded2qualified [cns] ; operateson a single one
  (let [nm (.getName (.getDeclaringClass (first (.getDeclaredMethods (.getClass cns)))))]
     (symbol (javastyle2clojure nm))))
(defn expanded2qualified [c]
    "Converts macro-classes to a fully-qualified clojure-function symbol.
   Macros may need to use functions not in the namespace of the file calling said macro.
   These are embodied as Class objects. This file extracts the fully-qualified symbol from them."
  (cond (coll? c) (cmap :flatten expanded2qualified c) ; recursive
        (or (string? c) (symbol? c) (number? c)) c
        (= (type (.getClass c)) java.lang.Class) (_expanded2qualified c)
        :else c)) ; does anything get down here?

(defn carets2line [text carets]
 "Converts caret locations into line locations.
  CARETS MUST BE SORTED!"
 (let [newlines (into [] (utils/which #(= % \newline) (into [] text)))
       n (count text) nc (count carets) nl (count newlines)]
   (loop [ic 0 il 0 vals (into [] (repeat nc 0))]
     (if (>= ic nc) vals
       (let [i (nth carets ic)
             ilnxt (loop [jl il] (if (>= jl nl) nl (if (> i (nth newlines jl)) (recur (inc jl)) jl)))]
         (recur (inc ic) ilnxt (assoc vals ic ilnxt)))))))

(defn lines2caret [text lines]
  "The caret goes at the end? of each line. Add one to each of lines if you are using line #'s starting at 1."
  (let [newlines (utils/which #(= % \newline ) text) nn (count newlines)
        out (map #(if (< % 0) 0 (if (>= % nn) (last newlines) (nth newlines %))) lines)]
    (into [] out)))

(defn line-char2char [text line char]
  "Converts line-char combos into characers given a text. Useful when we have an error message and want to know where on the string it is from."
  (let [caret (first (lines2caret text [(- line 2)])) ; will get 0 for negative numbers
        ]
   (+ caret char)))

(defn get-vars ([clas] (get-vars clas true))
  ([clas object?]
    "Gets all :methods and :fields from a single java class or instance object, including inheritance.
     object?: do we include the final level common to everything, it adds a few methods."
    (let [clas (if (= (type clas) java.lang.Class) clas (type clas))] ; instances => class object.
      (loop [fields [] methods [] cl clas]
        (if (or (nil? cl) (and (not object?) (= cl java.lang.Object))) {:fields fields :methods methods}
          (recur (into [] (concat fields (.getFields cl)))
                 (into [] (concat methods (.getMethods cl))) (.getSuperclass cl)))))))

(defn get-var-names 
  ([clas] (get-var-names clas true))
  ([clas object?]
     "Gets the names of the methods and fields from a class/instance. includes superclasses.
     (like get-vars but returns the names and does not differentiate method vs field, and removes duplicate names)."
    (let [vars (get-vars clas object?) get-names (fn [ls] (mapv #(.getName %) ls))]
     (apply hash-set (concat (get-names (:methods vars)) (get-names (:fields vars)))))))

; Type formats:
; Each type is {:ty :ch} format, except for leaf types which are strings such as "java.lang.Double" and branches.
; Simple children: {:ty "clojure.lang.PersistentVector" :ch "java.lang.Double"}, is a vector of doubles.
; Vectorized children (fixed keymap): {:ty "clojure.lang.PersistentVector" :ch ["java.lang.Double", "java.lang.Int"]}
;      :ch {:a "java.lang.Double" :b "java.lang.String"}
;      NOTE: we still use vector format for sequences!
; Vectorized children (variable keymap): {:ty "clojure.lang.PersistentVector" :ch #{"java.lang.Double" "java.lang.Int"} }
   ; this time we don't know that the first element is double and the second int, this the 
; Multi-type if statements: #{... ...}, the if-statement returns two different types! Note this is a also a set.
; Function type: {:function (actual code of function)}
  ; This is a placeholder as we calculate closures, etc, since our typing sytem inlines all code.

; Unknown type: "<Compile" "_" "unknown>". This occurs when we can't infer a type.
; Please don't use the exact string <Compile_branch> in your code, it will confuse us!

; NOTE: for code brevity numerical litterals are viewed as either longs or doubles.
  ; Nowever, a simple (num x) will ensure that standard clojure math (boxed, slower) is used.

; Makes a vector of even-odd pairs. [{:even 0 :odd 1} {:even 2 :odd 3} {:even 3 :odd 4} ...]
; Useful for let statements et al that use pairs arguments.
(defn even-odd [coll]
  (let [even (take-nth 2 coll) odd (take-nth 2 (rest coll))]
    (mapv #(hash-map :even %1 :odd %2) even odd)))


(defn is-def-statement [sym]
   "Any definition statement that defines a new symbol for the namespace."
   (let [sym (symbol sym)]
       (or (= sym 'def) (= sym 'clojure.core/def)
           (= sym 'defn) (= sym 'clojure.core/defn)
           (= sym 'defn-) (= sym 'clojure.core/defn-)
           (= sym 'defmacro) (= sym 'clojure.core/defmacro)
           (= sym 'defmulti) (= sym 'clojure.core/defmulti)
           (= sym 'defonce) (= sym 'clojure.core/defonce)
           (= sym 'defmethod) (= sym 'clojure.core/defmethod)
           (= sym 'definline) (= sym 'clojure.core/definline)
           (= sym 'defstruct) (= sym 'clojure.core/defstruct)
           (= sym 'defprotocol) (= sym 'clojure.core/defprotocol)
           (= sym 'deftype) (= sym 'clojure.core/deftype))))

(defn is-let-statement [sym]
   "Let statements that define local variables. There are some variants of let depending on macro expansion and full-name-qualification.
    Note: this system can be broken by rebinding let as a variable, etc. This would be very bad code practice so don't do it!"
   (let [sym (symbol sym)]
     (or (= sym 'let) (= sym 'clojure.core/let) (= sym 'let*) (= sym 'clojure.core/let*))))

;(defn fn-code-arity [code]
;  "Map from arity => fncode. -1 means variable arity."
;  (if (fn? code) (throw (Exception. "TODO grammer/fn-code-arity")) nil))
  
(defn fn-name [code] 
  "Gets the name, a string, of the function.
  After macro expansion, ther are three ways: (def x (fn ...)), (fn ...) and (fn x ...).
  nil means no name."
  (if (fn? code) (second code) nil))


(def _unk (str "<Compile" "_" "unknown>")) ; java interop, callbacks form external libraries (hint: you probably want to cast these if you know them!)

;; Gets type in our format (removes the class and is a string format)
(defn typef [obj] (subs (str (type obj)) 6) )


(defn literal-scalar [code] ; literal things like numbers, strings, etc. NOT map literals, etc.
  (if (sequential? code) nil
    (if (or (number? code) (string? code)) (typef code) nil))) ; the only literals for now.

;; Returns a type that maybe type t1 or t2, nil means said var does not exist (so nil + t1 -> t1, etc).
(defn combine-type [t1 t2]
  (if (or (= t1 _unk) (= t2 _unk)) _unk
    (if (= t1 t2) t1
      (if (nil? t1) t2
        (if (nil? t2) t1
          ; Ok now we must branch the type:
          (set [t1 t2]))))))

(defn with-locals [f-leaf code locals]
  "Applies (f-leaf code locals) recursivly to all leaf elements of the code.
   locals keeps changing based on what is put in the let statement.
   MUST be macroexpand-all first."
  (let [is-let? (and (coll? code) (or (= (first code) `let*) (= (first code) 'let*)))]
    (cond is-let?
      ; Running talley of which variables are kept-qualified:
      (let [lc (second code) n (count lc) _ (if (odd? n) (throw (Exception. "Let statement with odd # of forms in binding vector.")))
            x (loop [lvars locals code [] ix 0]
                (if (= ix n) {:code code :locals lvars}
                  ; Add to kept AFTER the blittify, a runny talley of vars that are defined.
                  (recur (conj locals (nth lc ix)) 
                         (conj code (nth lc ix) (f-leaf (nth lc (inc ix)) lvars))
                         (+ ix 2))))] 
        (apply list `let* {:code x} (map #(f-leaf % {:locals x}) (rest (rest code)))))
      (coll? code) (cmap :flatten #(with-locals f-leaf % locals) code)
      :else (f-leaf code locals))))

(defn _qualify-in-leaf [code nms locals]
  (if (and (symbol? code) (not (contains? locals code))) 
   (let [c (ns-resolve nms code)]
     (if c (var2sym c) code)) code))
(defn qualify-in [code nms]
  "Qualifies the code given the namespace nms."
  (with-locals #(_qualify-in-leaf %1 nms %2) code #{}))

(defmacro qualify [code]
  "Use inside a macro to get the qualified code.
   Macros, unlike functions, have *ns* to the *ns* calling the macro.
   WARNING: Variables in the namespace ahead of the macro will be 
     not used the firsst time the ns is loaded but used on subsequent loads.
     Put these toward the end of the file to minimize future-shadowing."
  (qualify-in code *ns*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Code for parsing strings in a way that preseves the mapping ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Drop-in inlined macros that work on char primitives.
(defmacro copen? [c]
  `(or (= ~c \() (= ~c \{) (= ~c \[)))

(defmacro cclose? [c]
  `(or (= ~c \)) (= ~c \}) (= ~c \])))

(defmacro creader? [c] 
  "Rader macro chars. code/syntax quotes, # signs, and the like."
  `(or (= ~c \@) (= ~c \') (= ~c \`) (= ~c \~) (= ~c \#) (= ~c \^)))

(defmacro cwhite? [c] 
  "White characters. Note: clojure treats , as identical to whites except in a string or escaped."
  `(or (= ~c \space) (= ~c \newline) (= ~c \tab)))

(defmacro ccom-white? [c] 
  "white or comma."
  `(or (= ~c \space) (= ~c \newline) (= ~c \tab) (= ~c \,)))

(defmacro cnumber? [c]
  "0-9, no other symbols."
  `(or (= ~c \0) (= ~c \1) (= ~c \2) (= ~c \3) (= ~c \4) (= ~c \5) (= ~c \6) (= ~c \7) (= ~c \8) (= ~c \9)))


(defn neutral-reader-macro-parse [s]
  "generates [I of :level, :mode, :token, :escape, and :breaks. :level is how nested we are, inclusive.
   Reader-macros such as '(foo) are NOT included in the ()-induced level increase.
   :mode is 0 usually, 1 for stuff in string-quotes and 2 for comments (this is inclusive).
   :token is 1 for strings, comments, and non-syntax stuff, inclusive. reader macro tokens can bridge across whitespace.
   :escape is 1 for characters that are bieng escaped.
   :break[i] is 1 where there are two back-to-back tokens in which s[i-1] and s[i] belong to different tokens.
     This demarcates separate tokens even when the :level is unchanged.
    This should work if we have valid code, but also works OK when the code would error on read-string.
    Performance: 111 ns/char."
  (let [^chars cs (.toCharArray (str s)) ; str is O(1) on strings. .toCharArray is 2 billion/second.
        n (int (count cs)) n0 (dec n)
       ^ints level (make-array Integer/TYPE n) ; not including the tokens.
       ^ints mode (make-array Integer/TYPE n)
       ^ints token (make-array Integer/TYPE n) ; includes macro chars.
       ^ints escape (make-array Integer/TYPE n)
       ^ints break (make-array Integer/TYPE n)
       btrue (boolean true) bfalse (boolean false)
       char-a (char \a) ; 5x performance boost once warmed up just by doing this!
       ]
       ; Step 1: ignore reader macro characters completely.
       (loop [ix (int 0) lev (int 0) comment? (boolean false)
           str? (boolean false) escape? (boolean false)]
         (if (= ix n) "Done"
           (let [c (if escape? char-a (aget ^chars cs ix))  ; neutralize escaped characters. Whitespace CAN be escaped, and if so it counts as a normal char.
                 escape-ix (if escape? 1 0)
          
                 next-escape? (if (and (not comment?) (= c \\)) btrue bfalse) ; escape is a toggle-switch.
                 next-comment? (if str? bfalse (if comment? 
                                 (if (= c \newline) bfalse btrue)
                                 (= c \;))) ; entry to comment condition.
                 ; Toggle whether we are inside a string or not:
                 str-toggle? (if (and (not comment?) (= c \")) btrue bfalse)
                 next-str? (if str-toggle? (if str? bfalse btrue) str?)
                 openc? (copen? c) closec? (cclose? c)
                 delta-lev (if (and (not comment?) (not str?)) (+ (if openc? 1 0) (if closec? -1 0)) 0)
                 popup (if (= delta-lev 1) 1 0) ; only one char boost.
                 ]
             (aset ^ints escape ix escape-ix) 
             (aset ^ints level ix (+ lev popup))
             (if (and (not comment?) (= c \")) (aset ^ints mode ix 1))
             (if str? ; inside a string: only another " can end the string. We can't immedaitly jump to a comment.
               (do (aset ^ints token ix 1) (aset ^ints mode ix 1))) ; strings are always in a token.
             (if next-comment? (do (aset ^ints mode ix 2) (aset ^ints token ix 1))) ; comments are also tokens.
             (if (and (not openc?) (not closec?) (not (ccom-white? c)))
                (aset ^ints token ix 1))
             (recur (inc ix) (+ lev delta-lev) next-comment? next-str? next-escape?))))
        ; Step 2: the break arrays:
	(loop [ix (int 0) quote-mode (int 0)]
	  (if (>= ix n0) "Done"
		(let [ca (aget ^chars cs ix)
		      cb (aget ^chars cs (inc ix))
			  ; toggle the quotes.
			  quote-mode (if (and (= ca \") (= (aget ^ints mode ix) 1) (= (aget ^ints escape ix) 0)) 
			               (- 1 quote-mode) quote-mode)]
		 ; Check for characters that can end a token back-to-back with the start of another token:
		 ; This is the preliminary filtering step.
		 (if (or (= ca \") (= cb \") (= cb \\) (= ca \newline) (= cb \;))
		   (let [ca (if (= (aget ^ints escape ix) 1) char-a ca) ; neutralize escape chars.
		         cb (if (= (aget ^ints escape (inc ix)) 1) char-a cb)
			 ma (aget ^ints mode ix) mb (aget ^ints mode (inc ix))
			 ta (aget ^ints token ix) tb (aget ^ints token (inc ix))]
			 ;(if (< (count s) 100) (println "stuff:" ca "|" cb "|" ma "|" mb "|" ta "|" tb))
                         (if (and (or (not= ma 2) (not= mb 2)) ; don't break within a comment.
                                (= ta 1) (= tb 1) ; only break across tokens.
			        (or (not= ma mb) ; Discontinuous mode.
                                    (and (= cb \") (= ma 0)) ; symbol -> string
                                    (and (= ca \") (= cb \") (= quote-mode 0)) ;back-to-back string rule.
                                    (and (not= ma 1) (= cb \\)))) ;something before a \-escaped something.
				  (do (aset ^ints break (inc ix) 1) (recur (inc ix) quote-mode))
				  (recur (inc ix) quote-mode)))
		    (recur (inc ix) quote-mode)))))
      ; Step 3: bridging the reader macro characters (technically \ and ; are reader macros, we don't consider them such). 
      ;   There are level bridges and char bridges, only induction chars can create a bridge.
      ;   Multible, nested reader macros are all lumbed into the same symbol.
      ; Note: stuff like foo'bar that is not special WILL be caught but it shouldn't do anything.
      (loop [ix (int 0)]
        ; Macro chars create bridges to the next non-macro char.
        (if (>= ix n0) "DONE"
          (if (and (creader? (aget ^chars cs ix)) (= (aget ^ints escape ix) 0) (= (aget ^ints mode ix) 0))
              ; Jump through all comments, other macro chars, and whitespace: Pound has effects on equals.
              ; hit-ix is the first thing the macro char "hits".
              (let [pound? (boolean (if (= (aget ^chars cs ix) \#) btrue bfalse))
                    hit-ix (loop [jx (int (inc ix))]
                             (if (= jx n) (dec n) ; off end of array.
                               (let [e (aget ^ints escape jx) c (aget ^chars cs jx) m (aget ^ints mode jx)] 
                                 (if (or (= e 1) (= m 1)) jx ; stop on a normal char of the start of the string.
                                   (if (or (ccom-white? c) (= m 2)) (recur (inc jx)) ; blow through white and comments.
                                     (if (or (creader? c) (and pound? (or (= c \=) (= c \?))))
                                       (recur (inc jx)) jx)))))) ; reader chars with the addition of = and ? for pound => keep going.
                    bl (aget ^ints level hit-ix)]
               ; Break off strings or comments right below us:
               (if (and (> ix 0) (> (aget ^ints mode (dec ix)) 0)) (aset ^ints break ix 1))
               ; Actual bridge building: 
               (loop [jx (int ix)]
                 (if (>= jx hit-ix) "Done"
                   (do ;DONT set the level yet: (aset ^ints level jx bl) 
                       (aset ^ints break (inc jx) 0) 
                       (aset ^ints token jx 1) (recur (inc jx)))))
               (recur (inc hit-ix))) ; jump to the next time we need it.
               (recur (inc ix)))))
      {:level level :mode mode :token token :escape escape :break break}))

(defn basic-parse [s]
  "Like neutral-reader-macro-parse but adds :boost. :boost is an array that
   shows the effect of the macro characters, the final level when read into a string is level + boost.
   Boost, like level, also inclusive. Boost does NOT include #{hash set} or regexp literals b/c they read as is.
   Also :break is set to 1 where there is a difference of boost levels.
   114 ns/char including the neutral-reader-macro-parse (very small increase)."
  (let [parse0 (neutral-reader-macro-parse s)
        ^chars cs (.toCharArray (str s))
        ^ints level (:level parse0) ^ints mode (:mode parse0) ^ints token (:token parse0)
        ^ints escape (:escape parse0) ^ints break (:break parse0)
        n (int (count cs))
        ^ints boost (make-array Integer/TYPE n)]
    ;(if (< (count s) 100) (println "levels0:" (into [] level)))
    ; Increase levels for reader macros:
    (loop [ix (int 0)]
      (if (= ix n) "Done"
        (let [c (aget ^chars cs ix)]
          (if (creader? c)
              (let [e (aget ^ints escape ix) 
                    m (aget ^ints mode ix)]
                (if (and (= e 0) (= m 0) ; start of a reader macro.
                      ; The ~ in @~ does not increase the level (it's an unquote splice).
                      (or (not= c \@) (= ix 0) (not= (aget ^chars cs (dec ix)) \~) 
                        (not= (aget ^ints mode (dec ix)) 0))
                      ; same with a #' varquote:
                      (or (not= c \') (= ix 0) (not= (aget ^chars cs (dec ix)) \#) 
                        (not= (aget ^ints mode (dec ix)) 0))
                      ; Block hash-sets and regexps as well (they read homioiconicly):
                      (or (not= c \#) (>= ix (dec n)) (and (not= (aget ^chars cs (inc ix)) \{) (not= (aget ^chars cs (inc ix)) \"))
                        (not= (aget ^ints mode (inc ix)) 0))) 
                  ; Level boost step:
                  ; TODO: for esoteric code this can O(n^2).
                  (let [t (aget ^ints token ix) l0 (aget ^ints level ix)] ;level of the ' char.
                    (loop [jx ix tspoil (int 0)]
                      (if (< jx n)
                        (let [l (aget ^ints level jx)]
                          ; Criteria to keep reading (inclusive of this char):
                          (if (or (> l (inc l0)) ; at least two levels deeper gaurentees keeping-going.
                                  ; one level deeper and not a back-to-back '()() or 'foo():
                                  (and (= l (inc l0)) ; not an unescaped ( OR a reader macro head.
                                       (or (= jx 0) (not (copen? (aget ^chars cs jx))) (= (aget ^ints escape jx) 1)
                                         (creader? (aget ^chars cs (dec jx))) ; obviously a reater macro.
                                         ; More complex case for macro bridges:
                                         (and (= (aget ^ints escape (dec jx)) 0) (ccom-white? (aget ^chars cs (dec jx))))))
                                  ;if at same level, we need a non-broken token.
                                  (and (= l l0) (= tspoil 0) (> (aget ^ints token jx) 0) (or (= jx 0) (= (aget ^ints break jx) 0))))
                            (do (aset ^ints boost jx (inc (aget ^ints boost jx))) 
                                 (recur (inc jx) (if (> l l0) 1 tspoil))) "done")) "done"))
                   (if (and (> ix 0) (= t 1) (= (aget ^ints token (dec ix)) 1)) ; set break to 1.
                       (aset ^ints break ix 1))))
                   (recur (inc ix)))
        (recur (inc ix))))))
    (assoc parse0 :boost boost))) ; mutated with the array ops.

(defn token-end [^ints token ^ints break ix]
  ; the last index still on the token, inclusive.
  ; ix is the first token thing.
  (let [n (int (count token)) ix (int (max 0 (min (dec n) ix)))]
    (loop [jx (int (inc ix))]
        (if (or (= jx n) (= (aget ^ints token jx) 0) (= (aget ^ints break jx) 1))
          (dec jx) (recur (inc jx))))))
(defn boring-end [^chars cs ^ints mode ix]
  ; The last element of cs that is not used (i.e. most whitespace, comments).
  ; ix can either be boring or interesting, if interesting the output will be ix-1.
  (let [n (int (count cs)) ix (int (max 0 (min (dec n) ix)))]
    (loop [jx (int ix)]
      (if (and (< jx n) (or (= (aget ^ints mode jx) 2) (ccom-white? (aget ^chars cs jx))))
        (recur (inc jx)) (dec jx)))))
(defn level-boost-end [^chars cs ^ints mode ^ints level ^ints boost ix & reduce-by-one]
  ; the ending (inclusive) of the chars at or above this level.
  (let [ix (int (max 0 (min (dec (count cs)) ix))) n (int (count cs)) l0 (aget ^ints level ix) b0 (aget ^ints boost ix)
        ; for the GUI: a hack for reducing the level if we are wedged in like ()|(), etc.
        l0 (if (first reduce-by-one) (dec l0) l0)
        b0 (if (second reduce-by-one) (dec b0) b0)]
    (loop [jx (int (inc ix))]
      (if (= jx n) (dec jx)
        (let [c (aget ^chars cs jx) l (aget ^ints level jx) 
              c1 (if (= jx (dec n)) (char \ ) (aget ^chars cs (inc jx)))
              b (aget ^ints boost jx) m (aget ^ints mode jx)]
      ; lucky that excaped characters just happen to not affect us.
      (if (or (and (> l l0) (>= b b0)) (and (> b b0) (>= l l0)) 
            ; Reader chars can bump up to the end of a macro in things like '(bar)'foo:
            ; also, parenthesis can bump into eachother like: ()()
            (and (>= l l0) (>= b b0) (or (> m 0) (= jx 0) (not (cclose? (aget ^chars cs (dec jx)))) 
                                       (and (or (not (creader? c)) (and (= c \#) (or (= c1 \{) (= c1 \")))) ; #{hash set} and #"regex" are NOT boosted.
                                            (not (copen? c))))))
        (recur (inc jx)) (dec jx)))))))
; Beginning functions (only really used for hilighting for now):
(defn token-beginning [^ints token ^ints break ix] 
  ; TODO: error with ''|foo
  (let [ix (int (max 0 (min (dec (count token)) ix)))]
    (loop [jx (int (dec ix))]
      (if (or (= jx -1) (= (aget ^ints token jx) 0))
        (inc jx) 
        (if (and (> jx 0) (= (aget ^ints break jx) 1)) jx (recur (dec jx)))))))
(defn level-boost-beginning [^chars cs ^ints mode ^ints level ^ints boost ix & reduce-by-one]
  (let [ix (max 0 (min (dec (count cs)) ix)) l0 (aget ^ints level ix) b0 (aget ^ints boost ix)
        ; for the GUI: a hack for reducing the level if we are wedged in like ()|(), etc.
        l0 (if (first reduce-by-one) (dec l0) l0)
        b0 (if (second reduce-by-one) (dec b0) b0)]
    (loop [jx (int (dec ix))]
      (if (= jx -1) 0
         (let [b (aget ^ints boost jx) m (aget ^ints mode jx)
               l (aget ^ints level jx) c (aget ^chars cs jx)
               c1 (if (= jx (dec (count cs))) (char \ ) (aget ^chars cs (inc jx)))
               c2 (if (>= jx (- (count cs) 2)) (char \ ) (aget ^chars cs (+ jx 2)))]
          (if (or (and (> l l0) (> b b0)) ; definitly deeper => keep going.
                  ; at least as deep for both level and boost:
                  (and (>= l l0) (>= b b0) 
                    (or (> m 0) (= jx (dec (count cs))) ; simple reasons.
                    (not (cclose? c)) (or (not (creader? c1)) (and (= c1 \#) (or (= c2 \{) (= c2 \")))) ; #{hash set} and #"regex" are NOT boosted.
                      )))  ; '()|'foo or '()|'() screen.
              (recur (dec jx)) (inc jx)))))))

; Reader-macros that don't map to functions well (excluding metadata). In paractice you rarely (ever?) call these fns.
(defn read-eval [code] (throw "TODO"))
(defn syntax-quote [code] (throw "TODO"))
(defn *reader-conditional [code] (throw "TODO"))
(defn ignore [code] code) ; Return the actual code, one level up it will be ignored.
(defn *unquote-splicing [code] code) ; one level up it will be spliced.

(defn _unpack-anon-fns [code ^chars cs ^ints mode ix last-ix]
  ; Unpacks anomonous functions, code is what goes inside. We can keep the % symbols, anon-fns don't nest.
  ; Code has already been read through the _read-string-pos.
  ; returns the :obj. Note: code is always a list at the outer level.
  (let [nargs (loop [acc 0 jx ix]
                (if (> jx last-ix) acc
                  (if (and (= (aget ^chars cs ix) \%) (= (aget ^ints mode ix) 0))
                      ; They limit it to 20 args, so our limit to 99 is fine:
                      (let [c1 (if (<= (inc ix) last-ix) (aget ^chars cs (inc ix)) (char \a))
                            c2 (if (<= (+ ix 2) last-ix) (aget ^chars cs (+ ix 2)) (char \a))
                            n (cond (and (cnumber? c2) (cnumber? c1)) (Integer/parseInt ^String (str c1 c2))
                                    (cnumber? c1) (Integer/parseInt ^String (str c1))
                                    :else 0)]
                        (recur (max acc n) (inc jx)))
                      (recur acc (inc jx)))))]
    ; fn and fn* seem equivalent. 
    ; set :begin to one more than :end on the 'fn and [args] so that it does not affect the string.
    ; careful to get the :obj nesting levels correct.
    (list {:obj 'fn :begin ix :end (dec ix)}
      {:obj (mapv #(hash-map :obj (symbol (str "%" %)) :begin ix :end (dec ix)) (range nargs)) :begin ix :end (dec ix)}
      code))) ; the code is unchanged, it has it's own :obj :begin and :end that does not include the initial #.

(defn _read-string-pos [^chars cs ^ints token ^ints mode ^ints level ^ints boost ^ints break
                        ix last-ix]
  ; Recursive reading of the string inclusive between ix and last-ix.
  ; ix must be gaurenteed to start at something of relavence (i.e. a beginning of a token) and ix <= last-ix
  (let [c (aget ^chars cs ix)
        c1 (if (< ix last-ix) (aget ^chars cs (inc ix)) (char \a))
        rread #(_read-string-pos cs token mode level boost break %1 %2)
        fun1 #(inc (boring-end cs mode (inc ix))); first fun character right after a macro.
        fun2 #(inc (boring-end cs mode (+ ix 2))); now for double-macro chars.
        w1 (fn [sym] (hash-map :obj sym :begin ix :end (dec (fun1))))
        w2 (fn [sym] (hash-map :obj sym :begin ix :end (dec (fun2))))
        l-end #(dec (level-boost-end cs mode level boost ix))
        ; vectorized reading. Used to read all the stuff inside the (), etc. The jx and last-jx are NOT inclusive of the ().
        ; Returns a vector if there is at least on element inside the {}, otherwise returns a description of empty code.
        readvf (fn [jx last-jx] ; jx may or may not be where the fun stuff starts.
                 (if (or (> jx last-jx) (>= (boring-end cs mode jx) last-jx)) 
                   {:obj []} ; empty [] () {} #{} or [ ] ( ) { } #{ }, later on we specify the :begin and :end
                   (let [jx1 (inc (boring-end cs mode jx))]
                     (loop [acc [] kx jx1] ; kx is the first interesting index.
                       (if (or (> kx last-jx) (cclose? (aget ^chars cs kx))) acc ; nothing left to read.
                         (let [; if we are in a non-reader token, use the token. Otherwise use the level:
                               end (if (and (not (creader? (aget ^chars cs kx))) (= (aget ^ints token kx) 1))
                                       (token-end token break kx) (level-boost-end cs mode level boost kx))
                               next-beginning (inc (boring-end cs mode (inc end))) ; first char on the next important bit.
                               codr (rread kx (dec next-beginning))
                               codr (if (= kx jx1) (assoc codr :begin jx) codr)]; the beginning junk
                           (recur (conj acc codr) next-beginning)))))))
        ; Pack the metadata into a :meta:
        meta-body (fn [m] (let [st (:begin (first (:obj m)))] ; have the :begin include the meta-data ^.
                            (assoc (second (:obj m)) :begin st))) ; remove the actual meta-f-fy itself.
        meta-pack (fn [v]
                    (let [n (count v)]
                      (loop [acc [] ix 0]
                        (if (= ix n) acc
                          (let [c (nth v ix)
                                meta? (and (list? (:obj c)) (= (:obj (first (:obj c))) 'clooj_coder_grammer_meta-i-fy))]
                            (if (and meta? (= ix (dec n))) (throw (Exception. "^ meta-data flag with nothing to attach to."))) ; Generates an EOF in vanilla.
                            ; add the next element and put this element into the :meta.
                            (recur (if meta? (conj acc (assoc (nth v (inc ix)) :meta (meta-body c))) (conj acc c))
                              (if meta? (+ ix 2) (inc ix))))))))
        readvf-box (fn [jx last-jx] ; normalizes to a map format with :obj bieng a vector.
                     (let [r (readvf jx last-jx)] ; ix not jx and last-ix not last-jx
                       (update (if (vector? r) {:obj r :begin ix :end last-ix} (assoc r :begin ix :end last-ix)) :obj meta-pack)))]
    ; c isn't bieng commented or escaped, so it's easier to switchyard:
    (if (and (creader? c) (or (not= c \#) (and (not= c1 \{) (not= c1 \")))) ; hash-sets and regexps block reader macros.
      ; reader-macros => we come up with names for them. The last-ix does not change.
      (cond (and (= c \#) (= c1 \')) {:obj (list (w2 'var) (rread (fun2) last-ix)) :begin ix :end last-ix}
            (and (= c \#) (= c1 \=)) {:obj (list (w2 'clooj.coder.grammer/read-eval) (rread (fun2) last-ix)) :begin ix :end last-ix}
            (and (= c \#) (= c1 \_)) {:obj (list (w2 'clooj.coder.grammer/ignore) (rread (fun2) last-ix)) :begin ix :end last-ix}
            (and (= c \#) (= c1 \?)) {:obj (list (w2 'clooj.coder.grammer/*reader-conditional) (rread (fun2) last-ix)) :begin ix :end last-ix}
            (= c \#) {:obj (_unpack-anon-fns (rread (fun1) last-ix) cs mode (fun1) last-ix) :begin ix :end last-ix}
            (= c \') {:obj (list (w1 'quote) (rread (fun1) last-ix)) :begin ix :end last-ix}
            (= c \@) {:obj (list (w1 'deref) (rread (fun1) last-ix)) :begin ix :end last-ix}
            (and (= c \~) (= c1 \@)) {:obj (list (w2 'clooj.coder.grammer/*unquote-splicing) (rread (fun2) last-ix)) :begin ix :end last-ix}
            (= c \~) {:obj (list (w1 'unquote) (rread (fun1) last-ix)) :begin ix :end last-ix}
            (= c \`) {:obj (list (w1 'clooj.coder.grammer/syntax-quote) (rread (fun1) last-ix)) :begin ix :end last-ix}
            ; Tempoararly add a clooj_coder_grammer_meta-i-fy symbol (this is the only way to get metadata when read with read-string):
            (= c \^) {:obj (list (w1 'clooj_coder_grammer_meta-i-fy) (rread (fun1) last-ix)) :begin ix :end last-ix}
            :else (throw (str "Unrecognized macro: " c c1)))
      ; opening chars => read the stuff inside.
      (cond (= c \() (update (readvf-box (inc ix) (l-end)) :obj #(apply list %))
            (= c \[) (readvf-box (inc ix) (l-end)) ; :obj is already a vector.
            (= c \{) (update (readvf-box (inc ix) (l-end)) :obj #(apply hash-map %)) ; # keys better be even.
            (and (= c \#) (= c1 \{)) (update (readvf-box (+ ix 2) (dec last-ix)) :obj #(apply hash-set %)) ; may change the order, but :begin and :end is still preserved.
            ; leaf level read-string (including regexps). They will ignore the boring stuff.
            :else (let [^chars csus (Arrays/copyOfRange ^chars cs ix (inc last-ix))
                        ^String sus (String. ^chars csus)]
                   {:obj (read-string sus) :begin ix :end last-ix})))))

(defn reads-string [s]
  "Like read-string but reads all the objects, in vector form."
  (read-string (str "[" s "\n]")))

(defn reads-string-pos [s]
  "Reads a string but in a position-aware way and doesn't stop with the first token like read-string does.
   At each level there is :obj, :begin, :end, and :meta. :begin, :end are INCLUSIVE.
   At all the other levels we have:
   :obj, which is is simply the code (of course, with nesting the code contains other :obj :begin :end stuff)
   Replacing, recursivly {:obj foo :begin 123 :end 456 :meta bar} with foo will recreate (with a few exceptions like syntax-quote)
    what read-string would do. :begin and :end is inclusive.
   Fluff is added to the end (except for the first element, for which fluff is also added to the beginning, and ( <fluff> ) for which fluff gets added to the parent list).
   ONLY USE FOR REFACTORING, where the output must preserve as much of the visual structure as the input.
     (use the faster vanilla reads-string to check if a refactoring needs to be done)."
  (let [s1 (str "[" s "\n]") ; wrap it up in a [].
        parse1 (basic-parse s1)
        code1 (_read-string-pos (.toCharArray s1) (:token parse1) (:mode parse1) (:level parse1) (:boost parse1) (:break parse1) 0 (dec (count s1)))
        ; reduce by one the :begin and :end of the code location since we have an extra :end.
        ; Also: remove the effects of the newline by previnting
        n (count s)
        one-less (fn one-less [c] (let [c1 (if (coll? (:obj c)) (update c :obj #(cmap :flatten one-less %)) c)
                                        c1 (if (:meta c) (update c1 :meta one-less) c1)
                                        c2 (if (:begin c1) (update c1 :begin dec) c1)]
                                    (if (:end c2) (update c2 :end #(dec (if (> % n) n %))) c2)))]
    (:begin 0 :end (dec n) :obj (mapv one-less (:obj code1)))))

(defn pos-to-blit-code [s c]
  "Converts a string and it's reads-string-pos format to a blitted format.
   The blitted format has :head :body and :tail as strings as well as :obj for the code object.
   :head and :tail are when the :obj has children; :body is for when it doesn't.
   the :head is typically an opening (, etc. The :tail often has whitespace, etc.
   The :body captures the entire code for the leaf levels."
  (let [; Extract the earliest beginning from a peice of code:
        hcoll? #(and (coll? %) (> (count %) 0)) ; non-mepty collections.
        mbegin #(if (:meta %) (:begin (:meta %)) (:begin %)) ; takes a single :begin :end , etc object.
        deep-begin (fn deep-begin [c] ;c is a collection.
                     (let [fdeep (fn [v] (apply min (mapv #(if (hcoll? (:obj %)) (deep-begin (:obj %)) (mbegin %)) v)))] ; works on a vector.
                       (cond (set? c) (fdeep (into [] c))
                         (map? c) (mbegin (first (keys c))) ; keys before valse.
                         :else (mbegin (first c))))) ; non-set non-map are ordered.
        deep-end   (fn deep-end [c] ;c is a collection.
                     (let [fdeep (fn [v] (apply max (mapv #(if (hcoll? (:obj %)) (deep-end (:obj %)) (:begin %)) v)))] ; works on a vector.
                       (cond (set? c) (fdeep (into [] c))
                         (map? c) (:end (last (vals c))) ; vals after keys. 
                         :else (:end (last c))))) ; non-set non-map are ordered.
        convert (fn convert [x] ; x has {:obj :begin: end}.
                   (let [begin (:begin x) end (:end x)
                         ; Recursive => empty body. Not recursive => empty head and tail.
                         recursive? (hcoll? (:obj x))
                         x (if (:meta x) (assoc x :meta (convert (:meta x))) x)]
                   (if recursive? 
                     (let [cbegin (deep-begin (:obj x)) cend (deep-end (:obj x))]
                       (assoc (update x :obj #(cmap :flatten convert %)) ; recursive step.
                                 :head (subs s begin cbegin) :body "" 
                                 :tail (subs s (inc cend) (inc end))))
                     (assoc x :head "" :body (subs s begin (inc end)) :tail ""))))]
    (convert c)))

(defn reads-string-blit [s]
  "Reads a string into a blitted format. Pass this through your refactoring code."  
  (pos-to-blit-code s (reads-string-pos s)))

(defn blit-code-to-code [c]
  "Converts the code-with-string-blit to the vanilla code object. See the test-blit-reader function.
   Preserves laziness if i.e. infinite seqs are put into the :obj."
  (let [hcoll? #(and (coll? %) (> (count %) 0)) ; non-mepty collections.
        convert (fn convert [x] ; x has :obj :begin and :end, etc.
                  (let [ob (if (hcoll? (:obj x)) (cmap :flatten convert (:obj x)) (:obj x))]
                    (if (:meta x) (with-meta ob (meta-expand (convert (:meta x)))) ob)))]
    (convert c)))

(defn blit-code-to-str [c]
  "Converts the code-with-string-blits back into a string. Used after the refactoring is done.
   In cases where pr-str is not esoteric: 
     Ensures that the code is valid code.
     (blit-code-to-str (reads-string-blit s)) = s, unless something goes wrong..."
  (throw "TODO: ensuring that the () type maps the collection type of :obj.")
  (let [; set ordering has a mind of it's own and even hash-maps still don't offically gaurentee sorting.
        hcoll? #(and (coll? %) (not (empty? %)))
        begin-sort #(sort-by :begin %)
        ; Add a newline if the comment is
        dcom (fn [^String s k] 
               (let [^String su (subs s k)]
                 (if (> (.lastIndexOf ^String su ";") ; lastIndexOf returns -1 if it fails.
                        (.lastIndexOf ^String su "\n")) (str s "\n") s)))
        ; Ensures that the head and tail match the collection type:
        match-type (fn [c] ; non-empty collections only.
                     ; The BEGINNING of the head and tail has the {}, and no macros are in the head here.
                     ; For reads-string-blit the [] encapsulates all fluff.
                     (let [o (:obj c) hd (if (and (set? o) (<= (count (:head c)) 1)) "#{" (:head c))
                           open (cond (map? o) "{" (vector? o) "[" (set? o) "#{" :else "(")
                           close (cond (map? o) "}" (vector? o) "]" (set? o) "}" :else ")")
                           hd1 (if (not= (subs hd 0 (count open)) open) (str open (subs hd (count open))) hd)
                           tl1 (if (not= (subs (:tail c) 0 1) close) (str close (subs (:tail c) 1)) (:tail c))]))
        ; gets how many chars we ignore for a leaf-object:
        get-k (fn [^String s leaf-obj]
                ; k is the index of the last token:
                ; It should always stop unless the :body is completly wrong.
                (let [^chars cs (.toCharArray s)
                      mp? (if (map? leaf-obj) (boolean true) (boolean false))
                      closec (if mp? (last (str leaf-obj)) (char \a))
                      ; For non-maps: first char that is part of the token.
                      ; For maps: the closing parenthesis.
                      ix0 (loop [ix (int 0) comment (int 0)]
                            (let [c (aget ^chars cs ix)]
                              (if (= comment 1)
                                (recur (inc ix) (if (= c \newline) 0 1))
                                (if (= c \;)
                                  (recur (inc ix) 1)
                                  ; Active mode:
                                  (if (or (and mp? (= c closec)) ; maps
                                          (and (not mp?) (not (ccom-white? c)))) ; not maps.
                                    ix (recur (inc ix) comment))))))] 
                  (if mp? ix0 (+ ix0 (count (pr-str leaf-obj)) -1))))
                      
        to-vec (fn to-vec [x] ; all non-empty collections become vectors and we sort by the :begin.
                 (let [ob (:obj x)
                       out (assoc x :obj (if (hcoll? ob)
                               (begin-sort (into [] (if (map? ob) (concat (keys ob) (vals ob)) ob))) ob))]
                   (if (:meta x) (update out :meta to-vec) out)))
        strs (fn strs [x] 
               (let [col? (hcoll? (:obj x)) x (if col? (match-type x) x)]
                 (if (and (not col?) (not= (:obj x) (read-string (:body x))))
                   (throw (Exception. "Mismatched :obj and :body")))
                 (str (if (:meta x) (strs (:meta x)) "") ; meta before even the head.
                      (dcom (:head x) 0)
                      (if col?
                        ; When to interpose a space?
                        ; There is no space, comma,etc.
                        ; Both are non-collection non-strings.
                        (let [ch-strs (mapv strs (:obj x)) n (count ch-strs)
                              mushy?s (mapv #(and (not (coll? (:obj %))) (not (string? (:obj %)))) (:obj x))
                              ch-strs-pad (mapv #(str (nth ch-strs %) 
                                                   (if (and (nth mushy?s %) (nth mushy?s (inc %)) (ccom-white? (last (nth ch-strs %)))) 
                                                     " " "")) (range (dec n)))]
                          (apply str ch-strs-pad))
                        "")
                      (if (not col?) (dcom (:body x) (get-k (:obj x) (:body x))) 
                        (:body x)) ; :body should be empty for hcolls but just in case.
                      (dcom (:tail x) 0))))]
    (strs (to-vec c))))

(defn test-blit-reader [s]
  "Compares the vanilla read-string vs our version that keeps track of position.
   :str? = does converting the string to code and back yield the original string?
   :code? = does our code agree with that of the vanilla reader?
   Note: reader macros will often fail for the :code? test even if it is working properly.
      :str? should ALWAYS work, however."
  (let [c (reads-string-blit s)]
;(println "c is:" (blit-code-to-code c) "vs: " (read-string (str "[" s "\n]")))
    {:str? (= s (blit-code-to-str c)) 
     :code? (= (read-string (str "[" s "\n]")) (blit-code-to-code c))}))