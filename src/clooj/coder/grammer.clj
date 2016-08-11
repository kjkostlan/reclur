; Abstractions of clojures syntax. In any non-lisp language this file would be at least 10 times more.
; Also, these functions are useful for non-code (homoiconicity).
(ns clooj.coder.grammer (:require [clooj.coder.io :as io] [clooj.collections :as collections] 
                                  [clojure.string :as string] [clojure.walk :as walk]
                                  [clojure.set :as set])
  (:import (clojure.lang Compiler) (java.util Arrays)))

(def debug (atom nil))

(defmacro pn [expr]
  "Prints the result and returns it. Useful for quick debugging, 
    but it's better to use a more powerful tool for serious bugs."
  `(let [out# ~expr]
      (println out#) out#))

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
  (cond (coll? c) (collections/cmap :flatten expanded2qualified c) ; recursive
        (or (string? c) (symbol? c) (number? c)) c
        (= (type (.getClass c)) java.lang.Class) (_expanded2qualified c)
        :else c)) ; does anything get down here?

(defn carets2line [text carets]
 "Converts caret locations into line locations.
  CARETS MUST BE SORTED!"
 (let [newlines (into [] (collections/which #(= % \newline) (into [] text)))
       n (count text) nc (count carets) nl (count newlines)]
   (loop [ic 0 il 0 vals (into [] (repeat nc 0))]
     (if (>= ic nc) vals
       (let [i (nth carets ic)
             ilnxt (loop [jl il] (if (>= jl nl) nl (if (> i (nth newlines jl)) (recur (inc jl)) jl)))]
         (recur (inc ic) ilnxt (assoc vals ic ilnxt)))))))

(defn lines2caret [text lines]
  "The caret goes at the end? of each line. Add one to each of lines if you are using line #'s starting at 1."
  (let [newlines (collections/which #(= % \newline ) text) nn (count newlines)
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

(def lvs (keyword (str "clooj_coder_grammer" "_locals_192837465_Manual")))
(def lqs (keyword (str "clooj_coder_grammer" "_inquote?_192837465_Manual")))
(defn calculate-locals
  "Calculates the local variables, storing them in the meta-data. Use get-locals on a collection or symbol.
   Optional initial seed of local variables. Also stores if we are in a quote."
  ([code] (calculate-locals code #{} false))
  ([code locals0] (calculate-locals code locals0 false))
  ([code locals0 in-quote0?]
    (let [add-l (fn [x lvars] (if (or (coll? x) (symbol? x)) (vary-meta x #(assoc % lvs lvars lqs in-quote0?)) x))
          is-let? (and (collections/listoid? code) (contains? #{'let `let 'clojure.core/let*} (first code)))
          recursivly 
         (cond is-let?
           ; Running talley of which variables are kept-qualified:
           (let [lc (second code) n (count lc) _ (if (odd? n) (throw (Exception. "Let statement with odd # of forms in binding vector.")))
                x (loop [lvars locals0 lcode [] ix 0]
                    (if (= ix n) {:code lcode :locals lvars}
                      ; A running talley of vars that are defined.
                      (let [lvars1 (conj lvars (nth lc ix))]
                        (recur lvars1 (conj lcode (add-l (nth lc ix) lvars1) 
                                        (add-l (nth lc (inc ix)) lvars1)) (+ ix 2)))))]
             (apply list `let* (:code x) (map #(calculate-locals % (:locals x)) (rest (rest code)))))
             (coll? code) (let [in-quote? (or in-quote0? (and (= (first code) 'quote) (not (contains? locals0 'quote))))]
                            (collections/cmap :flatten #(calculate-locals % locals0 in-quote?) code))
             :else code)]
      (add-l recursivly locals0))))
(defn get-locals [coll-or-sym]
  "Gets the local variables calculated with calculate-locals.
   Returns an empty set on stuff that is not a collection or symbol."
  (let [out (lvs (meta coll-or-sym))]
    (if out out #{})))
(defn inquote? [coll-or-sym]
  "Gets if we are in a quote, returns nil if not a coll-or-sym sadly.
   Like get-locals, calculate-locals must be used."
  (let [out (lqs (meta coll-or-sym))]
    (if out out nil)))
(defn remove-locals [coll-or-sym]
  "SHALLOW removal of local variables and in quote status."
  (if (meta coll-or-sym) (vary-meta coll-or-sym #(dissoc % lvs lqs)) coll-or-sym))

(defn _with-locals [f-leaf code locals0 exclude-symbols-in-quotes?]
  "Applies (f-leaf code locals) recursivly to all leaf elements of the code that have metadata.
   locals keeps changing based on what is put in the let statement,
      The x in (let [x 1]) WILL be included in locals for itself.
   MUST be macroexpand-all first OR use simple let statements.
   exclude-symbols-in-quotes? means don't process symbols inside quotes.
   Stuff that can't hold metadata, such as strings, are NOT affected." 
    (let [codel (calculate-locals code locals0)]
      (walk/postwalk #(if (and (symbol? %) (or (not exclude-symbols-in-quotes?) (not (inquote? %))))
                          (f-leaf (remove-locals %) (get-locals %)) %) codel)))

(defn _qualify-in-leaf [code nms locals]
  (if (and (symbol? code) (not (contains? locals code))) 
   (let [c (ns-resolve nms code)]
     (if c (var2sym c) code)) code))
(defn qualify-in 
  "Qualifies the code given the namespace nms.
   Use (grammer/qualify-in code *ns*) to qualify the code from inside a macro.
   The *ns* will be that of the one calling the macro.
    MUST be macroexpand-all first OR use simple let statements.
   Stuff in the quotes is qualified if qualify-stuff-in-quotes? is true (the default)."
  ([code nms] (qualify-in code nms true))
  ([code nms qualify-stuff-in-quotes?]
    (_with-locals #(_qualify-in-leaf %1 nms %2) code #{} (not qualify-stuff-in-quotes?))))


;;;;;; Getting code to strings:

(defn code-to-str [c]
  "Code to string that preserves all metadata."
  (binding [*print-meta* true] (pr-str c)))

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

(def ^ints sym-kwd-start
  "Whether the character, as an int, can start a cymbol or a keyword.
   Cast the chars to ints (casting is cheap)."
  (let [^ints outs (make-array Integer/TYPE 256)
        valid "-=qwertyuiopasdfghjklzxcvbnm!$%&*_+QWERTYUIOP|ASDFGHJKL:ZXCVBNM<>?"
        n (count valid)]
    (loop [ix (int 0)]
      (if (= ix n) outs
        (do (aset ^ints outs (int (nth valid ix)) 1) (recur (inc ix)))))))

(def ^ints sym-kwd-stop
  "Whether the chars-as-ints stops symbols or keywords."
  (let [^ints outs (make-array Integer/TYPE 256)
        valid "`~@^()\t[{]}\\;\"\n, "
        n (count valid)]
    (loop [ix (int 0)]
      (if (= ix n) outs
        (do (aset ^ints outs (int (nth valid ix)) 1) (recur (inc ix)))))))

(defn neutral-reader-macro-parse [s]
  "generates [I of :level, :mode, :token, :escape, and :breaks. :level is how nested we are, inclusive.
   :level does NOT include the # in hash-sets.
   Reader-macros such as '(foo) are NOT included in the ()-induced level increase.
   :mode is 0 usually, 1 for stuff in string-quotes and 2 for comments (this is inclusive).
   :token is 1 for strings, comments, and non-syntax stuff, inclusive.
     ; Reader macro(s) applied to a token are all lumped into the same token.
     ; Reader macro(s) applied to a collection are treated as a token in of themselves.
     ; They can bridge across whitespace sometimes such as ' foo.
       ; They inlude the whitespace, but not the () in ' ()
   :escape is 1 for characters that are bieng escaped. Examples: char literals, reader macros embedded in symbols, and string escape chars.
   :break[i] is 1 where there are two back-to-back tokens in which s[i-1] and s[i] belong to different tokens.
     This demarcates separate tokens even when the :level is unchanged.
    This should work if we have valid code, but also works OK when the code would error on read-string.
    Performance: around 120 ns/char? TODO: need to update this test."
  (let [^chars cs (.toCharArray (str s)) ; str is O(1) on strings. .toCharArray is 2 billion/second.
        n (int (count cs)) n0 (dec n)
       ^ints level (make-array Integer/TYPE n) ; not including the tokens.
       ^ints mode (make-array Integer/TYPE n)
       ^ints token (make-array Integer/TYPE n) ; includes macro chars.
       ^ints escape (make-array Integer/TYPE n)
       ^ints break (make-array Integer/TYPE n)
       btrue (boolean true) bfalse (boolean false)
       char-a (char \a)] ; 5x performance boost once warmed up just by doing this!
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
        ; Step 2: Escape # and ' macro chars embedded in symbols or keywords:
        (loop [ix (int 0) was-in-symwrd (int 0)] ;was-in-symwrd also 1 for the \char literals, but that doesn't affect us.
          (if (>= ix n) "Done"
            (do 
              (let [c (aget ^chars cs ix) ci (int c)
                    m (aget ^ints mode ix)]
                (if (and (= m 0) ; normal mode, not a quote or comment.
                    (= was-in-symwrd 1) ; Only if we were already in a symbol or keyword.
                    (or (= c \#) (= c \'))) ; only these two chars, not any other macro chars.
                  (aset ^ints escape ix 1))
                (recur (inc ix) (if (or (> m 0) (= (aget ^ints sym-kwd-stop ci) 1)) 0 ; Automatically leave
                                  (if (= was-in-symwrd 1) 1 ; Stay if the char lets us.
                                      (aget ^ints sym-kwd-start ci))))))))
        ; Step 3: the break arrays:
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
      ; Step 3: bridging the reader macro characters. 
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
                   (do (aset ^ints break (inc jx) 0) 
                       (aset ^ints token jx 1) (recur (inc jx)))))
               (recur hit-ix)) ; jump to the next time we need it.
               (recur (inc ix)))))
      {:level level :mode mode :token token :escape escape :break break}))

(defn basic-parse [s]
  "Like neutral-reader-macro-parse but adds :boost. :boost is an array that
   shows the effect of the macro characters, the final level when read into a string is level + boost.
   Boost, like level, also inclusive. Boost does NOT include #{hash set} or #regexp literals b/c they read as is.
   Boost DOES include meta-data, but meta-data is treated differently than macros down the road.
   In general these are not treated as macros even if they technically are.
   typically adda an extra few ns/char not including the neutral-reader-macro-parse (very small increase)."
  (let [parse0 (neutral-reader-macro-parse s)
        ^chars cs (.toCharArray (str s))
        ^ints level (:level parse0) ^ints mode (:mode parse0) ^ints token (:token parse0)
        ^ints escape (:escape parse0) ^ints break (:break parse0)
        n (int (count cs))
        ^ints boost (make-array Integer/TYPE n)]
    ; Increase the boost for reader macros:
    (loop [ix (int 0)]
      (if (= ix n) "Done"
        (let [c (aget ^chars cs ix)]
          (if (creader? c) ; outer loop to search for macro chars.
              (let [e (aget ^ints escape ix) 
                    m (aget ^ints mode ix)]
                (if (and (= e 0) (= m 0) ; Criteria to be a start of a reader macro.
                      ; The ~ in ~@ is not a seperate macro that increases the boost.
                      (or (not= c \@) (= ix 0) (not= (aget ^chars cs (dec ix)) \~) 
                        (not= (aget ^ints mode (dec ix)) 0))
                      ; same with a #' varquote:
                      (or (not= c \') (= ix 0) (not= (aget ^chars cs (dec ix)) \#) 
                        (not= (aget ^ints mode (dec ix)) 0))
                      ; Block hash-sets and regexps as well (they read homioiconicly):
                      (or (not= c \#) (>= ix (dec n)) (and (not= (aget ^chars cs (inc ix)) \{) (not= (aget ^chars cs (inc ix)) \"))
                        (not= (aget ^ints mode (inc ix)) 0))) 
                  ; Level boost step:
                  (let [t (aget ^ints token ix) l0 (aget ^ints level ix)] ;level of the ', etc char.
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
  ; Unpacks #(%1 %2 ...) functions, code is what goes inside. We can keep the % symbols, anon-fns don't nest.
  ; Code has already been read through the _read-string-pos.
  ; returns the :obj. Note: code is always a list at the outer level.
  (let [nargs (loop [acc 0 jx ix]
                (if (> jx last-ix) acc
                  (if (and (= (aget ^chars cs jx) \%) (= (aget ^ints mode jx) 0))
                      ; They limit it to 20 args, so our limit to 99 is fine:
                      (let [c1 (if (<= (inc jx) last-ix) (aget ^chars cs (inc jx)) (char \a))
                            c2 (if (<= (+ jx 2) last-ix) (aget ^chars cs (+ jx 2)) (char \a))
                            n (cond (and (cnumber? c2) (cnumber? c1)) (Integer/parseInt ^String (str c1 c2))
                                    (cnumber? c1) (Integer/parseInt ^String (str c1))
                                    :else 1)]
                        (recur (max acc n) (inc jx)))
                      (recur acc (inc jx)))))]
    ; fn and fn* seem equivalent. 
    ; set :begin to one more than :end on the 'fn and [args] so that it does not affect the string.
    ; careful to get the :obj nesting levels correct.
    (list {:obj 'fn :begin ix :end (dec ix)}
      {:obj (mapv #(hash-map :obj (symbol (str "%" %)) :begin ix :end (dec ix)) (mapv inc (range nargs))) :begin ix :end (dec ix)}
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
                                       (token-end token break kx)
                                     ; Special hash-sets: increase the index by one to get to the {.
                                     (let [start-of-hash-set? (and (< kx (dec last-jx)) (= (aget ^chars cs kx) \#) (= (aget ^chars cs (inc kx)) \{))]
                                       (level-boost-end cs mode level boost (if start-of-hash-set? (inc kx) kx))))
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
            (and (= c \#) (= c1 \=)) {:obj (list (w2 `read-eval) (rread (fun2) last-ix)) :begin ix :end last-ix}
            (and (= c \#) (= c1 \_)) {:obj (list (w2 `ignore) (rread (fun2) last-ix)) :begin ix :end last-ix}
            (and (= c \#) (= c1 \?)) {:obj (list (w2 `*reader-conditional) (rread (fun2) last-ix)) :begin ix :end last-ix}
            (= c \#) {:obj (_unpack-anon-fns (rread (fun1) last-ix) cs mode (fun1) last-ix) :begin ix :end last-ix}
            (= c \') {:obj (list (w1 'quote) (rread (fun1) last-ix)) :begin ix :end last-ix}
            (= c \@) {:obj (list (w1 `deref) (rread (fun1) last-ix)) :begin ix :end last-ix}
            (and (= c \~) (= c1 \@)) {:obj (list (w2 `*unquote-splicing) (rread (fun2) last-ix)) :begin ix :end last-ix}
            (= c \~) {:obj (list (w1 `unquote) (rread (fun1) last-ix)) :begin ix :end last-ix}
            (= c \`) {:obj (list (w1 `syntax-quote) (rread (fun1) last-ix)) :begin ix :end last-ix}
            ; Tempoararly add a clooj_coder_grammer_meta-i-fy symbol (this is the only way to get metadata when read with read-string):
            (= c \^) {:obj (list (w1 'clooj_coder_grammer_meta-i-fy) (rread (fun1) last-ix)) :begin ix :end last-ix}
            :else (throw (str "Unrecognized macro: " c c1)))
      ; opening chars => read the stuff inside.
      (cond (= c \() (update (readvf-box (inc ix) (l-end)) :obj #(apply list %))
            (= c \[) (readvf-box (inc ix) (l-end)) ; :obj is already a vector.
            (= c \{) (update (readvf-box (inc ix) (l-end)) :obj #(apply hash-map %)) ; # keys better be even.
            ; hash sets: may change the order, but :begin and :end is still preserved.
            (and (= c \#) (= c1 \{)) (update (readvf-box (+ ix 2) (dec last-ix)) :obj #(apply hash-set %))
            ; leaf level read-string (including regexps). They will ignore the boring stuff.
            :else (let [^chars csus (Arrays/copyOfRange ^chars cs ix (inc last-ix))
                        ^String sus (String. ^chars csus)]
                   {:obj (read-string sus) :begin ix :end last-ix})))))
(def macroy-stuff #{'deref `deref 'quote 'clojure.core/quote 'clooj.coder.grammer/syntax-quote 
                    'unquote `unquote}) 
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
        one-less (fn one-less [c] (let [c1 (if (coll? (:obj c)) (update c :obj #(collections/cmap :flatten one-less %)) c)
                                        c1 (if (:meta c) (update c1 :meta one-less) c1)
                                        c2 (if (:begin c1) (update c1 :begin dec) c1)]
                                    (if (:end c2) (update c2 :end #(dec (if (> % n) n %))) c2)))]
    {:begin 0 :end (dec n) :obj (mapv one-less (:obj code1))}))

(defn pos-to-blit-code [s c]
  "Converts a string and it's reads-string-pos format to a blitted format.
   The blitted format has :head :body and :tail as strings as well as :obj for the code object.
   :head and :tail are when the :obj has children; :body is for when it doesn't.
   the :head is typically an opening (, etc. The :tail often has whitespace, etc.
   The :body captures the entire code for the leaf levels."
  (let [; Extract the earliest beginning from a peice of code:
        hcoll? #(and (coll? %) (> (count %) 0)) ; non-mepty collections.
        mbegin #(if (:meta %) (:begin (:meta %)) (:begin %)) ; takes a single :begin :end , etc object.
        ; Maps and sets don't gaurentee ordering.
        deep-begin (fn deep-begin [c] ;c is a collection.
                     (let [fdeep (fn [v] (apply min (mapv #(if (hcoll? (:obj %)) (deep-begin (:obj %)) (mbegin %)) v)))] ; works on a vector.
                       (cond (set? c) (fdeep (into [] c))
                         (map? c) (fdeep (keys c)) ; keys before vals.
                         :else (mbegin (first c))))) ; non-set non-map are ordered.
        deep-end   (fn deep-end [c] ;c is a collection.
                     (let [fdeep (fn [v] (apply max (mapv #(if (hcoll? (:obj %)) (deep-end (:obj %)) (:end %)) v)))] ; works on a vector.
                       (cond (set? c) (fdeep (into [] c))
                         (map? c) (fdeep (vals c)) ; vals after keys. 
                         :else (:end (last c))))) ; non-set non-map are ordered.
        convert (fn convert [x] ; x has {:obj :begin: end}.
                   (let [begin (:begin x) end (:end x)
                         ; Recursive => empty body. Not recursive => empty head and tail.
                         col? (hcoll? (:obj x))
                         x (if (:meta x) (assoc x :meta (convert (:meta x))) x)]
                   (if col? 
                     (let [cbegin (deep-begin (:obj x)) cend (deep-end (:obj x))]
                       (assoc (update x :obj #(collections/cmap :flatten convert %)) ; recursive step.
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
                  (let [ob (if (hcoll? (:obj x)) (collections/cmap :flatten convert (:obj x)) (:obj x))]
                    (if (:meta x) (with-meta ob (meta-expand (convert (:meta x)))) ob)))]
    (convert c)))

(defn _de-dupe [bcode]
  "Removes duplicates in maps and sets that are the same when read into a string.
   The same value, when read into code, can have different values as a blitcode structure.
   This is not an issue if converting to vanilla code but WILL cause the compiler to complain
   when it is converted to a string and then read-string."
  (let [col? (and (coll? (:obj bcode)) (not (empty? (:obj bcode))))
        bcode1 (if col? (update bcode :obj #(collections/cmap :flatten _de-dupe %)) bcode)
        ob (:obj bcode1)]
    (if (and col? (or (map? ob) (set? ob))) ; only maps and sets, non-emptiness b/c we don't recursivly have code for empty colls.
        (let [kysb (collections/ckeys ob) kys (mapv blit-code-to-code kysb)
              s? (set? ob)]
          (if (= (count kys) (count (apply hash-set kys))) ; reduce the earthquake risk: do nothing if there is no duplication to.
              bcode1
            (assoc bcode1 :obj ; Use a first-one-standing convention because it's easier.
              (loop [acc (if s? #{} {}) used-up #{} ix 0] 
                  (if (= ix (count kys)) acc
                    (let [k (nth kys ix) kb (nth kysb ix)]
                      ; used-up means don't use:
                      (recur (if (contains? used-up k) acc (if s? (conj acc kb) (assoc acc kb (get ob kb))))
                        (conj used-up k) (inc ix)))))))) bcode1)))
(defn _collapse-meta [bcodev]
   "Puts the :meta before the given code structure.
    The code MUST be converted to sorted vectors first.
    The :meta object gets :meta? set to true so that it includes the ^ part.
    Note: the outerlevel of bcodev is never :meta bearing because we wrap in a vector."
  (let [o (:obj bcodev)]
    (if (and (coll? o) (not (empty? o)))
        (let [o (mapv _collapse-meta o) ; recursive part.
              m?s (mapv #(boolean (meta %)) o)]
          (assoc bcodev :obj
            (reduce #(if (:meta %2) (conj %1 (assoc (:meta %2) :meta? true) (dissoc %2 :meta)) 
                       (conj %1 %2)) [] o))) bcodev)))

(defn _end-in-comment? [s]
  "Whether s ends in a comment or not."
  (let [^ints m (:mode (neutral-reader-macro-parse s)) n (count s)] 
    (and (> n 0) (= (aget ^ints m (dec n)) 2))))
(defn _cold-safe-comment [s-cold]
  "Simalar to above. Faster, but only works on cold strings. 
   Also returns a safe string instead of a boolean."
  (let [^String s-cold (str s-cold) ^chars cs (.toCharArray s-cold) n (count s-cold)
        ending-comment?
        (loop [ix (int 0) in-comment (int 0)]
          (if (= ix n) (if (= in-comment 1) true false)
            (let [c (aget ^chars cs ix)]
              (recur (inc ix) (if (= c \newline) 0 (if (= c \;) 1 in-comment))))))]
    (if ending-comment? (str s-cold "\n") s-cold)))
(defn _hot-cold-pieces [^String s hot-first?]
  "Returns hot and cold pieces, hot bieng active code that is involved, cold bieng comments, whitespace, etc.
   Caution: Removing all the cold pieces may cause tokens to mush.
      The first peice is empty iff the string starts different than hot-first?."
  ; (defn piecy [s] (let [p (grammer/basic-parse s)] (grammer/_hot-cold-pieces s (:mode p) (:token p))))
  ; Due to the way this fn is used, we don' worry about the :breaks array.
  (let [n (count s) p (basic-parse s) ^ints mode (:mode p) ^ints token (:token p)]
    (if (= n 0) []
      (let [^chars cs (.toCharArray s)
            breaks (loop [acc [] ix 0 was-hot? hot-first?] ; a break = 5 means the 0-4 elements break from the 5+ elements.
                     (if (= ix n) acc
                       (let [mi (aget ^ints mode ix) ci (aget ^chars cs ix)
                            ti (aget ^ints token ix)
                            hot? (or (and (< mi 2) (= ti 1)) (and (= mi 0) (not (ccom-white? ci))))
                            b? (or (and was-hot? (not hot?)) (and (not was-hot?) hot?))] ; Break criterian: this char disagrees with what we had going in.   
                         (recur (if b? (conj acc ix) acc) (inc ix) (if b? (not was-hot?) was-hot?)))))]
        (mapv #(subs s %1 %2) (concat [0] breaks) (concat breaks [n]))))))

(defn _newline-power? [s]
  "Is there a newline before any active code? No active code and no newline = false, this convention (will it EVER be used) makes projection safer.
   If so there is no need to add a newline after the part before us."
  (let [pieces (_hot-cold-pieces s false)] ; The first piece is cold. Does it have a newline?
    (.contains (str (first pieces)) "\n")))
(defn _spacers [cv0]
   "Applies spacers to the code if nessessary. A space seperates some kinds of tokens, while a 
    newline is needed after comments so the next form does not get commented.
    Convert the code into VECTOR form first!"
  (let [leaf-only-body #(assoc % :head "" :tail "" :body (str (:head %) (:body %) (:tail %))); Lumps :head and :tail for any leaf objects into :body.
        col?f #(and (coll? (:obj %)) (not (empty? (:obj %))))
        leaf-space (fn leaf-space [c] ; recursive as well.
                     (assoc (if (col?f c) (update c :obj #(mapv _spacers %)) (leaf-only-body c))
                       ; the head is assumed to be cold-only.
                       :head (_cold-safe-comment (:head c))))        
        spacy (fn spacy [cv] ; Safe-to-end-in-comment means we don't have to add a newline.
                (let [col? (col?f cv)]
                  (if col?
                    (let [o (:obj cv) col?s (mapv col?f o) n (count o)

                          o (mapv spacy o) ; Recursive part.

                          ; does each child end in a comment:
                          comend?s (mapv #(if %2 (_end-in-comment? (:tail %1)) 
                                            (_end-in-comment? (:body %1))) 
                                    o col?s)
                          ; does each child have newline power:
                          nuline?s (mapv #(if %2 (_newline-power? (:head %1)) ; head not tail.
                                            (_newline-power? (:body %1))) 
                                    o col?s)
                          ; Simple fn to add a padding to the code:
                          add-s (fn [oi pad-str coli?] (if coli? (assoc oi :head (str (:head oi) pad-str))
                                                         (assoc oi :head "" :body (str (:head oi) (:body oi) pad-str))))
                          ; Comments: head-first-child inteference:
                          cv (if (_end-in-comment? (:head cv)) (update cv :head #(str % "\n")) cv) ; Macros that can't have ending wspace can't end in comments either. 

                          ; Comments: children-children inteference:
                          o (mapv #(if (or (= % (dec n)) (not (nth comend?s %)) (nth nuline?s (inc %)))
                                     (nth o %) (add-s (nth o %) "\n" (nth col?s %))) (range n))

                          ; Comments: last-child-tail inteference:
                          cv (if (last comend?s) (update cv :tail #(str "\n" %)) cv)

                          ; Do we have mushy starts and ends. Two mushy pieces stuck together means we need a space:
                          ; Calculated after updating o since we may have added newlines.
                          mush-start?s (mapv #(and (let [oi (:obj %)] (or (keyword? oi) (symbol? oi) (number? oi) (char? oi)))
                                                (let [pieces (_hot-cold-pieces (:body %) false)]
                                                  (= (count (first pieces)) 0))) o) ; Does not start cold => mushy potential.
                          mush-end?s (mapv #(and (let [oi (:obj %)] (or (keyword? oi) (symbol? oi) (number? oi) (char? oi)))
                                                (let [pieces (_hot-cold-pieces (:body %) false)]
                                                  (even? (count pieces)))) o) ; Ends hot => mushy potential.  
                       
                          ; Mushy: children-children inteference. This is the only mushy inteference we have to worry about.
                          o (mapv #(if (or (= % 0) (not (nth mush-end?s (dec %))) (not (nth mush-start?s %))) ; Check 4 mushy end b4 us + mushy start at us.
                                     (nth o %) (update (nth o %) :body (fn [bi] (str " " bi)))) (range n))]                         
                      (assoc cv :obj o)) cv)))]
    (spacy (leaf-space cv0)))) ; Leaf space first.
(def _long-to-short {'quote "'" `syntax-quote "`" `read-eval "#=" 'var "#'"
                     `ignore "#_" `*reader-conditional "#?" `*unquote-splicing "~@"
                     `deref "@" `unquote "~"})
(def _short-to-long (set/map-invert _long-to-short))
(defn _macro-unify [s]
  ; Gets the macro out of a string by removing unnessessary spaces or commas.
  ; Note: Never ~ @ with a space, they are two seperate macros; completly different objects in both vanilla read-string and our reads-string-blit.
  (let [s (string/replace s #"'[ ,]+" "'") s (string/replace s #"@[ ,]+" "@")
        s (string/replace s #"~[ ,]+" "~") s (string/replace s #"`[ ,]+" "`")] s))
(defn _anon-fn-macro? [c]
  "Detects whether c's obj is an anonomous function macro shorthand."
  (let [o (:obj c)]
    (if (collections/listoid? o) ; Function definitions are a list with the first element fn, second the args, etc.
      (let [fn-part (first o) arg-part (second o) get-s #(str (:head %) (:body %) (:tail %))]
        (and fn-part arg-part ; non-nil.
          (= (:obj fn-part) 'fn) ; a function call.
          (= (get-s fn-part) "") ; no actual head body or tail for the 'fn (it is implicitly expanded).
          (vector? (:obj arg-part)) ; The arguments are a vector.
          (let [na (count (:obj arg-part))] ;All arguments are % symbols and are in order, with no head body or tail.
            (and (= (mapv #(symbol (str "%" (inc %))) (range na)) (mapv :obj (:obj arg-part))) 
              (= (apply str (mapv get-s (:obj arg-part))) ""))))) false)))
(defn _local-project [c outer-level?]
  "Projectional editing on a local scale (does not worry about reader macros, thus the term local).
   The only non-local projection is that the non-body of an anon-function is not changed."
  (let [o (:obj c) s (str (:head c) (:body c) (:tail c))
        col? (and (coll? o) (not (empty? o)))
        anon-fn? (and col? (_anon-fn-macro? c))
        ; recursive part (anon-fns don't modify the first two sub-elements):
        c (cond anon-fn? ; Ensure that the :head, etc is right:
                         (let [colds (fn [s] (apply str (collections/evens (_hot-cold-pieces s false))))]
                           (assoc c :obj (apply list (first (:obj c)) (second (:obj c)) 
                                           (map #(_local-project % false) (rest (rest (:obj c)))))
                             ; Cold-hot pattern since # can't have any stuff after it
                             :head (str (_cold-safe-comment (colds (str (:head c) (:body c)))) "#") :body "" :tail (colds (:tail c))))
                col? (assoc c :obj (collections/cmap :flatten #(_local-project % false) o)) 
                :else c) 
        ; Recursive on the meta. Note the :meta? flag so we know to include the ^.
        c (if (:meta c) (update c :meta #(_local-project (assoc % :meta? true) false)) c)
        pieces (_hot-cold-pieces s false)
        open (if outer-level? "" (cond (map? o) "{" (set? o) "#{" (vector? o) "[" (coll? o) "(" :else ""))
        open (if (:meta? c) (str "^" open) open) ;metadata always just a ^ at the beginning.
        close (if outer-level? "" (cond (map? o) "}" (set? o) "}" (vector? o) "]" (coll? o) ")" :else ""))
        ; Equality check: does the string match what it should say?
        r=?-no-reader #(= (try (read-string (str "[" % "]")) (catch Exception e "oops")) [o])
        r=? (fn [s0] (let [s (_macro-unify s0)]
                         (if (and s (= (get _long-to-short o) s)) true (r=?-no-reader s))))]
    (cond ; Note: it is safe to put cold pieces back-to-back.
      anon-fn? c ; We already processed anonomous functions.
      (not col?) ; Elementary stuff: try to salvage the cold pieces.
      (if (r=? s) c ; no change if it it already agrees (preliminary step, reduces risk of error).
        (let [colds (collections/evensv pieces)
              ; Use reader macros when we were alread using them if possible:
              ; Note: only simple reader macros which go foo(x) => (bar (x)) make it here. 
              ; Stuff like anonomous function literals don't.
              ; It is possible (i.e. metadata or extra hot stuff) for hot to be correct but not = pr-str.
              hot (if (and (get _short-to-long (_macro-unify s)) (get _long-to-short o)) (get _long-to-short o) (pr-str o))
              hot (if (= (first hot) \^) (apply str (rest hot)) hot) ; remove metadata tags for comparison.
              hot (if (= (try (read-string hot) (catch Exception e ["oops"])) o) hot (pr-str o)) ; Match check.
              hot (if (:meta? c) (str "^" hot) hot)] ; meta-data always has a ^ at the beginning.
          (assoc c :head "" :tail "" :body ; The body contains the entire string.
            (if (or (not (coll? o)) (<= (count pieces) 3)) ; cold-hot-cold format in most cases.
                (apply str (_cold-safe-comment (first colds)) hot (rest colds))
                ; cold-open-cold-close-cold format, only for empty collections that have a space in them.
                (apply str (_cold-safe-comment (first colds)) open (_cold-safe-comment (second colds)) close (rest (rest colds)))))))
      :else ; non-empty collections excluding the anon-fn macro.
      ; The :head and :tail determines the collection. :body is treated as tail; :body is set to empty.
      ; Both are used as a cold hot cold pattern.
      ; Using macros affects the :tail (and sometimes? :head) but that it handled later.
      (let [hs (collections/evensv (_hot-cold-pieces (:head c) false)) 
            ts (collections/evensv (_hot-cold-pieces (str (:body c) (:tail c)) false))]
        (assoc c :head (apply str (_cold-safe-comment (first hs)) open (rest hs)) :body ""
          :tail (apply str (_cold-safe-comment (first ts)) close (rest ts))))))) 
(defn _macro-unbracket [c]
  "Detects reader macros and removes/modifies the head and tail.
   Use AFTER _local-project."
  (let [col? (and (coll? (:obj c)) (not (empty? (:obj c))))
        c (if col? (assoc c :obj (collections/cmap :flatten _macro-unbracket (:obj c))) c)
        hots (fn [s] (apply str (collections/evensv (_hot-cold-pieces s true)))); All hot stuff.
        colds (fn [s] (apply str (collections/evensv (_hot-cold-pieces s false)))); All cold stuff.
        ; Are we a macro char?
        mc? (fn [s] (let [cu (_macro-unify (hots s))] ; all hot pieces
                      (not (nil? (get _short-to-long cu)))))
        anon-fn? (and col? (_anon-fn-macro? c))]
    ; Macros always expand into a list of some sort (map literals are NOT treated as macros here).
    (if (and col? (collections/listoid? (:obj c)) ; Nessessary and sufficient criteria to be a reader macro.
          (or anon-fn?
            (let [o0 (:obj (first (:obj c))) s0 (#(str (:head %) (:body %) (:tail %)) (first (:obj c)))]
              (and (get _long-to-short o0) (mc? s0)))))
      ; Get rid of all hot stuff for the :head and :tail, in order to get rid of the brackets.
      ; :body has already been set to empty by the local projection.
      ; Exception: anonomous functions keep a # active.
      (assoc c :head (let [s0 (colds (:head c))] (if anon-fn? (str (_cold-safe-comment s0) "#") s0))
        :tail (colds (:tail c))) c)))
(defn _edit-project [c outer-level?] 
  "Modifies the head tail and body so that the string will match the :obj hirerchy. 
   EXCEPTION: does not add newlines or spaces in cases where code gets commented out or symbols jam toghether.
   This is the main projection function that gets the 'nearest' string to the blit code that matches the original string.
   Recursive. The outer level does not include the [ or ].
   Projection will be resonably-close to the original string if it is the wrong form,
   and should be EXACT when the string is the right form."
  (_macro-unbracket (_local-project c outer-level?)))
(defn _naive-str [x]
   "Naive conversion to string. Does not check for validity or anything."
  (let [hcoll? #(and (coll? %) (not (empty? %)))
        col? (hcoll? (:obj x))
        ob (if (and col? (map? (:obj x)) (:obj x)) (interleave (keys (:obj x)) (vals (:obj x))) (:obj x))]
     (str (if (:meta x) (_naive-str (:meta x)) "") ; meta before even the head. Note: meta data should have been collapsed already.
       (:head x) (if col? (apply str (mapv _naive-str (into [] ob))) "")
       (:body x) (:tail x))))
(defn blit-code-to-str [c & outer-level?]
  "Converts the code-with-string-blits back into a string. Used after the refactoring is done.
   In cases where pr-str is not esoteric: 
     Ensures that the code is valid code.
     (blit-code-to-str (reads-string-blit s)) = s, 
       unless s is not valid code to begin with or something goes wrong...
   outer-level? (default = true) => No need to match-type, as the outer level has no []"
  (let [; set ordering has a mind of it's own and even hash-maps still don't offically gaurentee sorting.
        outer-level? (if (< (count outer-level?) 1) true (first outer-level?))
        hcoll? #(and (coll? %) (not (empty? %)))
        begin-sort #(into [] (sort-by :begin %))
        ; Add a newline if the comment is at the end.
        dcom (fn [^String s k] 
               (if (nil? s) (throw (Exception. "Null string for dcom")))
               (let [k (if (creader? (first s)) 0 k)] ; Macro chars break this rule. 
                 (let [^String su (subs s k)]
                   (if (> (.lastIndexOf ^String su ";") ; lastIndexOf returns -1 if it fails.
                          (.lastIndexOf ^String su "\n")) (str s "\n") s))))
        ; The last character belonging to a token of a leaf-obj:
        get-k (fn [^String s leaf-obj]
                ; k is the index of the last token:
                ; It should always stop unless the :body is completly wrong.
                (if (= (count s) 0) 0
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
                    (if mp? ix0 (+ ix0 (count (pr-str leaf-obj)) -1)))))            
        to-vec (fn to-vec [x ol?] ; all non-empty collections become vectors and we sort by the :begin.
                 (let [ob (:obj x) col? (hcoll? ob)
                       ob (if col? (collections/cmap :flatten #(to-vec % false) ob) ob) ; recursive.
                       out (assoc x :obj (if (and col? (not (vector? ob)) (not (collections/listoid? ob)))
                               (begin-sort (into [] (if (map? ob) (concat (keys ob) (vals ob)) ob))) ob))]
                   (if (:meta x) (update out :meta #(to-vec % false)) out)))
         c (_de-dupe c) ; Remove duplicates for hash-sets and hash-maps. This ensures the output string doesn't have a duplicate key error.
         c (_edit-project c outer-level?) ; Project the string on the code, so it matches the code but tries to be close to the original string.                    
         c (to-vec c outer-level?) ; Make it easier to work with. _edit-project WILL fail since we force things to be vectors.
         c (_collapse-meta c) ; Put the metadata as normal data.
         c (_spacers c)] ; Add spaces and newlines if we absolutly have to.
    (_naive-str c)))

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