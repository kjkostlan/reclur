; Abstractions of clojures syntax. In any non-lisp language this file would be at least 10 times more.
; Also, these functions are useful for non-code (homoiconicity).
(ns clooj.coder.grammer (:require [clooj.collections :as collections] 
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

(defn code-to-str [c]
  "Code to string that preserves all metadata."
  (binding [*print-meta* true] (pr-str c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Useful for parsing strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  `(or (= ~c \space) (= ~c \newline) (= ~c \tab) (= ~c \return) (= ~c \formfeed)))

(defmacro ccom-white? [c] 
  "white or comma. This is the main function."
  `(or (= ~c \space) (= ~c \newline) (= ~c \tab) (= ~c \return) (= ~c \formfeed) (= ~c \,)))

(defmacro cnumber? [c]
  "0-9, no other symbols."
  `(or (= ~c \0) (= ~c \1) (= ~c \2) (= ~c \3) (= ~c \4) (= ~c \5) (= ~c \6) (= ~c \7) (= ~c \8) (= ~c \9)))

(def ^ints sym-kwd-start
  "Whether the character, as an int, can start either a symbol or a keyword.
   Add a check for : to differentiate keyword and symbol starts.
   Cast the chars to ints (casting is cheap).
   Includes / but for division but that can only be a symbol that standa alone."
  (let [^ints outs (make-array Integer/TYPE 65536)
        valid "-=qwertyuiopasdfghjklzxcvbnm./!$%&*_+QWERTYUIOP|ASDFGHJKL:ZXCVBNM<>?"
        n (count valid) i1 (int 1)] ; having fun with a tiny performace challange that isn't too important.
    (loop [ix (int 0)]
      (if (= ix n) outs
        (do (aset ^ints outs (int (nth valid ix)) i1) (recur (inc ix)))))
    (loop [ix (int 0)] ; control chars, etc.
      (if (= ix 32) outs
        (do (if (and (not= ix 8) (not= ix 9) (not= ix 10) (not= ix 12) (not= ix 13)) (aset ^ints outs ix i1))
            (recur (inc ix)))))
    (loop [ix (int 127)] ; all the bizarrre chars and unicode fun.
       (if (= ix 65536) outs
         (do (aset ^ints outs ix i1) (recur (inc ix)))))))

(def ^ints sym-kwd-stop
  "Whether the chars-as-ints stops symbols or keywords."
  (let [^ints outs (make-array Integer/TYPE 65536)
        valid "`~@^()\t[{]}\\;\"\n, "
        n (count valid)]
    (loop [ix (int 0)]
      (if (= ix n) outs
        (do (aset ^ints outs (int (nth valid ix)) 1) (recur (inc ix)))))))