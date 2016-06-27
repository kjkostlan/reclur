; Abstractions of clojures syntax. In any non-lisp language this file would be at least 10 times more.
(ns clooj.coder.grammer (:require [clooj.coder.io :as io] [clooj.utils :as utils] [clojure.string :as string])
  (:import (clojure.lang Compiler)))
; TODO: what functions here, if any, belong in collections?
; i.e. ckeys is more of a collection tool than a syntax tool?

;CHAR_MAP DEMUNGE_MAP
; String conversion tools:

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

(defn cdissoc [c k]
  "Like dissoc but works on most collections preserving the type. Not lazy.
   WARNING: O(n) for vectors."
  (cond (nil? c) nil
    (vector? c) (into [] (vals (dissoc (zipmap (range (count c)) c) k)))
    (list? c) (apply list (dissoc (zipmap (range (count c)) c) k))
    (map? c) (dissoc c k)
    (set? c) (disj c k)
    (coll? c) (throw (Exception. (str "Unrecognized or unimplemented collection: " (type c))))
    :else (throw (Exception. "Not a clojure collection.")))) 

(defn ckeys [c]
  "Like keys but also works for vectors. Not lazy.
   Example: (keys [:a :b :c]) fails but (ckeys [:a :b :c]) gives [0 1 2]."
  (cond (nil? c) nil
    (sequential? c) (into [] (range (count c)))
    (map? c) (into [] (keys c))
    (set? c) (into [] c) ; sets can get themselves.
    (coll? c) (throw (Exception. (str "Unrecognized or unimplemented collection: " (type c))))
    :else (throw (Exception. "Not a clojure collection.")))) 

(defn cvals [c]  
  "Like ckeys but for values. Not lazy."
  (cond (nil? c) nil
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
  "like map but it preserves the type (duplicates in sets will collapse if mapped to the same thing).
   What we do in for a map? has three options:
      :entry => apply f to each entry of a map (as a 2-long vector) returns a 2-long vector or a 1-key map.
        args are passed as additional two-long vectors instead of single elements.
      :flatten => apply f to keys and then to vals independently. Combine these back into a map.
        args are similarly flattened.
      :keys => apply f to only the keys (if f creates duplicate keys the map gets shorter as the earlier values are discarded).
      :vals => apply f to only the vals (keys are unchanged). 
        1:1 aligned with each of cvals for each args.
  Args don't have to be the same type of code"
  (cond (nil? code) nil
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Code for parsing strings in a way that preseves the mapping ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; These macros only work when we define the correct characters.
(defmacro open? [c]
  `(or (= ~c \() (= ~c \{) (= ~c \[)))

(defmacro close? [c]
  `(or (= ~c \)) (= ~c \}) (= ~c \])))

(defmacro reader? [c] 
  "Rader macro chars. code/syntax quotes, # signs, and the like."
  `(or (= ~c \@) (= ~c \') (= ~c \`) (= ~c \~) (= ~c \#) (= ~c \^)))

(defmacro white? [c] 
  "Reader macro chars. code/syntax quotes, # signs, and the like.
   other characters such as ? are normal characters but can still be special during a reader macro.
   However they are ignored by this function."
  `(or (= ~c \space) (= ~c \newline) (= ~c \tab)))

;(println (macroexpand-1 '(close? c)))

;TODO: include:
;:level = indent level, inclusive. :mode = 0 for normal, 1 = quote, 2 = comment.

; High performance: 86 ns per character <- TODO: reverify optimize.
; (require '[clooj.java.file :as file]) (def s (file/load-textfile "./src/clooj/coder/grammer.clj"))
; (require '[clooj.coder.grammer :as grammer]) (grammer/basic-parse "| ( #(     '(y)     ))" false false)

(defn basic-parse [s add-one-for-tokens? ignore-meta?]
  "Reads a string into an {:levels and :mode}.
   :level is [I of how nested the character becomes, inclusive. 0 = indicates no nesting.
   :mode is [I of = 0 for normal, 1 = quote, 2 = comment. Inclusive (for comments includes ; and \newline)
   add-one-for-tokens? = do we add an extra level for tokens.
      false means  (inc x (+ 'y z))  true means (inc x (+ 'y z))
      becomes      1111111222332221             1222121232342321 for :level
      The 'y adds a level despite no explicit () because it expands to (quote y).
      Leading/trailing spaces would be 0 in this example
  erase-meta? = do we replace metadata with whitespace first so it becomes invisible:
      false means                                   (^int x)  true means (^int x)
      becomes (with add-one-for-tokens? = true)     12333121             11111121 for :level.
  Comments, \newline, and stuff inside of #_ are treated as spaces for :level
  Stuff in (comment ...) is NOT ignored since it's essentially a normal function.
  #? and #= are (for lack of a better rule) treated like normal reader macro.
    they WILL make it very difficult to track.
  Syntax-quote is treated like quote even though it can drastically alter stuff inside it."
  (let [;_ (/ 0) ;DEBUG: add-one-for-tokens? true
        ^chars cs (.toCharArray (str s)) ; str is O(1) on strings. .toCharArray is 2 billion/second.
        n (int (count cs))
        ^ints levels (make-array Integer/TYPE n) ; does NOT include the macro boost nor the tokens.
        ^ints boosts (make-array Integer/TYPE n) ; only includes the boosts.
        ^ints tokens (make-array Integer/TYPE n) ; tokens (0 = not inside a token, 1 = is). Tokens are normal variables.
        ; datums-stack[i] = x means that at i reader macro levels in the lowest indent level that's inside is x for whitespace.
        ; For almost all long strings we won't fill the array very far.
        ^ints datums-stack (make-array Integer/TYPE (inc n))
        ^ints boosts-stack (make-array Integer/TYPE (inc n))
        ^ints mode (make-array Integer/TYPE n)  
        em (boolean ignore-meta?)
        
        ;tweaks {} ; key => delta 
        ; They have trouble with primitive initializers with chars.
        ; This makes the code cleaner and slightly impacts performace.
        cspace (char \space) cescape (char \\)
        bfalse (boolean false) btrue (boolean true)]

    ; All these state variables are "as we approach this char", they don't include what this char will do:
       ; thus the next-xyz flags uasually depend on what this character is.
    ; lev = indent level, not including tokens.
    ; readmacro? = are we inside a reader macro (and before the token or opening ()?
    ; comment? = are we in a standard ; comment that ends after the newline.
    ; str? = are we in a string literal. escape? = escape the next character.
    ; macro-boost = how many extra levels are due to bieng inside a reader macro 
        ; (lev includes the boost, so we subtract the boost at the end)
    ; stack-ix: how many macro levels in we are. Example: 2 for the inner symbols in #(list '+ 'x '%).
    ; Note: this is BEFORE ignoring the #_ and if ignore-meta? the ^{:meta}.
    ; Note: cond CAN be extremly slow, adding about 100 ms for 20k chars!
    (loop [ix (int 0) lev (int 0) readmacro? (boolean false) comment? (boolean false)
           str? (boolean false) escape? (boolean false) macro-boost (int 0) stack-ix (int 0)]
      (if (= ix n) levels
        (let [c (aget ^chars cs ix)  ; ^char unnessessary.
              nescape (if escape? bfalse btrue) ; escaping breaks almost everything.
              _ (if (and (< n 200) (= (aget ^chars cs 0) \|)) (println "ix: " ix "c" c "macro-boost" macro-boost "readmacro?" readmacro? "lev: " lev "stack-ix: " stack-ix "datums:" (into [] datums-stack)))
              next-escape? (if (and (not comment?) (= c \\)) nescape bfalse) ; escape is a toggle-switch
              next-comment? (if str? bfalse (if comment? 
                               (if (= c \newline) bfalse btrue)
                               (and nescape (= c \;)))) ; entry to comment condition.
              ; Toggle whether we are inside a string or not:
              str-toggle? (if (and (not comment?) (= c \") nescape) btrue bfalse)
              next-str? (if str-toggle? (if str? bfalse btrue) str?)
              ]
          (aset ^ints mode ix (if (or str? next-str?) 1 (if (or comment? next-comment?) 2 0)))
          (if str? ; inside a string: only another " can end the string. We can't immedaitly jump to a comment.
            (do (aset ^ints levels ix lev) (aset ^ints tokens ix 1); strings are always in a token.
              (aset ^ints boosts ix macro-boost)
              (recur (inc ix) lev bfalse bfalse next-str? next-escape? macro-boost
                stack-ix))
            (if readmacro? ; inside a reader macro's HEAD (NOT including it's body).
                (let [; macro chars, except for the #, stack for the most part, but there are exceptions:
                      c0 (if (> ix 0) (aget ^chars cs (dec ix)) cspace) ; default if we aren't the first one.
                      block-stack? (or (and (= c0 \#) (= c \')) ; two characters become one function.
                                       (and (= c0 \~) (= c \@)))
                      openc? (open? c) closec? (close? c)
                      delta-macroboost (if (and (not block-stack?) (reader? c)) 1 0)
                      next-readmacro? (or (white? c) (reader? c) (and (= c0 \#) (or (= c \?) (= c \=) (= c \_))))
                      token (if (or (white? c) (open? c) (close? c) next-readmacro? (= c \;)) (int 0) (int 1)) ; tokens can appear at the end.
                      popup (if openc? 1 0) ; increase level for this char by one only.
                      delta-lev (+ (if openc? 1 0) (if closec? -1 0))
                      ] 
                  ; Lock in the final macro boost approapiate to the current stack-ix:
                  (if (not next-readmacro?)
                    (aset ^ints boosts-stack stack-ix (+ macro-boost delta-macroboost)))
                  (aset ^ints boosts ix (+ macro-boost delta-macroboost))
                  (aset ^ints levels ix (+ lev popup))
                  ; we ignore tokens when inside the macrohead, they don't become tokens upon read-string, (aset ^ints tokens ix 0) it's zero already.
                  (recur (inc ix) (+ lev delta-lev) next-readmacro?; no way to subtract levels when in reader macro mode.
                    next-comment? next-str? next-escape? (+ macro-boost delta-macroboost)
                    stack-ix)) ; comments within readmacros ARE allowed.
                ; the \newline ending all comments => we can't immediatly jump from a comment to a string, escaped char, etc etc.
                (if comment? (do (aset ^ints levels ix lev) ; token = 0 but it's 0 already.
                           (aset ^ints boosts ix macro-boost)
                           (recur (inc ix) lev readmacro? next-comment? ; readmacro? is fixed as is.
                             bfalse bfalse macro-boost stack-ix))
                  ; Most of the code:
                  (let [next-readmacro? (and nescape (boolean (reader? c))) ; induction into reader macros.
                        ; oops: tokens block any reader macros (it can't quite figure out that it's a boolean, 
                        ;  but macro entry chars are so rare the boxing is miniscule and boxing is not nearly as bad as refleciton):
                        next-readmacro? (boolean (if next-readmacro?
                                          (loop [iix (int (dec ix))] 
                                            (if (= iix -1) btrue
                                              (let [cii (aget ^chars cs iix)]
                                                (if (reader? cii)
                                                    (recur (dec iix))
                                                  (if (not (or (white? cii) (open? cii) (close? cii) (= cii \;)))
                                                    bfalse btrue))))) bfalse))
                        openc? (and nescape (open? c))
                        closec? (and nescape (close? c))
                        token (if (or (white? c) openc? closec? next-readmacro? (= c \;)) 0 1) ; tweaks the levels a bit.
                        ; this char is no longer in a reader-macro's body:
                        macro-datum (aget ^ints datums-stack stack-ix)
                        nolonger-macro? (or (< lev (dec macro-datum)) (and (< lev macro-datum) (= token 0)))
                        next-stack-ix (if next-readmacro? (inc stack-ix) 
                                             (if (= stack-ix 0) 0
                                               (if nolonger-macro? (dec stack-ix) stack-ix)))
                        ^int next-macroboost (if next-readmacro? (inc macro-boost) ; macroboosts add one from the get go.
                                          (if (= stack-ix next-stack-ix) macro-boost
                                            (aget ^ints boosts-stack (dec stack-ix)))) ;pull the last one from the stack.
                        popup (if openc? 1 0) 
                        delta-lev (+ (if openc? 1 0) (if closec? -1 0))] ; down one level (and subtract out the macro boost).

                     ; Entering a macro: set the datum to the lev (lev should = the next level as well).
                     ; Set the datum if we enter a macro: (inc lev since our level is not sufficient to have whitespace be the macro)
                     (if next-readmacro? (aset ^ints datums-stack (inc stack-ix) (inc lev)))
                     ; Decrease stack-ix by one if we leave a macro's tail:
                     (aset ^ints tokens ix token) 
                     ; Set the boosts accordingly:
                     (aset ^ints boosts ix next-macroboost)
                     (aset ^ints levels ix (+ lev popup))
                     (recur (inc ix) (+ lev delta-lev) next-readmacro? next-comment?
                       ; increase stack-ix if we enter a macro:
                       next-str? next-escape? next-macroboost next-stack-ix))))))))
    ; add the tokens:
    (if add-one-for-tokens?
      (dotimes [i (int n)]
        (aset ^ints levels i (+ (aget ^ints levels i) (aget ^ints tokens i)))))
    ; Add the boosts. TODO: make this optional. TODO: re-enable (debug)
    (dotimes [i (int n)]
      (aset ^ints levels i (+ (aget ^ints levels i) (aget ^ints boosts i))))
    ; Step 2: ignoring stuff. Let l be the level right before the # in #_.
    ;                         Start at # and flatten the array to l until we get back to l.
    ; Inert ^ and #_ will confuse this function BUT we still will get the right answer.
    (loop [ix (int 0) datum (int 2147483647)]
      (if (= ix n) levels
        (let [l (aget ^ints levels ix)]
          ;(println "ix: " ix "datum:" datum "level:" l "c: " (aget ^chars cs ix))
          (if (< datum l)
              (do (aset ^ints levels ix datum) (recur (inc ix) datum)) ; keep flattening.
              (let [c1 (if (< ix (dec n)) (aget ^chars cs (inc ix)) \space)
                    c2 (if (< ix (- n 2)) (aget ^chars cs (+ ix 2)) \space)]
                (if (or (and (= c1 \#) (= c2 \_)) (and em (= c1 \^)))
                    (recur (inc ix) l) ; start flattening.
                    (recur (inc ix) 2147483647))))))) ; don't flatten
    {:level levels :mode mode}))
