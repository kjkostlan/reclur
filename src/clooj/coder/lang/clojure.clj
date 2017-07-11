; Clojure parser tools that can be plugged into rcode.
; Eventually (LONG way down the road) other languages will go in this folder.
  ; 0 = empty space, comments, and delimiters such as , in java, python, etc.
  ; 1 = symbols (basically the same in any language). Variables and stuff like = in other languages also.
  ; 2 = keywords (clojure keywords and reserved words like "class" in java, python, etc). Including the metadata keyword as :^
  ; 3 = literals (boolean, number, string, regexp, etc).
  ; 4 = opening ([{#{.
  ; 5 = closing }]).
  ; 6 = reader macros (does not include metadata).
   ; Supported token-match entity formats for the rest of the elements:
     ; Strings (forced to be as-is).
     ; head body: fixed head with body being a list of matching characters, arbitrary length.
       ; The longest string and/or head-body takes precedence (if there are no head-function matches).
     ; head function, fixed head and function is (fn [char-array ix-body-start length-of-array]) and returns the ix right after the end.
     ; For non-functions the longest match is used, the only thing that makes sense.
       ; functions with matching heads take priority, the longest matching head that is.
; TODO: we may be able to speedify if we lump like characters toghether, i.e. 0-9

; Useful repl functions:
;(clc)
;(defn deep-str [x] (mapv #(if (vector? %) (deep-str %) (:strings %)) x))
;(defn deep-ty [x] (mapv #(if (vector? %) (deep-ty %) (:type %)) x))
;(defn rl [] (require '[clooj.coder.lang.clojure :as clojurelang] :reload))
;(do (rl) (binding [*print-meta* true] (pr-str (:05meta-local-assigned (rcode/intermediate-states "^foo bar" (clojurelang/clang) #(conj % 1))))))

(ns clooj.coder.lang.clojure
 (:require [clojure.string :as string] [clojure.pprint :as pprint]
   [clooj.coder.rcode :as rcode] [clooj.collections :as collections]
   [clooj.coder.grammer :as grammer]))

; TODO: Many of these functions are useful for a wider scope than just clojure. Refactor them out. 
(def controls (apply str (mapv char (range 32))))
(defn s2s [x] (apply str x))
(defn escape-str [^chars cs ^chars cs0 ix-start n] 
  (loop [escape? false ix (int ix-start)]
    (if (>= ix n) n
      (let [c (aget ^chars cs ix)]
        (if (and (= escape? false) (= c \")) (inc ix)
          (recur (if (= c \\) (not escape?) false) (inc ix)))))))

(def ^:dynamic *resolve-in-ns* nil) ; the namespace to use for syntax quotes.

; Resolving a syntax quote given a namespace:
(defn syntax-resolve [x nms]
   "Resolves a syntax quote in a given namespace, storing the :pre-resolved value in the tokenK.
    It must work with mwrapped code (see rcode/mwrap, rcode/munwrap, rcode/mwrapped?).
    Unlike read-string (which is forced to destroy the `) we keep around a syntax quote so that there is no need to recursivly quote everything.
    However we still recursivly resolve symbols (also in metadata) unless we hit an ~ or ~@."
  (if nms
    (cond (and (coll? x) (let [x0 (first x)] (and (not= x0 `unquote) (not= x0 `unquote-splicing) (not= x0 'clojure.core/unquote-conditional) (not= x0 'clojure.core/unquote-conditional-splicing)))) 
      (collections/cmap :flatten #(syntax-resolve % nms) x)
      (symbol? x) (rcode/tmet x (if-let [y (grammer/var2sym (ns-resolve nms x))] y x) :pre-ns-resolve x)
      :else x) x))

(defn syntax-unresolve [x nms] 
  "Undoes the effect of syntax-resolve using the :resolved? metadata.
   Resolving is only undone if the pre-resolved symbol is a match.
   This doesn't care if we are still inside a syntax quote (caring or not caring only matters if the ` is changed, which is rare)."
  (throw (Exception. "TODO: implement clojure/syntax-unresolve")))

(def simple-reader-expands ; simple reader macros that are converted with just a substitution.
  (let [expv (mapv #(vector (keyword (first %)) (second %)) ; reader macros are tokenized as keywords.
              [["'" `quote] ["@" `deref] ["#'" `var] ["~?@" 'clojure.core/unquote-conditional-splicing]
               ["~?" 'clojure.core/unquote-conditional] ["#?" `reader-conditional]
               ["~@" `unquote-splicing] ["~" `unquote] ["#=" 'clojure.core/read-eval]])]
    (zipmap (mapv first expv) (mapv second expv))))
(def simple-reader-unexpands (zipmap (vals simple-reader-expands) (keys simple-reader-expands)))

(defn max% [x] ; the maximum % in a % argument.
  (cond (and (symbol? x) (= (str x) "%")) 1
    (and (symbol? x) (= (first (str x)) \%)) (Long/parseLong (subs (str x) 1)) ; parse the stuff after the %.
    (coll? x) (apply max (mapv max% (if (map? x) (concat (keys x) (vals x)) x))) ; recursive.
    :else 0))
(defn replace%-%1 [x] ; replaces all % with %1, sotring the old symbol so we can retrieve it on the way back.
  (cond (= x '%) (rcode/tmet x '%1 :%0 '%) (coll? x) (collections/cmap :flatten replace%-%1 x) :else x))

(declare jump-after-form) ; jump-after-form <-> token-matchers circular dependency.
(def tok-matchers 
  [^:picky [3 "false"] ^:picky [3 "true"] ^:picky [3 "nil"] ; vary few symbol-like literals.
   [0 " " " \n"] [0 ";" ".0e@#$%^()[]{}\\'`~?/+_"] [0 "\n"] ; spaces and comments (and the newline that ends all comments).
   [0 "#_" jump-after-form] ; This isn't really a reader macro since #_foo doesn't become [] it is just empty space.
   [3 "\\\\"] [3 "\\ "] [3 "\\\n"] [3 "\\("] [3 "\\)"] ; char literals as-is + unicode.
   [3 "\\["] [3 "\\]"] [3 "\\{"] [3 "\\}"] [3 "\\#"] ; character literals as-is.
   [3 "\\^"] [3 "\\~"] [3 "\\@"] [3 "\\`"] [3 "\\;"] ; character literals as-is.
   [3 "0" "0./e"] [3 "+0" "0./e"] ; numbers, including fractions decimals etc
   [3 "\"" escape-str] [3 "#\"" escape-str] [2 "^"] ; string literals, regexps, and metadata.
   [3 "\\" "e0+.?'#:"] ; char literals as-id and spelled out. The lowest priority for a \
   [4 "("] [4 "["] [4 "{"] [4 "#{"] [5 "}"] [5 ")"] [5 "]"] ; plenty of these in lisps. 
   [6 "#:" "e:'0._+?#'"] [6 "'"] [6 "@"] [6 "#'"] [6 "#?"] [6 "#="] [6 "#"] [6 "`"] [6 "~?@"] [6 "~@"] [6 "~?"] [6 "~"]  ; all the reader macros except #_.
   [2 ":" "e:'0._+?#'/"] [1 "e" "e:0._'+/?#'"] [1 "0" "e:0._'+/?#'"] [1 "." "e:0._'+/?#'"] [1 "_" "e:0._'+/?#'"] 
   [1 "+" "e:0._'+/?#'"] [1 "?" "e:0._'+/?#'"]]) ;kwds + syms, syms lowest priority

(defn jump-after-form [^chars cs ^chars cs0 ix-start n] ; jumps after the upcoming form, a single form bieng i.e. everything within ().
  (loop [ix ix-start level 0]
    (if (>= ix n) n
      (let [tok (rcode/next-token cs cs0 ix tok-matchers) ty (first tok) ix1 (second tok) ; ix is the end of the token.
            meta? (= (aget cs ix) \^)]
        (cond (and (= level 0) (not= ty 0) (not= ty 4) (not= ty 6) (not meta?)) ix1 ; standard stop.
          (and (= level 1) (= ty 5)) ix1 ; ending bracket.
          (or (= ty 0) (= ty 4) (> level 0)) (recur ix1 (+ level (cond (= ty 4) 1 (= ty 5) -1 :else 0))) ; nested OR didn't hit land yet.
          meta? (let [after-meta (jump-after-form cs cs0 ix1 n)] ; after all the metadata is done.
                  (jump-after-form cs cs0 after-meta n)) ; jump after the target of the metadata.
          (= ty 6) (jump-after-form cs cs0 ix1 n) ; reader macros jump to the next form.
          :else (throw (Exception. "Can this happen?")))))))

(defn readl-str [s]
  "Read leaf, nil if not leaf or invalid. Never throws an error."
  (try (let [v (read-string (str "[" s "\n]"))]
         (if (= (count v) 1) (first v))) (catch Exception e nil)))

(defn ensure-sep [s need-space?]
  "Projects s onto a string that is a valid spacer.
   need-space? means that s must contain non-zero whitespace."
  (cond (and (= (count s) 0) need-space?) " "
    (= (count (reduce #(.replace ^String %1 ^String (str %2) "") s " ,\n\t\r\n")) 0) s ; just blanck space.
    (= (count (try (read-string (str "[" s "]")) (catch Exception e [[][]]))) 0) s
    :else (apply str (mapv #(if (= % \ ) \ \,) s)))) ; basically complete failure, this is not a robust projection.

; This matches a very small superset of valid clojure syntax.
(deftype ClojureLang [] rcode/TwoWayParser
    (character-tokenize-groups [this]
      "Do the token breaks matter whether a char is an a,b, and c?
       Here we define groups of characters that behave the same syntactically.
       The token matchers will see this simplified version unless they have the metadata ^:picky."
      [["0123456789" "0"] ["abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ*!|<>&%=" "e"]
       [",\t\b" " "] ["\r\n" "\n"] ["-+" "+"]])

    (token-matchers [this]
      "Returns matching rules for tokenizing the string.
       Each token has a fixed or variable length.
       Earlier tokens take priority over later tokens when there are multible matches.
       There are three different formats supported, the ty bieng the type of token (space, symbol, literal, bracket, etc):
         [ty head] = the token must match the head literally.
         [ty head body] = head (can be empty) + a body that is the set of characters that allows us to continue a match.
         [ty head fn] = head (can be empty) + (fn [^chars cs ^chars cs0 ix-start n]) that returns the token's ending index (exclusive). -1 or nil = not a match."
       tok-matchers)

    (non-bracket-group [this x]
     "In java et al statements like a*b+c are grouped ((a*b)+c) and then (in fnfirst-order) rearranged.
      It must work with mwrapped code (see rcode/mwrap, rcode/munwrap, rcode/mwrapped?).
      It must preserve the metadata of each element of x."
      x)

    (meta-assign [this xs xt] 
      "Returns a vector 1:1 with xs and xt where each element is the element that x is (immediatly) a meta of, false or nil if not.
       The ^ part is also assigned.
       xs is the string representation of x, not inclusing any spaces. xt is the type (4.5 for vectors)."
       ; beware nested metadata such as (meta (:tag (meta (read-string "^^foo bar baz"))))
       ; beware multible metadata such as (meta (read-string "^foo ^:bar ^:baar baz"))
      (let [n (count xs) m?s (mapv #(= % "^") xs) err #(throw (Exception. "Metadata tag has nothing to attach to."))
            jump-after-meta (fn jump-after-meta [ix-caret] ; the non-meta that ix-caret goes to.
                              (cond (>= ix-caret (dec n)) (err) ; trying to attach off the end of the array.
                                (nth m?s (inc ix-caret)) ; nested metadata.
                                (let [ti (jump-after-meta (inc ix-caret))]
                                  (if (nth m?s (inc ti)) (jump-after-meta (inc ti)) (inc ti))) ; nested followed by multible vs just nested.
                                (nth m?s (+ ix-caret 2)) (jump-after-meta (+ ix-caret 2)) ; multible metadata = our jump is thier jump.
                                :else (+ ix-caret 2))) ; just plain old metadata.
            meta-jumps (reduce #(if (nth m?s %2) (assoc %1 %2 (jump-after-meta %2)) %1) {} (range n))
            set-chunk (fn [v lo hi x] (println "setting chunk: " lo hi) (reduce #(assoc %1 %2 x) v (range lo hi)))] ; slice notation.
        ; This will set the outer meta layers first so inner layers won't be overwritten:
        (reduce #(set-chunk %1 (first %2) (second %2) (second %2))
          (into [] (repeat n false)) (into (sorted-map) meta-jumps))))

    (type-of-group [this outer-strings x]
      "x is our token collection with :strings and :type.
       we return :vector :map :list or :set.
       Like most of these functions this only needs to operate on the leafs."
      (let [opener (first outer-strings)]
        (cond (= opener "(") :list (= opener "{") :map (= opener "#{") :set :else :vector)))

    (leaf-parse [this s s-sp ty] 
      "Returns the value of the token whose string is s and is followed by space s-sp.
       ty is the type of tshe token (symbol, etc)."
      (if (or (= ty 1) (and (= ty 2) (= (first s) \:)) (= ty 3))
        (try (read-string s) (catch Exception e (throw (Exception. (str "Token not valid in clojure as a literal, keyword, or symbol: " s)))))
        (keyword s))) ; store ^, etc as keywords.

    (meta-parse [this x] 
      "Takes the metadata-as-vector and converts it into a map.
       ^foo bar is given to us as [:^ foo], with no bar (as bar is not part of the metadata).
      It must work with mwrapped code (see rcode/mwrap, rcode/munwrap, rcode/mwrapped?)."
      ; Three types of metadata: ^{ ... } keep as is.
      ; ^foo => {:tag foo} ^:foo {:foo true}
      (let [err (fn [] (throw (Exception. "Metadata is invalid format for clojure (error may be in this file or in the code given).")))
            xu (mapv rcode/munwrap x) n (count x)]
        (mapv #(if (not= (= %1 (keyword "^")) (even? %2)) (err)) xu (range)) ; must be alternating ^'s and stuff.       
        (loop [acc {} ix 1] 
          (if (= ix n) (err))
          (if (= ix (inc n)) acc
            (let [xi (nth x ix) xui (nth xu ix)]
              (recur (cond (map? xui) (merge acc xi)
                       (keyword? xui) (assoc acc xi true)
                       :else (assoc acc :tag xi))
                (+ ix 2)))))))

    (readermacro-apply [this x]
      "Takes in two-element lists such as ({:mapwrapK :'} {:mapwrapK :bar})
       and applies the reader macros.
      It must work with mwrapped code (see rcode/mwrap, rcode/rmunwrap, rcode/mwrapped?).
      The only non-simple reader macros are currently  #:***  #  `    "
      (let [rm (rcode/munwrap (first x))
            body (second x)]
        (if-let [simple (get simple-reader-expands rm)] (list simple body)
          (cond (= rm (keyword "`"))
            (list 'clojure.core/syntax-quote (syntax-resolve body *resolve-in-ns*)) ; almost simple, just need to qualify the symbols.
            (= rm (keyword "#")) ; Make sure this common macro works!
            (replace%-%1
              (list `fn (let [maxp (max% body)]
                          (if (= maxp 0) []
                            (mapv #(symbol (str "%" (inc %))) (range (max% body))))) body))
            (= (subs (str rm "   ") 0 3) ":#:") ; 1.9 clojure feature still in alpha as of July 6 2017. Currently we don't support #::
            (let [need-qual? #(if (or (symbol? %) (keyword? %)) ; qualification (use after rcode/munwrap).
                                (let [^String s (str %)] (or (not (.contains s "/")) (.contains s "_/"))) false)
                  head (keyword (subs (str rm) 3)) no_ #(.replace ^String (.replace ^String % "_/" "") "/" "")
                  ks (mapv #(if (not (need-qual? %)) %
                              (if (keyword? %) (keyword (str (subs (str head) 1) "/" (subs (no_ (str %)) 1))) 
                                (symbol (str head "/" (no_ (str %))))))  
                       (mapv rcode/munwrap (keys body)))]
               (with-meta (zipmap (mapv #(with-meta (rcode/mwrap %1) (meta %2)) ks (keys body)) (vals body))
                 (meta (second body)))) ; the second element of the body (i.e. the map) should have enough metadata.
            :else (throw (Exception. (str "This reader macro wasn't included: " (subs (str rm) 1))))))))

    (fnfirst-order [this x]
      "Returns the order that puts the function first (i.e. prefix).
       In languages like java (a * b) is reordered as (* a b), for an ordering of [1 0 2]"
      (range (count x)))

    (fnfirst-unorder [this x]
      "The inverse of fnfirst-order."
      (range (count x)))

    (readermacro-unapply [this x pre-h pre-t]
     ;(syntax-unresolve (throw (Exception. "TODO")))
      (throw (Exception. "TODO")))

    (meta-unparse [this x r]
      "Projects the unparsed metadata r onto the parsed (and possibly processed) metadata x.
       The result read-strings into x but resembles r as closely as possible."
 (throw (Exception. "TODO")))

    (non-bracket-ungroup [this x]
      "Reverse of non-bracket group for java et al. Everything with the same :unique-non-bracket-group in it's tokenK
       should ideally become regrouped."
      x)

    (leaf-project [this x r] 
      "Projects r onto x. x is the leaf value that we must take on.
       r has two elements, the first is the string that represents the token, the second is the space after said string.
       The output should have two elements; typically we don't project the space element here, instead using coll-project"
      (let [s0 (first r)]
        [(if (and (number? x) (= (readl-str s0) x)) s0 (pr-str x)) (second r)]))

    (coll-project [this x0 x0-tstrs x outer-strs meta?s ty outer-level?]
      "Collection projection (ensuring the right kind of spacers between tokens).
       We already un extra-grouped the collections.
       x0, x0-tstrs, x, and meta?s are 1:1 arrays.
       x0 is the pre-unparsed value with meta-data depacked. It contains symbols, etc.
       x0-tstrs are the token strings cooresponding to each element of x0. Either 2 or 4 elements.
       x has all of it's children un coll-parsed, and we need to modify x and add the outer-strs so that x is correct.
          (we only need to worry about this level or maybe one level below for array literals in java since this function is called at all levels).
       meta?s is true for elements that come from user metadata at the top level of x0.
       ty = :vector, :map, or :list.
       outer-level? is true iff we we have no parent, telling us to omit the brackets.
       We simply return the strings bundled into vectors (the modified x)."
      ;(println "coll project thingies:" (pr-str x0) (pr-str x0-tstrs) (pr-str x) (pr-str outer-strs) (pr-str meta?s) ty outer-level?)
     (let [need-sp? #(not (or (coll? %1) (string? %1) (coll? %2) (string? %2))) ; do we need a space in between these two elements?
           x (mapv (fn [xi x0i x0i-next] (update xi (dec (count xi)) #(ensure-sep % (need-sp? x0i x0i-next)))) 
               x x0 (concat (rest x0) [""])) ; adjust spacers (the last element of each x), the closing ] doesn't need a space so is equivalent to a string.
           outer-strs (assoc outer-strs 0 (cond outer-level? "" (= ty :vector) "[" (= ty :map) "{" (= ty :set) "#{" :else "(")) ; opening bracket.
           outer-strs (assoc outer-strs 2 (cond outer-level? "" (= ty :vector) "]" (= ty :map) "}" (= ty :set) "}" :else ")")) ; closing bracket.
           outer-strs (update outer-strs 1 #(ensure-sep % false)) ; space between us and first element.
           outer-strs (update outer-strs 3 #(ensure-sep % false))] ; space after end. Parent collections may later force this to be a spacer.
       (into [] (concat [(first outer-strs) (second outer-strs) x (nth outer-strs 2) (nth outer-strs 3)])))))


;;;;;;;;;;; Testing the parser below ;;;;;;;;;;;;

(defn clang [] (ClojureLang.))


;(require '[clooj.coder.repl :as repl]) (repl/clc)


;(prn 

;(rcode/intermediate-states "foo bar baz" clang #(conj % 1))

;)


