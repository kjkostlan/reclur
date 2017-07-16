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
; It is useful to resolve 

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
;(def ^:dynamic *error-on-horrible-code?* false) ; TODO: strict 1:1 reading that may throw errors in vary-badly written code (i.e. metadata duplicate keys) that would otherwise not be perfectly reversable. Mainly used for testing.

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
  (if nms
    (cond (and (coll? x) (let [x0 (first x)] (and (not= (rcode/munwrap x0) (keyword "~")) (not= (rcode/munwrap x0) (keyword "~@")) (not= (rcode/munwrap x0) (keyword "~?@")) (not= x0 `unquote) (not= x0 `unquote-splicing) (not= x0 'clojure.core/unquote-conditional) (not= x0 'clojure.core/unquote-conditional-splicing)))) 
      (collections/cmap :flatten #(syntax-unresolve % nms) x)
      (and (symbol? x) (.contains ^String (str x) "/")) 
      (if-let [s (:pre-ns-resolve (rcode/tokenK (meta x)))] 
        (with-meta (symbol (string/replace (str x) #"[^\/]*\/" ""))
          (update (meta x) rcode/tokenK #(dissoc % :pre-ns-resolve))) x)
      :else x) x))

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
(defn un-replace%-%1 [x] ; replaces all %1 with % if it was the original symbol.
  (cond (and (= x '%1) (= (:%0 (rcode/tokenK (meta x))) '%)) 
    (with-meta '% (update (meta x) rcode/tokenK #(dissoc % :%0))) 
    (coll? x) (collections/cmap :flatten un-replace%-%1 x) :else x))

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
            set-chunk (fn [v lo hi x] (reduce #(assoc %1 %2 x) v (range lo hi)))] ; slice notation.
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
      "Takes in two-element lists such as ({:mapwrapK :'} bar), note the keyword on the reader macro.
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
            (let [need-qual? #(if (or (symbol? %) (keyword? %)) ; qualification (use after rcode/munwrap) including erasuer of the _/.
                                (let [^String s (str %)] (or (not (.contains s "/")) (.contains s "_/"))) false)
                  head (subs (str rm) 3) 
                  ks (mapv #(if (not (need-qual? (rcode/munwrap %))) %
                              (with-meta 
                                (let [ku (rcode/munwrap %)
                                      ^String s (if (keyword? ku) (subs (str ku) 1) (str ku))
                                      pieces (string/split s #"\/")
                                      s1 (cond (not (.contains s "/")) (str head "/" s)
                                           (= (first pieces) "_") (subs s 2) :else s)]
                                  (if (keyword? ku) (rcode/mwrap (keyword s1)) (symbol s1)))
                                (assoc-in (meta %) [rcode/tokenK :map-ns-pre-rmacro] (rcode/munwrap %))))  
                       (keys body))]
               (with-meta (zipmap ks (vals body))
                 (meta body))) ; the second element of the body (i.e. the map) should have enough metadata.
            :else (throw (Exception. (str "This reader macro wasn't included: " (subs (str rm) 1))))))))

    (fnfirst-order [this x]
      "Returns the order that puts the function first (i.e. prefix).
       In languages like java (a * b) is reordered as (* a b), for an ordering of [1 0 2]"
      (range (count x)))

    (fnfirst-unorder [this x]
      "The inverse of fnfirst-order."
      (range (count x)))

    (readermacro-unapply [this x pre-h pre-t]
      "Un-applies the reader-macros, but doesn't splice the lists just yet. 
       It must work with mwrapped code (see rcode/mwrap, rcode/rmunwrap, rcode/mwrapped?).
       x is the expanded reader macro, pre-h is the readermacro itself before applying the macro, pre-t is the stuff the reader macro acts on."
      ;(println "read macro unapply: " x pre-h pre-t) 
      (let [;simple-reader-expands, simple-reader-unexpands.
            a (rcode/munwrap pre-h) b (rcode/munwrap (first x)) ; before and after reader macro values, kind of.
            a->b (get simple-reader-expands a) b->a (get simple-reader-unexpands b)]
          (cond (and a->b b->a) ; both are simple reader macros, may or may not be the same reader macro.
            (with-meta (collections/lassoc x 0 (rcode/with-rcode-meta (rcode/mwrap b->a) (meta pre-h))) (meta x)) ; pre-h's idioms with b->a as the new value.
            (and (or (= (first x) `fn) (= (first x) 'fn)) (= a (keyword "#")) ; sign of an expanded #()
              (= (count x) 3) (vector? (second x)) (list? (nth x 2)) ; valid 3 element list vector list strucure
              (= (second x) (mapv #(symbol (str "%" (inc %))) (range (count (second x))))) ; valid function args.
              (not (.contains ^String (binding [*print-meta* true] (pr-str (nth x 2))) (pr-str (rcode/mwrap (keyword "#")))))) ; no internal nested #().
            (with-meta (list pre-h (un-replace%-%1 (nth x 2))) (meta x))
            (and (= (first x) 'clojure.core/syntax-quote) (= a (keyword "`")))
            (with-meta (apply list pre-h (syntax-unresolve (rest x) *resolve-in-ns*)) (meta x))
            (and (map? x) (.contains ^String (str a) "#:") (> (count (str a)) 3))
            (with-meta 
              (list pre-h
                (with-meta ; idiomatic formatting ONLY at the inside level (user metadata attached to us must be reattached to the pre-rmacro list, read-string will error if the ^ is between the reader macro and map).
                  (zipmap (mapv #(if-let [y (:map-ns-pre-rmacro (rcode/tokenK (meta %)))]
                                   (let [^String s (str (rcode/munwrap %)) pieces (string/split s #"\/")
                                         h (first pieces) h (if (= (first h) \:) (subs h 1) h)
                                         h0 (subs (str a) 3) t (apply str (interpose "/" (rest pieces)))
                                         s1 (cond (< (count pieces) 2) (str "_/" s) (= h h0) t :else s)]
                                     (with-meta (if (symbol? %) (symbol s1) (rcode/mwrap (keyword s1))) (meta %))) %) (keys x)) (vals x)) 
                  {rcode/tokenK {:strings (if-let [y (:strings (rcode/tokenK (meta x)))] y [""""""""])}})) (meta x))
            :else x))) ; do nothing b/c a change rendered the reader-macro irreversable.

    (meta-unparse [this x r]
      "Projects the unparsed metadata r onto the parsed (and possibly processed) metadata x.
       The result must be a vector that meta-parses to x (including user metadata of x) but resembles r as closely as possible.
       x,r, and our modified x are mwrapped code (see rcode/mwrap, rcode/rmunwrap, rcode/mwrapped?)."
      (let [m-hat (rcode/mwrap (keyword "^"))
            r-hss (mapv #(if (map? %) (apply hash-set (keys %))) r) ; special hash-set version.
            kys (into [] (keys x))
            ; convert elements of r to position, including within maps within r. 
            irm (reduce #(let [ri (nth r %2)]
                           (if (map? (rcode/munwrap ri)) (merge %1 (zipmap (keys ri) (repeat %2))) ; map within r.
                             (assoc %1 ri %2))) {} (range (dec (count r)) 0 -1)) ; In conflicts (i.e. due to sloppy coding) earlier elements supercede earlier ones.
            hat?s (mapv #(= % m-hat) r)
            map?s (mapv #(map? (rcode/munwrap %)) r)

            ; ix&simp?s = for pairs of r, [the matching position for each element in x (nil fails), whether we are a simplification].
            ; A match doesn't always mean that the key nor value is identical (this must be checked seperatly). It DOES mean the cooresponding meta tag can be used.
            ix&simpl?s (mapv #(if (and (= (rcode/munwrap %1) :tag) (not (map? (rcode/munwrap %2)))) ; :tag's are special, they don't put the key into r when in simplified mode (but they can't be maps in simplified mode).
                               (if-let [ix (get irm %2)] [ix true] [nil nil]) ; simplify tag case.
                               (if-let [ix (get irm %1)] 
                                 [ix (and (= (rcode/munwrap %2) true) (keyword? (rcode/munwrap %1)) (keyword? (rcode/munwrap (get r ix))))] [nil nil])) (keys x) (vals x))
            x-want-destinations (mapv first ix&simpl?s) simplified?s (mapv #(boolean (second %)) ix&simpl?s) ; destinations bieng one after a ^.
            max-ix0 (apply max 0 (filterv identity x-want-destinations))

            map-ix (if-let [y (first (filterv #(nth map?s %) (range (count map?s))))] (if (<= y max-ix0) y (inc max-ix0)) (inc max-ix0)) ; non-simplified keys go here, unlimited capacity.

            x-destinations (mapv #(if % % map-ix) x-want-destinations) ; where x actually goes.
            max-ix (apply max x-destinations) 

            ; Put the metadatas back in the loop, putting mis-fits in the first map or making a map if one doesn't exist.
            ; Use the elements of r as much as possible for the idiomatic metadata but keep x's user-meta.
            destinations (apply hash-set x-destinations)
            vm (zipmap destinations ; start with the m-hat (with metadata maybe) for each destination
                 (mapv #(vector (rcode/with-rcode-meta m-hat (meta (if (get hat?s (dec %)) (nth r (dec %)) {})))) destinations)) ; the hat is always one index earlier than the x's elements, keep it's idiomatic data.
            v1m (reduce ; indexes are locations where each x goes.
                  (fn [acc ix] 
                    (let [rix (nth x-destinations ix) simp? (nth simplified?s ix) ; simp? only t
                          xk (nth kys ix) xv (get x xk)
                          mp? (not simp?) ; maps mean dig deeper to get the idiomatic metadata.
                          ; idiomatic meta source for the key and value.
                          idiok (if mp? (get (get r-hss rix) xk) (get r rix)) ; get from a hash-set!? it's the metadata we want.
                          idiov (if mp? (get (get r rix) xk) (get r rix)) 
                          ; k and val are concatinated toghether onto the end of rix or map-ix on the v1m.
                          tag? (= (rcode/munwrap xk) :tag)
                          k (if (and simp? tag?) [] ; special case of no key (empty vec if we are a tag AND simplified
                              [(rcode/with-rcode-meta xk (if-let [m (meta idiok)] m {}))]) ;the [key]
                          v (if (and simp? (not tag?)) []
                              [(rcode/with-rcode-meta xv (if-let [m (meta idiov)] m {}))])] ;the [val]
                    (update acc rix #(apply conj % (concat k v)))))
                  vm (range (count x)))
             v1m (into (sorted-map) v1m)
             v2 (mapv #(if (= (count %2) 2) %2 ; package maps in non-simplified cases, again using the idiomadic metadata 
                         (rcode/with-rcode-meta (vector (first %2) (apply hash-map (rest %2))) (meta (if-let [y (get r %1)] y [])))) 
                  (keys v1m) (vals v1m)) 
             v3 (into [] (apply concat v2))] v3))

    (non-bracket-ungroup [this x]
      "Reverse of non-bracket group for java et al. Everything with the same :unique-non-bracket-group in it's tokenK
       should ideally become regrouped."
      x)

    (leaf-project [this x r rmacro?] 
      "Projects r onto x. x is the leaf value that we must take on.
       r has two elements, the first is the string that represents the token, the second is the space after said string.
       The output should have two string elements; typically we don't project the space element here, instead using coll-project."
      (let [s0 (first r)]
        [(cond (= x (keyword "^")) "^" ; metatags are keywords.
           rmacro? (subs (str x) 1) ; rmacro?s are held as keywords, remove the leading :
           (and (number? x) (= (readl-str s0) x)) s0 
           :else (pr-str x)) (second r)]))

    (coll-project [this x0 x0-tstrs x outer-strs meta?s ty outer-level?]
      "Collection projection (ensuring the right kind of spacers between tokens).
       We already un extra-grouped the collections.
       x0, x0-tstrs, x, and meta?s are 1:1 arrays.
       x0 is the pre-un-collparsed version of x with meta-data depacked. It contains symbols, mapwrapped stuff, etc. rather than strings.
       x0-tstrs are the token strings cooresponding to each element of x0. Either 2 or 4 elements.
       x has all of it's children un coll-parsed and is leaf-parsed. We need to modify x and add the outer-strs so that x has correct brackets (or lack therof) and spaces between each token.
          (we only need to worry about this level or maybe one level below for array literals in java since this function is called at all levels).
       meta?s is true for elements that come from user metadata at the top level of x0.
       ty = type of collection x represents :vector, :map, or :list.
       outer-level? is true iff we we have no parent, telling us to omit the brackets.
       We simply return the strings bundled into vectors (the modified x)."
      ;(println "coll project thingies:" (pr-str x0) (pr-str x0-tstrs) (pr-str x) (pr-str outer-strs) (pr-str meta?s) ty outer-level?)
     (let [rmacros #{"'" "~" "`" "#" "@" "#'" "#=" "#?" "#?@" "~@"} ; not including the #:foo format, but that always preceeds a collection so it will not need a space.
           rmacro-intefere-pairs #{["~" "@"] ["#?" "@"] ["#" "'"]} ; must put a space between these. The #' inteference isn't really needed as it doesn't make valid code...
           rmacro-compat? #(and (or (get rmacros %1) (get rmacros %2)) (not (get rmacro-intefere-pairs [%1 %2]))) ; we can tell rmacroness from the first element of each x (which is a string).
           need-sp? #(not (or (coll? %1) (string? %1) (coll? %2) (string? %2) (= %3 "^") (= %4 "^") (rmacro-compat? %3 %4))) ; do we need a space in between these two elements?
           x (mapv (fn [xi x0i xi-next x0i-next] (update xi (dec (count xi)) #(ensure-sep % (need-sp? x0i x0i-next (first xi) (first xi-next))))) ; update the last string.
               x x0 (concat (rest x) [""]) (concat (rest x0) [""])) ; adjust spacers (the last element of each x), the closing ] doesn't need a space so is equivalent to a string.
           outer-strs (assoc outer-strs 0 (cond outer-level? "" (= ty :vector) "[" (= ty :map) "{" (= ty :set) "#{" :else "(")) ; opening bracket.
           outer-strs (assoc outer-strs 2 (cond outer-level? "" (= ty :vector) "]" (= ty :map) "}" (= ty :set) "}" :else ")")) ; closing bracket.
           outer-strs (update outer-strs 1 #(ensure-sep % false)) ; space between us and first element.
           outer-strs (update outer-strs 3 #(ensure-sep % false))] ; space after end. Parent collections may later force this to be a spacer.
       (into [] (concat [(first outer-strs) (second outer-strs) x (nth outer-strs 2) (nth outer-strs 3)])))))

(defn clang [] (ClojureLang.))