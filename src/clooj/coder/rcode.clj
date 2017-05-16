; Reads the string into a form that stores idiomatic data (comments, readmacros, etc) into the metadata.
; Due to this bieng loss-less, we can't run read-eval, etc. 
; Used for refactoring: run functions on "normal" code, and don't lose the idioms.

(ns clooj.coder.rcode
 (:require [clojure.string :as string] [clooj.coder.grammer :as grammer]
           [clooj.collections :as collections] [clojure.pprint :as pprint]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Description of projection workflow:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Integer format (1:1 with the character array):
  ; 0 = empty space, comments, and delimiters such as , in java, python, etc.
  ; 1 = symbols (basically the same in any language).
  ; 2 = keywords (clojure keywords and reserved words like "class" or "=" in java, python, etc).
  ; 3 = literals (boolean, number, string, regexp, etc).
  ; 4 = opening ([{#{.
  ; 5 = closing }]).
  ; 6 = reader macros.
  ; 7 = placeholder fill tokens.
; NEED fast-inter-depth field in the language for code hi-lighting.

;;;; Order of operations and needed vars in the lang:

; parse the string into a stream of tokens, alternating fill space fill space ... fill (some tokens may be empty to enforce this parity).
  ; each token is {:strings :value :type}, :value starts non-existant.
  ; NEED token-matchers = vector format, the first element is the token type number.
   ; Supported token-match entity formats for the rest of the elements:
     ; Strings (forced to be as-is).
     ; head body: fixed head with body being a list of matching characters, arbitrary length.
       ; The longest string and/or head-body takes precedence (if there are no head-function matches).
     ; head function, fixed head and function is (fn [char-array ix-body-start length-of-array]) and returns the ix right after the end.
     ; For non-functions the longest match is used, the only thing that makes sense.
       ; functions with matching heads take priority, the longest matching head that is.

; Fill the :value
  ; NEED token-parse: returns actual value of a token when given a token-as-string (i.e. read-string for clojure).

; group tokens based on syntactical (i.e. bracket) level, does not include non-bracket-based groupings.
  ; No need for any language-specific code.

; perform extra grouping not caught by the brackets (i.e. whitespace in python).
  ; NEED non-bracket-group: group any sequence of tokens or groups thereof by non-bracket stuff.
    ; Called recursively on all bracket groups, reducing the need for this fn being recursive.
    ; gets a vector and returns a vector or vectors within vectors, etc.

; Perform assigning based on meta-data.
  ; NEED meta-assign: 
    ;gets a vector of tokens/token groups, some tokens are as actual value and map-expanded, as detailed before.
    ;returns a vector of integer assignments that is 1:1 with the outer level, -1 means don't assign as metadata.
    ; remember we are still in token land so spaces are interposed.
  ; adds the :meta-of field for cases where it's not -1.

; coll-parse (the BIGGIE in the forward direction).
  ; Converts our nested vectors of tokens into the actual collections, and tokens into thier :values.
    ; Reader macro tokens get a map-wrap syntax unique to readers, will be processed later.
    ; Packs away user metadata (based on the groupings above) into usermetaK
    ; Has to map-wrap token values in cases when they don't store metadata.
    ; stores (in tokensK of the metadata) the token.
      ; lumps spaces to the previous token, so a non-collection token's :strings becomes [us space].
      ; lumps the space after the [ into the [ so for collections it's [opening, space b4 first element, closing, space after us].
        ; opening and closing are usually empty for our extra groupings.
      ; adds an :order field that dictates the order in the next collection. 
      
    ; stores in tokensK of the meta the
      ; :order = where we are (used for hash-sets only).
      ; :strings
        ; For non-collections, it's two strings: [us, space after us].
        ; for collections it is 5 strings: [opening, space, closing, space].
  ; NEED type-of-group: returns the type of a vector of token groups :set :map :ist :vector (i.e. based on brackets, etc).
    ; in clojure, reader macros become lists.
  ; NEED token-parse: parses a single token (type 1 2 or 3 only).

; meta-parse.
  ; takes what's in usermetaK of the meta (a list starting with ^ for clojure).
  ; parses it into a map form.
    ; NEED meta-parse.
  ; the input collection includes the ^ in clojure as per how it went through the grouping and collectioning.
  ; Stores the untransformed list in usermeta-preparsedK

; reader-macro.
  ; applies reader macros, the collection collection including the ', etc.
    ; Reader macros are unwraped of thier special read-map-wrap and vanilla mao-wrapped.
    ; Stores the reader macro's string into the readmacroK of the meta.
    ; NEEDS readermacro-apply. Be wary that many things may be mapwrapped.

; un-map-wrap:
  ; Store non-metable stuff's metadata one level up.
    ; Simple dump of all metadata into the childmetaK, a value-addressed map.
      ; only non-metable stuff gets dumped.
  ; no need for any code-specific function.
      
; User process the code! (try to preserve metadata).

; map-wrap:
  ; Retrieve the childmetaK and put it into stuff
    ; only if it exists and the stuff isn't metable.

; un-readermacro:
  ; when we see readmacroK we try to reverse it.
  ; Be wary that many things may be mapwrapped (and we will automatically mapwrap or readmap-wrap stuff for you).
  ; NEEDS readermacro-unapply.
  	; gets the mapwrapped code and the particular macro-as-string.
  	; tries to reverse reader macros.
  	; identity if it can't be reversed.

; un-metaparse:
  ; Tries to match usermeta-idiomaticK.
  ; identity if nothing.
  ; NEEDS meta-unparse.

; un-collparse:
  ; Goes back to tokens, unpacking metadata.
  ; Applies the orderK if it can find it.
  ; NEEDS leaf-project
  ; NEEDS coll-project
    ; Given [bracket space a space b space c space bracket] as the format, a,b,c are strings already or collections.
    ; Must ensure the spaces are proper (no sticky and comments, etc).

; token-groups-to-string
  ; Simple recursive function.
  ; No need for our coder tool, it already is projected.

(defprotocol TwoWayParser
 "The language functions needed to make a two way parser."
  (token-matchers [this])
  (non-bracket-group [this x])
  (meta-assign [this x])
  (type-of-group [this x])
  (meta-parse [this x])
  (readermacro-apply [this x])
  (readermacro-unapply [this x])
  (meta-unparse [this x])
  (coll-project [this x])
  (leaf-project [this x]))

;;;;;;;; metadata machina

(def tokenK (keyword "tokensK")) ; TODO: gensym then conversion to keyword for unique symbol.
(def usermetaK (keyword "usermetaK"))
(def usermeta-preparsedK (keyword "usermeta-preparsedK")) 
(def children-nonuser-metaK (keyword "children-nonuser-metaK"))
(def prereaderK (keyword "prereaderK")) 

(defn user-meta [x] (dissoc (meta x) usermetaK tokenK usermeta-preparsedK prereaderK)) ; helper functions.
(defn rcode-meta [x] (reduce #(let [v (get (meta x) %2)] (if (not (nil? v)) (assoc %1 %2 v) %1)) {} [orderK usermetaK usermeta-preparsedK prereaderK]))
(defn vary-user-meta [x f] ; helper functions.
  "Applies f to the meta of x without our keys.
   If no metadata is specified the metadata is nil not the empty map, preventing most infinite meta recursions."
  (with-meta x (merge (rcode-meta x) (f (user-meta x)))))
(defn metable? [x] "Can x have metadata?"
  (instance? clojure.lang.IMeta x))

(defn mwrap [x] "Map-wraps x if it can't hold metadata."
  (if (metable? x) x {mapwrapK x}))
(defn munwrap [x] "Unwraps x if it has been mapwrapped"
  (if (and (map? x) (mapwrapK x)) (mapwrapK x) x))
(defn mwrapped? [x] "Are we a map-wrapped collection."
  (and (map? x) (mapwrapK x)))

;;;;;;;; The main pipeline

(defn _match-head-body? [hb jx c] ; false if longer than head with empty body.
  (let [h (nth hb 1) b (nth hb 2)]
    (if (< jx (count h)) (= (str (nth h jx)) (str c)) (.contains ^String (str b) ^String (str c)))))
(defn parse-string [^String s lang] ; step 1.
  "TODO: slow dumb algorithm. make this faster (given we have a fast-inter-depth field in the lang this is not that big of a deal)."
  (let [^chars cs (.toCharArray s) n (count s)
        p (into [] (token-matchers lang))
        strms (filterv #(and (string? (second %)) (= (count %) 2)) p) ; string formats.
        head-bodies (filterv #(and (string? (second %)) (string? (get % 2))) p) ; head-body format.
        head-bodies (into [] (concat (mapv #(conj % "") strms)) head-bodies) ; convert string formats into head-body formats.
        head-fns (filterv #(and (string? (second %)) (fn? (get % 2))) p) ; functions.
        all-in (apply hash-set (range (count head-bodies)))
        all-fin (apply hash-set (range (count head-fns)))]
    (mapv #(if (= (count (second %)) 0) (throw (Exception. "All heads must have length >0"))) (concat head-bodies head-fns))
    (loop [types [] strs [] ix 0 ix0 0 in-game?s all-in in-gamef?s all-fin]
      (if (>= ix n) (mapv #(hash-map :strings [%1] :type %2)) ; end of array.
        (let [c (aget ^chars cs ix) tx (inc (- ix ix0))
              in-game?s1 (apply hash-set (filterv #(_match-head-body? (nth head-bodies %) tx c) in-game?s))
              in-gamef?s1 (apply hash-set (filterv #(_match-head-body? (let [x (nth head-fns %)] (vector (first x) (second x) "")) tx c) in-gamef?s))
              mature-fn-ixs (filterv identity (mapv #(if (= tx (count (get (nth head-fns %) 1))) in-gamef?s1))) ; functions at head-length match.
              fn-returns (mapv ((nth (nth head-fns %) 2) cs (+ ix tx) n) mature-fn-ixs) ; apply funs that matured.
              max-tymax (reduce #(if (and (first %2) (> (first %2) (first %1))) %2 %1) [-1 -1] 
                                   (mapv vector fn-returns (mapv #(first (nth head-fns %)) mature-fn-ixs)))]
          (if (and (= (first max-tymax) -1) (= (count in-game?s1) 0) (throw (Exception. (str "Can't find a unique parse match for: " (subs s ix0 (inc ix)))))))
          (cond (> (first max-tymax) -1) ; function matches take priority once the head gets fully matched. ix1 is just after the end of the token.
            (let [ix1 (first max-tymax)] (recur (conj types (second max-tymax)) (conj strs (subs s ix0 ix1)) ix1 ix1 all-in all-fin))
            (or (= ix (dec n)) (and (= (count in-game?s1) 1)  (not _match-head-body? (nth head-bodies (first in-game?s1)) (inc tx) (aget ^chars cs (inc ix))))) ; one unique match or end of array.
            (recur (conj types (first (nth head-bodies (first in-game?s1)))) (conj strs (subs s ix0 (inc ix))) (inc ix) (inc ix) all-in all-fin)
            :else (recur types strs (inc ix) ix0 in-game?s1 in-gamef?s1)))))))

(defn even-odd-enforce [tokens] ; step 2 and helper to step 4. Not recursive.
  (let [f? #(or (vector? %) (not= (:type %) 0))
        tokens (reduce (fn [acc t] (if (and (> (count acc 0)) (not (f? (last acc))) (not (f? t)))
                                     (update-in acc [(dec (count acc)) :strings 0] #(str % (first (:strings t))))
                                     (conj acc t))) [] tokens) ; preprocessing of lumping consecutive space tokens.
        n (count tokens) sp {:type 0 :strings [""]} ph {:type 7 :strings [""]}]
    ; fill-space-fill-space format.
    (#(if (even? (count %)) (conj % ph) %) ; totka # is odd as it's fill space fill.
      (loop [acc [] ix 0]
        (if (= ix n) acc
          (let [t (nth acc ix) need-f? (even? (count acc)) ; vector? for step 4 only.
                acc1 (cond (= (f? t) need-f) (conj acc t) (not (f? t)) (conj acc sp t) :else (conj acc ph t))] ; the enforcement.
            (recur acc1 (inc ix)))))))) 

;(defn fill-value [x lang] ; helper to step 3
;  (if (vector? x) (mapv #(fill-value % lang) x) ; recursive.
;    (let [t (:type x)]
;      (if (or (= t 1) (= t 2) (= t 3))
;        (assoc x :value (token-parse lang x)) x))))

(defn inclusive-indents [tys] ; helper to step 3.
  "Calculates the indent level of tokens, in which [ ] are included in bieng one level deeper.
   Only includes collections, not reader macros."
  (let [n (count tys)]
    (loop [acc [] clevel 0 ix 0]
      (if (= ix n) acc
        (let [tyi (nth ty ix) tsi (nth ts ix) ix1 (inc ix)]
          (cond (= tyi 6) (recur (conj acc (inc clevel)) (inc clevel) ix1) ; open
            (= tyi 7) (recur (conj acc clevel) (dec clevel) ix1) ; close.
            :else (recur (conj acc clevel) clevel ix1))))))) ; neither open nor close.

(defn bracket-pair [indents ty] ; helper to step 3.
  "Map from opening bracket index to closing bracket index. No special treatment for reader macro tokens."
  (let [n (count indents)
        close-to-open ; backwards map.
        (loop [acc {} ix 0 openings []]
          (if (= ix n) acc
            (let [tyi (nth ty ix) l (nth indents ix)]
              (cond (= tyi 6) (recur acc (inc ix) (assoc openings l ix))
                (= tyi 7) (recur (assoc acc ix (nth openings l)) (inc ix) openings)
                :else (recur acc (inc ix) openings)))))
        m-not-sorted (assoc (zipmap (vals close-to-open) (keys close-to-open)) 0 (dec (count ty)))]
     (into (sorted-map) m-not-sorted)))

(defn _braket-group [x ixs pairs] ; helper to step 3.
  (let [n (count ixs)
        xv-ixsv
        (loop [xv [] ixsv [] jx 0]
          (if (>= jx n) [xv ixsv]
            (let [kx (nth ixs jx) cl (get pairs kx)]
              ; ignore jx = 0 as that's the opening bracket for the entire collection.
              (if (and (> jx 0) cl) (recur (conj xv (subvec x jx (inc cl))) (conj ixsv (subvec x jx (inc cl)))) (inc cl)
                (recur (conj xv (nth x jx)) (conj ixsv (nth ixs jx)) (inc jx))))))]
    (mapv #(if (vector? %1) (_bracket-group %1 %2 pairs) %1) ; recursive on nested collections. 
      (first xv-ixsv) (second xv-ixsv))))
(defn bracket-group [x] ; step 3.
  (let [tys (mapv :type x)]
    (_bracket-group x (range (count x)) (bracket-pair (inclusive-indents tys) tys))))

(defn _extra-group [x lang] ; helper to step 4.
  (if (vector? x)
    (non-bracket-group lang (mapv #(_extra-group % lang) x)) x)) ; depth first.
(defn _enforce-parity-recursive [x] ; adds empty tokens to enforce the parity.
  (mapv #(if (vector? %) (_enforce-parity-recursive %) %) (even-odd-enforce x))) ; helper to step 4, buffer against coding errors.
(defn extra-group [x lang] ; step 4
  (_enforce-parity-recursive (_extra-group x lang)))


(defn meta-local-assign [x lang] ; step 5.
  (if (vector? x)
    (let [assignments (meta-assign lang (mapv #(meta-group x %) x))]
      (mapv #(if (and %2 (> %2 -1)) (assoc %1 :meta-of %2) %1) x assignments)) x))

(defn coll-parse [x lang]; step 6 is a biggie.
  (if (vector? x)
    (let [ty (type-of-group lang x) n (count x) xmap (zipmap (range n) x)
          pack-as-meta (fn [m ix-from ix-to] ; ix-from is called in order, making the usermeta vector in order.
                         (let [fr (get m ix-from) t (get m ix-to)] ; ix-froms are called in order, ensure the meta is in order.
                           (if (or (not fr) (not t)) (throw (Exception. "Trying to meta-pack stuff that was already packed (meta or parity).")))
                           (dissoc (vary-meta t #(conj (if-let [y (usermetaK %)] y []) fr)) ix-from)))
          pack-to (fn [m ix-from ix-to] ; even-odd and neck packing.
                    (let [fr (get m ix-from) t (get m ix-to) vify #(if (vector? %) % [%]) vcat #(into [] (concat %1 %2))]
                      (if (or (not fr) (not t)) (throw (Exception. "Trying to parity-pack into a map stuff that was already packed. Coding error in this function?"))) 
                      (dissoc (assoc m ix-to (if (< ix-from ix-to) (update t :strings #(vcat (vify (:strings f)) (vify %)))
                                               (update t :strings #(vcat (vify %) (vify (:strings f)))))) ix-from)))
          xmap-pack (-> (reduce #(cond (odd? %2) (pack-to %1 %2 (dec %2)) %1) xmap (range 3 n)) ; pack up the meta and space tokens.
                      (pack-to 1 0) (pack-to (dec n) 0)) ; head and tail.
          xmap-pack-m (reduce #(if-let [to-ix (:meta-of (get %1 %2))] (pack-as-meta %1 %2 to-ix) %1) xmap-pack (keys xmap))
    
          ; Now the conversion away from tokens. Recursive on both the metadata and the standard collections.
          xvec1 (mapv (fn [xi] (coll-parse (vary-meta xi (fn [mxi] (update mxi usermetaK (fn [v] (mapv (coll-parse % lang) v))))) lang)) (vals xmap-pack)))
          ; Assign order to the tokenK:
          xvec2 (mapv (fn [xi ix] (vary-meta xi #(assoc-in % [tokenK :order] ix))) xvec1 (range))]
      (cond (= ty :vector) xvec2
        (= ty :map) (if (odd? (count xvec2)) (throw (Exception. "A map must have an even number of entries.")) (apply hash-map xvec2))
        (= ty :list) (apply list xvec2)
        (= ty :set) (apply hash-set xvec2)
        :else (throw (Exception. (str "Unrecognized collection type: " ty " (should be :list, :map, :vector, or :set).")))))
      (with-meta (mwrap (if (:value x) (:value x) (first (:strings x)))) (assoc (meta x) tokensK (dissoc x :value))))) ; leaf level parse and map-wrap.

(defn parse-metas [x lang] ; step 7.
  (let [um (usermetaK (meta x)) par-met #(parse-metas % lang)
        um1 (if um (collections/cmap :flatten par-met (meta-parse lang um))) ; recursive also on the meta.
        x1 (if um1 (vary-meta x #(merge um1 (assoc (dissoc % usermetaK) usermeta-preparsedK um))) x)] ; no longer does all our usermeta hide in usermetaK.
    (if (coll? (munwrap x1)) #(collections/cmap :flatten par-met x1) x1)))


(defn reader-macro [x lang] ; step 8.
  (let [x1 (if (coll? (munwrap x)) (collections/cmap :flatten #(reader-macro % lang) x) x) ; recursive.
        x2 (vary-user-meta x1 (fn [m] (collections/cmap :flatten #(reader-macro % lang) m)))] ; recursive on the meta.
    (if (sequential? x2) ; only sequentials have reader macros. Example: the :{foo bar} is (: {foo bar}) b4 processing.
      (let [t0 (first x2)] ; detect reader macros.
        (if (= (:type (tokenK (meta x2))) 6) ; reader macro detection.
          (vary-meta (readermacro-apply lang x2) #(assoc % prereaderK (with-meta k2 nil))))) x2))) ; store the prereaderK and then apply the reader.

(defn un-map-wraps [x] ; step 9.
  (if (coll? x)
    (vary-meta (collections/cmap :flatten #(un-map-wraps x) x) 
      (fn [m] (assoc m children-nonuser-metaK
                (let [vsw (filterv mwrapped? (if (map? x) (concat (keys x) (vals x)) (collections/cvals x)))] ; only the wrapped ones.
                  (zipmap (mapv unwrap vsw) (mapv meta vsw))))))
    (munwrap x)))

;;; The main reverse pipeline. Most reverse-recursive act in reverse order: we applied the reader macros inside out but the inverse readermacros outside in.

(defn map-wraps [x] ; step inv9.
  (if (coll? x)
    (let [ch-m (children-nonuser-metaK (meta x))
          x1 (collections/cmap :flatten 
               #(let [cm (get ch-m %) xw (mapwrap x)] 
                  (if (and (not (meta x)) cm) (with-meta xw cm) xw)))]
      (collections/cmap :flatten map-wraps (vary-user-meta x map-wraps))) x)) ; recursive part.

(defn un-readermacro [x lang] ; step inv8
  (let [x1 (if-let [y (prereaderK (meta x))] (vary-meta (readermacro-unapply lang x y) #(dissoc % prereaderK)) x)
        x2 (if (meta x1) (vary-user-meta x1 #(un-readermacro % lang)) x1)] 
    (if (coll? x2) (collections/cmap :flatten x2 #(un-readermacro % lang)) x2)))

(defn un-metaparse [x lang] ; step inv7.
  (let [x1 (if-let [y (usermeta-preparsedK (meta x))]
             (vary-meta (vary-meta x #(assoc % usermetaK (meta-unparse lang (user-meta x) y)))
               #(dissoc % usermeta-preparsedK)) x1)
        x2 (if-let [um (usermetaK (meta x1))] 
             (vary-meta x #(assoc % usermetaK (un-metaparse um lang))) x1)] ; recursive on meta.
    (if (coll? (munwrap x2)) #(collections/cmap :flatten #(un-metaparse %) x2 lang) x2)))

(defn un-collparse [x lang] ; step inv6-5 feels much easier.
  (let [vs (into [] ; order equivalent to how most languages describe it. The lang fns can always change it.
             (cond (map? x) (interleave (keys x) (vals x))
               (set? x) (let [n (count x)
                              y (reduce #(let [o (orderK (meta %2))] 
                                           (if (and o (not (get %1 o))) (assoc %1 o %2)
                                             (assoc %1 (+ n (count %1)) %2) {} x)))]
                          (mapv #(get y %) (sort (vals y))))
                :else (into [] (collections/cvals x))))
        ensure-len (fn [v n] (into [] (subvec (concat (mapv str v) (repeat n "")) 0 n))) ; to make reversal slightly easier.
        tstrs (fn [x] (ensure-len (if-let [y (:strings (tokenK (meta x)))] y []) 
                        (if (coll? x) 4 2))) ; get the token strs.
        ; no need to pull out space tokens.
        vs-unpackmeta (apply concat #(if-let [x (usermetaK (meta %))] [x %] %) vs) ; put metadata before each element.
        vs-tokens (coll-project lang (mapv #(if (coll? %) % (leaf-project lang % (tstrs %)))) vs-unpackmeta)] ; leaf-project should give back a string.
    ; There is no inside out on the forward but we still do outside in on the reverse, running the lang's projection b4 recursive.
    (mapv #(if (coll? %) (un-collparse %) %) vs-tokens)))

(defn un-group [x] ; step inv 4-3-2 is even easier.
  (into [] (apply concat (mapv #(if (vector? %) (un-group %) [%]) x))))

(defn un-tokenize [x] (apply concat x)) ; step inv 1 has a longer definition than body.


;(binding [*ns* (find-ns 'clooj.coder.rcode)] (resolve 'grammer/sym-kwd-stop)) ; how to handle syntax quotes.
