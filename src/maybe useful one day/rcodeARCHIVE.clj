; ************************************************************************
; ************************************************************************
; WARNING: This file may be replaced by xsc/rewrite-clj, depending on how much of this it can replace.
; TODO: see if that lib is useful. Even if not this file is due a significant refactor.
; ************************************************************************
; ************************************************************************

; Reads the string into a form that stores idiomatic data (comments, readmacros, etc) into the metadata.
; Due to this bieng loss-less, we can't run read-eval, etc. 
; Used for refactoring: run functions on "normal" code, and don't lose the idioms.

(ns coder.rcode
 (:require [clojure.string :as string]
           [collections :as collections] [clojure.pprint :as pprint]
           [clojure.set :as set]
           [coder.rcodeprotocol :refer :all]))
; TODO documentation is horrible.
; TODO: can optimize small diffs (for the editor case) with token detector.
; TODO: the main forward pipeline should be (with support functions first and reverse pipeline is this backwards).
  ; string -> tokens (after even odd parse) -> nested tokens -> object parse (m-wrapped and :order in parent) -> rmacros --> un-mwrap -> pack-metadata.

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
  ; Converts our nested vectors of tokens into the actual collections, and tokens into thier values.
    ; Reader macro tokens get a map-wrap syntax unique to readers, will be processed later.
    ; Packs away user metadata (based on the groupings above) into usermetaK
    ; Has to map-wrap token values in cases when they don't store metadata.
    ; stores (in tokensK of the metadata) the token.
      ; lumps spaces to the previous token, so a non-collection token's :strings becomes [us space].
      ; lumps the space after the [ into the [ so for collections it's [opening, space b4 first element, closing, space after us].
        ; opening and closing are usually empty for our extra groupings.
      ; adds an :order field that dictates the order in the next collection. 
      
    ; stores in tokensK of the meta the.
      ; :order (for sets and maps. array maps will preserve order but hashmaps won't).
      ; :strings
        ; For non-collections, it's two strings: [us, space after us].
        ; for collections it is 4 strings: [opening, space, closing, space].
        ; TODO: put ignore in metadata of b.
    ; Order is stored 
  ; NEED type-of-group: returns the type of a vector of token groups :set :map :ist :vector (i.e. based on brackets, etc).
    ; in clojure, reader macros become lists.
  ; NEED token-parse: parses a single token (type 1 2 or 3 only).

; meta-parse.
  ; takes what's in usermetaK of the meta (a list starting with ^ for clojure).
  ; parses it into a map form.
    ; NEED meta-parse.
  ; the input collection includes the ^ in clojure as per how it went through the grouping and collectioning.

; reader-macro. (mat-mult [a c])
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
  ; Applies the (:order tokenK) if it can find it.
  ; NEEDS leaf-project
  ; NEEDS coll-project
    ; Given [bracket space a space b space c space bracket] as the format, a,b,c are strings already or collections.
    ; Must ensure the spaces are proper (no sticky and comments, etc).

; token-groups-to-string
  ; Simple recursive function.
  ; No need for our coder tool, it already is projected.

;;;;;;;; collection clarity

(defn vec-to-map [v] "keys are 0,1,2,3,..." (zipmap (range) v))
(defn map-to-vec [m] "Get keys in a sorted way" (mapv #(get m %) (sort (keys m))))

;;;;;;;; metadata machina
; TODO: gensym then conversion to keyword for unique symbol.
(def tokenK (keyword "tokensK")) ; Stores the idiomatic information.
(def usermetaK (keyword "usermetaK")) ; Stores user metadata temporarally.
(def children-tokenK (keyword "children-tokenK")) ; Stores the children's tokenKs. Needed when the child in question can't have metadata.
(def mapwrapK (keyword "mapwrapK")) ; wraps 1223 as {mapwrapK 123} so it can store metadata.

(defn mmeta [x] (if-let [y (meta x)] y {})) ; {} instead of nil for meta. They pr-str to the same result. 
(defn user-meta [x] (dissoc (mmeta x) usermetaK tokenK children-tokenK)) ; user metadata when NOT stored in usermetaK.
(defn rcode-meta [x] (reduce #(let [v (get (mmeta x) %2)] (if (not (nil? v)) (assoc %1 %2 v) %1)) {} 
                       [tokenK children-tokenK usermetaK])) ; rcode metadata.
(defn vary-user-meta [x f] ; helper functions.
  "Applies f to the meta of x without our keys.
   If no metadata is specified the metadata is nil not the empty map, preventing most infinite meta recursions."
  (with-meta x (merge (rcode-meta x) (if-let [y (f (user-meta x))] y {}))))
(defn metable? [x] "Can x have metadata?"
  (instance? clojure.lang.IMeta x))
(defn update-in-meta [x ks f & args] ; avoids one nested level of functions. TODO: refactor to use this when it would reduce code.
  (vary-meta x (fn [m] (apply update-in (if m m {}) ks f args))))
(defn assoc-in-meta [x ks v] ; avoids one nested level of functions. TODO: refactor to use this when it would help.
  (vary-meta x (fn [m] (assoc-in m ks v))))
(defn with-rcode-meta [x m]
  "Takes the rcode meta of m and puts it into the meta of x."
  (with-meta x (merge (user-meta x) (rcode-meta (with-meta [] m)))))

(defn mwrap [x] "Map-wraps x if it can't hold metadata. mwrap^2 = mwrap"
  (if (metable? x) x {mapwrapK x}))
(defn munwrap [x] "Unwraps x if it has been mapwrapped. munwrap^2 = munwrap"
  (if (and (map? x) (mapwrapK x)) (mapwrapK x) x))
(defn mwrapped? [x] "Are we a map-wrapped collection."
  (and (map? x) (mapwrapK x)))

(defn fmet [x f k v] "Applies f to x and stores k (unless k is nil) and v within tokenK. 
                      Used when f is lossy and we want to preserve x's old value as a token in the metadata."
  (let [m (if-let [y (meta x)] y {})]
    (with-meta (f x) (if k (assoc-in m [tokenK k] v) m))))
(defn tmet [x y k v] "like fmet but sets the token to a particular value."
  (let [m (if-let [y (meta x)] y {})]
    (with-meta y (if k (assoc-in m [tokenK k] v) m))))

(defn _tok2str [tok] (apply str (:strings tok)))

;;;;;;;; The main pipeline

(defn bitcrush-chars [s parse-char-groups] ; helper to step 1
  "Lumps similar characters for the tokenizer. Replacing a with b won't change which token each character goes (almost?) every language.
   Thus it makes sense to lump them toghether.
   First we maps all characters > 127 (all non-ascii characters) to the letter a (i.e. a generic letter). 
   Then we map character of the string s into the second element of each group."
  (let [s (apply str (mapv #(if (> (int %) 127) \a %) s))
        chm (reduce (fn [acc group] (reduce #(assoc %1 %2 (second group)) acc (first group))) ; char -> char mapping.
              {} parse-char-groups)] 
    (apply str (mapv (fn [c] (if-let [x (get chm c)] x c)) s))))
(defn _match-head-body? [hb jx c] ; helper to step 1.
  (let [h (nth hb 1) b (nth hb 2)] ; hb = head body. jx = position on hb (starts at 0). c = character on array that we see if matches.
    ;(if (> jx 0) (println " jx: " jx " c: " c "hb: " hb))
    (if (< jx (count h)) (= (str (nth h jx)) (str c)) (.contains ^String (str b) ^String (str c)))))
(defn _still-in? [hb-or-hf tx c] ; helper to step 1.
  (if (fn? (get hb-or-hf 2)) (_match-head-body? [-1 (second hb-or-hf) ""] tx c) ; fns match the head only, the fn is triggered only at the end.
     (_match-head-body? hb-or-hf tx c))) ; does the hb-or-hd continue to match the character?
(defn _end-cap [tokens] ; helper to step 1. adds a 4 token to the beginning and 5 to end as the [] aren't explicitly added. ;(if-let [x (+ (:n-charsb4 (last tokens)) (count (_tok2str (last tokens))))] x 0)
  (into [] (concat [{:strings [""] :type 4 :n-charsb4 0}] tokens [{:strings [""] :type 5 :n-charsb4 (if-let [te (last tokens)] (+ (:n-charsb4 te) (count (_tok2str te))) 0)}])))
(defn parenthesis-cap [x] ; helper to step 6, related to _end-cap. Assumes fill/open space fill ... fill space fill/close format.
  ;Makes sure it is open space fill space ... fill space close space format by adding empty open and closing tokens and spaces.
  (let [sp {:type 0 :strings [""]} op {:type 4 :strings [""]} cl {:type 5 :strings [""]}
        x (if (= (get-in x [0 :type]) 4) x (into [] (concat [op sp] x)))
        x (if (= (get-in x [(- (count x) 2) :type]) 5) x (into [] (concat x [cl sp])))] x))

(defn _next-token [ix m n ^chars cs ^chars cs0 heads types ^ints head-counts ^ints is-fn?s ^ints pickie?s matchers bodies] ; like next-token but requiring more precomputed stuff.
  (loop [jx 0]
    (if (= jx m) (throw (Exception. (str "No token matches for: ix=" ix " n=" n " cs="  (apply str cs))))
      (let [^chars head (nth heads jx) 
            last-head (+ ix (aget head-counts jx)) ; index after the end of the head.
            picky? (= (aget pickie?s jx) 1)
            hm? (loop [kx (int ix)] ; head match? (some heads are empty so they will always head-match).
                  (if (= kx last-head) true
                    (if (= kx n) false
                      (if (= (aget head (- kx ix)) (if picky? (aget cs0 kx) (aget cs kx))) 
                        (recur (inc kx)) false))))]
        (if hm? (let [mx1 (if (= (aget is-fn?s jx) 1) (if-let [y ((nth (nth matchers jx) 2) cs cs0 last-head n)] y -1) ; run the function.
                             (let [^ints body (nth bodies jx)] ; 1 = allowed chars.
                               (loop [kx (int last-head)]
                                 (if (= kx n) kx
                                   (if (= (aget body (int (if picky? (aget cs0 kx) (aget cs kx)))) 1) ; char is in the body.
                                     (recur (inc kx)) kx)))))]
                  (if (> mx1 ix) [jx mx1] (recur (inc jx)))) (recur (inc jx))))))) ; take the first match.
(defn _parser-precomputed [tok-matchers]
  (let [matchers (into [] tok-matchers) ^ints is-fn?s (into-array Integer/TYPE (mapv #(if (fn? (get % 2)) 1 0) matchers))
        body-to-array (fn [s] (let [^ints x (make-array Integer/TYPE 255)]
                                (mapv #(aset x (int %) 1) s) x))
        heads (mapv #(.toCharArray ^String (second %)) matchers)
        pickie?s (into-array Integer/TYPE (mapv #(if (:picky (meta %)) 1 0) matchers))]
   {:matchers matchers :heads heads
    :types (mapv first matchers) :is-fn?s is-fn?s :m (count matchers)
    :head-counts (into-array Integer/TYPE (mapv count heads))
    :pickie?s pickie?s
    :bodies (mapv #(body-to-array (if (or (= %2 1) (= (count %1) 2)) "" (nth %1 2))) matchers is-fn?s)}))

(defn next-token [^chars cs ^chars cs0 ix tok-matchers] 
  "gets the next token (in [type ix-end] form) from a string parsed with lang. Useful for recursive functions in the lang itself."
  (let [prec (_parser-precomputed tok-matchers)
        match (_next-token ix (:m prec) (count cs) cs cs0 (:heads prec) (:types prec) (:head-counts prec) (:is-fn?s prec) (:pickie?s prec) (:matchers prec) (:bodies prec))]
    [(nth (:types prec) (first match)) (second match)]))

(defn parse-string [^String s0 lang] ; step 1.
  ; extra funky char at end that will match nothing.
  (let [^String s (bitcrush-chars s0 (character-tokenize-groups lang))
        ^chars cs (.toCharArray s) ^chars cs0 (.toCharArray s0) n (count s)
        prec (_parser-precomputed (token-matchers lang))
        n (count s0)
        matchers (:matchers prec) ^ints head-counts (:head-counts prec)
        heads (:heads prec) types (:types prec) m (:m prec) pickie?s (:pickie?s prec)
        ^ints is-fn?s (:is-fn?s prec) bodies (:bodies prec)] 
    (loop [tys [] strs [] ix 0 nb4 0 nb4s []]
      (if (>= ix n) (_end-cap (mapv #(hash-map :strings [%1] :type %2 :n-charsb4 %3) strs tys nb4s)) ; end of array.
        (let [match (_next-token ix m n cs cs0 heads types head-counts is-fn?s pickie?s matchers bodies)
              ni (- (second match) ix)] 
           (recur (conj tys (nth types (first match))) 
             (conj strs (subs s0 ix (second match))) (second match) 
               (+ nb4 ni) (conj nb4s nb4)))))))

(defn _binsearch-ix [old-tokens ix] ; first token partially or fully beyond the cursor.
  (let [n (count old-tokens)]
    (if (>= ix (+ (:n-charsb4 (last old-tokens)) (count (_tok2str (last old-tokens))))) n
      (loop [lo 0 hi (dec n)]
        (if (= lo hi) lo
          (let [mid (int (Math/ceil (+ (* lo 0.5) (* hi 0.5))))]
            (if (> ix (:n-charsb4 (nth old-tokens mid)))
              (if (> hi (inc lo)) (recur mid hi) hi)
              (if (> hi (inc lo)) (recur lo mid) lo))))))))

(defn _token-from [old-tokens insert tix0 jx0 tix1 jx1 lang] ; start from the beginning of the first inside token, gives back a single token.
  (let [first-in-str (_tok2str (nth old-tokens tix0)) snd-in-str (_tok2str (nth old-tokens tix1))
        early-part (str (subs first-in-str 0 jx0) insert (subs snd-in-str jx1))
        
        stuff-after (subvec old-tokens (inc tix1)) ; O(1) operation.
        n-afr (count stuff-after)]
    ;(println "stuff: " early-part (first stuff-after))
    (loop [jump 1] ; try going out jump tokens, double until we parse the whole string.
      (let [s (apply str early-part (mapv _tok2str (subvec stuff-after 0 (min jump n-afr)))) p (parse-string s lang) 
            t1 (second p) t1s (_tok2str t1)] ; the parse is end-capped with empty tokens, so the 2nd is the main token.
        (if (or (< (count t1s) (count s)) (>= jump n-afr)) 
          (assoc t1 :n-charsb4 (:n-charsb4 (nth old-tokens tix0))) ; t1 is the token index.
          (recur (* jump 2)))))))

(defn _shift-nb4 [toks sh] (mapv (fn [t] (update t :n-charsb4 #(+ % sh))) toks))

(defn _ensure-endcap [tokens]
  (let [tokens1 (_end-cap tokens)
        h (first tokens1) t (last tokens1)]
    (into [] (concat (if (= (first tokens) h) [] [h]) tokens (if (= (last tokens) t) [] [t])))))
(defn parse-string-diff [old-tokens cursor-ix0 cursor-ix1 ^String insert lang]
  "An mostly O(1) diff-based parsed for faster editing. Isn't lazy."
  ;(println (subvec old-tokens 1 4))
  ;(if (not= old-tokens (parse-string (apply str (mapv _tok2str old-tokens)) lang)) ; DEBUG ONLY.
  ;  (throw (Exception. (str "DEBUG errorbad old tokens")))) ;MUST stay commented for performance.
  (if (< (count old-tokens) 3) (parse-string insert lang) ; no old string (note the empty end-caps that are used).
    (let [old-len (+ (count (apply str (:strings (last old-tokens)))) (:n-charsb4 (last old-tokens)))
          _ (if (> (max cursor-ix0 cursor-ix1) old-len) (throw (Exception. "cursor-ix0 and/or cursor-ix1 goes off the end of the string.")))
          tix0 (_binsearch-ix old-tokens cursor-ix0)
          rm-zero (fn [ts] (filterv #(not= (_tok2str %) "") ts))]
        ; TODO: figure out this message -> WARNING: these will only work if the string is buffered with at least one empty on each side.
        (let [tix0 (min (dec (count old-tokens)) tix0)
              jx0 (- cursor-ix0 (:n-charsb4 (nth old-tokens tix0))) ; jx is within the token, 0 = token fully after cursor.
              tix1 (min (dec (count old-tokens)) (_binsearch-ix old-tokens cursor-ix1))

              jx1 (- cursor-ix1 (:n-charsb4 (nth old-tokens tix1)))
              
              stuff-before (subvec old-tokens 0 tix0) ; all tokens that are fully before the cursor-ix0.
              last-pristine-cix (:n-charsb4 (nth old-tokens tix0))
              
              ; back-token = (after the edit) the token starting from the beginning of the first token after stuff-before.
              back-token (_token-from old-tokens insert tix0 jx0 tix1 jx1 lang) ; the pieces of the old token.
              bts (_tok2str back-token) n-tback (count bts)

              ; Does the back-token go at least the length of the inserted string:
              long-back? (>= n-tback (+ (count insert) (- cursor-ix0 last-pristine-cix)))
              
              ; First token completely after a cursor at cursor-ix1, before editing
              ix-fa (min (dec (count old-tokens)) (max 1 (+ tix1 (if (= jx1 0) 0 1))))
              first-aftermath (if-let [t (get old-tokens ix-fa)] t {:strings [""]})

              ; Tokens after the first-aftermath token
              after-after-tokens (subvec old-tokens (inc ix-fa))
              
              ; Does the back token go beyond the end of the first-aftermath token?
              vlong-back? (> n-tback (+ (count insert) (- (:n-charsb4 first-aftermath) last-pristine-cix)
                                        (count (_tok2str first-aftermath))))            

              ; The string after the end of the back-token to the end of the first aftermath-token. Only defined if not vlong-back?
              endback2one+ (if (not vlong-back?) 
                             (str (subs bts 0 (min (count bts) (- cursor-ix0 last-pristine-cix)))
                               insert (let [t (nth old-tokens (dec ix-fa)) s (_tok2str t)]
                                        (subs s (min (count s) (- cursor-ix1 (:n-charsb4 t))))) 
                               (_tok2str first-aftermath)))
              
              ; ea-toks may be cutoff, which will cause downstreaming and a structural rearrangment.
              ea-toks (if endback2one+ (rest (butlast (parse-string endback2one+ lang)))) ; rest butlast to remove end-caps.
              ea-toks (if ea-toks (_shift-nb4 ea-toks last-pristine-cix))

              delta-chars (- (count insert) (- cursor-ix1 cursor-ix0))

              ; Does this insert create structural changes down the road (most inserts shouldn't thus the efficiency)?
              downstreaming? (or vlong-back? (not= (_tok2str (last ea-toks)) (_tok2str first-aftermath)))]
          (_ensure-endcap
          (if downstreaming?
            ; Everything downstream (beyond the stuff-before) is redone (occasional and slow):
            (let [str-b4 (apply str (mapv _tok2str stuff-before))
                  str-all-old (apply str (mapv _tok2str old-tokens))
                  str-afr (str (subs str-all-old (count str-b4) cursor-ix0) insert (subs str-all-old cursor-ix1))]
              (into [] (concat stuff-before (_shift-nb4 (rest (parse-string str-afr lang)) (count str-b4)))))
            ; The downstream isn't redone (more frequent and fast):
            (into [] (concat stuff-before ea-toks (_shift-nb4 after-after-tokens delta-chars))))))))) 

(defn even-odd-enforce [tokens] ; step 2 and helper to steps 3,4, and 6. Not recursive. TODO: since we use this so much have no official step 2, wrap it into step 3.
   "Enforces an open-space-fill-...-space-fill-close-space format on the tokens. This makes further processing easier.
    Empty space tokens and placeholder fill tokens are inserted when necessary."
  (let [f? #(or (vector? %) (not= (:type %) 0)) ; vector? for steps 4 and 6.
        tokens (reduce (fn [acc t] (if (and (> (count acc) 0) (not (f? (last acc))) (not (f? t)))
                                     (update-in acc [(dec (count acc)) :strings 0] #(str % (first (:strings t))))
                                     (conj acc t))) [] tokens) ; preprocessing of lumping consecutive space tokens.
        n (count tokens) sp {:type 0 :strings [""]} ph {:type 7 :strings [""]}]
    ; starting with a space token breaks the parity pattern and adding a fake token to fix parity would add a fake element in the vector.
    (if (not (f? (first tokens))) (throw (Exception. "The languages token grouper started a collection without a fill token.")))
    (#(parenthesis-cap (if (odd? (count %)) (conj % sp) %))
      (loop [acc [] ix 0]
        (if (= ix n) acc
          (let [t (nth tokens ix) need-f? (even? (count acc)) 
                acc1 (cond (= (f? t) need-f?) (conj acc t) (f? t) (conj acc sp t) :else (throw (Exception. "Some bug here.")))] ; the enforcement.
            (recur acc1 (inc ix)))))))) 

(defn inclusive-indents [tys] ; helper to step 3.
  "Calculates the indent level of tokens, in which [ ] are included in bieng one level deeper.
   Only includes collections, not reader macros."
  (let [n (count tys)]
    (loop [acc [] clevel 0 ix 0]
      (if (= ix n) acc
        (let [tyi (nth tys ix) ix1 (inc ix)]
          (cond (= tyi 4) (recur (conj acc (inc clevel)) (inc clevel) ix1) ; open
            (= tyi 5) (recur (conj acc clevel) (dec clevel) ix1) ; close.
            :else (recur (conj acc clevel) clevel ix1))))))) ; neither open nor close.

(defn bracket-pair [indents ty] ; helper to step 3.
  "Map from opening bracket index to closing bracket index. No special treatment for reader macro tokens.
   Indents are the inclusive indentation level."
  (let [n (count indents) nobal #(throw (Exception. "Syntax error: Closing bracket with no opening bracket to match it."))
        close-to-open ; backwards map.
        (loop [acc {} ix 0 openings {}]
          (if (= ix n) acc
            (let [tyi (nth ty ix) l (nth indents ix)]
              (cond (= tyi 4) (recur acc (inc ix) (assoc openings l ix)) ; opening (type 4) is always before it's matching close (type 5), except for syntax errors.
                (= tyi 5) (recur (assoc acc ix (if-let [x (get openings l)] x (nobal))) (inc ix) openings)
                :else (recur acc (inc ix) openings)))))
        m-not-sorted (zipmap (vals close-to-open) (keys close-to-open))]
     (into (sorted-map) m-not-sorted)))

(defn _apply-groups [x] ; :tmp-ix and :tmp-pair are added to x. Pairs are inclusive. We could have also cheated with read-string...
  (let [n (count x) xix-to-ix (zipmap (mapv :tmp-ix x) (range))]
    (loop [acc [] ix 0]
      (if (= ix n) acc
        (let [xi (nth x ix) p (:tmp-pair xi)]
          (if (and p (> ix 0)) ; exclude the outer level. 
            (let [xip (get xix-to-ix p)] ; xip is which ix cooresponds to p.
              (recur (conj acc (_apply-groups (subvec x ix (inc xip)))) (inc xip))) 
            (recur (conj acc xi) (inc ix))))))))
(defn _enforce-parity-recursive [x] ; adds empty tokens to enforce the parity. helper to step 3.
  (mapv #(if (vector? %) (_enforce-parity-recursive %) (dissoc % :tmp-pair :tmp-ix)) (even-odd-enforce x))) ; helper to steps 3 and 4, buffer against some coding errors.
(defn bracket-group [x] ; step 3. No metadata yet.
  (let [tys (mapv :type x) pairs (bracket-pair (inclusive-indents tys) tys) 
        x (mapv #(assoc %1 :tmp-ix %2) x (range)) x (mapv #(if-let [p (get pairs %2)] (assoc %1 :tmp-pair p) %1) x (range))
        xg (_apply-groups x)]
    (_enforce-parity-recursive xg)))

;(defn _extra-group [x lang] ; helper to step 4.
;  (if (vector? x)
;    (non-bracket-group lang (mapv #(_extra-group % lang) x)) x)) ; depth first.
;(defn extra-group [x lang] ; step 4
;  (_enforce-parity-recursive (_extra-group x lang)))

;(defn meta-local-assign [x lang] ; step 5.
;  (if (vector? x)
;    (let [assignments (meta-assign lang x) x1 (mapv #(meta-local-assign % lang) x)]
;      (mapv #(if (and %2 (> %2 -1)) (with-meta %1 {:meta-of %2}) %1) x1 (concat assignments (repeat false)))) x))

(defn _place-umeta [from to] ; helper to step x.
  (vary-meta to (fn [m] (update m usermetaK #(conj % from)))))
(defn user-meta-pack [xm meta-of] ; helper to step 6. Packs the elements into the usermetaK.
  ; due to nested meta we have to be careful to pack leaves first.
  (let [n (count xm)]
    (loop [acc xm need-to-pack (apply hash-set (filterv #(nth meta-of %) (keys xm)))]
      (if (= (count need-to-pack) 0) acc 
        (let [do-not-pack (reduce (fn [acc1 ft]
                                    (let [f (first ft) t (second ft)] ; block the "to" unless "from" has been packed. 
                                      (if (and t (get acc f)) (conj acc1 t) acc1))) #{} (mapv vector (range) meta-of))
              will-pack (set/difference need-to-pack do-not-pack)]
          (if (= (count will-pack) 0) (throw (Exception. "Circular meta dependencies.")))
          (recur (reduce (fn [acc1 from-ix] ; the packing itself.
                           (let [from (get acc1 from-ix) to-ix (nth meta-of from-ix) to (get acc1 to-ix)]
                             (assoc (dissoc acc1 from-ix) to-ix (_place-umeta from to))))
                   acc (sort will-pack))
            (set/difference need-to-pack will-pack)))))))
(defn mmapv [f x & args] "mapv that preserves metadata." 
  (with-meta (apply mapv f x args) (meta x))) 

(defn pack-core [x from-ixs to-ixs is-umeta?s] ; helper to step 6. from-ixs and to-ixs must be leaf-first ordered. is-umeta?s false = reader macro make a two-element list, true = add to usermetaK
  (let [xm (vec-to-map (mapv #(assoc-in-meta % [usermetaK] {}) x)) ; create empty usermetaK maps (later to convert to vectors) and convert to map format (later to convert to vector).
        xmp (reduce (fn [acc tuple]
                      (let [f-ix (first tuple) fr (get acc f-ix) t-ix (second tuple) m? (nth tuple 2)]
                        (update (dissoc acc f-ix) t-ix ; remove fr and put it into the element at index t-ix.
                          (fn [t0] (if m? (update-in-meta t0 [usermetaK] #(assoc % f-ix fr)) 
                                     ; this vector will later be converted into a list for reader macro processing.
                                     (with-meta (vector t0 fr) {tokenK {:strings ["" "" "" ""] :rmacro? true} usermetaK {}}))))))
               xm (mapv vector from-ixs to-ixs is-umeta?s))] 
    (map-to-vec xmp))) ; only the outer level of xmp is a map. All inner levels are vectors => no need to recursive.

(defn leaf-first [from-ixs to-ixs user-data] ; returns tuples of [from to] in an order that ensures that no to equals a previous from. Error if it there are loops.
  (loop [acc [] need?-ixs (apply hash-set (range (count from-ixs)))]
    (if (= (count need?-ixs) 0) acc
      (let [blocked?s (reduce #(assoc %1 (nth to-ixs %2) true) {} need?-ixs) ; don't pack these just yet.
            allowed-ixs (filterv #(not (get blocked?s (nth from-ixs %))) need?-ixs)]
        (if (= (count allowed-ixs) 0) (throw (Exception. "Circular dependencies.")))
        (recur (reduce #(conj %1 [(nth from-ixs %2) (nth to-ixs %2) (nth user-data %2)]) acc allowed-ixs)
          (reduce disj need?-ixs allowed-ixs))))))

(defn package [x00 lang]
  (let [; vector x00 is open-space-fill-space-fill-space-close-space format, which makes it easier to assign tokens.
        ; pack the spaces into the fills. All :strings only should have one element but we lump them toghether.
        ; also places the :strings into (:strings (tokenK (meta xi))).
          ; this is b/c we need to put the vector's () strings somewhere after removing the explicit () tokens.
          ; for vectors this somewhere must be metadata, and we do that to leaves as well for consistancy.

        ; put all :strings in the metadata. Vectors already have :strings in thier metadata, and we only have vectors or maps.
        x00 (mapv #(if (map? %) (assoc-in-meta (dissoc % :strings) [tokenK :strings] (:strings %)) %) x00)
        
        svcat (fn [& args] (mapv #(apply str %) args)) getstrs #(get-in (meta %) [tokenK :strings])
        edge-strs (apply svcat (mapv #(getstrs (nth x00 %)) [0 1 (- (count x00) 2) (- (count x00) 1)])) ; the 4 strings for the brackets.

        body (subvec x00 2 (- (count x00) 2)) ; body is fill,space,fill,space.

        body1 (mapv #(if (vector? %) (package % lang) %) body) ; recursive. Metadata at this point doesn't incude user metadata => no need to meta-recursive.
        
        ; even-odd packing. The tokenK's string are, for collections [open spaceb4 closing spaceafr] and for non-collections [token spaceafr]
        x0 (mapv (fn [fi sp] 
                   (let [xi (nth body1 fi) sxi (getstrs xi) ssp (apply str (getstrs (nth body1 sp)))
                         _ (if (not (vector? sxi)) (throw (Exception. "Rcode error: the :strings was set to not a vector.")))
                         strs (cond (= (count sxi) 1) (conj sxi ssp) (= (count sxi) 4) (update sxi 3 #(str % ssp))
                               :else (throw (Exception. "Rcode error: string count isn't working.")))]
                     (assoc-in-meta xi [tokenK :strings] strs))) (range 0 (count body1) 2) (range 1 (count body1) 2))
        ;x0 (mapv (fn [fi sp] (update-in-meta (nth body fi) [tokenK :strings] #(svcat %1 (getstrs (nth body sp))))) (range 0 (count body) 2) (range 1 (count body) 2)) ; even-odd packing.

        ; Pack readermacros as two-element vectors (later will be a list) and metadata:
        meta-to (meta-assign lang (mapv #(first (:strings (tokenK (meta %)))) x0) 
                  (mapv #(if (vector? %) 4.5 (:type %)) x0))
        meta-from-to (filterv identity (mapv #(if %2 [%1 %2]) (range) meta-to))

        n (count x0)
        rmacro-tos (filterv #(= (:type (nth x0 %)) 6) (range n)) ; the stuff after the reader macro is bundled into the macro itself.
        rmacro-froms (mapv #(if (= % n) (throw (Exception. "Reader macro is last element of collection/file")) (inc %)) rmacro-tos) ; inc with last one error checking.

        pack-from-tos (leaf-first (into [] (concat (mapv first meta-from-to) rmacro-froms)) ; meta and readermacros.
                         (into [] (concat (mapv second meta-from-to) rmacro-tos))
                         (into [] (concat (repeat (count meta-from-to) true) (repeat (count rmacro-froms) false))))

        x (pack-core x0 (mapv first pack-from-tos) (mapv second pack-from-tos) (mapv #(nth % 2) pack-from-tos)) 

        x (mapv (fn [xi ix] (assoc-in-meta xi [tokenK :order] ix)) x (range)) ; store the order (for maps and sets).
        sy (gensym "toJavaAndBeyond") x (mapv #(assoc-in-meta % [tokenK :unique-non-bracket-group] sy) x) ; for reversing the non-bracket group sub step.
        x (with-meta (non-bracket-group lang x) (meta x))] ; non-bracket group sub step (depth first order). TODO: consider moving this out to it's own function.
   (assoc-in-meta x [tokenK :strings] edge-strs)))

(defn valueify [x lang] ; big helper to step 6.
  (let [mmwrap (fn [x x1] (with-meta (mwrap x1) (meta x)))
        to-v-umk (fn [y] (update-in-meta y [usermetaK] #(if (map? %) (map-to-vec %) %))) ; make the usermeta in vector form.
        x (to-v-umk (mmapv to-v-umk x)) s-sp-ty #(let [sts (:strings (tokenK (meta %)))] [(first sts) (apply str (rest sts)) (:type %)]) ; leaf parse is: [this s s-sp ty]
        x (mmapv #(if (vector? %) (valueify % lang) (mmwrap % (apply leaf-parse lang (s-sp-ty %)))) x) ; recursive and the parse itself.
        x (mmapv (fn [xi] (if (> (count (usermetaK (meta xi))) 0) ; recursive on the metadata.
                            (vary-meta xi (fn [m] (update m usermetaK #(valueify % lang)))) xi)) x) 
        ; What collection type?
        ty (if (:rmacro? (tokenK (meta x))) :list ; reader macros => list collection type.
             (type-of-group lang (:strings (tokenK (meta x))) (mapv #(:strings (tokenK (meta %))) x)))
        _ (if (and (= ty :map) (odd? (count x))) (throw (Exception. "Odd number of elements in a map literal.")))
        _ (if (and (= ty :set) (< (count (apply hash-set x)) (count x))) (throw (Exception. "Duplicate members in set")))
        v (cond (= ty :map) (apply hash-map x) (= ty :set) (apply hash-set x) (= ty :vector) (into [] x) (= ty :list) (apply list x)
           :else (throw (Exception. (str "unrecognized type (should be :map :set :vector or :list): " ty))))]
  (with-meta v (meta x))))

(defn coll-parse [x lang]; step 6 is a biggie and is really two steps.
  (valueify (package x lang) lang)) 

(defn parse-metas [x lang] ; step 7.
  (let [um (usermetaK (meta x)) ; usermeta is pulled out of usermetaK.
        um1 (if um (collections/cmap :flatten #(mwrap (parse-metas % lang)) (meta-parse lang um))) ; recursive on the usermeta.
        x1 (if um1 (vary-meta x #(merge um1 (assoc-in (dissoc % usermetaK) [tokenK :preparsed-umeta] um))) x)] ; no longer does all our usermeta hide in usermetaK.
    ;(if um (println "um: " um "\num1: " um1))
    (if (coll? (munwrap x1)) (collections/cmap :flatten #(parse-metas % lang) x1) x1))) ;recursive.

(defn reader-macro [x lang] ; step 8.
  (let [x1 (if (coll? (munwrap x)) (collections/cmap :flatten #(reader-macro % lang) x) x) ; recursive.
        x2 (vary-user-meta x1 (fn [m] (collections/cmap :flatten #(reader-macro % lang) m)))] ; recursive on the meta.
    (if (and (sequential? x2) ; pre-reader-macro-expanded is a list with 2 elements: Example: the :{foo bar} is (: {foo bar})
          (:rmacro? (tokenK (meta x2)))) ; detect reader macros.
      (let [t0 (first x2) body (second x2)]
        (vary-meta (readermacro-apply lang x2) ; core
          ; Store the unmacroed contents and the macro token: 
          #(-> % (assoc-in [tokenK :prereader-head] t0)
             (assoc-in [tokenK :prereader-body] body)))) x2)))

(defn reorder [x reorderf] ; main part of step 10 or inv step 10.
  (let [x (if (meta x) (vary-user-meta x #(reorder % reorderf)) x)] ; recursive on meta.
    (if (coll? x)
      (let [x (collections/cmap :flatten #(reorder % reorderf) x)] ;recursive.
        (if (sequential? x)
          (let [o (reorderf x)
                _ (if (not= (sort o) (sort (range (count o)))) (throw (Exception. "Order is not a permutation.")))
                xv0 (into [] x)
                xv (reduce #(assoc %1 (nth o %2) (nth x %2)) xv0 (range (count o)))]
            (with-meta (if (vector? x) xv (apply list xv)) (meta x)) x) x)) x)))

(defn fnfirst [x lang]
  (reorder x #(fnfirst-order lang %))) ; step 9

(defn map-unwraps [x] ; step 10 is a limitation of clojure: stuff like numbers and strings can't be assigned metadata. 
  ;This should be done last b/c it makes x's idiomatic details more brittle to transformations.
  (if (coll? (munwrap x))
    (vary-meta (collections/cmap :flatten map-unwraps ; recursive
                 (vary-user-meta x #(if (> (count %) 0) (map-unwraps %) %))) ; recursive on the meta
      (fn [m] (assoc m children-tokenK
                (let [vs (filterv mwrapped? (if (map? x) (concat (keys x) (vals x)) (collections/cvals x)))] ; only the wrapped ones.
                  (zipmap (mapv munwrap vs) (mapv #(tokenK (meta %)) vs)))))) ;map from the value to the stuff in tokenK
    (munwrap x))) ; munwrap will be lossy of metadata for numbers, strings, etc.

;;; The main reverse pipeline. Most reverse-recursive act in reverse order: we applied the reader macros inside out but the inverse readermacros outside in.

(defn un-map-unwraps [x] ; step inv10.
  (let [x (vary-user-meta x #(if (> (count %) 0) (un-map-unwraps %) %))] ; recursive on the meta.
    (let [ch-tok-meta (children-tokenK (meta x)) ; the meta in each tokenK, in map form.
          x (vary-meta x #(dissoc % children-tokenK))]
      (collections/cmap :flatten ; the core step of putting the metadata into each child.
        (fn [xi] 
          (let [xi (if (coll? xi) (un-map-unwraps xi) xi) ; recursive (on collections only).
                tk (tokenK (meta xi)) ; the token information in the given child.
                tk-stored (if-let [y (get ch-tok-meta xi)] y {})] ; what we think it is.
            (if (not tk) (vary-meta (mwrap xi) #(assoc % tokenK tk-stored)) xi))) x))))

(defn un-fnfirst [x lang]
  (reorder x #(fnfirst-unorder lang %))) ; step inv9

(defn un-readermacro [x lang] ; step inv8.
  (let [ph (:prereader-head (tokenK (meta x))) pb (:prereader-body (tokenK (meta x))) ; stored before step 8.
        rm-prereader (fn [m] (update m tokenK #(dissoc % :prereader-head :prereader-body)))
        xu (if (and ph pb) (readermacro-unapply lang x ph pb)) ; compare xu to x to see if we contracted the reader macro.
        x1 (if (and ph pb) (assoc-in-meta (vary-meta xu rm-prereader) [tokenK :contracted-rmacro?] (not= x xu)) x)
        x2 (if (meta x1) (vary-user-meta x1 #(un-readermacro % lang)) x1)] ; recursive on the meta.
    (if (coll? x2) (collections/cmap :flatten #(un-readermacro % lang) x2) x2))) ; recursive.

(defn un-metaparse [x lang] ; step inv7.
  (let [x1 (if-let [y (:preparsed-umeta (tokenK (meta x)))]
             (vary-meta (vary-meta x #(assoc % usermetaK (if (> (count (user-meta x)) 0) (meta-unparse lang (user-meta x) y) []))) ; the core.
               (fn [m] (update m tokenK #(dissoc % :preparsed-umeta)))) x)
        um (usermetaK (meta x1))
        x2 (assoc-in-meta x1 [usermetaK] (if (> (count um) 0) (un-metaparse um lang) []))] ; recursive on meta.
    (if (coll? (munwrap x2)) (collections/cmap :flatten #(un-metaparse % lang) x2) x2)))

(defn un-collparse [x0 lang & not-outer-level] ; step inv6-5 feels much easier.
  (let [ensure-len (fn [v n] (into [] (subvec (into [] (concat (mapv str (if v v [])) (repeat n ""))) 0 n))) ; to make reversal slightly easier.
        get-tstrs (fn [y n] (ensure-len (if-let [z (:strings (tokenK (meta y)))] z []) n)) ; get the token strs of length n, empty string fill in nil or too short vectors.
        outer-strs (get-tstrs x0 4) ; the outer head and body.

        ty (cond (vector? x0) :vector (set? x0) :set (map? x0) :map :else :list) ; which collection type.

        x0 (non-bracket-ungroup lang x0) ; undo our non-bracket grouping (most outside first, reverse of the grouping).

        ; re-order the values for maps and sets as a vector:
        x0 (into [] ; order the values as per how we stored them.
             (if (or (map? x0) (set? x0))
               (let [n (count x0) ks (if (map? x0) (keys x0) x0) ; keys sorted by the order field (with a default if said field is missing).
                     y (reduce #(let [o (:order (tokenK (meta %2)))] 
                                  (if (and o (not (get %1 o))) (assoc %1 o %2)
                                    (assoc %1 (+ n (count %1)) %2))) {} ks)
                     ks-sort (mapv #(get y %) (sort (keys y)))]
                  (if (map? x0) (interleave ks-sort (mapv #(get x0 %) ks-sort)) ks-sort))
                (collections/cvals x0)))

        ; Unpack readers and metadata over and over untill nothing further can be unpacked.
        ; For reader macros: The last of the 4 tokens goes into the reader macro.
        _unr (fn [xi crmacro?] ; single reader macro unpack step. No tstrs from xi are used, but tstrs from it's elements ARE used.
               (cond (not crmacro?) [xi] ; Do nothing (package to cancel the unpacking with apply concat).
                 (and (sequential? xi) (= (count xi) 2)) ; most reader-macros.
                 (let [tstrs (get-tstrs (second xi) (if (coll? (munwrap (second xi))) 4 2))]
                   [(assoc-in-meta (first xi) [tokenK :rmacro-kwd?] true) (assoc-in-meta (second xi) [tokenK :strings] tstrs)])
               :else (throw (Exception. "Undon reader macros must turn into two-element lists."))))
        _unp #(let [um (usermetaK (meta %))] ; puts the user-meta vector before the element itself.
                (if (and um (> (count um) 0)) 
                  (concat (mapv (fn [umi] (assoc-in-meta umi [tokenK :meta?] true)) um) 
                    [(assoc-in-meta % [usermetaK] [])]) [%]))

        x0 (loop [x0i x0] ; loop to remove nested reader macros.
             (let [rmacro?s0 (mapv #(boolean (:contracted-rmacro? (tokenK (meta %)))) x0i)
                   x0i1 (into [] (apply concat (mapv _unr x0i rmacro?s0))) ; I don't think the _unr first or _unp first order matters.
                   x0i2 (into [] (apply concat (mapv _unp x0i1)))]
               (if (> (count x0i2) (count x0i)) (recur x0i2) x0i2))) ; go untill the count stops increasing.

        ; Extract various things from x0 for the coll-project 
        meta?s (mapv #(boolean (tokenK (:meta? (meta %)))) x0)
        x0-tstrs (mapv #(let [c? (coll? (munwrap %))]
                          (get-tstrs % (if c? 4 2))) x0) ; strings attached to each element.
        
        rm-meta-munwrap #(munwrap (if (meta %) (with-meta % nil) %)) ; We extracted every thing we need and expanded user metadata => no more metadata.

        ; Recursivly run on the elements of x0, including leaf-projection. We do not need to recursivly run on mx0's usermetadata
        ; since the outer level has no useremetadata and the inner levels are processed after the unwrapping the meta.
        x (mapv #(rm-meta-munwrap (if (coll? (munwrap %)) (un-collparse % lang true) (leaf-project lang (rm-meta-munwrap %) (get-tstrs % 2) (boolean (:rmacro-kwd? (tokenK (meta %))))))) x0)]
    ; The core coll-project step:
    (coll-project lang (collections/cmap :flatten rm-meta-munwrap x0) x0-tstrs x outer-strs meta?s ty (not (first not-outer-level)))))

(defn un-group [x] ; step inv 4-3-2 is even easier.
  (into [] (apply concat (mapv #(if (vector? %) (un-group %) [%]) x))))

(defn un-tokenize [x] (apply str x)) ; step inv 1 has a longer definition than body.

(defn fn-pipeline [lang refactor-f]
  "Returns a pipeline of one-arity fns, closing the lang."
  (let [fns [parse-string   even-odd-enforce      bracket-group          coll-parse     parse-metas    reader-macro        fnfirst            map-unwraps    refactor-f    un-map-unwraps        un-fnfirst        un-readermacro       un-metaparse      un-collparse         un-group      un-tokenize]
        nargs [    2                1                  1                     2               2                2              2                    1              1              1                    2                  2                    2                 2                  1               1]]
    (mapv (fn [f arity] (if (= arity 2) #(f % lang) f)) fns nargs)))
;TODO: a simple round-trip fn (will be easy to make).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Debugging tools ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn intermediate-states [^String s lang & f]
  "Generates a map of intermediate representations, halting and returing the exception if it encounters an error.
   s is the string, lang is the parser, and f (optional) is the user function."
  (let [f (first f) 
        nargs [          2                1                  1                     2               2                2              2                    1              1              1                    2                  2                    2                 2                  1               1]
        fns [     parse-string   even-odd-enforce      bracket-group          coll-parse     parse-metas    reader-macro        fnfirst            map-unwraps        f         un-map-unwraps        un-fnfirst        un-readermacro       un-metaparse      un-collparse         un-group      un-tokenize]
        kys [:00identity :01parsed-string :02even-odd-enforced :03bracketed-group  :06coll-parsed :07parsed-metas :08reader-macroed :09fnfirstordered :10map-unwraped :11user-f-applied :12un-map-unwraped :13un-fnfirstordered :14un-readermacroed :15un-metaparseed :16un-collparseed :17un-grouped :18un-tokenized]
        intvals (loop [acc {(first kys) s} x s ix 0] ; intermediate values.
                  (if (= ix (count fns)) acc
                    (let [e? (instance? java.lang.Exception x)
                          x1 (if e? x
                               (if-let [fi (nth fns ix)]
                                 (try (if (= (nth nargs ix) 2) (fi x lang) (fi x)) 
                                   (catch Exception e e))))]
                      (recur (assoc acc (nth kys (inc ix)) x1) x1 (inc ix)))))]
     (into (sorted-map) intvals)))

(defn _vector6 [x] ; converts x to vector form, preserving the :order and metadata and converting all leaves to a list that holds one str (so they can hold metadata).
  (let [x0 (munwrap x) ty (type x0) 
        m (assoc-in (meta x) [tokenK :coll-type] (cond (not (coll? x)) nil (vector? x) :vector (map? x) :map (set? x) :set :else :list))
        m (if (> (count (usermetaK m)) 0) (update m usermetaK #(mapv _vector6 %)) m) ; meta-recursive.
        arrange (fn [v ord] (if (not ord) (throw (Exception. "No order supplied. This fn can't accept reverse parsing.")))
                  (if (not= (sort ord) (sort (range (count ord)))) (throw (Exception. "Ord not a permutation of (range). This fn can't accept reverse parsing.")))
                  (let [mp (zipmap ord v)] (mapv #(get mp %) (range (count ord))))) 
        wm #(with-meta % m) get-ord (fn [v] (mapv #(:order (tokenK (meta %))) v))
        s (if-let [_s (:strings (tokenK (meta x)))] _s []) ns (count s)
        unpack-brackets #(wm (into [] (concat [(list (str (first s) (second s)))] % [(list (str (nth s 2) (nth s 3)))])))] ; unpacking of the ().
    (if (or (and (not= ns 2) (not (coll? x0))) (and (not= ns 4) (coll? x0)))
      (throw (Exception. "Wrong number of strings in (:strings (tokenK meta)). This fn can't accept reverse parsing.")))
    (if (not (coll? x0)) (wm (list (apply str (:strings (tokenK (meta x)))))) ; leafs just concatinate thier strings.
      (unpack-brackets (mapv _vector6 ; recursive on each element.
                         (cond (map? x0) (let [kvs (concat (keys x0) (vals x0))] (arrange kvs (get-ord kvs)))
                           (set? x0) (arrange x0 (get-ord x0)) :else (into [] x0)))))))
(defn _assign-meta6 [xv mdepth] ; Adds the :mdepth flag.
  (let [m (meta xv) m (assoc-in (if (usermetaK m) (update m usermetaK (fn [um] (_assign-meta6 um (inc mdepth)))) m) [tokenK :mdepth] mdepth)] ; recursive on meta.
    (with-meta (if (list? xv) xv ; Do nothing to lists.
                 (mapv (fn [xvi] (_assign-meta6 xvi mdepth)) xv)) m))) ; recursive on non-lists.
(defn _string6 [xv] ; generates an unwrapped vector of tuples of [string mdepth deltaindent colltype]. deltaindent is 1 = [ and -1 = ]. colltype is for the brackets.
  (let [mdepth (:mdepth (tokenK (meta xv)))
        col-type (:coll-type (tokenK (meta xv)))
        braket #(-> % (assoc-in [0 2] 1) (assoc-in [0 3] col-type) 
                  (assoc-in [(dec (count %)) 2] -1) (assoc-in [(dec (count %)) 3] col-type))
        um (if (> (count (usermetaK (meta xv))) 0) (_string6 (usermetaK (meta xv))) []) ; recursive on the meta.
        xv (if (list? xv) [[(first xv) mdepth 0 nil]] 
             (braket (into [] (apply concat (mapv _string6 xv)))))] ; leaf level vs recursive.
    (into [] (concat um xv))))
(defn extract6 [x parent-type] ; helper to pp6
 "Converts the coll-parsed component (the :06coll-parseed element) to an easy-to-visualize format.
  Returns :tokens, the strings of tokens. :depths = the indent depth of the space just before us.
  :metatargets = which token our token belongs to. nil = no target.
  :coll-types = what collection type the code is a bracket (or a virtual bracket) of.
  Leaves are lumped with thier space but the bracket spaces are kept seperate."
  (let [xv (_vector6 x) xv (_assign-meta6 xv 0)
        s-k (_string6 xv) n (count s-k)
        tokens (mapv first s-k) mdepths (mapv second s-k) delta-indents (mapv #(nth % 2) s-k)
        ; not the most efificent algorythim oh well:
        mts (mapv (fn [ix] (let [d (nth mdepths ix)] (first (filterv #(< (nth mdepths %) d) (range ix n))))) (range n))] 
    {:tokens tokens :metatargets mts :depths 
    (loop [acc [] ix 0 d 0]
      (if (= ix n) acc
        (let [del (nth delta-indents ix)]
          (cond (= del 0) (recur (assoc acc ix d) (inc ix) d)
            (= del -1) (recur (assoc acc ix d) (inc ix) (dec d))
            (= del 1) (recur (assoc acc ix d) (inc ix) (inc d))))))
    :coll-types (mapv #(nth % 3) s-k)}))