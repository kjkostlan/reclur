; Reads the string into a form that stores idiomatic data (comments, readmacros, etc) into the metadata.
; Due to this bieng loss-less, we can't run read-eval, etc. 
; Used for refactoring: run functions on "normal" code, and don't lose the idioms.

(ns clooj.coder.rcode
 (:require [clojure.string :as string] [clooj.coder.grammer :as grammer]
           [clooj.collections :as collections] [clojure.pprint :as pprint]))

; TODO Prepare for multi-language further refactor:
; seperate meta entry token (rather than a readermacro).
; basic parse is monolithic (speed).
; pull out clojure specific stuff, plug and play into deeper code.
; Have a custom override for those picky things maybe.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; Integer format for each character in a language:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 0 = empty space or comments (does not include the leading whitespace in python, as that conveys meaning).
; 1 = symbols (basically the same in any language).
; 2 = keywords (reserved words like "class" or "=" in java, python, etc).
; 3 = opening ([{, Most require inference in other languages in addition to literal brackets.
; 4 = closing }]). Most require inference in other languages in addition to literal brackets.
; 5 = metadata tags (^ in clojure, annotations in java, decorators in python, attributes in C#, etc).
; 6 = metadata closing tags. Only used in C# for now the closing ] for attributes.
; 7 = reader macro characters. In C and C++ a different meaning, the # in #define is used but these reader macros aren't expanded.
; 10 = number literals (the entire number)
; 11 = string literals (from quote to quote).
; 12 = char literals (including the escape character).
; 13 = regexp literals (from funky thing to funky thing).
; 20 = necessary delimiters (not used in clojure but : and , in python sometimes, and , in java? etc).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Unique metadata tags to store the idiomatic information.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Comments, spacing, and other idiomatic data is stored in the meta-data.
; The meta-data keys used and what is put in them:
; TODO: use gensym and convert to kwds to ensure a unique keyword (after the code gets working in the first place).

(def orderK (keyword "orderK")) ; What order do we appear in the collection one level up.
   ; This is needed for hash-sets since they have a fixed order.
   ; sequential?'s ignore this, as the colleciton one level up's meaning would be changed.
   ; Maps are semi-ordered: (= {:a 1 :b 2} {:b 2 :a 1}) => true, but (= (keys {:a 1 :b 2}) (keys {:b 2 :a 1})) => false.
   ; Becuase it's "only" semi ordered, we will not ignore this key but keys stay even and vals stay odd.

(def tokensK (keyword "tokensK")) ; Stores the tokens "attached" to us, as strings uasually.
   ; It is a vector with a very specific pattern:
   ; For collections: 
      ; [0] is the opening "[", or 'quote, etc for reader macro expansions.
      ; [1] is space between the opening and first element of the collection.
      ; [2] is the closing "]" , which is empty for most reader macros such as '
      ; [3] is the space after us.
      ; Note: this processor wraps the entire string in an outer [], so there is nothing outside the outer collection.
   ; For non-collections:
      ; [0] is a string representation of us.
      ; [1] is space after us and before the next token. It is arbitary to attach comments/whitespace to the previous token but doing so feels the most natrual.

(def mapwrapK (keyword "mapwrapK")) 
  ; Wrap the meta-haters!
  ; Strings, numbers, etc don't support metadata.
  ; Solution: map-wrap x into {mapwrapK x}.
  ; Later on in the processing pipeline we unwrap and put the metadata into the collection one level up in childtokensK.

(def readmacroK (keyword "readmacroK")) 
  ; Are we a macro or not? 
  ; Stores the expanded symbol if so (i.e. 'quote).
  ; Doesnot include user metadata.

(def usermeta-idiomaticK (keyword "usermeta-idiomaticK"))
  ; Provides idiomatic data about the usermetadata.
    ; :pad = empty space between the ^ and body. Uasually the empty string 
    ; :shorthand? = ^foo => ^{:tag 'foo} bar or ^:foo => {:foo true}.

(def childtokensK (keyword "childtokensK")) 
  ; What happens when we can't add metadata to objects (i.e. strings, etc)?
  ; Store them one level up!
  ; The storage is a map from the values (or keys and values) of said collection to the tokensK value of each element.
  ; Only non-metable objects (not the map-wrapped form) are included, to make debugging printouts easier.
  ; The order of the keys of this map follows the orderK key, since (keys ...) preserves the order.
  ; Changing the value of something without a metadata WILL erase formatting.
  ;    Loss-on-change is one of several "compromises" to choose, and probably not the best one.

(defn rcode-meta [x] (reduce #(let [v (get (meta x) %2)] (if (not (nil? v)) (assoc %1 %2 v) %1)) {} 
                        [orderK tokensK mapwrapK readmacroK childtokensK usermeta-idiomaticK]))
(defn user-meta [x] (dissoc (meta x) orderK tokensK mapwrapK readmacroK childtokensK usermeta-idiomaticK))
(defn vary-user-meta [x f] 
  "Applies f to the meta of x without our keys.
   If no metadata is specified the metadata is nil not the empty map, preventing most infinite meta recursions."
  (with-meta x (merge (rcode-meta x) (f (user-meta x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Helper functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

TODO bind a language's functions (defprotocol or multimethods).
(defn token-markv [s]
  "Token-mark but converts java arrays into vectors.
   Token-mark is a blazing fast method that works with native arrays and us used for syntax hilighting (where speed matters).
   The refactoring code, where speed is less important, can't be optimized as easily since we need to build clojure data structures.
   So it's best to work with vectors."
  (let [tk (token-mark s)] (collections/cmap :vals #(into [] %) tk)))

(defn next-token [tok-type ix tyi] ; tok-type = (:token-type tok-markv) 
  "The next time we hit a different token than type ty. We do not count the current caracter.
   Will return the length of the array if it goes over."
  (let [n (count tok-type)]
    (loop [jx (inc ix)] ; inc since we don't count what we are on already.
      (if (>= jx n) n ; off the end.
        (let [tyj (nth tok-type jx)]
          (if (not= tyj tyi) jx) (recur (inc jx))))))); stop if it is different.

(defn tokenize [tok-markv]
  "Creates these 1:1 vectors (one element per token): 
   :token = strings that represent each token, (apply str x) returns the original string.
   :type = type of tokens (i.e. string, number, space/comment, etc).
   Odd-index tokens have meaning (such as a reader macro, etc). 
   Even-index tokens are whitespace (with comments lumped and counted as type = 0).
     When we have back-to-back meaningful tokens in the string we interpose empty tokens.
     Comments and whitespace are lumped and counted as type 0."
TODO fix single rmacro jump and the type id.
  (let [ty (:token-type tok-markv) n (count ty) cs (:chars tok-markv) s (apply str cs)
        ix0 (if (= (first ty) 0) (next-token ty 0 0) 0)] ; first land.
    (loop [acc-tok [(subs s 0 ix0)] acc-ty [0] ix ix0]
      ; At this point we are at the beginning of a land token.
      (if (>= ix n) {:token acc-tok :type acc-ty} ; no more land tokens.
        (let [j (if (= ty 9) (single-rmacro-jump cs ix)) ty0 (nth ty ix)
              ix1 (if j j (next-token ty ix ty0)) ; One after the end of our token.
              ix2 (if (or (= ix1 n) (not= (nth ty ix1) 0)) ix1 ; land immediatly starts or off the edge.
                    (next-token ty ix1 (nth ty ix)))] ; Start of the next land token.
          (recur (conj acc-tok (subs s ix ix1) (subs s ix1 ix2))
            (conj acc-ty ty0 0) ix2))))))

(defn inclusive-indents [tok]
  "Calculates the indent level of tokens, in which [ ] are included in bieng one level deeper.
   Only includes collections, not reader macros."
  (let [ty (:type tok) ts (:token tok) n (count ty)]
    (loop [acc [] clevel 0 ix 0]
      (if (= ix n) acc
        (let [tyi (nth ty ix) tsi (nth ts ix) ix1 (inc ix)]
          (cond (= tyi 6) (recur (conj acc (inc clevel)) (inc clevel) ix1) ; open
            (= tyi 7) (recur (conj acc clevel) (dec clevel) ix1) ; close.
            :else (recur (conj acc clevel) clevel ix1))))))) ; reader macro add a macro level.

(defn bracket-pair [indents ty] 
  "Map from opening bracket index to closing bracket index. No special treatment for reader macro tokens."
  (let [n (count indents)
        close-to-open ; backwards map.
        (loop [acc {} ix 0 openings []]
          (if (= ix n) acc
            (let [tyi (nth ty ix) l (nth indents ix)]
              (cond (= tyi 6) (recur acc (inc ix) (assoc openings l ix))
                (= tyi 7) (recur (assoc acc ix (nth openings l)) (inc ix) openings)
                :else (recur acc (inc ix) openings)))))]
     (zipmap (vals close-to-open) (keys close-to-open))))

(defn _group-tokens [tokens ty pairs start end n] ; ix0 inclusive ix1 exclusive.
  (let [x (loop [starts [] ends [] ix start]
            (if (>= ix end) {:starts starts :ends ends}
              (let [tyi (nth ty ix) cnext (first (get tokens (inc ix)))]
                (cond (and (= ty 9) (= cnext \{)) (recur (conj starts ix) (conj ends (get pairs (inc ix))) (get pairs (inc ix))) ; not expanded as it isn't really a macro.
                  (and (= ty 9) (or (= cnext \") (= (get ty (inc ix)) 1))) (recur (conj starts ix) (conj ends (inc ix)) (inc ix)) ; not expanded either.
                  (= ty 6) (recur (conj starts ix) (conj ends (get pairs ix)) (get pairs ix)) ; no check for ty = 7 as we always jump over that.
                  (not= ty 9) (recur (conj starts ix) (conj ends ix) (inc ix)) ; boring.
                  :else ; (= tyi 9) reader macro: search for the next non-macro character.
                  (let [cl (loop [jx ix] ; closing bracket.
                             (if (= jx end) (dec end) ; oops.
                               (let [tyj (nth ty jx)]
                                 (cond (= tyj 6) (get pairs jx) ; closing index of this bracket.
                                   (not (or (= tyj 0) (= tyj 8))) jx ; non-closing symbol.
                                   :else (recur (inc jx))))))] ; continue through white symbols.
                    (recur (conj starts ix) (conj ends cl) (inc cl))))))) 
        starts (:starts x) ends (:ends x)] ; outer level inclusive indexes of the tokens, including reader macros.
    (mapv #(cond (>= %3 n) "" ; anti array-out-of-bounds.
            (> %3 %2) (_group-tokens tokens ty pairs %2 (inc %3)) ; recursive when there are multible enclised characters. 
            :else %1) tokens starts ends)))
(defn group-tokens [parsev]
  "Groups tokens into nested vectors. Reader macros start the group of each token.
   The metadata of the vector stores the :rmacro function as per the :value of single-rmacro-jump.
   The first and last element are the head and tail brackets.
   The other elements have a parity rule where odds are comment/whitespace and evens are actual code." 
  ; Example: "[foo [...] bar,]" => ["[" "" "foo" " " [...] "bar" "," "]"], and "',foo" => ["'" "," "foo" ""] because the ' acts like a ( with an empty/missing closing ).
  (let [tok (tokenize parsev) ty (:type tok) n (count ty)
        indents (inclusive-indents tok) ; inclusive, but we need to add in reader macros.
        pairs (bracket-pair indents ty)] ; mapping from opening brackets to closing brackets.
    (_group-tokens (:token tok) ty pairs 0 n n)))


(defn metable? [x] "Can x have metadata?"
  (instance? clojure.lang.IMeta x))
(defn mwrap [x] "Map-wraps x if it can't hold metadata."
  (if (metable? x) x {mapwrapK x}))
(defn munwrap [x] "Unwraps x if it has been mapwrapped"
  (if (and (map? x) (mapwrapK x)) (mapwrapK x) x))
(defn mwrapped? [x] "Are we a map-wrapped collection."
  (and (map? x) (mapwrapK x)))

(defn _pack-meta [wprecodem ix] ; packs the ix element of map wprecodem into the ix+1 element. 
  ; wprecodem is a wprecode but with the outer level in map form.
  (let [metadata (second (get wprecodem ix)) ; The second element is metadata; the first is 'meta-tag.
        pad (if-let [x (second (tokensK (meta (get wprecodem ix))))] x "") ; the stuff between the ^ and the first non-space token after.
        add-readm (fn [x short?] (vary-meta x #(dissoc (assoc % usermeta-idiomaticK {:pad pad :shorthand? short?}) orderK tokensK readmacroK)))
        metadata (cond (map? metadata) (add-readm metadata false) ; unabbreviated meta notation.
             (keyword? metadata) (add-readm {metadata true} true) :else (add-readm {:tag metadata} true)) ; the 2 abbreviated meta notations.
        metatarget (get wprecodem (inc ix))] (assoc wprecodem (inc ix) (vary-meta metatarget #(merge % metadata)))))

(defn order-solve [ch-vals-no-meta-in-order order-of-ch-with-meta]
  "Fills in the missing elements of order-of-ch-with-meta (the non-metable ones) using
   the fact that ch-vals-no-meta-in-order has it's keys in-order.
   Will spit out duplicate orders if order-of-ch-with-meta has duplicates, projection should help with this.
   Only used for hash-sets where there are no duplicates (duplicates got collapsed)."
  (let [filled-ixs (into [] (vals ch-vals-no-meta-in-order)) ; we know what the ixs are.
        n (+ (count ch-vals-no-meta-in-order) (count order-of-ch-with-meta))
        empty-ixs (into [] (reduce #(dissoc %1 %2) (apply hash-set (range n)) filled-ixs))]
    (merge order-of-ch-with-meta (zipmap ch-vals-no-meta-in-order empty-ixs)))) ; fill the empty indexes in order.

(defn _wrap-add-ch-meta [ch o tokens-old-no-meta-children] 
  ; adds to the tokensK if it finds it and we don't have one. 
  ; Adds order o if it exists and none is specified, otherwise removes the orderK from the meta.
  (let [wch (mwrap ch) tks (get tokens-old-no-meta-children ch)
        wch (if (and tks (not (tokensK (meta ch)))) (vary-meta #(assoc % tokensK tks) wch) wch)
        wch (cond (not o) (vary-meta #(dissoc % orderK) wch) ; Remove as it's not needed.
              (not (orderK (meta wch))) (vary-meta #(assoc % orderK o) wch) 
             :else wch)] wch))

(defn project-?space [s] "Makes sure s is a valid spacer string, or nil/empty; comments must have a newline." TODO
)

(defn project-obj [s x] "Makes sure (read-string s) = x. If not (pr-str x) is used." TODO)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; The forward pipeline that converts the string into annotated code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn str-to-tokegroup [s]
  "Converts the code-as-string into nested token groups."
TODO)

(defn tokgroup-to-wprecode [tok-group]
  "Converts the grouped tokens into collections using the even/odd parity rule.
   Collections are maps, sets, lists, or vectors dependent on the braces surroinding them (the first and last token).
   reads-string on leaf-level x's, meta-wrapping them if need be.
   Stores tokens and order (needed for maps and sets) in the meta.
   Packs metadata.
   Also populates the readmacroK for reader macros."
  (if (not (vector? tok-groupe)) ; leaf level = read as string, meta-wrap, and store our string value in tokensK. One level up => add in the space after us.                               
    (with-meta  
      (if (symbol? tok-groupe) tok-groupe ; symbols created from the naive-expand.
        (mwrap (try (read-string tok-groupe) (catch Exception e tok-groupe)))) ; attempt to read the string, keeping it as a string if otherwise.
      {tokensK [tok-groupe]}) ; Just us in the vector for now, will add a space later.
    ; Even-odd parity pattern (enforced by tokenize and preserved by group-tokens):
    ;   opening [, space, fill, space, fill, space, ... , fill, space, closing ].
    (let [head-tok (first tok-groupe) tail-tok (last tok-groupe) ; head and tail tokens i.e. "[" and "]". 
          neck-tok (second tok-groupe) ; space just after the first "[".
          middles (rest (rest (butlast tok-groupe))) ; fill,space,fill,space even-odd pattern of tokens or token groups.
          m0 (first middles) ; symbols here = reader macro.
          fills1 (mapv tokgroup-to-wprecode (keep-indexed #(if (even? %1) %2) middles)) ; recursive on the fills.
          spaces (keep-indexed #(if (odd? %1) %2) middles) ; spaces after each fill.
          ; add the space to the end of each tokensK and populate the orderK of each element:
          as-vecu (mapv (fn [x sp o] (vary-meta x (fn [m] (assoc (update m tokensK #(conj % sp)) orderK o)))) fills1 spaces (range))
          ; Pack the metatata of 'meta-hat into the next element, reducing over a map makes removing elements easy. 
          as-mapu (zipmap (range) as-vecu) metatag? (fn [m ix] (let [mx (get m ix)] (and (list? mx) (= (readmacroK (meta mx)) 'meta-hat))))
          as-vec (into [] (vals (reduce #(if (metatag? %1 %2) (_pack-meta %1 %2) %1) as-mapu (range (count as-mapu)))))] 
       ; Use the type of bracket to tell us which collection to be.
       ((fn [x] (if (symbol? m0) (vary-meta x #(assoc % readmacroK m0)) x)) ; add-in metadata here.
         (cond (= head-tok "[") as-vec 
           (= head-tok "#{") (apply hash-set as-vec) ; duplicate entries generate a compile time error in vanilla.
           (= head-tok "{") (zipmap (keep-indexed #(if (even? %1) %2) as-vec) (keep-indexed #(if (odd? %1) %2) as-vec)) 
           :else ; both ( and reader macros.
           (apply list as-vec))))))

(defn wprecode-to-precode [wprecode]
  "Un map-wraps the leaf elements that can't store metadata.
   Thier meta-data gets stored in a value-addresable map one level up."
  (cond (mwrapped? wprecode) (munwrap wprecode); unwrap the leaf. One level above this we have already stored the meta safely.
    (coll? wprecode) ; collections form the meat of the code.
    (let [welements (into [] (if (map? wprecode) (concat (keys wprecode) (vals wprecode)) (collections/cvals wprecode))) ;metables? (mapv metable? vs)
          wprecode1 (vary-user-meta (collections/cmap :flatten wprecode-to-precode wprecode) wprecode-to-precode) ; recursive on each element and the metadata.
          o2wel (zipmap (mapv #(orderK (meta %)) welements) welements) ; order -> welements within vs. Used to ensure the reduction is in the correct order.
          ch-tokens (reduce (fn [acc ix] (let [wel (get o2wel ix)]
                                           (if (mwrapped? wel) (assoc acc (munwrap wel) (tokensK (meta wel))) acc))) ; only tokensK is needed.
                      {} (range (count o2wel)))] ; only includes non-metable stuff.
       (if (> (count o2wel) 0) (vary-meta wprecode1 #(assoc % childtokensK ch-tokens)) wprecode1)) ; store children's meta if they aren't metable.
    :else wprecode)) ; leaf with meta.

(defn precode-to-code [precode]
  "Applies reader macros."
  (cond (coll? precode) ; macros can only be applied to collections
    (let [precode1 (vary-user-meta (collections/cmap :flatten precode-to-code precode) precode-to-code) ; recursive also on the meta.
          read-f (get read-appliers (readmacroK (meta precode)))] 
      (if read-f (read-f precode1) precode1)) 
    (metable? precode) (vary-user-meta precode precode-to-code)
    :else precode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; The reverse pipeline that converts the modified annotated code back into a string.
;;;;;;;;;;;; It includes a projection step: annotations are preserved as best as possible without changing the meaning.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn code-to-precode [code]
  "Un-applies reader macros if they have been specified as bieng reader macros and if a reversal path."
  (cond (coll? code)
    (let [r-unap (get read-unappliers (readmacroK (meta code))) ; which unapplier to use.
          code0 (if r-unap (r-unap code) code) ; leaf first.
          code00 (vary-user-meta code0 code-to-precode)] ; recursive also on the meta.
      (collections/cmap :flatten code-to-precode code00)) 
    (metable? code) (vary-user-meta code code-to-precode)
    :else code))

(defn precode-to-wprecode [precode]
  "map-wraps (mapwrapK) non-metable code,
   when childtokensK is supplied it fills the children's values of tokensK and orderK,
   removing childtokensK from the parent. Of course, we will eventually need to project down tokens."
  (cond (coll? precode) ; collections are always metable, no need to wrap.
    (collections/cmap :flatten precode-to-wprecode ; recursive.
      (let [tokens-old-no-meta-children (if-let [x (childtokensK (meta precode))] x {}) ; tokens of non-metable old children.
            wrappy (fn [ch o] (_wrap-add-ch-meta ch o tokens-old-no-meta-children))
            precode0 (vary-user-meta (vary-meta #(dissoc % childtokensK) precode) precode-to-wprecode)] ; recursive on meta.
        (if (set? precode) ; sets mean order matters.
          (let [ch-with-ometa (filter #(orderK (meta %)) precode) ; all children with an order in thier meta.
                order-of (order-solve (keys tokens-old-no-meta-children) 
                           (zipmap ch-with-ometa (mapv #(orderK (meta %)) ch-with-ometa)))]
            (apply hash-set (mapv #(wrappy % (if-let [x (get order-of %)] x -1)) precode0))) ; -1 or nil when we don't find it?
           (collections/cmap :flatten #(wrappy % nil) precode0))))
     (metable? precode) (vary-user-meta precode precode-to-wprecode); recursive on meta.
    :else precode)) ; any wrapping is doen one level up.


(defn wprecode-to-tokgroup [wprecode]
  "Converts the precode to the tokengroup format, applying any naive contractions to reader macros.
   Does any needed projections to gaurentee the tokengroup reads properly.
   Metadata is unpacked and the tokgroup format is respected as to the original tok-group."
  (if (coll? wprecode) ; here we handle the meta's of the children.
    (let [m (meta wprecode)
          head (cond (readmacroK (meta wprecode))) ; head and tail brackets.
])
    (let [tg (tokensK (meta wprecode))]
      [(project-obj (first tg) (munwrap wprecode)) (project-?space (second tg))]))

  (let [meta-val (if (or ()))
]

)
  (if (coll? wprecode)
    ()
   ...))

(defn tokgroup-to-str [tokgroup] TODO)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Old stuff, may or may not be useful.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn naive-expand [tok-group]
  "Converts the grouped tokens to 'naively expanded' code, which expands reader macros:
   ['foo] => [quote foo], the quote is a symbol but foo is still a string. 
   To preserve the even-odd parity we need to add an empty token after the quote.
   All reader macros (which doesn't include hash-sets nor regexps) are expanded the same way.
   Regexps are left alone and hash-sets group toghether the reader macro and head token.
   We put the reader macro token into readmacroK of each vector level.
   Metadata is also naively expanded into a 'meta-hat symbol.
   We do not yet populate readmacroK."
  (if (not (vector? tok-group)) tok-group ; leaf level.
    (let [tok-group1 (mapv naive-expand tok-group) ; recursive.
          r (get rmacro-map (first tok-group1))]
      (if r (into [] (concat [""] (assoc tok-group1 0 r))) tok-group1))))

(defn indent-level [^String s]
  "Returns the indentation level BETWEEN each character as ^ints.
   Used for code hilighting. The output array has one more element in it than the lenth of the string."
  (let [tk (token-mark s) ^chars cs (:chars tk) n (count cs)
        ^ints ty (:token-type tk) ^ints tix (:token-index tk)
        ^ints inter-depth (make-array Integer/TYPE (inc n))]
    ; Depth field: counting paranethesis.
    (loop [ix (int 0) l (int 0)]
      (if (< ix n)
        (let [ti (aget ^ints ty ix)
              ti (if (= ti 6) (if (and (> ix 0) (= (aget ^chars cs ix) \{) (= (aget ^chars cs (dec ix)) \#)) -1 ti) ti) ; don't double count hash-sets.
              l1 (int (if (= ti 6) (inc l) (if (= ti 7) (max (dec l) 0) l)))] ; up or down one level.
          (aset ^ints inter-depth (inc ix) l1) ; the decrement is delayed one char.
          (recur (inc ix) l1)) inter-depth))))

;(binding [*ns* (find-ns 'clooj.coder.rcode)] (resolve 'grammer/sym-kwd-stop)) ; how to handle syntax quotes.