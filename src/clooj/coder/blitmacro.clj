(ns clooj.coder.blitmacro
  (:require [clooj.coder.grammer :as grammer] [clooj.coder.blitcode :as blitcode]
            [clooj.collections :as collections]
            [clojure.set :as set]
            [clojure.walk :as walk]))

; The core of this file is a macro that converts functions built to operate on vanilla code
; operate on blitted code code.
; Use for refactoring, for which the output needs to be a string and preserve the input as much as possible.

;; Notes:
; TODO: some level of "strength" of blittedness, i.e. fns like tree-seq would return blitted code but at a lower strength.
; Data has two flavors: blitted and unblitted. Blitting puts information as to the string, etc
;   as well as the code itself. We assume that a partially blitted variable is never created.
; Equality: checks for equality on unblitted code.
; Reporter functions (most functions) are work on non-blitted code, return a non-map.
  ; the output is reblitted iff the input is a leaf-blitted code and the output is a leaf.
    ; Leafs are empty collections as well as non-collections that can be represetned as strings.
  ; Custom or third party library functions, which are NOT defined in the code sent to the macro,
    ; are assumed to be reporter functions. This can be overriden by 
      ; replacing (f a b c) with (refactor/ident-fn f a b c), or even other fn-appliers 
      ; (each fn-applier itself is an ident fun so the arguments will be applied normally).
; Respecting Laziness with blitted code:
  ; Most things don't need to be changed much:
    ; get/nth/take/map/etc: The :obj(s) simply get passed through the vanilla functions.
    ; concat/conj/next/rest/etc: Operates on the :obj(s) just like vanilla as well.
    ; lazy-seq/lazy-cat: It still gets wrapped into the clojure.lang.LazySeq constructure and not immediatly evaluated.
      ; These are macros so there is no need to include functions.
  ; blit and unblit are lazy if the data on them is lazy (leaf-blit is not but that doesn't matter).
     ; This is because of the use of cmap.
  ; Conversion at the end: all lazy seqs will become lists (since lists and lazy sequences print the same way).
; Functions:
  ; (fn ...) recursivly prepares the stuff inside to allow fn to act as an identity function as is.
    ; Inside functions are treated as identities, because they get compatablized upon bieng passed in.
    ; Apply will ensure all arguments are blitted or unblitted. Within reason it will blit all args if one is blitted.
      ; Unlike the "in-place" functions like inc, there is less of a reason to use the blitted code as a template to blit other code.
    ; Variable arguments are treated as b-apply does.
; Multimethods:
  ; Not changed, but the functions inside of them are changed when defined.
; Transduce:
  ; Stuff like (map inc) acts normally. Like most fns inc first gets modified to dual-use vanilla and blitted, but this has nothing to do with transducers.
  ; Inside transducers defined elsewhere:
     ; Scalar inputs blitted, vector inputs not (at the outer level) blitted.
     ; Both should work fine, no outside function gets to see the dangerous vector.
  ; Inside transducers defined in the macro code:
     ; Nothing inside will change, since there are no calls to outside functions.
  ; eduction is unchanged, the call to reduce will handle the blitting stuff.
  ; Thus we don't have to modify functions that make transducers, but we DO have to modify transduce.

(def debug (atom nil))

;;;;;;;;;;;;;;;;; Combining normal code and blit-code.

(defn impatient-find [f coll]
  "Infinitite-safe for variable function arguments.
   For non-lazy it simply uses find(filter).
   For lazy it only tries the first element of coll."
  (first (filter f (if (collections/lazy? coll) (take 1 coll) coll))))

; grammer/blit-code-to-code useful?
(defn default-blit-code [code]
  "Generates default blitted code. This is nessessary when code is created.
   Note: code must be vanilla code."
  (let [cstr (grammer/code-to-str code)]
    (try (read-string cstr) (catch Exception e (throw (Exception. (str "Esoteric code makes an invalid string, trying to read it back gives: " e)))))
    ; TODO: formatting of the string to make it look pretty?
    (first (blitcode/reads-string-blit cstr))))

(defn mpr-str [x]
  "Like pr-str but with the meta enabled."
  (binding [*print-meta* true] (pr-str x)))

(def _secret-salt (keyword (str "clooj_coder_refactor_salt_1248421" "qwerty")))
(defn _salt-blit-code [c]
  "Takes c and salts it so we know it's blitted code. Even if the user provides something with
   the same fields as blitted code, they won't have the salt."
    (assoc (if (:head (:obj c)) (update c :obj #(collections/cmap :flatten _salt-blit-code %)) c) 
      _secret-salt true))
(defn _unsalt-blit-code [c]
  "Removes the salt."
  (dissoc (if (:head (:obj c)) (update c :obj #(collections/cmap :flatten _unsalt-blit-code %)) c) 
      _secret-salt))

(defn blitted? [c]
  "Are we blitted code? All code MUST be salted first.
   Only returns true if the OUTER level is blitted, but all data is fully blitted or not blitted,
     unless the user enters mixed data."
  (boolean (_secret-salt c))) ; empty vectors always are false, as it's meaningless to see what's blitted.

(defn unblit [x]
  "Deblits x if x is blitted?, otherwise just returns x.
   get-obj can be used to peel off only one level, rather than blitting all the way down."
  (if (blitted? x) (blitcode/blit-to-code x) x)) ; this puts in the metadata as well.
(defn unblit! [x]
  "Deblits x if x is blitted?, otherwise just returns x.
   get-obj can be used to peel off only one level, rather than blitting all the way down."
  (if (blitted? x) (transient (blitcode/blit-to-code (persistent! x))) x)) ; this puts in the metadata as well.

(defn leaf-fluff [x]
  ; The fluff part of the :body of x. Changes such as inc will change the body but not the fluff.
  ; x must be a leaf, so that (:obj x) is a normal object and :head and :body are empty.
  ; nil x produces one space.
  (if x 
    (let [o (mpr-str (:obj x)) b (mpr-str (:body x))]
      (subs o (count b)))))

(defn can-leaf-blit? [x]
  "Can we apply the leaf blit to this object. Stuff like numbers, string, and empty collections."
  (or (and (coll? x) (= (count (take 1 x)) 0)) ; use take to not count infinite lazies.
           (number? x) (string? x) (symbol? x) (keyword? x)))

(defn leaf-blit 
  "Makes an x into a blitted (and salted) leaf (with adjustable fluff), with a default blit setting.
   Only one level of meta fluff can be specified, but meta rarely goes more than one level anf fluff is for looks only.
   Stuff that can't be blitted, such as atoms or collections, will be unchanged."
  ([x] (leaf-blit x " ")) 
  ([x f] (leaf-blit x f " "))
  ; DONT print the meta. The meta goes to :meta x.
  ([x f metaf] 
    ; blittability criterian (zero-element collection or standard clojure non-collection):
    (if (can-leaf-blit? x)
      (let [out (assoc {:head "" :tail "" :body (str (pr-str x))} _secret-salt true)]
        (if (meta x) (assoc out :meta (leaf-blit (meta x) metaf)) out)) x)))

(defn blit [x]
  "Also salts. Respects laziness. Does not yet ensure a valid string-form."
  (let [obj (if (coll? x) (collections/cmap :flatten x blit) x)
        body (if (coll? x) "" (pr-str x))] 
    ; most collections are ()
    (assoc {:head (cond (map? x) "{" (set? x) "#{" (vector? x) "[" (coll? x) "(" :else "")
            :body body 
            :tail (cond (map? x) "}" (set? x) "}" (vector? x) "]" (coll? x) ")" :else "")
            :obj obj} _secret-salt true)))
(defn blit! [x]
  "Also salts. Respects laziness. Does not yet ensure a valid string-form."
  (let [obj (if (coll? x) (collections/cmap :flatten x blit!) x)
        body (if (coll? x) "" (pr-str x))] 
    ; most collections are ()
    (assoc! {:head (cond (map? x) "{" (set? x) "#{" (vector? x) "[" (coll? x) "(" :else "")
             :body body 
             :tail (cond (map? x) "}" (set? x) "}" (vector? x) "]" (coll? x) ")" :else "")
             :obj obj} _secret-salt true)))

(defn leaf-blit? [x]
  "blitted and it's :obj is also a leaf."
  (and (blitted? x) (can-leaf-blit? (:obj x))))

(defn get-obj [x]
  "Gets the :obj of a blitted x, otherwise returns x.
   The :obj will still be blitted if it's not a leaf."
  (if (blitted? x) (:obj x) x))

(defn blit-odd-unblit-even [kvs]
  "Blits all odd and unblits all even. The even is the key and the odd is the value
   in a kvs array."
  (map #(if (odd? %2) (blit %1) (unblit %1)) kvs (range)))
(defn blit-odd-unblit-even! [kvs]
  (map #(if (odd? %2) (blit! %1) (unblit! %1)) kvs (range)))

(defn _blit-key [mb k] ; So that (get (get-ob %) (_blit-key % k)) works when given a vanilla or blitted k.
  (let [ob (:obj mb)]
    (cond (nil? ob) k
          (empty? ob) k
          (or (not (map? mb)) (not (set? mb))) (unblit k)
          (contains? ob k) k
          (contains? ob (unblit k)) (unblit k)
          (contains? ob (blit (unblit k))) (blit (unblit k))
          ; slow O(n) part.
          :else (let [ku (unblit k)] (first (filter #(= (unblit %) ku) (keys ob)))))))
(defn _blit-dkey [mb k] ; like _blit-key but uses the default blitting. Used when adding keys with assoc, avoids O(n) search.
  (let [b? (and (blitted? mb) (or (set? mb) (map? mb)))] 
   (if b? (blit k) (unblit k))))

(defn blit-key-even [mb kvs]
  "Applies _blit-key to all the even elements (the keys, not the vals) of kvs."
  (map #(if (odd? %2) (_blit-key mb %1) %1) kvs (range)))

(defn blit-dkey-even [mb kvs]
  "Like blit-key-even but has default blits for sets."
  (let [b? (and (blitted? mb) (or (set? mb) (map? mb)))]
    (map #(if (odd? %2) ((if b? blit unblit) %1)) kvs (range))))
(defn robust-get [m k]
  "Like (get m k) but valid for unblitted k and blitted m, valid for all three other combos as well with m determine output blitting.
   WARNING: This is O(n) on code (but O(1) on defaultly-blitted data)."
    (if (blitted? m) (get (:obj m) (_blit-key m k)) (get m (unblit k))))

(defn path-to-blit-path [collb path] 
  "Converts a path of keys from non-blitted code to blitted code, collb must be blitted.
   It both interleaves :obj and blits keys for hash-maps (and hash-sets?), but does not blit for sequential.
   [1 :two :tres] => [:obj (b 1) :obj (b :two) :obj ( b'tres)], and gets the blited code. Not lazy.
   (b x) = x for non-hash-maps, but (b x) will depend on how the key is blitted in the map.
   Returns a mixed blitted/unblitted dataset when b blits stuff."
  (if (not (blitted? collb)) (throw (Exception. "Error in blitcode.clj: path-to-blit-path given non-blitted coll.")))
  (let [path (get-obj path)] ; incase it is blitted.
    (loop [acc [] c collb ix 0]
      (if (= ix (count path)) acc
        (let [ki (nth path ix)
              ; Empty collections bieng leaf-blitted does not hurt us here.
              ki1 (if (or (map? (:obj c)) (set? (:obj c))) (_blit-key c ki) (unblit ki))]
          (recur (conj acc :obj ki1) (get-in c [:obj ki1]) (inc ix)))))))

;;;The tool to turn a function that works on normal code to a function on blitted code.

(defn reporter-fn [f]
  "The BULK of functions, which take NON blitted input.
   These return NON-collections such as numbers, symbols, strings, etc OR take in java objects, 
   that are NOT elements of said collections.
   The output will be blitted based on an input iff at least one input argument is blitted AND is a leaf (Note: includes empty collections).
   (the first blitted argument sets how the output will be blitted)."
  (fn [& args]
    (let [first-blit (impatient-find leaf-blit? args)
          result (apply f (map unblit args))]
      (if first-blit
        ; The fluff in the first-blit = the fluff in the output.
        (leaf-blit result (leaf-fluff first-blit) (str (leaf-fluff (:meta first-blit)))) result)))) ; unblitted mode.

(def fns-unblit-reporter 
  #{`flatten `hash `hash-ordered-coll `hash-unordered-coll `mix-collection-hash
    `set/join `set/index})
(defn unblit-reporter-fn [f]
  "Like reporter-fn but the output is never blitted. It's not meaningful to do so."
  (fn [& args] (apply f (map unblit args))))

(def fns-single-get
  #{`but-last `first `get `find `nth `peek `second })
(defn single-get-fn [f]
  "Gets a single element from collection.
   For blitted code, simply pass the :obj into f."
  (fn [coll & args]
    (apply f (if (blitted? coll) (:obj coll) coll) (map unblit args))))

; Functions that don't need modification to run on the blitted code (including all special forms):
; Typically functions that return non-clojure collections such as atom (although stuff like atom is rarely needed).
; This includes packing functions such as atom that TAKE a clojure but RETURN a non-clojure.
; We store blitted code into atoms, etc but deref with non-blitted code.
; Note: many functions such as aclone can go here but they don't need to.
; Note: atom creation et al goes here because they should store the blitted code.
;   the (fn ...) is key as it produces a function that IS compatable with both types.
; not needed, causes errors with special forms (defn ident-fn [f] "No blitting or unblitting done." f)
(def fns-ident #{`. `..
                 `agent `agent-error `agent-errors 
                 `alter `alter-meta `atom `cat
                 `comment `commute `compare-and-set! `completing
                 `constantly `def `delay `deliver `deref `do `finally `eduction `fn `identity
                 `if `let `letfn `loop `monitor-enter `monitor-exit `persistent! 
                 `prn `pr `println
                 `quote `recur
                 `reduced
                 `ref `ref-set `reset! `reset-meta! `restart-agent `run
                 `send `send-off `send-via `transient `throw `try `unreduced `volatile!
                 `vreset! 
                  ; add our function appliers:
                  `_salt-blit-code `blitted? `unblit `unblit! `leaf-blit `blit `blit!
                  `reporter-fn `single-get-fn  `unblit-reporter-fn `identity-fn
                  `grammer/blit-code-to-str `grammer/blit-code-to-code})

;; Convience stuff.
(defmacro if-blit [code-coll code-blit code-vanilla]
  "Checks if coll is blitted. 
   If so it replaces (:obj coll) with (code-blit1) which is code-blit but using (:obj coll) in place of coll. 
     It keeps the other fields unchanged.
   If not it runs code-vanilla on coll."
  (let [code-blit1 (walk/postwalk #(if (= % code-coll) (list :obj %) %) code-blit)]
   `(if (blitted? ~code-coll) (assoc ~code-coll :obj ~code-blit1) ~code-vanilla)))
(defmacro if-blit! [code-coll code-blit code-vanilla]
  (let [code-blit1 (walk/postwalk #(if (= % code-coll) (list :obj %) %) code-blit)]
   `(if (blitted? ~code-coll) (assoc! ~code-coll :obj ~code-blit1) ~code-vanilla)))
(defmacro ifblit [code-coll code-blit-and-vanilla]
  "a little shorter when there what's done to :obj in blitcode = what's done to vanilla."
  (let [code-blit1 (walk/postwalk #(if (= % code-coll) (list :obj %) %) code-blit-and-vanilla)]
   `(if (blitted? ~code-coll) (assoc ~code-coll :obj ~code-blit1) ~code-blit-and-vanilla)))
(defmacro ifblit! [code-coll code-blit-and-vanilla]
  (let [code-blit1 (walk/postwalk #(if (= % code-coll) (list :obj %) %) code-blit-and-vanilla)]
   `(if (blitted? ~code-coll) (assoc! ~code-coll :obj ~code-blit1) ~code-blit-and-vanilla)))
(defn bf [f] #(blit (f %))) ; Blitted-gaurentee and unblitted gaurentee of a function.
(defn uf [f] #(unblit (f %)))


;; Individual functions that need thier own wierdness:
; They work on either blitted or unblitted inputs.
; Outputs are blitted iff it makes sense to blit based on inputs and what the fn does.
(defn _b-apply [f args-fixed args-vary]
  "A single blitted argument, if we can find them without risking (near)infinite searchings,
   maks all arguments blitted."
  (let [first-blit (first (filter blitted? args-fixed))
        first-blit (if first-blit first-blit (first (filter blitted? (take 20 args-vary)))) ; up to 20 fixed args.
        first-blit (if first-blit first-blit (impatient-find blitted? args-vary))
        args (concat args-fixed args-vary)]
    (apply f (if first-blit (map blit args)) args)))
(defn _ag-body [body]
  (let [args (first body)]
    (if (= (last (butlast args)) '&)
      (update body 1 #(list 'let* [(last args) `(b-apply identity ~(last args))] %)) 
      body)))
(defn _b-concat [& args]
  "Creates laziness. The first blitted argument sets the properties (and ensures everything gets blitted)."
  (let [first-blit (impatient-find blitted? args)]          ; :obj b/c we are pulling out the internal array.
    (if first-blit (assoc first-blit :obj (apply concat (map #(:obj (blit %)) args)))
      (apply concat (map unblit args)))))
(defn _b-first [coll]
  (ifblit coll (first coll)))
(defn _b-next [coll]
  (ifblit coll (next coll)))
(defn _b-vector [& args] 
  "Not lazy because vector is not lazy."
  (let [first-blit (first (filter blitted? args))]
    (apply vector (mapv (if first-blit blit unblit) args))))
(defn _b-hash-map [& kvs]
  "Iff any of the hash-kvs are blitted default blit. Not lazy as hash-map is not lazy."
  (let [first-blit (first (filter blitted? kvs))]
    (if first-blit (blit (apply hash-map (blit-odd-unblit-even kvs))) (apply hash-map (map unblit kvs)))))
(defn _b-hash-set [& kys] 
  (let [first-blit (first (filter blitted? kys))]
    (if first-blit (apply hash-set (map blit kys)) (apply hash-set (map unblit kys)))))
(defn _b-map
  ([f] (map f))
  ([f & args] 
    (let [first-blit (first (filter blitted? (take 20 args)))
          first-blit (if first-blit first-blit (impatient-find blitted? args))] 
      (if first-blit (assoc first-blit :obj (map blit (map (bf f) (map blit args))))
                     (map unblit (map (uf f) (map unblit args)))))))
(defn _b-repeat 
  ([x] (if (blitted? x) (assoc (blit ()) :obj (repeat (:obj x))) (repeat x)))
  ([n x] (if (blitted? x) (assoc (blit ()) :obj (repeat (unblit n) (:obj x))) (repeat (unblit n) x))))
(defn _b-meta [x]
  "Blitted metadata is stored in :meta."
  (if (blitted? x) (:meta x) (meta x)))
(defn _b-walk_walk [inner outer form]
  (if (blitted? form)
    (let [obj (get-obj form) inner #(blit (inner %)) outer #(unblit (outer %))]
      (assoc form :obj 
        (cond
          (list? obj) (outer (apply list (map inner obj)))
          (instance? clojure.lang.IMapEntry obj) (outer (vec (map inner obj)))
          (seq? obj) (outer (doall (map inner obj)))
          (instance? clojure.lang.IRecord obj)
            (outer (reduce (fn [r x] (conj r (inner x))) obj obj))
          (coll? obj) (outer (into (empty form) (map inner obj)))
          :else (outer obj))))
    (walk/walk #(unblit (inner %)) #(unblit (outer %)) form)))
(defn _b-walk_prewalk [f form] (_b-walk_walk (partial _b-walk_prewalk f) identity (f form)))
(defn _b-walk_postwalk [f form] (_b-walk_walk (partial _b-walk_postwalk f) f form))

(def fns-manual-overrides { ; map from fully-qualified symbol to fn definition.

`apply (fn 
  ; some common arities, avoind variable arg reduces extranoius lazyness.
  ([f args] (_b-apply [f] args)) ([f a args] (_b-apply [f a] args))
  ([f a b args] (_b-apply [f a b] args)) ([f a b c args] (_b-apply [f a b c] args))
  ([f a b c d args] (_b-apply [f a b c d] args)) ([f a b c d e args] (_b-apply [f a b c d e] args))
  ([f a b c d e f args] (_b-apply [f a b c d e f] args)) ([f a b c d e f g args] (_b-apply [f a b c d e f g] args))
  ([f a b c d e f g h & args] (_b-apply [f a b c d e f g h] args)))

`assoc (fn [m k v & kvs]
  "Can't easily check for duplicate keys with different fluff without O(n) lookup, so skipped this step.
   But giving a properly blitted key (i.e. with our get) will avoid this issue."
  (let [kvs (concat [k v] (get-obj kvs))]
    (if-blit m (apply assoc m (blit-dkey-even m (map blit-odd-unblit-even kvs))) 
      (apply assoc m (map unblit kvs)))))
`assoc! (fn [m k v & kvs]
  (let [kvs (concat [k v] (get-obj kvs))]
    (if-blit m (apply assoc! m (blit-dkey-even m (map blit-odd-unblit-even kvs))) 
      (apply assoc! m (map unblit kvs)))))

`assoc-in (fn [m ks v]
 ; Extract the stuff out of :obj.
 (if (blitted? m) (assoc-in m (path-to-blit-path m ks) (blit v)) (assoc-in m ks v)))

`concat _b-concat

`conj (fn [coll x & args]
  "Respect laziness. The apply conj will not create laziness on vectors.
   No good way without always O(n) to prevent dublicate keys in sets"
  (if-blit coll (apply conj coll (map blit args)) (apply conj coll (map unblit args))))
`conj! (fn [coll x & args]
  (if-blit! coll (apply conj! coll (map blit args)) (apply conj! coll (map unblit args))))

`cons (fn [x sq]
  (if-blit sq (cons (blit x) sq) (cons (unblit x) sq)))

`cycle (fn [coll]
  (cycle (get-obj coll)))

`dedupe (fn
  ([] (dedupe))
  ([coll] (ifblit coll (dedupe coll))))

`disj (fn [st & ks]
  "Set keys are values so are (un)blitted as values."
  (if-blit st (apply disj st (map blit ks)) (apply disj st (map unblit ks))))
`disj! (fn [st & ks]
  (if-blit! st (apply disj! st (map blit ks)) (apply disj st (map unblit ks))))

`dissoc (fn [mp & ks] ; not the default key since dissoc must work fine:
  (if-blit mp (apply dissoc mp (map #(_blit-key mp %) ks)) (apply dissoc mp (map unblit ks))))
`dissoc! (fn [mp & ks]
  (if-blit! mp (apply dissoc! mp (map #(_blit-key mp %) ks)) (apply dissoc! mp (map unblit ks))))

`distinct (fn ; doesn't check for keys bieng the same with different fluff.
  ([] (distinct))
  ([coll] (ifblit coll (distinct coll))))

`doall (fn
  ([coll] (ifblit coll (doall coll)))
  ([n coll] (ifblit coll (doall (unblit n) coll))))

`dorun (fn
  ([coll] (ifblit coll (dorun coll)))
  ([n coll] (ifblit coll (dorun (unblit n) coll))))

`drop (fn
  ([n] (drop))
  ([n coll] (ifblit coll (drop n coll))))

`drop-last (fn
  ([s] (ifblit s (drop-last s)))
  ([n s] (ifblit s (drop-last (unblit n) s))))

`drop-while (fn
  ([pred] (drop-while))
  ([pred coll] (ifblit coll (drop-while (uf pred) coll))))

`ffirst (fn [coll] (_b-first (_b-first coll)))

`filter (fn ([pred] (filter (uf pred))) 
  ([pred coll] (ifblit coll (filter (uf pred) coll))))

`filterv (fn [pred coll]
  ([pred coll] (ifblit coll (filterv (uf pred) coll))))

`first _b-first

`fn* (fn [& name-bodies]
  "Uses b-apply identity on any variable arguments to normalize the blittedness."
  (apply list `fn* (map #(if (list? %) (_ag-body %) %) name-bodies)))

`get-in (fn [coll ks] 
  (let [ks (unblit ks)]
    (if (blitted? coll) (get-in coll (path-to-blit-path coll ks)) (get-in coll ks))))

`hash-map _b-hash-map

`hash-set _b-hash-set

`interleave (fn [& colls] 
  "Are some collections blitted and some not?
   Checks the first 20 collections to capture all but apply."
  (let [first-blit (first (filter blitted? (take 20 colls)))]
    (if first-blit (apply interleave (map blit colls)) (apply interleave (map unblit colls)))))

`interpose (fn
  ([sep] (interpose))
  ([sep coll] (if-blit coll (interpose (blit sep) coll) (interpose (unblit sep) coll))))


`into (fn ;from or it's transduced value determines blitting and comments, etc.
  ([to from] 
    (if-blit from (into (:obj (blit to)) from) (into (unblit to) from)))
  ([to xform from]
    ; Let the transducer determine if we are blitted (is into ever lazy?):
    (let [tr-vec (into [] xform (get-obj from))]
      (cond (and (blitted? from) (blitted? (first tr-vec)))
        (assoc from :obj (into (collections/cmap :flatten blit (get-obj to) tr-vec))) ; reuse the :from object.
        (blitted? (first tr-vec)) 
          (let [out (into (collections/cmap :flatten blit (get-obj to) tr-vec))]
            (assoc (cond (map? out) (blit {0 1}) (set? out) (blit #{0}) (vector? out) (blit [0]) :else (blit (list 0))) 
              :obj out))
        ; not blitted.
        :else (into (collections/cmap :flatten unblit (get-obj to)) tr-vec)))))

`keep (fn
  ([f] (keep (uf f)))
  ([f coll] (ifblit coll (keep (uf f) coll))))

`keep-indexed (fn
  ([f] (keep-indexed (uf f)))
  ([f coll] (ifblit coll (keep-indexed (uf f) coll))))

`keys (fn [mp] (keys (get-obj mp)))

`last (fn [coll] (last (get-obj coll)))

`list (fn [& items] 
  "Iff any are blitted the output is blitted."
  (let [first-blit (first (filter blitted? items))] ; list is NOT lazy.
    (if first-blit (blit (apply list (map blit items))) 
      (apply list (map unblit items)))))

`list* (fn ;Args determines whether stuff is blitted. This fn IS lazy, unlike list.
  ([args] args)
  ([a args] (if-blit args (list* (blit a) args) (list* (unblit a) (map unblit args))))
  ([a b args] (if-blit args (list* (blit a) (blit b) args) (list* (unblit a) (unblit b) (map unblit args))))
  ([a b c args] (if-blit args (list* (blit a) (blit b) (blit c) args) (list* (unblit a) (unblit b) (unblit c) (map unblit args))))
  ([a b c d & more] (if-blit more (list* (blit a) (blit b) (blit c) more) 
                      (list (unblit a) (unblit b) (unblit c) (unblit d) (map unblit more)))))

`map _b-map

`map-indexed  (fn
  ([f] (map-indexed f))
  ([f coll] (if-blit coll (map (bf f) coll) (map (uf f) coll))))

`mapcat (fn
  ([f] (mapcat f))
  ([f & colls] (apply _b-concat (apply _b-map f colls))))

`mapv (fn [f & colls] 
    (let [first-blit (first (filter blitted? colls))] ; mapv is not lazy. 
      (if first-blit (assoc first-blit :obj (mapv blit (mapv (bf f) (mapv blit colls))))
                     (mapv unblit (mapv (uf f) (mapv unblit colls))))))

`merge (fn [& mps] ; not lazy.
  (let [first-blit (first (filter blitted? mps))]
    (if first-blit (assoc first-blit :obj (apply merge (map #(:obj (blit %)) mps)))
        (apply merge (map unblit mps)))))

`merge-with (fn [f & mps] ; not lazy.
  (let [first-blit (first (filter blitted? mps))]
    (if first-blit (assoc first-blit :obj (apply merge-with (bf f) (map #(:obj (blit %)) mps)))
        (apply merge-with (uf f) (map unblit mps)))))

`meta _b-meta

`next _b-next 

`nfirst (fn [coll]
  (_b-next (_b-first coll)))

`nnext (fn [coll]
  (_b-next (_b-next coll)))

`nthnext (fn [coll]
  (ifblit coll (nthnext coll)))

`nthrest (fn [coll]
  (ifblit coll (nthrest coll)))

`partition (fn
  ([n coll] (ifblit coll (partition (unblit n) coll)))
  ([n step coll] (ifblit coll (partition (unblit n) (unblit step) coll)))
  ([n step pad coll] (if-blit coll (partition (unblit n) (unblit step) (:obj (blit pad)) coll)
                                   (partition (unblit n) (unblit step) (unblit pad) coll))))

`partition-all  (fn
  ([n] (partition-all n))
  ([n coll] (ifblit coll (partition-all (unblit n) coll)))
  ([n step coll] (ifblit coll (partition-all (unblit n) (unblit step) coll))))

`partition-by (fn
  ([f] (partition-by f))
  ([f coll] (if-blit coll (partition-by (bf f) coll) (partition-by (uf f) coll))))

`pmap (fn
  ([f & args] 
    (let [first-blit (first (filter blitted? (take 20 args)))
          first-blit (if first-blit first-blit (impatient-find blitted? args))] 
      (if first-blit (assoc first-blit :obj (pmap blit (pmap (bf f) (pmap blit args))))
                     (pmap unblit (pmap (uf f) (pmap unblit args)))))))

`pop (fn [coll] (ifblit coll (pop coll)))
`pop! (fn [coll] (ifblit! coll (pop! coll)))

`reduce (fn
  ([f coll] (ifblit coll (reduce f coll)))
  ([f val coll] (if-blit coll (reduce (bf f) (blit val) coll) (reduce (uf f) (unblit val) coll))))

`reduce-kv (fn [f init coll]
  (if-blit coll (reduce-kv (bf f) (blit init) coll) (reduce-kv (uf f) (unblit init) coll)))

`reductions (fn ;Now :obj becomes the reductions of :obj.
  ([f coll] (if-blit coll (reductions (bf f) coll) (reductions (uf f) coll)))
  ([f init coll] (if-blit coll (reductions (bf f) (:obj (blit init)) coll) (reductions (uf f) (unblit init) coll))))

`remove (fn
  ([pred] (remove (uf pred)))
  ([pred coll] (ifblit coll (remove (uf pred) coll))))

`repeat _b-repeat

`replace (fn ; Can cause time complexity issues since each key in smap must be looked-up
  ([smap] (replace smap))
  ([smap coll] (if-blit coll (replace (zipmap (map #(_blit-key smap %) (keys (get-obj smap))) 
                                              (map blit (vals (get-obj smap)))) coll)
                 (replace (unblit smap) coll))))

`replicate (fn [n x] (_b-repeat n x))

`rest (fn [coll] (ifblit coll (rest coll)))

`reverse (fn [coll] (ifblit coll (reverse coll)))

`rseq (fn [coll] (ifblit coll (rseq coll)))

`rsubseq (fn
  ([sc test ky] (ifblit sc (rsubseq sc test (unblit ky))))
  ([sc start-test start-key end-test end-key] 
    (ifblit sc (rsubseq sc start-test (unblit start-key) end-test (unblit end-key)))))

`select-keys (fn [mp kys] ; can cause time complexity issues with large blitted collections.
  (if-blit mp (select-keys mp (map #(_blit-key mp %) kys)) (select-keys mp (unblit kys))))

`seq (fn [coll] (ifblit coll (seq coll)))

`seque (fn
  ([s] (ifblit s (seque s)))
  ([n-or-q s] (ifblit s (seque (unblit n-or-q) s))))

`sequence (fn
  ([coll] (ifblit coll (sequence coll)))
  ([xform & colls]
    (let [first-blit (first (filter blitted? (take 20 colls)))
          first-blit (if first-blit first-blit (impatient-find blitted? colls))] 
      (if first-blit (assoc first-blit :obj (map blit (apply sequence xform (map get-obj colls))))
                     (apply sequence xform (map unblit colls))))))

`take (fn
  ([n] (take (unblit n)))
  ([n coll] (ifblit coll (take (unblit n) coll))))

`take-last (fn [n coll] (ifblit coll (take-last (unblit n) coll)))

`take-nth (fn
  ([n] (take-nth (unblit n)))
  ([n coll] (ifblit coll (take-nth (unblit n) coll))))

`take-while (fn
  ([pred] (take-while (uf pred)))
  ([pred coll] (ifblit coll (take-while (uf pred) coll))))

`test (fn [v] (test (_b-meta v)))

`transduce (fn
 ([xform f coll] (if-blit coll (transduce (bf f) coll) (transduce (uf f) coll)))
 ([xform f init coll] (if-blit coll (transduce (bf f) (blit init) coll) (transduce (uf f) (unblit init) coll))))

`tree-seq (fn [branch? children root]
  (if (blitted? root) (blit (tree-seq branch? children root)) (tree-seq branch? children root)))

`update (fn [m k f & more]
 (if-blit m (apply update (_blit-key m k) (bf f) more)
   (apply update m (unblit k) (uf f) more)))

`update-in (fn [m ks f & more]
  (let [ks (unblit ks)]
    (if (blitted? m) (apply update-in m (path-to-blit-path m ks) (bf f) more) 
      (apply update-in m (unblit ks) (uf f) more))))

`vec (fn [coll] (ifblit coll (vec coll)))

`vector _b-vector

`with-meta (fn [obj m]
  "Blitted metadata is stored in :meta"
  (if (blitted? obj) (assoc obj :meta (blit m)) (with-meta obj (unblit m))))

`zipmap (fn [kys vals]
  "vals determines blitting."
  (if-blit vals (zipmap (map (blit kys)) vals) (zipmap (unblit kys) vals)))

;; Now other namespaces
`set/difference (fn [& sets]
  (let [first-blit (first (filter blitted? sets))]
    (if first-blit (assoc first-blit :obj (apply set/difference (map #(get-obj (blit %)) sets)))
      (apply set/difference sets))))

`set/map-invert (fn [m]
  (ifblit m (set/map-invert m)))

`set/project  (fn [xrel ks]
  (ifblit xrel (set/project (unblit ks))))

`set/rename (fn [xrel kmap]
  (ifblit xrel (set/rename (unblit kmap))))

`set/rename-keys (fn [mp kmap]
  (ifblit mp (set/rename-keys mp (unblit kmap))))

`set/select (fn [pred xset]
  (ifblit xset (set/select (uf pred) xset)))

`set/union (fn [& sets]
  (let [first-blit (first (filter blitted? sets))]
    (if first-blit (assoc first-blit :obj (apply set/union (map #(get-obj (blit %)) sets)))
      (apply set/union sets))))

`walk/keywordize-keys (fn recur-f [m]
  (if-blit m 
    (let [om1 (reduce #(assoc %1 (assoc %2 :obj (if (string? (:obj %2)) (keyword (:obj %2)) (:obj %2))))
               m (keys m))]
      (reduce (fn [acc k] (update acc k #(if (map? (:obj %)) (recur-f %) %))) om1 (keys om1)))
 (walk/keywordize-keys m)))

`walk/postwalk _b-walk_postwalk

`walk/postwalk-demo (fn [form] 
  (let [f (fn [x] (print "Walked: ") (prn (unblit x)) x)]
    (_b-walk_postwalk f form)))

`walk/postwalk-replace (fn [smap form] 
  (let [f (fn [x] (if-blit x (if (contains? (unblit smap) (unblit x)) (blit (smap x)) x) 
                             (if (contains? (unblit smap) (unblit x)) (unblit (smap x)) x)))]
    (_b-walk_postwalk f form)))

`walk/prewalk _b-walk_prewalk

`walk/prewalk-demo (fn [form] 
  (let [f (fn [x] (print "Walked: ") (prn (unblit x)) x)]
    (_b-walk_prewalk f form)))

`walk/prewalk-replace (fn [smap form] 
  (let [f (fn [x] (if-blit x (if (contains? (unblit smap) (unblit x)) (blit (smap x)) x) 
                             (if (contains? (unblit smap) (unblit x)) (unblit (smap x)) x)))]
    (_b-walk_prewalk f form)))

`walk/stringify-keys (fn recur-f [m]
  (if-blit m 
    (let [om1 (reduce #(assoc %1 (assoc %2 :obj (if (keyword? (:obj %2)) (str (:obj %2)) (:obj %2))))
               m (keys m))]
      (reduce (fn [acc k] (update acc k #(if (map? (:obj %)) (recur-f %) %))) om1 (keys om1)))
 (walk/stringify-keys m)))

`walk/walk _b-walk_walk

`collections/cdissoc (fn [c k] (ifblit c (collections/cdissoc c (unblit k))))
`collections/ckeys (fn [c] (ifblit c ((if (set? c) identity unblit) (collections/ckeys c))))
`collections/cmap (fn [map-option f code & args] 
                (if-blit code (apply collections/cmap (unblit map-option) f code (map unblit args))
                              (apply collections/cmap (unblit map-option) f code (map blit args))))
`collections/cvals (fn [c] (ifblit c (collections/cvals c)))
`collections/lassoc 

(fn [l k v & kvs]
  "l determines the blitting."
  (if-blit l (apply list (apply assoc (into [] l) (unblit k) (unblit v) (map unblit kvs)))
    (apply list (apply assoc (into [] l) (unblit k) (blit v) (blit-odd-unblit-even unblit kvs)))))


`blitcode/vcode-to-blit (fn [s c] (_salt-blit-code (blitcode/vcode-to-blit (unblit s) (unblit c))))
`blitcode/reads-string-blit (fn [s] (_salt-blit-code (blitcode/reads-string-blit (unblit s))))

})
; Assert that everything is working:
(let [kys (keys fns-manual-overrides)
      vs (vals fns-manual-overrides)
      t (fn [k m] (throw (Exception. (str "error at: " k ": " m))))]
  (mapv #(cond (not (symbol %1)) (t %1 "not a symbol")
               (and (not= %1 'fn*) (not (.contains ^String (str %1) "/"))) (t %1 "can't resolve symbol")
               (not (fn? %2)) (t %1 "doesn't map to a function.") :else true) kys vs))

(defn coll-literal-convert [code]
  "Converts literal vectors, maps, and sets non-literals.
   Example: [a b c] => (b-vector a b c).
   This allows the b-vector to get blitted iff at least one element is blitted."
  ; These function handle both empty and non-empty sets.
  (cond (vector? code) (apply list `_b-vector code)
        (map? code) (apply list `_b-hash-map code)
        (set? code) (apply list `_b-hash-set code)
        :else code))

(defn keyword-get [kwd coll]
  "Keyword getting."
  (kwd (get-obj coll)))

; The main macro that converts the code:
; IMPORTANT: macroexpand the code before we manipulate it.
; IMPORTANT: coll-literal-convert.
; IMPORTANT: blit the output argument but wrapping the entire thing into a blit.
; IMPORTANT: ensure-valid-fluff (should be done by grammer).
; Types of fns: fns-ident, fns-single-get, fns-unblit-reporter.
  ; default is reporter-fn.
(defn wrap-var [f-sym local-vars nms]
  ;wraps a variable-as-symbol, if nessessary, otherwise just returns the var.
  ; Sometimes we don't know the function at runtime, so have to defensivly wrap the function in checks.
  ; Note: Namespace variables won't collide with fns-manual-overrides, etc b/c they are fully qualified.
  ; Example: (inc x) => ((reporter-fn inc) x)
  (let [fns-id (set/union fns-ident local-vars)]
    (cond (get fns-id f-sym) f-sym ; include local variables.
         (get fns-manual-overrides f-sym) `(get fns-manual-overrides (quote ~f-sym))
         (get fns-single-get f-sym) (`single-get-fn (quote ~f-sym))
         (get fns-unblit-reporter f-sym) (`unblit-report-fn (quote ~f-sym))
         :else
         (let [vr (ns-resolve nms f-sym)]
            (cond (nil? vr) `(if (fn? ~f-sym) (reporter-fn ~f-sym) (quote ~f-sym)) ; we don't know. 
                 (fn? (var-get vr)) `(reporter-fn (quote ~f-sym)) ; known function.
                 :else f-sym))))) ; known non-fn.
(defn _blit-translate-leaf [code local-vars nms]
  (if (symbol? code) (wrap-var code local-vars nms) code))
(defn _blit-translate-step [c nms]
  ; quoteing code blocks blitifying the code:
  (cond (and (collections/listoid? c) (= (first c) 'quote) (not (contains? (grammer/get-locals c) 'quote))) c
        ; Collections are recursive:
        (coll? c) (collections/cmap :flatten #(_blit-translate-step % nms) c)
        ; symbols are leaf-translated:
        (symbol? c) (_blit-translate-leaf c (grammer/get-locals c) nms)
        ; Other stuff is not changed:
        :else c))
(defn _blit-translate-fn-body [f-body arg-c-sym arg-b-sym nms] ;f-body does not include the fn and vector.
  (let [f-body1 (grammer/calculate-locals f-body #{arg-c-sym arg-b-sym})]
    ; keywords must be handled using the entire list.
    (walk/postwalk grammer/remove-locals (_blit-translate-step f-body1 nms))))
(defn blit-translate [f-code nms]
  "For use in macros. Accepts a function of two arguments: the-code (a vector), and blit?s (1:1 with code).
   The body of the function was built to work on NON blitted code.
   Returns a body that is built to work on blittified code.
   nms is the namespace that f-code lives in."
  (if (string? f-code) (throw (Exception. "Must be code, not a string representation of code.")))
  (let [f-code (grammer/qualify-in (apply list (walk/macroexpand-all f-code)) nms false)]
    (if (not= (first f-code) `fn*) (throw (Exception. "Must be a function's code.")))
    (if (not= (count (first (second f-code))) 2) (throw (Exception. "Function must have two arguments.")))
    ; Salt what's blitted.
    (let [arg-c (first (first (second f-code))) arg-b? (second (first (second f-code)))]
     `(fn [~arg-c ~arg-b?]
        ; shadow ~arg-c.
        (let [~arg-c (mapv #(if (nth ~arg-b? %) (_salt-blit-code (nth ~arg-c %)) (nth ~arg-c %)) 
                       (range (count ~arg-b?)))] 
          (_unsalt-blit-code ~@(_blit-translate-fn-body (rest (second f-code)) arg-c arg-b? nms)))))))