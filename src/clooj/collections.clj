; Generic functions to work with clojure's collections.
(ns clooj.collections
  (:require [clojure.set :as set]))


(def ^:dynamic *err-on-long-list* false) ; Errors on O(n) operations lists exceding 21 elements (the max vlaid code).

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
          
(defn listoid? [x]
  "Does pr-str use ()? Things like lists, seqs, etc. Returns false for non-collections."
  (and (coll? x) (not (vector? x)) (not (map? x)) (not (set? x))))

(defn _el [l] (if (and *err-on-long-list* (> (count (take 22 l)) 21)) (throw (Exception. "Performance Assertion error on O(n) point-update operation for a list too long to be valid code (> 21 elements)."))) l)

(defn lassoc [l k v & kvs]
  "List assoc: treat lists like vectors. O(n+kvs) but not a big deal for most lists based on very short code datastructures.
   Like assoc it will throw out-of-bounds when we try to assoc and out of bounds thing."
  (apply list (apply assoc (into [] (_el l)) k v kvs)))

(defn l-end-cat [l x]
  "Like (concat l [x]) but returns a list not a lasy seq. O(n)"
  (apply list (conj (into [] (_el l)) x)))

(defn cdissoc [c k]
  "Like dissoc but works on most collections preserving the type. Not lazy.
   WARNING: O(n) for vectors and lists."
  (cond (nil? c) nil
    (vector? c) (into [] (vals (dissoc (zipmap (range (count c)) c) k)))
    (list? c) (apply list (dissoc (zipmap (range (count (_el c))) c) k))
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
  "like map but it (except in esoteric situations) preserves the type of code AND preserves code's metadata.
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
  (let [code-meta (meta code)] ; preserve metadata. f operates on elements so cannot change the overall metadata.
    (with-meta 
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
         (throw (Exception. (str "Code type is not a collection: " (str code))))) code-meta)))

; Makes a vector of even-odd pairs. [{:even 0 :odd 1} {:even 2 :odd 3} {:even 3 :odd 4} ...]
; Useful for let statements et al that use pairs arguments.
(defn even-odd [coll]
  (let [even (take-nth 2 coll) odd (take-nth 2 (rest coll))]
    (mapv #(hash-map :even %1 :odd %2) even odd)))

; TODO: are keys mapped to nil treated the "right" way?
(defn fillin-defaults [c default]
  "When given two maps: adds fields that are in default but not in state to state.
   Note: nil counts as a missing field even if there is an explicit key.
   Example: (collections/fillin-defaults {:a :one :c 3} {:a 1 :b 2}) => {:a :one, :c 3, :b 2}.
     :a and :c weren't changed because we specified non-nil values for them (and there isn't even a default :c).
     :b is the added default we didn't specify.
   This function is useful when dealing with complex input arguments:
     have defaults but let the user override them if they know what they are doing."
  (reduce #(if (nil? (get %1 %2)) (assoc %1 %2 (get default %2)) %1) c (keys default)))

(defn asoc [m & kvs] 
  "Like assoc but also works with list-like stuff."
  (if (listoid? m) (apply list (apply assoc (into [] (_el m)) kvs)) (apply assoc m kvs)))

(defn gett 
  "Like get but also works with lists, for which it is O(k)."
  ([m k] (gett m k nil))
  ([m k not-found]
    (if (listoid? m) (if (or (not (integer? k)) (< k 0) (>= k (count (_el m)))) not-found (nth m k)) 
        (get m k not-found))))

(defn updayte [m k f & args]
  "Like update but also works with lists, for which it is O(k)."
  ((if (listoid? m) lassoc assoc) m k (apply f ((if (listoid? m) nth get) m k) args)))

(defn _asoc-in [m [k & ks] v] ; only use this if k is not empty.
  (if ks ; taken from clojure.core source.
    ((if (listoid? m) lassoc assoc) m k (_asoc-in ((if (listoid? m) nth get) m k) ks v))
    ((if (listoid? m) lassoc assoc) m k v)))
(defn asoc-in [m ks v]
  "Like assoc-in but if ks is [] or nil we will simply replace m with v.
   Also works with lists et al (but O(n) beware, use for code).
   Like assoc-in it generates out of bounds errors."
  (if (> (count ks) 0) (_asoc-in m ks v) v))

(defn _updayte-in
  ([m [k & ks] f & args]
   (if ks
     ((if (listoid? m) lassoc assoc) m k (apply _updayte-in ((if (listoid? m) nth get) m k) ks f args))
     ((if (listoid? m) lassoc assoc) m k (apply f ((if (listoid? m) nth get) m k) args)))))

(defn updayte-in [m ks f & args]
  "Like update-in but if ks is [] or nil we simply apply f to m.
   Also works for lists."
  (if (> (count ks) 0) (apply _updayte-in m ks f args) (apply f m args)))

(defn updates-in [m ks f & args]
  "Applies f to multible elements of m with different paths. Not lazy.
   m = the data structure. ks = vector of vector of keys.
   f = function taking one argument per vector + any args.
     f returns a vector of elements one of each elemnt goes to each thing."
  (let [xs (mapv #(get-in m %) ks)
        vs (into [] (apply f (concat xs args)))]
    (reduce #(assoc-in %1 (nth ks %2) (nth vs %2)) m (range (count vs)))))

(defn gett-in
  ([m ks]
     (reduce gett m ks))
  ([m ks not-found]
     (loop [sentinel (Object.) ; their code is quite strange.
            m m
            ks (seq ks)]
       (if ks
         (let [m (gett m (first ks) sentinel)]
           (if (identical? sentinel m)
             not-found
             (recur sentinel m (next ks))))
         m))))

(defn f-bin-search [f n]
  "Binary search with functions.
   Rather than an array, f is a function that takes an integer argument from 0 to n-1.
   If (f i) is monotonicly increasing (from false to true) it finds the first true in O(log(n)).
   If f is always false it returns n."
;Adapted from http://algs4.cs.princeton.edu/11model/BinarySearch.java.html
  (cond (= n 0) 0 
        (f 0) 0
        (not (f (dec n))) n 
        ; always: (f lo) is false but (f hi) is true.
        :else
        (loop [lo 0 hi (dec n)]
          (if (>= (inc lo) hi) hi ; no more searching to do.
          ; hi and lo are at least 2 apart, so mid will always be in between.
            (let [mid (+ lo (long (/ (- hi lo) 2)))] 
              (if (f mid) (recur lo mid) (recur mid hi)))))))

(defn sortt [coll]
  "Like sort but doesn't throw a ClassCastException when the collections are different types.
   It will prioritize sorting by the type before the concent, but all numbers are treated as one type.
   Numbers are ordered up to 10^64 in increments of 10^-64."
  (let [tg (keyword (str "coll-meta-key" "-for-dontblockothermetas"))
        ns (fn [n] (format "%064.64f" (double n)))]
    (seq (mapv #(get (meta %) tg) (sort (mapv #(with-meta [(if (number? %) "" (str (type %))) (if (number? %) (ns %) (str %))] {tg %}) coll))))))

(defn which [pred coll]
  "returns the keys/indexes for which pred is true on a given item of coll. Like filter but keys instead of values."
  (if (map? coll)
    (reduce-kv #(if (pred %3) (conj %1 %2) %1) [] coll)
    ; sequences stay lazy:
    (filter #(> % -1) (map #(if (pred %1) %2 -1) coll (range)))))

(defn cget [coll k]
  "Like get but works on all collections. Like get, returns nil if failure."
  (cond (not (coll? coll)) (throw (Exception. "Not a collection"))
        (or (map? coll) (set? coll) (vector? coll)) (get coll k)
        (and *err-on-long-list* (listoid? coll) (number? k) (> k 20) (> (count (take 22 coll)) 21)) (_el coll) ; perofrmance assertion.
        :else (try (nth coll k) (catch IndexOutOfBoundsException e))))

(defn map-pinvert [mp]
  "Like map-invert but creates hash-sets, so duplicate values don't reduce. Thus it's lossless."
  (reduce #(let [v (get mp %2) acc (get %1 v)] 
             (assoc %1 v (if acc (conj acc %2) (hash-set %2)))) {} (keys mp)))

(defn collisions? [mp] 
  "Does the map have any collisions in it? O(n)."
  (not= (count (keys (set/map-invert mp))) (count (keys mp))))

(defn resolve-collisions [f mp & mode]
  "Resolves collisions (keys mapping to the same value) using f.
   f takes two arguments: (f froms to) where each element in the vector froms maps to.
   f must return the tos.
   f will still run when there is no collision (froms will have one value).
   mode can be :single, :errcatch, or :deep.
   :single and :errcatch run f once, with :errcatch throwing an error if there is a duplicate key.
   :deep (the default) will keep running resolve-collisions until there are no collisions.
   Example fn for numeric values: (fn [ks v] (mapv #(+ v %) (range (count ks))))."
       ; Hash-sets of the keys mapping to values.
  (let [mp-pinv (map-pinvert mp)
        mode (if (first mode) (first mode) :deep)
        ; single iteration:
        mp1 (reduce #(let [ks (get mp-pinv %2) ks_ (into [] ks)] ; all keys that map to this value.
                       (merge %1 (zipmap ks_ (f ks_ %2)))) {} (keys mp-pinv))]  
    (cond (= mode :single) mp1
          (= mode :errcatch) (if (collisions? mp1) (throw (Exception. "f ran once didn't resolve collisions.")) mp1)
          (= mode :deep) (if (collisions? mp1) (resolve-collisions f mp1 :deep) mp1)
          :else (throw (Exception. (str "Unrecognized mode: " mode))))))

(defn reduce-to-map [f-red init]
  "Converts a function suitable for reduce into one for map, with initial value init.
   Used on stuff like walk where we want to accumilate stuff rather than do an in-place modification.
   Call f with no arguments to get the atom's value."
  (let [at (atom init)]
    (fn ([] @at) 
     ([& xis] (swap! at #(apply f-red % xis)) nil))))

(defn _paths-walked [acc t p] ; p = path to get to this tree t, acc is the traversal.
  (let [kys (reverse (if (coll? t) (ckeys t) []))
        acc1 (reduce (fn [a k] (_paths-walked a (gett t k) (conj p k))) acc kys)] 
          (into [] (concat [p] acc1))))
(defn paths-walked [t]
  "Returns a vector of all paths in the tree t, in depth-first order.
   Handles all clojure collections (but isn't lazy)."
    (_paths-walked [] t []))

(defn sus 
  "Like subs but does not throw an error for out of bounds. 
  Instead, indexes are clamped to the bounds."
  ([s lo]
    (sus s lo (count s)))
  ([s lo hi]
    (let [lo_ (min (max 0 lo) (count s)) hi_ (min (max 0 hi) (count s))]
      (if (< lo_ hi_)
        (subs s lo_ hi_) ""))))

(defn asus [^chars cs lo hi]
  "Array to string like sus. Clamps to the end of the array instead of throwing errors.
   hi must always be specified, use a large value to go all the way to the end."
  (let [n (alength ^chars cs)]
    (if (= n 0) "" ; woops.
      (apply str (mapv #(aget ^chars cs %) (range (max lo 0) (min hi n)))))))

(defn get-and-reset! [at newval]
  "Resets atom to newval and returns the old value. Useful for dequeueing-like operations. Atomic"
  (let [tmp (atom [])] (swap! at #(do (reset! tmp %) newval)) @tmp))

; These simple functions come up a lot, because of let statements, etc:
(defn evens [coll] (take-nth 2 coll)) 
(defn odds [coll] (take-nth 2 (rest coll)))
(defn evensv [coll] (into [] (take-nth 2 coll))) ; vector form.
(defn oddsv [coll] (into [] (take-nth 2 (rest coll))))
(defn col? [x]
  "Is x a non-empty collection?
   Mainly used when dealing with blitted code."
  (and (coll? x) (not (empty? x))))