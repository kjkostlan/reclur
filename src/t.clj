; T = Trees. Functions specalized to work with trees, i.e. those that go multiple levels.

(ns t (:require [c]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Support functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn path-order-compare [p1 p2]
  "Compares two paths using 'sign(p1-p2)' which is approximatly <.
   If neither path is before as in hash-maps the other returns zero.
   Note: Hash maps or sets with integer keys WILL confuse it.
   [1 2] is before [1 2 0]."
  (let [p1 (into [] p1) p2 (into [] p2)
        n1 (count p1) n2 (count p2)
        n (max n1 n2)]
    (loop [ix 0]
      (cond (= ix n) 0 (= ix n1) -1 (= ix n2) 1
        :else (let [a (nth p1 ix) b (nth p2 ix)]
                (cond (= a b) (recur (inc ix))
                  (or (not (int? a)) (not (int? b))) 0
                  (< a b) -1 (> a b) 1 :else "Oops"))))))

;;;;;;;;;;;;;;;; Small extraction functions (e.g. take one element) ;;;;;;;;;;;;;;;;

(defn cget-in [x ks]
  "Like get-in but works for listy collections as well."
  (reduce c/cget x ks))

(defn _find-value-in [x v p include-map-keys?]
  (cond (= x v) p
    (coll? x)
    (let [kys (c/ckeys x) vls (c/cvals x)
          mk? (and (map? x) include-map-keys?
                (first (filter #(_find-value-in % v p true) kys)))]
      (if mk? (conj p mk?)
        (first (filter identity (mapv #(_find-value-in %2 v (conj p %1) include-map-keys?) kys vls)))))))
(defn find-value-in [x v & include-map-keys?]
  "Finds the first path to value v in x.
   The path can't go through a key in maps.
   Value is determied by cvals. nil for not found.
   include-map-keys?: Stuff that is or is inside of a map's keys paths to the map key itself."
  (_find-value-in x v [] (first include-map-keys?)))

;;;;;;;;;;;;;;;; Small modification fns, i.e. most elements unchanged may be shifted ;;;;;;;;;;;;;;;;

(defn cassoc-in [x ks v]
  (if (empty? ks) v
      (c/cupdate x (first ks) cassoc-in (rest ks) v)))

(defn cupdate-in [x ks f & args]
  (let [v (apply f (cget-in x ks) args)]
    (cassoc-in x ks v)))

;;;;;;;;;;;;;;;; Large extraction functions (e.g. take one element) ;;;;;;;;;;;;;;;;

#_(defn- flatten-path [path step]
  (if (coll? step)
    (->> step
         to-indexed-seqs
         (map (fn [[k v]] (flatten-path (conj path k) v)))
         (into {}))
    [path step]))

(defn find-values-in [x v]
  "All paths to v, not just the first one."
  (let [stop (if v false true)]
    (loop [xi x out []]
      (let [ph1 (find-value-in xi v)]
        (if (and ph1 (= ph1 (last out))) (throw (Exception. "Something is not working.")))
        (if ph1
          (recur (cassoc-in xi ph1 stop) (conj out ph1))
          out)))))

(defn _leaves [x]
  (if (not (coll? x)) [x]
    (apply concat (map _leaves (if (map? x) (vals x) x)))))
(defn leaves [x]
    "Leaf elements of x, walked depth-first."
  (into [] (_leaves x)))

(defmacro _vc2 [v1 v2] ; Inlined vector concatenate of two vector.
  `(let [n# (count ~v2)]
     (loop [acc# ~v1 ix# 0]
       (if (= ix# n#) acc#
         (recur (conj acc# (nth ~v2 ix#)) (inc ix#))))))
(defn _paths [x root]
  (cond
    (set? x) (let [n (count x) xv (into [] x)]
                   (loop [acc [root] ix 0]
                     (if (= ix n) acc
                       (let [chiphs (_paths (nth xv ix) (conj root (nth xv ix)))]
                         (recur (_vc2 acc chiphs) (inc ix))))))
    (map? x) (let [n (count x) kys (into [] (keys x))]
                   (loop [acc [root] ix 0]
                     (if (= ix n) acc
                       (let [chiphs (_paths (get x (nth kys ix)) (conj root (nth kys ix)))]
                         (recur (_vc2 acc chiphs) (inc ix))))))
    (coll? x) (let [n (count x) xv (into [] x)]
                      (loop [acc [root] ix 0]
                        (if (= ix n) acc
                          (let [chiphs (_paths (nth xv ix) (conj root ix))]
                            (recur (_vc2 acc chiphs) (inc ix))))))
    :else [root]))
(defn paths [x]
  "Paths to elements in x. Returns [[]] for non-collections"
  (_paths x []))

;;;;;;;;;;;;;;;; Walking functions ;;;;;;;;;;;;;;;;

(defn pwalk1 [f ph x] "pwalk with a root-path to specify"
  (f ph
    (cond (map? x) (zipmap (keys x) (mapv #(pwalk1 f (conj ph %1) %2) (keys x) (vals x)))
      (set? x) (set (mapv #(pwalk1 f (conj ph %) %) x))
      (vector? x) (mapv #(pwalk1 f (conj ph %1) %2) (range) x)
      (coll? x) (apply list (mapv #(pwalk1 f (conj ph %1) %2) (range) x))
      :else x)))
(defn pwalk [f x]
  "Pathed post walk, calls (f path subform). Not lazy."
  (pwalk1 f [] x))

(defn _dpwalk [f ph x max-depth]
  (f ph
    (cond (<= max-depth 0) x
      (map? x) (zipmap (keys x) (mapv #(_dpwalk f (conj ph %1) %2 (dec max-depth)) (keys x) (vals x)))
      (set? x) (set (mapv #(_dpwalk f (conj ph %) % (dec max-depth)) x))
      (vector? x) (mapv #(_dpwalk f (conj ph %1) %2 (dec max-depth)) (range) x)
      (coll? x) (apply list (mapv #(_dpwalk f (conj ph %1) %2 (dec max-depth)) (range) x))
      :else x)))
(defn dpwalk [f x max-depth] "Depth-limited pathwalk that applies (f path subform).
                              Max-depth zero means just applying f to [] the entire collection."
  (_dpwalk f [] x max-depth))

;;;;;;;;;;;;;;;; Reshuffling array structure functions ;;;;;;;;;;;;;;;;

(defn dim-mix [x dim-source & convert-dims-to-vector-form?s]
  "Generic map dimension rearrange and recombine. Stubby paths get removed if stubbier than dim-source's maximum.
   dim-source: dimensions going into each output dimension. Each element is a scalar or vector.
   Multible dimensions into one dimension: later varies faster (as numpy's np).
   convert-dims-to-vector-form?s: convert map entries to vectors (sorts the keys if activated).
   This can only rearrange or combine dimensions, it can't split them.
   (splitting would need a size)."
  ; TODO: does this belong in collecitons?
  (let [paths (set (paths x))
        dim-source (mapv #(if (coll? %) (into [] %) [%]) dim-source)
        ndim (inc (apply max (apply concat dim-source))) ; # of dims we actualy care about.
        cut #(if (> (count %) ndim) (subvec % 0 ndim) %)
        paths-short (set (filterv #(>= (count %) ndim) (mapv cut paths)))
        ; dim-source [1 [2 3] 2]
        old2new (reduce (fn [acc ix]
                          (reduce #(assoc %1 %2 ix)
                            acc (nth dim-source ix)))
                  {} (range (count dim-source))) ; Old path index to new path index collapsed.
        map2vec (fn [m] (mapv #(get m %) (sort (keys m))))
        change-ph (fn [ph] ; Change from old to new path.
                    (mapv #(if (= (count %) 1) (first %) %)
                      (map2vec (reduce #(let [jx (get old2new %2)]
                                          (c/entry-conj %1 jx (nth ph %2)))
                                 {} (range (count ph))))))
        xr (reduce
             (fn [acc ph]
               (let [add (cget-in x ph)]
                 (assoc-in acc (change-ph ph) add)))
             {} paths-short)
        ; Convert map levels to vec levels:
        convert?s (first convert-dims-to-vector-form?s)]
    (if (map? convert?s) (throw (Exception. "convert-dims-to-vector-form?s, if supplied, must be a vector.")))
    (if (not convert?s) xr
      (dpwalk
        (fn [path val]
          (let [depth (count path)]
            (if (and (map? val) (get convert?s depth))
              (map2vec val) val))) xr (count convert?s)))))

(defn _dim-split1 [x]
  (cond (not (map? x)) x
    (= (count x) 0) x
    :else
    (let [kys (into [] (keys x))
          nk (mapv count kys)
          c0 (apply min nk) c1 (apply max nk)]
      (cond (< c0 c1) (throw (Exception. "Uneven number of key lengths; can't dim split (or it would be more complex)."))
        (= c1 1) x
        :else (reduce-kv #(assoc-in %1 %2 %3) {} x)))))
(defn dim-split [x dims]
  (let [max-dim (apply max 0 dims)
        split?s (reduce #(assoc %1 %2 true) (into [] (repeat max-dim false)) dims)]
    (dpwalk (fn [ph x] (if (get split?s (count ph)) (_dim-split1 x) x))
      x max-dim)))

(defn dim-map2vec [x dims]
  "Converts maps to vectors at specified nesting levels.
   Maps must have integer keys."
  (let [max-dim (apply max 0 dims)
        vec?s (reduce #(assoc %1 %2 true) (into [] (repeat max-dim false)) dims)]
    (dpwalk (fn [ph x]
                          (if (or (not (map? x)) (not (get vec?s (count ph)))) x
                            (let [kys (into [] (keys x)) max-k (apply max kys)]
                              (if (first (remove integer? kys))
                                (throw (Exception. "Map key not an integer or long for converting to vector.")))
                              (reduce #(assoc %1 %2 (get x %2))
                                (into [] (repeat max-k false)) kys))))
      x max-dim)))