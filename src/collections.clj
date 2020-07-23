; Functions to unify the behavior of vectors, lists, sets, and maps which are better at preserving metadata, useful within the refactoring functions.
; This unified behavior means O(n) instead of O(1) costs in various places with the "wrong" collection.
; There are two main ways the unification happens:
; Allowing a wider range of collections, i.e. cassoc allows lists and nil-pads vectors when out of bounds.
; Matching the input type, i.e. cmap (and it's cousin rmap).
; Several design choices were made, there is no "best" option:
;    false or nil elements in sequentials or maps don't count as bieng there for keys or set operations, unless overidden for ckeys.
;      But filter doesn't set elements to nil it just contracts the vector.
;    keys and vals are the same for sets. keys are indexes for vectors.
;    The output metadata is always the first collection or the union of input collections (even for difference operations), 
;      as some collections may be missing their metadata. 

(ns collections
  (:require [clojure.set :as set]))

;;;;;;;;;;;;;;;; Foundational functions ;;;;;;;;;;;;;;

(defn listy? [x]
  "Does pr-str use ()? These all are treated as lists."
  (and (coll? x) (not (vector? x)) (not (map? x)) (not (set? x))))

(defn evens [x] (into [] (take-nth 2 x)))
(defn odds [x] (into [] (take-nth 2 (rest x))))

(defn wtf [x] (if (coll? x) (throw (Exception. (str "Unrecognized collection type:" (type x))))
                  (throw (Exception. "Not a collection."))))

(defn vs2m [v] "vector or set to map."
  (if (map? v) v (zipmap (if (set? v) v (range)) v)))

(defn bool2num [x]
  (cond (not x) 0 (= x true) 1 :else x))

(defn reduce-unchecked "Doesn't check if ^clojure.lang.IReduce, which is true for most vector or listy collections." 
  ([f coll] (.reduce ^clojure.lang.IReduce coll f))
  ([f val coll] (println "Coll is:" coll) (.reduce ^clojure.lang.IReduceInit coll f val)))

;;;;;;;;;;;;;;;; Modified functions (an accent on functions we know) ;;;;;;;;;;;;;;

(defn vcat [& cs] ; TODO: would catv be a better name consistency?
  "Returns a vector not a lazy thingy."
  (into [] (apply concat cs)))

(def vconcat vcat)

(defn argmax 
  "Different from max-key in that it returns the key not the value."
  ([x]
    (argmax identity x))
  ([f x]
    (let [x (vs2m x) kys (into [] (keys x)) n (count kys)]
      (loop [ix 0 record -1e234 best-k nil]
        (if (= ix n) best-k
          (let [ki (nth kys ix)
                fv (bool2num (f (get x ki)))
                gold-medal? (>= fv record)]
            (recur (inc ix) (if gold-medal? fv record)
              (if gold-medal? ki best-k))))))))

(defn argmin 
  "Different from max-key in that it returns the key not the value."
  ([x] (argmax #(- %) x))
  ([f x] (argmax #(- (bool2num (f %))) x)))

(defn get- 
  "Python would be x[k] for k < 0. Plenty of (dec (count x)) code could be easier."
  ([x k] (get- x k nil))
  ([x k not-found]
    (if (and (sequential? x) (< k 0))
      (get (if (vector? x) x (into [] x))
        (+ (count x) k) not-found) not-found)))

(defn update- [x k f & args]
  "Python would be x[k]=f(x[k]) for k < 0. Plenty of (dec (count x)) code could be easier."
  (let [k (+ (count x) k)
        v? (vector? x) x1 (if v? x (into [] x))
        v (apply f (get x k) args)
        x2 (assoc x k v)]
    (if v? x2 (apply list x2))))

(defn linspace
  ;https://crossclj.info/ns/anglican/1.0.0/anglican.ais.html#_linspace
  "returns a equally spaced sequence of points"
  [start end size]
  (let [delta (/ (- end start) (dec size))]
    (map (fn [n] (+ start (* n delta)))
         (range size))))

(defn third [x] (first (rest (rest x))))
(defn fourth [x] (first (rest (rest (rest x)))))

;;;;;;;;;;;;;;;; Generalized functions (wider valid argument set) ;;;;;;;;;;;;;;

(defn cget [x k]
  (cond (or (map? x) (set? x) (vector? x)) (get x k)
    (not (coll? x)) nil
   :else (get (into [] x) k)))

(defn cget-in [x ks] (reduce cget x ks))

(defn cassoc [x k v & kvs]
  (let [metax (meta x)]
    (with-meta
      (cond
       (or (nil? x) (map? x)) (apply assoc x k v kvs)
       (sequential? x)
       (let [v? (vector? x) xm (zipmap (range (count x)) x)
             xm (apply assoc xm k v kvs) nks (sort (filterv number? (keys xm)))
             n (inc (if (last nks) (last nks) -1))
             xv (mapv #(get xm %) (range n))] ; nil fill, ignores non-numerical keys.
         (if v? xv (apply list xv)))
       (set? x) (let [kys (set (concat [k] (evens kvs)))
                      vs (set (concat [v] (odds kvs)))]
                  (set/union (set/difference x kys) vs)) ; Replace the key with the val.
       :else (wtf x)) metax)))

(defn cupdate [x k f & args]
  (let [v (apply f (cget x k) args)]
    (cassoc x k v)))

(defn cassoc-in [x ks v]
  (if (empty? ks) v
      (cupdate x (first ks) cassoc-in (rest ks) v)))

(defn cupdate-in [x ks f & args]
  (let [v (apply f (cget-in x ks) args)]
    (cassoc-in x ks v)))

(defn cpeek [x]
  (if (sequential? x) (peek x) (first x)))

(defn cpop [x]
  (cond (nil? x) nil
        (sequential? x) (pop x)
        (map? x) (dissoc x (first (first x)))
        (set? x) (disj x (first x))
        :else (wtf x)))

(defn cconj [x v] ; doesn't generalize well.
  (cond (nil? x) (list v) (or (sequential? x) (list? x) (set? x)) (conj x v)
      (map? x) (assoc x (gensym) v) :else (wtf x)))

(defn creverse [x] ; doesnt affect maps or sets.
  (cond (vector? x) (with-meta (into [] (reverse x)) (meta x))
    (sequential? x) (reverse x)    
    :else x))

(defn cdissoc [x k & ks]
  (if (empty? ks)
    (let [metax (meta x)]
      (with-meta 
        (cond (nil? x) [nil]
              (sequential? x)      
              (cond (= (count x) (inc k))
                (if (vector? x) (into [] (butlast x))
                    (apply list (butlast x)))
                (> (count x) (inc k))
                (cassoc x k nil)
                :else x)
              (map? x) (dissoc x k)
              (set? x) (disj x k)
              :else (wtf x))
        metax)) (apply cdissoc (cdissoc x k) ks)))

(defn cselect-keys [x kys]
  (let [metax (meta x)]
    (with-meta 
      (cond (nil? x) {}
            (sequential? x)
            (let [xv (if (vector? x) x (into [] x))]
              (reduce #(if-let [xi (get xv %2)] (conj %1 xi) %1)
                      (if (vector? x) [] ()) kys))
            (map? x) (select-keys x kys)
            (set? x) (set/intersection x (set kys))
            :else (wtf x))
      metax)))

(defn cmerge [& xs]
  "Metadata also merged, the type of the first element determines the output type."
  (let [xs (mapv #(if % % {}) xs)
        metax (apply merge (mapv meta xs)) ; merge metamaps.
        xms (mapv #(cond (set? %) (zipmap % %) (map? %) % :else (zipmap (range) (into [] %))) xs)
        x0 (first xs)
        xms (if (sequential? x0) (mapv (fn [mp] (reduce #(if (get %1 %2) %1 (dissoc %1 %2)) mp (keys mp))) xms) xms) ; remove nil/false.
        big-map (apply merge xms)]
    (if (not x0) #{}
        (with-meta 
          (cond (sequential? x0)
                (let [n (apply max 0 (keys big-map))
                      xv (reduce #(assoc %1 %2 (get big-map %2)) (into [] (repeat n nil)) (keys big-map))]
                  (if (vector? x0) xv (apply list xv)))
                (map? x0) big-map
                (set? x0) (set (keys big-map))
                :else (wtf x0))
          metax))))

(defn cmerge-with [f & xs]
  "Metadata also merged, the type of the first element determines the output type."
  (let [xs (mapv #(if % % {}) xs)
        metax (apply merge (mapv meta xs)) ; merge metamaps.
        xms (mapv #(cond (set? %) (zipmap % %) (map? %) % :else (zipmap (range) (into [] %))) xs)
        x0 (first xs)
        xms (if (sequential? x0) (mapv (fn [mp] (reduce #(if (get %1 %2) %1 (dissoc %1 %2)) mp (keys mp))) xms) xms) ; remove nil/false.
        big-map (apply merge-with f xms)]
    (if (not x0) {}
        (with-meta 
          (cond (sequential? x0)
                (let [n (apply max 0 (keys big-map))
                      xv (reduce #(assoc %1 %2 (get big-map %2)) (into [] (repeat n nil)) (keys big-map))]
                  (if (vector? x0) xv (apply list xv)))
                (map? x0) big-map
                (set? x0) (set (keys big-map))
                :else (wtf x0))
          metax))))

(defn ccontains? [x k]
  (if (and (not (vector? x)) (sequential? x)) (contains? (into [] x) k)
      (contains? x k)))

(defn cfind [x k]
  (if (and (not (vector? x)) (sequential? x)) (find (into [] x) k)
      (find x k)))

(defn ckeys [x]
  (let [include-seq-falses true] ; the controversy.
      (cond (and (sequential? x) include-seq-falses)
            (apply list (range (count x)))
            (sequential? x) (let [xv (into [] x)]
                              (apply list (filterv #(get xv %) (range (count x)))))
            (set? x) (apply list x)
            :else (keys x))))

(defn cvals [x]
  (cond (vector? x) (apply list x)
        (sequential? x) x
        (set? x) (apply list x)
        :else (vals x)))

(def cunion cmerge) 

(defn cdifference [x & xs]
  "Keps the metadata of the first x. Uses keys not vals for non-set colls."
  (if x
  (let [x (if x x #{}) xs (mapv (fn [xi] (if xi xi #{})) xs)
        shadow (apply cmerge xs)
        xk (set (ckeys x)) shk (set (ckeys shadow))
        keep-keys (set/difference xk shk)]
    (with-meta
      (cond (sequential? x)
            (let [n (apply max 0 keep-keys) xv (into [] x)
                  xv (reduce #(assoc %1 %2 (get xv %2)) (into [] (repeat n nil)) keep-keys)]
              (if (vector? x) xv (apply list xv)))
            (map? x) (zipmap keep-keys (mapv #(get x %) keep-keys))
            (set? x) keep-keys
            :else (wtf x))
      (meta x)))))

(defn cintersection [& xs]
  "Unions the metadata, as metadata may be missing on some components."
  (let [keep-keys (apply set/intersection (mapv #(set (ckeys %)) xs))
        x0 (first xs)]
  (if x0
    (with-meta
      (cond (sequential? x0)
            (let [n (apply max 0 keep-keys) xv (into [] x0)
                  xv (reduce #(assoc %1 %2 (get xv %2)) (into [] (repeat n nil)) keep-keys)]
              (if (vector? x0) xv (apply list xv)))
            (map? x0) (zipmap keep-keys (mapv #(get x0 %) keep-keys))
            (set? x0) keep-keys
            :else (wtf x0))
      (apply merge (mapv meta xs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Iterative functions ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cmap [f x & xs]
  "cmap treats map entires as [k v] vectors, passing them into f."
  (with-meta 
    (cond (nil? x) ()
      (sequential? x)
      (#(if (vector? x) (into [] %) (apply list %))
       (apply map f x xs))
      (map? x) (zipmap (keys x) (apply map f x xs))
      :else (set (apply map f x xs)))
    (meta x)))

(defn rmap [f x & xs]
  "Doesn't tuple map elements, just runs f on all keys and vals separately, useful in recursive functions."
  (with-meta
    (cond (or (not x) (vector? x)) (apply mapv f x xs)
          (sequential? x) (apply list (apply mapv f x xs))
          (map? x) (zipmap (apply mapv f (keys x) xs) (apply mapv f (vals x) xs))
          (set? x) (set (apply map f x xs))
          :else (wtf x))
    (meta x)))

(defn vmap [f x & xs]
  "Doesn't tuple map elements, just runs f on all vals but NOT keys. Also useful in recursive fns."
  (with-meta
    (cond (or (not x) (vector? x)) (apply mapv f x xs)
          (sequential? x) (apply list (apply mapv f x xs))
          (map? x) (zipmap (keys x) (apply mapv f (vals x) xs))
          (set? x) (set (apply map f x xs))
          :else (wtf x))
    (meta x)))

(defn cfilter [f x]
  (with-meta
    (cond (nil? x) ()
          (vector? x) (filterv f x)
          (sequential? x) (apply list (filterv f x))
          (set? x) (set (filterv f x))
          (map? x) (reduce #(assoc %1 %2 (get x %2)) {} (filterv #(f (get x %)) (keys x)))
          :else (wtf x))
    (meta x)))

(defn cfilter-1 [f x] "Only one element gets returned." (first (cfilter f x)))

(defn juice-1 [pred coll]
  "Returns the first result of pred applied to coll.
   Contrast with cfilter-1 which returns the first *element*
   Not lazy (could be made lazy but with difficulty)."
  (let [vals (into [] (cvals coll)) n (count vals)]
    (loop [ix 0]
      (if (= ix n) nil
        (if-let [xtract (pred (nth vals ix))] xtract (recur (inc ix)))))))

(defn where [pred coll]
  "Returns where in coll (the key) pred is true. 
   Not lazy (could be made lazy but with difficulty)."
  (let [kys (into [] (ckeys coll)) vals (into [] (cvals coll))
        ixs (filterv #(pred (nth vals %)) (range (count vals)))]
    (apply list (mapv #(nth kys %) ixs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Working with metadata ;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn keep-meta [x f & args] 
  "Applies f w/o affecting meta. Throws an error if x had meta and (f x) can't hold meta."
  (if (meta x)
    (with-meta (apply f x args) (meta x))
    (apply f x args)))

(defn dual-get-in [x ph-mph]
  "Simplify your paths.
   ph-mph is a tuple of paths, the first within x the second path within the meta."
  (let [ph (first ph-mph) mph (second ph-mph)]
    (cget-in (meta (cget-in x ph)) mph)))

(defn dual-assoc-in [x ph-mph v]
  "Simplify your paths.
   ph-mph is a tuple of paths, the first within x the second path within the meta."
  (let [ph (first ph-mph) mph (second ph-mph)]
    (cupdate-in x ph
      (fn [xi] (vary-meta xi #(cassoc-in % mph v))))))

(defn dual-update-in [x ph-mph f & args]
  "Simplify your paths.
   ph-mph is a tuple of paths, the first within x the second path within the meta."
  (let [ph (first ph-mph) mph (second ph-mph)]
    (cupdate-in x ph
      (fn [xi] (vary-meta xi #(apply cupdate-in % mph f args))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Tree functions ;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn _find-value-in [x v p]
  (cond (= x v) p
    (coll? x)
    (let [kys (into [] (ckeys x))
          vls (into [] (cvals x))]
      (first (filter identity (mapv #(_find-value-in %2 v (conj p %1)) kys vls))))))
(defn find-value-in [x v]
  "Finds the first path to value v in x.
   The path can't go through a key in maps.
   Value is determied by cvals. nil for not found."
  (_find-value-in x v []))


(defmacro _vc2 [v1 v2] 
  `(let [n# (count ~v2)]
     (loop [acc# ~v1 ix# 0]
       (if (= ix# n#) acc#
         (recur (conj acc# (nth ~v2 ix#)) (inc ix#))))))
(defn _paths [x root]
  (cond 
    (set? x) (let [n (count x) xv (into [] x)]
                   (loop [acc [[]] ix 0]
                     (if (= ix n) acc
                       (let [chiphs (_paths (nth xv ix) (conj root (nth xv ix)))]
                         (recur (_vc2 acc chiphs) (inc ix))))))
    (map? x) (let [n (count x) kys (into [] (keys x))]
                   (loop [acc [[]] ix 0]
                     (if (= ix n) acc
                       (let [chiphs (_paths (get x (nth kys ix)) (conj root (nth kys ix)))]
                         (recur (_vc2 acc chiphs) (inc ix))))))
    (coll? x) (let [n (count x) xv (into [] x)]
                      (loop [acc [[]] ix 0]
                        (if (= ix n) acc
                          (let [chiphs (_paths (nth xv ix) (conj root ix))]
                            (recur (_vc2 acc chiphs) (inc ix))))))
    :else [root]))
(defn paths [x] (_paths x []))

;; https://stackoverflow.com/a/33701239
;; Helper function to have vector's indexes work like for get-in
(defn- to-indexed-seqs [coll]
  (if (map? coll)
    coll
    (map vector (range) coll)))
(defn- flatten-path [path step]
  (if (coll? step)
    (->> step
         to-indexed-seqs
         (map (fn [[k v]] (flatten-path (conj path k) v)))
         (into {}))
    [path step]))
 
(defn _pwalk [f ph x]
  (f ph
    (cond (map? x)
      (zipmap (keys x) (mapv #(_pwalk f (conj ph %1) %2) (keys x) (vals x)))
      (set? x) (set (mapv #(_pwalk f (conj ph %) 1) x))
      (vector? x) (mapv #(_pwalk f (conj ph %1) %2) (range) x)
      (coll? x) (apply list (mapv #(_pwalk f (conj ph %1) %2) (range) x))
      :else x)))
(defn pwalk [f x] 
  "Pathed post walk, calls (f path coll). Not lazy."
  (_pwalk f [] x))

(defn dwalk [f x] "pwalk where f is given the path length (depth)." ; TODO: do we need this fn?
  (pwalk #(f (count %1) %2) x))
