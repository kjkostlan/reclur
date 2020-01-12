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

(defn evens [v] (keep-indexed #(if (even? %1) %2) v)) ; useful for picking apart vectorized lets and maps.
(defn odds [v] (keep-indexed #(if (odd? %1) %2) v))

(defn wtf [x] (if (coll? x) (throw (Exception. (str "Unrecognized collection type:" (type x))))
                  (throw (Exception. "Not a collection."))))

;;;;;;;;;;;;;;;; Generalized functions ;;;;;;;;;;;;;;

(defn cget [x k]
  (if (or (map? x) (set? x) (vector? x)) (get x k)
      (get (into [] x) k)))

(defn cget-in [x ks]
  (if (empty? ks) x
      (cget-in (cget x (first ks)) (rest ks))))

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
       (set? x) (apply conj x k (evens kvs)) ; only keys matter.
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

(defn cmap [f & xs]
  "cmap treats map entires as [k v] vectors, passing them into f."
  (let [x0 (first xs)]
    (with-meta 
      (cond (nil? x0) ()
        (sequential? x0)
        (#(if (vector? x0) (into [] %) (apply list %))
         (apply map f xs))
        (map? x0) (zipmap (keys x0) (apply map f xs))
        :else (set (apply map f xs)))
      (meta x0))))

(defn cfilter [f x]
  (with-meta
    (cond (nil? x) ()
          (vector? x) (filterv f x)
          (sequential? x) (apply list (filterv f x))
          (set? x) (set (filterv f x))
          (map? x) (reduce #(assoc %1 %2 (get x %2)) {} (filterv #(f (get x %)) (keys x)))
          :else (wtf x))
    (meta x)))


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
  (let [ph (first ph-mph) mph (second ph-mph)]
    (cupdate-in x ph
      (fn [xi] (vary-meta xi #(cassoc-in % mph v))))))

(defn dual-update-in [x ph-mph f & args]
  (let [ph (first ph-mph) mph (second ph-mph)]
    (cupdate-in x ph
      (fn [xi] (vary-meta xi #(apply cupdate-in % mph f args))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; More unique functions ;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn rmap [f x]
  "Doesn't tuple map elements, just runs f on all keys and vals separately, useful in recursive functions."
  (with-meta
    (cond (or (not x) (vector? x)) (mapv f x)
          (sequential? x) (apply list (mapv f x))
          (map? x) (zipmap (mapv f (keys x)) (mapv f (vals x)))
          (set? x) (set (map f x))
          :else (wtf x))
    (meta x)))

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
