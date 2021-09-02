; c = collections
; On the core collections, i.e. on vectors, lists, sets, and maps.
; We are NOT: Functions specalized in trees (use t).
; We are NOT: Functions specalized for handling metadata (use mt). However, some functions here are better at preserving metadata.

(ns c (:require [clojure.set :as set]))

;;;;;;;;;;;;;;;; Foundational functions ;;;;;;;;;;;;;;

(defn third [x] (first (rest (rest x))))
(defn fourth [x] (first (rest (rest (rest x)))))
(defn evens [x] (into [] (take-nth 2 x)))
(defn odds [x] (into [] (take-nth 2 (rest x))))

(defn listy? [x]
  "Does pr-str use ()? These all are treated as lists."
  (and (coll? x) (not (vector? x)) (not (map? x)) (not (set? x))))

(defn wtf [x] (if (coll? x) (throw (Exception. (str "Unrecognized collection type:" (type x))))
                  (throw (Exception. "Not a collection."))))

(defn queue
  "Creates or converts a coll to clojure's builtin queue type, from https://admay.github.io/queues-in-clojure/"
  ([] (clojure.lang.PersistentQueue/EMPTY))
  ([coll]
    (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

;;;;;;;;;;;;;;;; Small extraction functions (e.g. take one element) ;;;;;;;;;;;;;;;;

(defn cget [x k]
  "Like get but works for listy collections as well."
  (cond (or (map? x) (set? x) (vector? x)) (get x k)
    (not (coll? x)) nil
    (integer? k) (get (into [] (take (inc k) x)) k)
    :else nil))

(defn cpeek [x]
  (if (sequential? x) (peek x) (first x)))

(defn get-
  "Python would be x[k] for k < 0. Plenty of (dec (count x)) code could be easier."
  ([x k] (get- x k nil))
  ([x k not-found]
    (if (and (sequential? x) (< k 0))
      (get (if (vector? x) x (into [] x))
        (+ (count x) k) not-found) not-found)))

(defn ccontains? [x k]
  (if (and (not (vector? x)) (sequential? x)) (contains? (into [] x) k)
      (contains? x k)))

(defn cfind [x k]
  (if (and (not (vector? x)) (sequential? x)) (find (into [] x) k)
      (find x k)))

(defn juice-1 [pred coll]
  "Returns the first result of pred applied to coll.
   Contrast with cfilter-1 which returns the first *element*
   Not lazy (could be made lazy but with difficulty)."
  (let [coll1 (if (map? coll) (vals coll) coll)]
    (loop [tail coll1]
      (if (empty? tail) nil
        (if-let [xtract (pred (first tail))] xtract (recur (rest tail)))))))

(defn cfilter-1 [f x] "Only one element gets returned."
  (first (filter f (if (map? x) (vals x) x))))

;;;;;;;;;;;;;;;; Small modification fns, i.e. most elements unchanged may be shifted ;;;;;;;;;;;;;;;;

(defn update- [x k f & args]
  "Python would be x[k]=f(x[k]) for k < 0. Plenty of (dec (count x)) code could be easier."
  (let [k (+ (count x) k)
        v? (vector? x) x1 (if v? x (into [] x))
        v (apply f (get x k) args)
        x2 (assoc x k v)]
    (if v? x2 (apply list x2))))

(defn cpop [x]
  (cond (nil? x) nil
        (sequential? x) (pop x)
        (map? x) (dissoc x (first (first x)))
        (set? x) (disj x (first x))
        :else (wtf x)))

(defn cassoc [x k v & kvs]
  "Like assoc but works for listy collections as well."
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
  "Like update but works for listy collections as well."
  (let [v (apply f (cget x k) args)]
    (cassoc x k v)))

(defn cconj [x v] ; doesn't generalize well.
  (cond (nil? x) (list v) (or (sequential? x) (list? x) (set? x)) (conj x v)
      (map? x) (assoc x (gensym) v) :else (wtf x)))

(defn entry-conj [m k v]
  "Conj's v to (get m k [])."
  (assoc m k (conj (get m k []) v)))

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

;;;;;;;;;;;;;;;; Large extraction fns (e.g. take many elements) ;;;;;;;;;;;;;;;;

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

(defn cfilter [f x]
  (with-meta
    (cond (nil? x) ()
          (vector? x) (filterv f x)
          (sequential? x) (apply list (filterv f x))
          (set? x) (set (filterv f x))
          (map? x) (reduce #(assoc %1 %2 (get x %2)) {} (filterv #(f (get x %)) (keys x)))
          :else (wtf x))
    (meta x)))

(defn wfilterv [f x-test x-target]
  "With-filter: if x-test satisifies f include the cooreponding element from x-target.
   Returns a vector."
  (let [pairs (map vector x-test x-target)]
    (mapv second (filterv #(f (first %)) pairs))))

(defn filter-kv [pred map]
  "Selects the keys in map for which (pred k v) is true. Returns a map."
  (reduce #(let [v (get map %2)]
             (if (pred %2 v) (assoc %1 %2 v) %1)) {} (keys map)))

(defn where [pred coll]
  "Returns where in coll (the key) pred is true.
   Not lazy (could be made lazy but with difficulty)."
  (let [kys (into [] (ckeys coll)) vals (into [] (cvals coll))
        ixs (filterv #(pred (nth vals %)) (range (count vals)))]
    (apply list (mapv #(nth kys %) ixs))))

;;;;;;;;;;;;;;;; Reduction/summary fns ;;;;;;;;;;;;;;;;

(defn reduce-unchecked "Doesn't check if ^clojure.lang.IReduce, which is true for most vector or listy collections."
  ([f coll] (.reduce ^clojure.lang.IReduce coll f))
  ([f val coll] (println "Coll is:" coll) (.reduce ^clojure.lang.IReduceInit coll f val)))

;;;;;;;;;;;;;;;; Large modification fns, i.e. combining collections ;;;;;;;;;;;;;;;;

(defn vs2m [v] "vector or set to map."
  (if (map? v) v (zipmap (if (set? v) v (range)) v)))

(defn vcat [& cs] ; TODO: would catv be a better name consistency?
  "Returns a vector not a lazy thingy."
  (into [] (apply concat cs)))

(def vconcat vcat)

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

(defn creverse [x] ; doesnt affect maps or sets.
  (cond (vector? x) (with-meta (into [] (reverse x)) (meta x))
    (sequential? x) (reverse x)
    :else x))

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

;; https://stackoverflow.com/a/33701239
;; Helper function to have vector's indexes work like for get-in
(defn to-indexed-seqs [coll]
  (if (map? coll)
    coll
    (map vector (range) coll)))
