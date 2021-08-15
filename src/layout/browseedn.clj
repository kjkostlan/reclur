; Browsing large amounts of data in small amounts of space.

(ns layout.browseedn
  (:require [clojure.set :as set] [clojure.string :as string]
    [layout.blit :as blit]
    [collections]))


(def ^:dynamic *max-dig-range* 1e5)
(def ^:dynamic *target-fillchars* 512)
(def ^:dynamic *min-child-fraction* 0.1)
; Format examples:
; (1 2 3 ...(10-5)... 9 10), or [], or (), or #{}
; (1 2 3 ...???) ; (range)
; {:a,:b :c,:d}
; There is a path that prepends us.

(defn num2str [x]
  "No need to include all decimal places."
  (str x)) ;TODO

(defn nfill-char [x]
  (count (string/replace (string/replace (str x) "\n" "") " " "")))

(defn _as-counted [coll counted-version-of]
  "Makes coll counted? with the same paren or brackets as counted-version-of.
   If coll is a sequential? and counted-version-of is a map it will use [[k0 v0] [k1 v1] ...] format.
   TODO: belongs in collections?"
  (cond (vector? counted-version-of) (into [] coll) (set? counted-version-of) (set coll)
    (collections/listy? counted-version-of) (apply list (into [] coll))
    (and (map? counted-version-of) (map? coll)) coll
    (map? counted-version-of) (zipmap (mapv first coll) (mapv second coll))
    :else coll))

(declare summarize) ; summarize <-> _summarize1 circular.
(defn _summarize1 [path x n from-head? from-tail? opts]
  "One step, on a collection, with specified head and tail directions."
  (let [m? (map? x) s? (set? x) chars (:target-fillchars opts) dig (opts :max-dig-range opts)
        uno-dos (if (and from-head? from-tail?) 2 1)]
    (loop [n-used 0 c-used 0 acc-head [] acc-tail (list) xhead (seq x) xtail (if from-tail? (reverse x))]
            ; Bi-directional loop.
      (let [subsummary (fn [xi k] 
                         (let [xs (summarize (conj path k) (if m? (second xi) xi) opts)]
                           (if m? [(first xi) xs] xs)))
            
            k0 (cond m? (first (first xhead)) s? (first xhead) :else n-used)
            k1 (cond m? (first (first xtail)) s? (first xtail) (number? n) (- n n-used 1))             
            get-h (fn [] (subsummary (first xhead) k0))
            get-t (fn [] (subsummary (first xtail) k1))
            add-dot3 (fn []
                       (let [ellipse (if (= n "???") "(???)"
                                        (str "<" n "-" (* n-used uno-dos) ">"))]
                         (concat acc-head [(symbol (str (if from-head? "..." "") ellipse 
                                                     (if from-tail? "..." "")))] acc-tail)))]
        (cond (and from-head? from-tail? (= (* n-used uno-dos) (dec n)))
          (_as-counted (concat acc-head [(get-h)] acc-tail) x)
          (= (* n-used uno-dos) n)
          (_as-counted (concat acc-head acc-tail) x)
          (and (> (count acc-head) 0) (> c-used (- chars 5)))
          (_as-counted (add-dot3) x)
          :else (let [h (if from-head? (summarize (conj path k0) (first xhead) opts))
                      t (if from-tail? (summarize (conj path k1) (first xtail) opts))
                      c-used1 (+ c-used (if from-head? (nfill-char h) 0) 
                                (if from-tail? (nfill-char t) 0))]
                  (recur (inc n-used) c-used1 (if from-head? (conj acc-head h) acc-head) 
                     (if from-tail? (conj acc-tail t) acc-tail)
                     (if from-head? (rest xhead)) (if from-tail? (rest xtail)))))))))

(defn summarize [path x & opts]
  "Generates a human-friendly representation of x if x is large, and removes lazyness.
   Returns a nested data-structure, with symbols at the leaves
   Can handle infinite datastructures.
   Prepends path into the metadata of each symbol.
   (pr-str (summarize x)) is guarenteed to be resonablly sized on any non-malicious x."
  (let [opts (first opts)
        chars (get opts :target-fillchars *target-fillchars*)
        dig (get opts :max-dig-range *max-dig-range*)
        ch-frac-min (get opts :min-child-fraction *min-child-fraction*)
        n (cond (not (coll? x)) 1 (counted? x) (count x)
            :else (let [n0 (count (take dig x))] (if (< n0 dig) n0 "???")))
        sym+meta #(with-meta (symbol %) {:path path})
        ch-frac (max ch-frac-min (/ 1.0 (if (number? n) n dig)))
        opts1 (assoc opts :target-fillchars (Math/ceil (* chars ch-frac)) :max-dig-range (Math/ceil (* dig ch-frac)))]
    (cond
      (and (coll? x) (not= n "???"))
      (_summarize1 path x n true true opts1)
      (coll? x) ; Uncounted colls, such as infinite sequences.
      (_summarize1 path x n true false opts1)
      (number? x) (sym+meta (num2str x))
      :else (sym+meta (str x)))))

;coder.cnav/tree-diff
;blit/vps
