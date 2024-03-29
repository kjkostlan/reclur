; Browsing large amounts of data in small amounts of space.

(ns layout.browseedn
  (:require [clojure.set :as set] [clojure.string :as string] [clojure.walk :as walk]
    [app.codebox :as codebox]
    [coder.crosslang.langs :as langs]
    [c] [t]))

(def ^:dynamic *max-dig-range* 1e5)
(def ^:dynamic *shallow-digfurther-ratio* 8)
(def ^:dynamic *target-fillchars* 512)
(def ^:dynamic *min-child-fraction* 0.1)
(def ^:dynamic *min-outerlevel-nshow* 64)
; Format examples:
; (1 2 3 ...(10-5)... 9 10), or [], or (), or #{}
; (1 2 3 ...???) ; (range)
; {:a,:b :c,:d}
; There is a path that prepends us.

(defn num2str [x]
  "No need to include all decimal places."
  (str x)) ;TODO

(defn to-sym [x]
  "Symbols can hold metadata."
  (symbol (if (number? x) (num2str x) (pr-str x))))

(defn valid? [tok] (try (read-string tok) true (catch Exception e false)))

(defn nfill-char [x]
  (count (string/replace (string/replace (str x) "\n" "") " " "")))

(defn _as-counted [coll counted-version-of]
  "Makes coll counted? with the same paren or brackets as counted-version-of.
   If coll is a sequential? and counted-version-of is a map it will use [[k0 v0] [k1 v1] ...] format.
   TODO: belongs in c?"
  (cond (vector? counted-version-of) (into [] coll) (set? counted-version-of) (set coll)
    (c/listy? counted-version-of) (apply list (into [] coll))
    (and (map? counted-version-of) (map? coll)) coll
    (map? counted-version-of) (zipmap (mapv first coll) (mapv second coll))
    :else coll))

(declare _summarize) ; _summarize <-> _summarize1 circular.
(defn _summarize1 [path x n from-head? from-tail? opts]
  "One step, on a collection, with specified head and tail directions."
  (let [m? (map? x) s? (set? x) chars (:target-fillchars opts) dig (:max-dig-range opts)
        uno-dos (if (and from-head? from-tail?) 2 1)
        ch-frac-min (get opts :min-child-fraction *min-child-fraction*)
        ch-frac (max ch-frac-min (/ 1.0 (if (number? n) n dig)))
        opts1 (assoc opts :target-fillchars (Math/ceil (* chars ch-frac))
                :max-dig-range (Math/ceil (* dig ch-frac)))
        mk2sym #(if m? (zipmap (mapv to-sym (keys %)) (vals %)) %)
        outer-lev? (= (count path) 0)]
    (loop [n-used 0 c-used 0 acc-head [] acc-tail (list) xhead (seq x) xtail (if from-tail? (reverse x))]
            ; Bi-directional loop.
      (let [subsummary (fn [xi k]
                         (let [path1 (conj path k)
                               xs (_summarize path1 (if m? (second xi) xi) opts1)]
                           (if m? [(to-sym (_summarize path1 (first xi) opts1)) xs] xs)))

            k0 (cond m? (first (first xhead)) s? (first xhead) :else n-used)
            k1 (cond m? (first (first xtail)) s? (first xtail) (number? n) (- n n-used 1))
            get-h (fn [] (subsummary (first xhead) k0))
            get-t (fn [] (subsummary (first xtail) k1))
            add-dot3 (fn []
                       (let [ellipse (if (= n "???") "(???)"
                                        (str "<" n "-" (* n-used uno-dos) ">"))
                             midpiece [(str (if from-head? "..." "") ellipse (if from-tail? "..." ""))]
                             midpiece (mapv symbol midpiece)
                             midpiece (if (map? x) [(conj midpiece (symbol "<kys>"))] midpiece)]
                         (concat acc-head midpiece acc-tail)))]
        (cond (and from-head? from-tail? (= (* n-used uno-dos) (dec n)))
          (mk2sym (_as-counted (concat acc-head [(get-h)] acc-tail) x))
          (= (* n-used uno-dos) n)
          (mk2sym (_as-counted (concat acc-head acc-tail) x))
          (and (> (count acc-head) 0) (> c-used (- chars 5)) (or (not outer-lev?) (>= (* n-used uno-dos) *min-outerlevel-nshow*)))
          (mk2sym (_as-counted (add-dot3) x))
          :else (let [h (if from-head? (_summarize (conj path k0) (first xhead) opts1))
                      t (if from-tail? (_summarize (conj path k1) (first xtail) opts1))
                      c-used1 (+ c-used (if from-head? (nfill-char h) 0)
                                (if from-tail? (nfill-char t) 0))]
                  (recur (inc n-used) c-used1 (if from-head? (conj acc-head h) acc-head)
                     (if from-tail? (conj acc-tail t) acc-tail)
                     (if from-head? (rest xhead)) (if from-tail? (rest xtail)))))))))

(defn _summarize [path x opts]
  (let [chars (get opts :target-fillchars)
        dig (int (get opts :max-dig-range))
        shallow (int (get opts :shallow-digfurther-ratio))
        n (cond (not (coll? x)) 1 (counted? x) (count x)
            :else (let [n0 (count (take dig x))] (if (< n0 dig) n0 "???")))
        sym+meta #(with-meta (to-sym %) {::path path})
        coll+meta #(with-meta % {::path path})
        meta1 #(try (with-meta %1 (meta %2)) (catch Exception e %1))
        meta-kvs-if-map (fn [x1] (if (map? x1) (zipmap (mapv meta1 (keys x1) (vals x1))
                                                 (vals x1)) x1))]
    (cond
      (and (coll? x) (not= n "???"))
      (coll+meta (meta-kvs-if-map (_summarize1 path x n true (or (vector? x) (< n (* dig shallow))) opts)))
      (coll? x) ; Uncounted colls, such as infinite sequences.
      (coll+meta (_summarize1 path x n true false opts))
      :else (sym+meta x))))
(defn summarize [x & opts]
  "Generates a human-friendly representation of x if x is large, and removes lazyness.
   Returns a nested data-structure, with symbols at the leaves
   Can handle infinite datastructures.
   Prepends path into the metadata of each symbol.
   (pr-str (summarize x)) is guarenteed to be resonablly sized on any non-malicious x."
  (let [opts-default {:target-fillchars *target-fillchars*
                      :max-dig-range *max-dig-range*
                      :shallow-digfurther-ratio *shallow-digfurther-ratio*}
        opts1 (merge opts-default opts)]
    (_summarize [] x opts1)))

(defn path-at-cursor [x-summarized x-summarized-txt cursor-ix]
  "What path on the original x the cursor in x-summarized is at, nil if failure. Does not work for *print-meta*."
  (if-let [hot-path (into [] (rest (langs/stringlang-to-wpath x-summarized-txt cursor-ix :clojure)))]
    (let [_ (if (string/includes? (str (first hot-path)) "IAmUnique")
              (println "You must click on map values, not keys (bug)."))
          hot-path (if (string/includes? (str (last hot-path)) "IAmUnique") (into [] (butlast hot-path)) hot-path)
          x-piece (reduce (fn [branch k]
                            (c/cget branch
                              (if (map? branch) (to-sym k) k)))
                    x-summarized hot-path)]
      (::path (meta x-piece)))))