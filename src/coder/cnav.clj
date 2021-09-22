; Code NAVigation

(ns coder.cnav
  (:require [clojure.walk :as walk]
    [clojure.set :as set]
    [clojure.string :as string]
    [coder.textparse :as textparse]
    [c] [t] [np]))

;;;;;;;; Support fns ;;;;;;;;;

(def specials #{'def 'let* 'fn* 'var 'quote 'if 'loop* 'recur '. 'new 'throw 'catch 'monitor-enter 'monitor-exit 'set! 'do})
(def def-variants #{'def 'defn `defn 'defmacro `defmacro 'def*})

(defn _lty [x]
  (cond (not (coll? x)) -1
    (vector? x) 0
    (set? x) 2
    (map? x) 3
    :else 1)) ; listes et al.

(defn list-is-list [t]
  (clojure.walk/prewalk #(if (= (_lty %) 1) (apply list %) %) t))

(defn _class? [code]
  (and (c/listy? code)
    (contains? #{'defclass 'definterface `definterface 'defenum 'defprotocol `defprotocol} (first code))))

;;;;;;;;;;;;;;;;;;;;;;; Walk-a-collection functions ;;;;;;;;;;;;;;;;;;;;;;

(defn symbols-in [x]
  "Acts recursivly, returns a set. Does not include special forms or map keys."
  (cond (contains? specials x) #{}
    (symbol? x) #{x}
    (map? x) (set/union #_(symbols-in (keys x)) (symbols-in (vals x)))
    (coll? x) (apply set/union (mapv symbols-in x))
    :else #{}))

(defn unique-leaves [x exclude-f]
  "Makes leaves unuqie, unless exclude-f is true on the leaf."
  (let [a (atom 0)
        xform! #(let [ix (str @a) _ (swap! a inc)]
                  (cond (and exclude-f (exclude-f %)) %
                    (symbol? %) (symbol (str "sym" ix))
                    (string? %) (str ix)
                    (keyword? %) (keyword (str "kwd" ix))
                    (coll? %) %
                    (number? %) ix
                    :else (str "leafy" ix)))]
    (walk/postwalk xform! x)))

;;;;;;;; Comparing two collections ;;;;;;;

(defn leaf-path-map [x y]
  "Map from path in x to path in y for any leaf element. If a leaf element appears more than once in y
   only one path is chosen. Using ^:leaf-meta also will let us associcate."
  (let [lget-in (fn [x ph] (let [yi (t/cget-in x ph)
                                 lm (:leaf-meta (meta yi))]
                             (cond (not (coll? yi)) yi lm lm :else false)))
        leaf2py (reduce #(let [v (lget-in y %2)]
                           (if v (assoc %1 v %2) %1)) {} (t/paths y))]
    (reduce #(let [xi (lget-in x %2)]
               (if (get leaf2py xi) (assoc %1 %2 (get leaf2py xi)) %1))
      {} (t/paths x))))

(defn leaf-branch-path-map [x y]
  "Not just leaves."
  (let [lpm (leaf-path-map x y)

        max-depth (apply max (mapv count (keys lpm)))
        cut (fn [v n-cut]
              (if (>= (count v) n-cut) (subvec (into [] v) 0 (- (count v) n-cut))))
        no-nil (fn [m] (select-keys m (filter #(and % (get m %)) (keys m))))
        ; Sucessively more stubby branch-maps:
        branch-phms (mapv (fn [n-cut] (no-nil (zipmap (mapv #(cut % n-cut) (keys lpm))
                                                (mapv #(cut % n-cut) (vals lpm)))))
                      (range max-depth))
        phx (t/paths x)
        get-ph (fn [px]
                 (let [phs (mapv #(get % px) branch-phms)]
                   (first (filterv #(and % (= (t/cget-in x px) (t/cget-in y %))) phs))))
        paths-x (t/paths x)]
    (c/filter-kv
      (fn [k v] v) (zipmap paths-x (mapv get-ph paths-x)))))

(defn tree-diff [x y]
  "Finds a shortest path that differences x and y, false if x=y.
   Did this get written b4?
   Should this (and other fns?) go in c?"
  (let [x (list-is-list x) y (list-is-list y) ; May not be necessary.
        diff
            (cond (not= (_lty x) (_lty y)) []
              (and (not (coll? x)) (not= x y)) []
              (= x y) false
              (set? x) []
              (and (map? x) (not= (keys x) (keys y))) []
              (not= (count x) (count y)) []
              (map? x) (if-let [k (first (filter #(not= (get x %) (get y %)) (keys x)))] [k] false)
              :else (let [x (into [] x) y (into [] y)
                          diff (first (filter #(not= (nth x %) (nth y %)) (range (count x))))]
                      (if diff [diff] false)))
        diff0 (if diff (first diff))]
    (cond (not diff) false
      (= (count diff) 0) []
      (map? x) (into [] (concat diff (tree-diff (get x diff0) (get y diff0))))
      :else (let [x (into [] x) y (into [] y)]
              (into [] (concat diff (tree-diff (nth x diff0) (nth y diff0))))))))

(defn drag-path [code-old code-new path-old]
  "Tries to find a corresponding path, nil if failure.
   No obvious algorithim here, more of a heuristic. Room for much improvement."
  (let [np (count path-old)
        nesting-keys (mapv #(t/cget-in code-old
                              (subvec path-old %))
                       (range np)) ; shallow -> deep.
        pathss-old (mapv #(t/find-values-in code-old %) nesting-keys)
        pathss-new (mapv #(t/find-values-in code-new %) nesting-keys)
        shallowest (first (filter #(> (count %) 0) pathss-new))
        deepest (last (filter #(> (count %) 0) pathss-new))]
    (if deepest ; Which is most similar? How deep can we go?
      (let [pathdif (fn [a b] (reduce + (mapv #(if (= %1 %2) 0.0 1.0) a b)))
            diffs (mapv #(pathdif path-old %) shallowest)]
        (nth deepest (np/argmax diffs)))
      (if (and (> np 0) ; very simple subsitution
            (let [v0 (t/cget-in code-old (subvec path-old 0 (dec np)))
                  v1 (t/cget-in code-new (subvec path-old 0 (dec np)))]
              (and (vector? v0) (vector? v1) (= (count v0) (count v1))))
            (t/cget-in code-new path-old))
       path-old false))))

;;;;;;;; Clojure-aware, single path return ;;;;;;

(defn path2subdef-path [codes path]
  "The part of path that digs into the def.
   Handles java classes and other stuff, but for normal defns it simply removes the first element from path.
   defclass, definterface, defenum, and defprotocol trigger going deeper."
  (loop [codes1 (nth codes (first path)) path1 (into [] (rest path))]
    (if (_class? codes1) (recur (c/cget codes1 (first path)) (into [] (rest path1)))
      path1)))

(defn symbol2defpath-qual [codes sym]
  "The path to the path enclosing def in codes.
   In java and other languages, it usually wouldn't be explicitly a 'def'."
  (let [def-paths (into [] (apply concat (mapv #(t/find-values-in codes %) def-variants)))
        defenclose-paths (mapv #(into [] (butlast %)) def-paths)
        def-vals (mapv #(t/cget-in codes (conj % 1)) defenclose-paths)
        sym-unqual (textparse/unqual sym)
        path-ix (first (filter #(or (= (nth def-vals %) sym-unqual) (= (nth def-vals %) sym)) (range (count def-vals))))
        _ (if (not path-ix) (throw (Exception. (str "Can't find the def-path for: " sym))))
        path+1 (nth def-paths path-ix)]
    (subvec path+1 0 (dec (count path+1)))))

;;;;;;;; Clojure-aware, leaf return ;;;;;;

(defn path2defsym [codes path]
  "The foo in (def foo)."
  (let [fpath (path2subdef-path codes path) path2def (subvec (into [] path) 0 (- (count path) (count fpath)))]
    (t/cget-in codes (conj path2def 1))))

(defn sym-def? [code path]
  "Is path in code the location of a local defined symbol?
   I.e. the path to 'x in '(let [x 1]).
   Class members don't count as local, variables defined within fns do."
  (let [x (t/cget-in code path)
        first? #(if (coll? %) (first %) false)
        pairbind? (contains? #{`let `let* `loop `loop* 'let 'loop} (first? (t/cget-in code (drop-last 2 path))))
        fn-unpacked? (contains? #{`fn `fn* 'fn} (first? (t/cget-in code (drop-last 2 path))))
        fn-packed? (contains? #{`fn `fn* 'fn} (first? (t/cget-in code (drop-last 3 path))))
        in-vector? (vector? (t/cget-in code (drop-last 1 path)))
        second? (= (last (butlast path)) 1)]
    (cond
      (< (count path) 2) false (not (symbol? x)) false (not in-vector?) false (not second?) false
      (or (and pairbind? (even? (last path)))
        fn-unpacked? fn-packed?) true
      :else false)))

;;;;;;;; Clojure-aware, vector of paths return ;;;;;;

(defn fnargpack-paths [defn-code]
  "Paths to the argpacks in code. Returns a vector with only one element for single arity functions."
  (if (contains? #{'def 'def*} (first defn-code))
    (let [ix (dec (count defn-code))]
      (mapv #(c/vcat [ix] %) (fnargpack-paths (last defn-code))))
    (if (vector? (second defn-code)) [[1]]
      (let [ixs (filterv #(c/listy? (c/cget defn-code %)) (range (count defn-code)))]
        (mapv #(conj [%] 0) ixs)))))

(defn fnresult-paths [code]
  "Log paths to the function's result. One path per each arity. Flexible to macroexpanding vs not and other formatting."
  (let [cl (last code) cl0 (if (coll? cl) (first cl))
        explicit-fn? (contains? #{'fn* `fn 'fn} cl0) ; Is it (def ... (fn ...))
        prepend (if explicit-fn? [(dec (count code))] [])
        fcode (t/cget-in code prepend)
        packed? (not (first (filter vector? fcode))); (fn ([a b] ...)) vs (fn [a b] ...)
        paths-in-fcode (if packed? (mapv #(vector % (dec (count (c/cget fcode %))))
                                     (filterv #(c/listy? (c/cget fcode %)) (range (count fcode))))
                         [[(dec (count fcode))]])]
    (mapv #(c/vcat prepend %) paths-in-fcode)))

(defonce _core-stuff (set (keys (ns-map (find-ns 'clojure.core)))))
(defn fncall-paths [code & ns-sym]
  "Paths to forms that call external, non clojure core and non java Math functions.
   It will be tricked by some bad coding styles such as functions that shadow clojure.core.
   The path takes us to the whole function call, i.e (foo/bar 1 2 3)."
  (let [path-atom (atom []) ns-sym (first ns-sym)
        ns-ob (cond (not ns-sym) (find-ns 'clojure.core)
                (symbol? ns-sym) (find-ns ns-sym) :else ns-sym)
        walk-fn (fn [path x]
                  (if (c/listy? x)
                    (let [x0 (first x)]
                      (cond (not (symbol? x0)) "Not a symbol"
                        (string/includes? (str x0) "clojure.core/") "We ignore the core namespace"
                        (or (string/starts-with? (str x0) "Math/")
                          (string/starts-with? (str x0) "java.lang.Math/")) "We ignore java.lang/Math"
                        (textparse/qual? x0) (swap! path-atom #(conj % path))
                        (let [symr (ns-resolve ns-ob x0)]
                          (or (not symr) (string/includes? (str symr) "clojure.core/"))) "Local sym OR clojure.core sym"
                        :else (swap! path-atom #(conj % path)))
                      x) x))]
    (t/pwalk walk-fn code) @path-atom))

(defn all-defpaths [codes]
  "All paths to the codes that enclose the defs."
  (apply c/vcat
    (mapv (fn [codei ix]
            (cond (_class? codei)
              (let [ipaths (all-defpaths codei)]
                (mapv (c/vcat [ix] ipaths)))
              (and (c/listy? codei) (contains? def-variants (first codei)))
              [[ix]]
              :else []))
      codes (range))))

(defn local-downhills [cpath code-fully-qual]
  "Returns the where-it-is-used paths of a variable addressed by path in code.
   [] if it can't find anything or no symbol is defined."
  (let [sym-qual (first cpath)
        path (into [] (rest cpath))
        codeu code-fully-qual]
    (if (sym-def? codeu path)
      (let [paths (t/paths codeu)
            target (t/cget-in codeu path)
            path-ix (first (filter #(= (nth paths %) path) (range)))]
        (filterv #(= (t/cget-in codeu %) target) (subvec paths (inc path-ix))))
      [])))

(defn local-uphill [cpath code-fully-qual]
  "Returns the path to the local variable that defined us, false if we can't find anything."
  (let [sym-qual (first cpath)
        path (into [] (rest cpath))
        codeu code-fully-qual
        target (t/cget-in codeu path)]
    (if (symbol? target)
      (let [first-use (first (filter #(= (t/cget-in codeu %) target) (t/paths codeu)))]
        (if (and (sym-def? codeu first-use) (and (not= path first-use)))
          (c/vcat [sym-qual] first-use) false))
      false)))

;;;;;;;; Clojure-aware, other walking fns ;;;;;;

(defn locals-walk [f x locals]
  "Applies (f x locals) recursively, locals is a set.
   Locals includes any symbols defined before x, as well as x itself.
   Does not work for multible vars at once fancy let statements (TODO)."
  (let [locals (set locals)]
    (f
      (cond (not (coll? x)) x
        (vector? x) (mapv #(locals-walk f % locals) x)
        (set? x) (set (mapv #(locals-walk f % locals) x))
        (map? x) (zipmap (keys x) (mapv #(locals-walk f % locals) (vals x)))
        (contains? #{'let `let `let* `loop `loop* 'loop} (first x)) ; Note: (binding [] ...) can't add locals.
        (let [binding-vec (into [] (if (coll? (second x)) (second x) [(second x)]))
              n (count binding-vec)
              bvec1l1 (loop [code [] ix 0 locals1 locals]
                        (if (>= ix n) [code locals1]
                          (let [locals2 (conj locals1 (nth binding-vec ix))
                                sym (locals-walk f (nth binding-vec ix) locals2)
                                val (locals-walk f (get binding-vec (inc ix) 'nil) locals1)]
                            (recur (conj code sym val) (+ ix 2) locals2))))
              bvec1 (f (first bvec1l1) locals)
              locals1 (second bvec1l1)
              tail (rest (rest x))]
          (apply list (f (first x) locals) ; old locals.
               bvec1 (mapv #(locals-walk f % locals1) tail)))
        (and (contains? #{'fn `fn `fn*} (first x)) (vector? (second x))) ; unwrapped.
        (let [locals1 (set/union locals (set (second x)))
              head1 (f (first x) locals)]
          (apply list head1 (mapv #(locals-walk f % locals1) (rest x))))
        (contains? #{'fn `fn `fn*} (first x)) ; wrapped.
        (let [pieces (rest x)
              bindingss (mapv #(set (first %)) pieces)
              head1 (f (first x) locals)
              pieces1 (mapv #(locals-walk f %1 (set/union locals %2)) pieces bindingss)]
          (apply list head1 pieces1))
        (contains? #{'defn `defn 'defn- `defn- 'definline `deflinline 'defmacro `defmacro} (first x)) ; only handles basic defn, use macroexpand to handle more stuff.
        (let [bindings (set (first (filter vector? x)))
              locals1 (set/union bindings locals)]
          (apply list (mapv #(locals-walk f % locals1) x)))
        :else (apply list (mapv #(locals-walk f % locals) x)))
      locals)))

(defn unbound-non-fn-syms [x]
  "Unbound symbols that aren't called as a function. Returns a set.
   Excludes clojure.core qualified symbols."
  (let [ubs (atom #{})
        blacklist (atom #{})
        f (fn [x locals]
            (if (and (symbol? x) (not (contains? locals x))
                  (not (string/starts-with? (str x) "clojure.core/")))
              (swap! ubs #(conj % x)))
            (if (and (c/listy? x) (symbol? (first x)))
              (swap! blacklist #(conj % (first x)))) x)
        _ (locals-walk f x #{})]
    (set/difference @ubs @blacklist)))

(defn unbound-syms [x]
  "Unbound unqualified symbols. Returns a set.
   Excludes clojure.core qualified symbols."
  (let [ubs (atom #{})
        blacklist (atom specials)
        f (fn [x locals]
            (if (and (symbol? x) (not (contains? locals x))
                  (not (string/includes? (str x) "/"))
                  (not (string/starts-with? (str x) "clojure.core/")))
              (swap! ubs #(conj % x))) x)
        _ (locals-walk f x #{})]
    (set/difference @ubs @blacklist)))

;;;;;;;; Summary fns, mainly for debugging ;;;;;;;;;

(defn lucky-branch [code path]
  "Takes a sample, this fn is intended as a debugger."
  (let [p0 (first path) pr (if (> (count path) 0) (into [] (rest path)))
        myst-char \u2601 ; clouds obscure whatever is beneath.
        myst-str (symbol (apply str (repeat 3 myst-char)))
        empty-coll (fn [x]
                     (cond (set? x) #{myst-str}
                       (map? x) {myst-str myst-str}
                       (vector? x) [myst-str]
                       :else (list myst-str)))
        lb1 (if (and (coll? code) p0)
              (lucky-branch (c/cget code p0) pr))]
    (if (not p0) code
      (c/vmap #(cond (= %2 p0) lb1
                           (coll? %1) (empty-coll %1)
                           :else %1)
        code (c/ckeys code)))))

(defn lucky-leaf [code search-key]
  "Includes only the branches that contain key from code.
   Collections are collapsed to 1 element, but the depth remains."
  (lucky-branch code (t/find-value-in code search-key)))