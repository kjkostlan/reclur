; Code NAVigation

(ns coder.cnav
  (:require [clojure.walk :as walk]
    [clojure.string :as string]
    [app.fbrowser :as fbrowser]
    [javac.file :as jfile]
    [clojure.set :as set]
    [collections]))

;;;;;;;; Support fns ;;;;;;;;;

(def specials #{'def 'let* 'fn* 'var 'quote 'if 'loop* 'recur '. 'new 'throw 'catch 'monitor-enter 'monitor-exit 'set! 'do})
(def def-variants #{'def 'defn `defn 'defmacro `defmacro 'def*})

(defn vcons [a x]
  (into [] (concat [a] x)))

(defn _lty [x]
  (cond (not (coll? x)) -1
    (vector? x) 0
    (set? x) 2
    (map? x) 3
    :else 1)) ; listes et al.

(defn list-is-list [t]
  (clojure.walk/prewalk #(if (= (_lty %) 1) (apply list %) %) t))

;;;;;;;; Pathing fns ;;;;;;;;;

(defn tree-diff [x y]
  "Finds a shortest path that differences x and y, false if x=y.
   Did this get written b4?
   Should this (and other fns?) go in collections?"
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

(defn path-of [code search-key include-map-keys?]
  "Finds the first path of search-key in code. False when nothing found.
   include-map-keys?: Stuff that is or is inside of a map's keys paths to the map itself."
  (cond (= code search-key) []
    (not (coll? code)) false
    :else
    (let [kys (into [] (collections/ckeys code)) vals (into [] (collections/cvals code))
          n (count vals)
          ph (loop [ix 0]
               (if (= ix n) false
                 (if-let [p (path-of (nth vals ix) search-key include-map-keys?)]
                   (vcons (nth kys ix) p) (recur (inc ix)))))]
      (cond ph ph
        (and include-map-keys? (map? code)) 
        (if (first (filter #(path-of % search-key true) (keys code))) [] false)
        :else false))))

(defn paths-of [code search-key include-map-keys?]
  "Paths that lead to search-key in code."
  (let [stop (if search-key false true)]
    (loop [x code out []]
      (let [ph1 (path-of x search-key include-map-keys?)]
        (if (= ph1 (last out)) (throw (Exception. "Something is not working.")))
        (if ph1 
          (recur (collections/cassoc-in x ph1 stop) (conj out ph1))
          out)))))

(defn drag-path [code-old code-new path-old]
  "Tries to find a corresponding path, nil if failure.
   No obvious algorithim here, more of a heuristic. Room for much improvement."
  (let [np (count path-old)
        nesting-keys (mapv #(collections/cget-in code-old
                              (subvec path-old %))
                       (range np)) ; shallow -> deep.
        pathss-old (mapv #(paths-of code-old %) nesting-keys)
        pathss-new (mapv #(paths-of code-new %) nesting-keys)
        shallowest (first (filter #(> (count %) 0) pathss-new))
        deepest (last (filter #(> (count %) 0) pathss-new))]
    (if deepest ; Which is most similar? How deep can we go?
      (let [pathdif (fn [a b] (reduce + (mapv #(if (= %1 %2) 0.0 1.0) a b)))
            diffs (mapv #(pathdif path-old %) shallowest)]
        (nth deepest (collections/argmax diffs)))
      (if (and (> np 0) ; very simple subsitution
            (let [v0 (collections/cget-in code-old (subvec path-old 0 (dec np)))
                  v1 (collections/cget-in code-new (subvec path-old 0 (dec np)))]
              (and (vector? v0) (vector? v1) (= (count v0) (count v1))))
            (collections/cget-in code-new path-old))
       path-old false))))

(defn sym-def? [code path]
  "Is path in code the location of a local defined symbol?
   I.e. the path to 'x in '(let [x 1]).
   Class members don't count as local, variables defined within fns do."
  (let [x (collections/cget-in code path)
        first? #(if (coll? %) (first %) false)
        pairbind? (contains? #{`let `let* `loop `loop* 'let 'loop} (first? (collections/cget-in code (drop-last 2 path))))
        fn-unpacked? (contains? #{`fn `fn* 'fn} (first? (collections/cget-in code (drop-last 2 path))))
        fn-packed? (contains? #{`fn `fn* 'fn} (first? (collections/cget-in code (drop-last 3 path))))
        in-vector? (vector? (collections/cget-in code (drop-last 1 path)))
        second? (= (last (butlast path)) 1)]
    (cond 
      (< (count path) 2) false (not (symbol? x)) false (not in-vector?) false (not second?) false
      (or (and pairbind? (even? (last path)))
        fn-unpacked? fn-packed?) true
      :else false)))

;;;;;;;; Big thing reducing functions ;;;;;;;;;
 
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
              (lucky-branch (collections/cget code p0) pr))]
    (if (not p0) code
      (collections/vmap #(cond (= %2 p0) lb1 
                           (coll? %1) (empty-coll %1)
                           :else %1) 
        code (collections/ckeys code)))))

(defn lucky-leaf [code search-key]
  "Includes only the branches that contain key from code.
   Collections are collapsed to 1 element, but the depth remains."
  (lucky-branch code (path-of code search-key)))

;;;;;;;; Code navigaton fns ;;;;;;;;

(defn unqual [sym-qual] "Duplicated from textparse to avoid circles." 
  (symbol (last (string/split (str sym-qual) #"\/"))))

(defn symbol2defpath-qual [codes sym]
  "The path to the path enclosing def in codes.
   In java and other languages, it usually wouldn't be explicitly a 'def'."
  (let [def-paths (into [] (apply concat (mapv #(paths-of codes % false) def-variants)))
        defenclose-paths (mapv #(into [] (butlast %)) def-paths)
        def-vals (mapv #(collections/cget-in codes (conj % 1)) defenclose-paths)
        sym-unqual (unqual sym)
        path-ix (first (filter #(or (= (nth def-vals %) sym-unqual) (= (nth def-vals %) sym)) (range (count def-vals))))
        _ (if (not path-ix) (throw (Exception. (str "Can't find the def-path for: " sym))))
        path+1 (nth def-paths path-ix)]
    (subvec path+1 0 (dec (count path+1)))))

(defn local-downhills [cpath code-fully-qual]
  "Returns the where-it-is-used paths of a variable addressed by path in code.
   [] if it can't find anything or no symbol is defined."
  (let [sym-qual (first cpath)
        path (into [] (rest cpath))
        codeu code-fully-qual]
    (if (sym-def? codeu path)
      (let [paths (collections/paths codeu)
            target (collections/cget-in codeu path)
            path-ix (first (filter #(= (nth paths %) path) (range)))]
        (filterv #(= (collections/cget-in codeu %) target) (subvec paths (inc path-ix))))
      [])))

(defn local-uphill [cpath code-fully-qual]
  "Returns the path to the local variable that defined us, false if we can't find anything."
  (let [sym-qual (first cpath)
        path (into [] (rest cpath))
        codeu code-fully-qual
        target (collections/cget-in codeu path)]
    (if (symbol? target)
      (let [first-use (first (filter #(= (collections/cget-in codeu %) target) (collections/paths codeu)))]
        (if (and (sym-def? codeu first-use) (and (not= path first-use))) 
          (collections/vcat [sym-qual] first-use) false))
      false)))

(defn _class? [code]
  (and (collections/listy? code)
    (contains? #{'defclass 'definterface `definterface 'defenum 'defprotocol `defprotocol} (first code))))

(defn path2subdef-path [codes path]
  "The part of path that digs into the def.
   Handles java classes and other stuff, but for normal defns it simply removes the first element from path.
   defclass, definterface, defenum, and defprotocol trigger going deeper."
  (loop [codes1 (nth codes (first path)) path1 (into [] (rest path))]
    (if (_class? codes1) (recur (collections/cget codes1 (first path)) (into [] (rest path1)))
      path1)))

(defn all-defpaths [codes]
  "All paths to the codes that enclose the defs."
  (apply collections/vcat
    (mapv (fn [codei ix]
            (cond (_class? codei)
              (let [ipaths (all-defpaths codei)]
                (mapv (collections/vcat [ix] ipaths)))
              (and (collections/listy? codei) (contains? def-variants (first codei)))
              [[ix]]
              :else [])) 
      codes (range))))

(defn path2defsym [codes path]
  "The foo in (def foo)."
  (let [fpath (path2subdef-path codes path) path2def (subvec (into [] path) 0 (- (count path) (count fpath)))]
    (collections/cget-in codes (conj path2def 1))))

;;;;;;;;;;;;;;;;; Recursive functions ;;;;;;;;;;;;;;;;

(defn all-syms [x]
  "Acts recursivly, returns a set. Does not include special forms or map keys."
  (cond (contains? specials x) #{}
    (symbol? x) #{x}
    (map? x) (set/union #_(all-syms (keys x)) (all-syms (vals x)))
    (coll? x) (apply set/union (mapv all-syms x))
    :else #{}))

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
        (let [pairs (into [] (if (coll? (second x)) (second x) [(second x)]))
              n (count pairs)
              pairs1b1 (loop [code [] ix 0 locals locals]
                        (if (>= ix n) [code locals]
                          (let [locals1 (conj locals (nth pairs ix))
                                sym (locals-walk f (nth pairs ix) locals1)
                                val (locals-walk f (get pairs (inc ix) 'nil) locals)]
                            (recur (conj code sym val) (+ ix 2) locals1))))
              pairs1 (f (first pairs1b1) locals)
              locals1 (second pairs1b1)
              tail (rest (rest x))]
          (apply list (f (first x) locals)
               pairs1 (mapv #(locals-walk f % locals1) tail)))
        (and (contains? #{'fn `fn `fn*} (first x)) (vector? (second x))) ; unwrapped.
        (let [locals1 (set/union locals (set (second x)))
              head1 (f (first x) locals)]
          (apply list head1 (mapv #(locals-walk f % locals1) (rest x))))
        (contains? #{'fn `fn `fn*} (first x)) ; wrapped.
        (let [pieces (rest x)
              bindingss (mapv #(set (first %)) pieces)
              head1 (f (first x) locals)
              pieces1 (mapv #(locals-walk f %1 (set/union %1 %2)) pieces bindingss)]
          (apply list head1 pieces1))
        (contains? #{'defn `defn 'defn- `defn- 'definline `deflinline 'defmacro `defmacro} (first x)) ; only handles basic defn, use macroexpand to handle more stuff.
        (let [bindings (set (first (filter vector? x)))
              locals1 (set/union bindings locals)]
          (apply list (mapv #(locals-walk f % locals1) x)))
        :else (apply list (mapv #(locals-walk f % locals) x)))
      locals)))

;;;;;;;;;;;;;;;;; Other ;;;;;;;;;;;;;;;;

(defn unbound-non-fn-syms [x]
  "Unbound symbols that aren't called as a function. Returns a set.
   Excludes clojure.core qualified symbols."
  (let [ubs (atom #{})
        blacklist (atom #{})
        f (fn [x locals]
            (if (and (symbol? x) (not (contains? locals x))
                  (not (string/starts-with? (str x) "clojure.core/")))
              (swap! ubs #(conj % x)))
            (if (and (collections/listy? x) (symbol? (first x)))
              (swap! blacklist #(conj % (first x)))) x)
        _ (locals-walk f x #{})] 
    (set/difference @ubs @blacklist)))

(defn unbound-syms [x]
  "Unbound symbols. Returns a set.
   Excludes clojure.core qualified symbols."
  (let [ubs (atom #{})
        blacklist (atom specials)
        f (fn [x locals]
            (if (and (symbol? x) (not (contains? locals x))
                  (not (string/starts-with? (str x) "clojure.core/")))
              (swap! ubs #(conj % x))) x)
        _ (locals-walk f x #{})] 
    (set/difference @ubs @blacklist)))
