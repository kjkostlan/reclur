; Code NAVigation

(ns coder.cnav
  (:require [clojure.walk :as walk]
    [clojure.string :as string]
    [app.fbrowser :as fbrowser]
    [javac.file :as jfile]
    [collections]
    [coder.sunshine :as sunshine] [coder.cbase :as cbase] [coder.clojure :as clojure]))

;;;;;;;; Support fns ;;;;;;;;;

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

(defn path-of [code search-key]
  "Finds the first path of search-key in code. False when nothing found."
  (cond (= code search-key) []
    (not (coll? code)) false
    :else
    (let [kys (into [] (collections/ckeys code)) vals (into [] (collections/cvals code))
          n (count vals)]
      (loop [ix 0]
        (if (= ix n) false
          (if-let [p (path-of (nth vals ix) search-key)]
            (vcons (nth kys ix) p) (recur (inc ix))))))))

(defn paths-of [code search-key]
  "Paths that lead to search-key in code."
  (let [stop (if search-key false true)]
    (loop [x code out []]
      (let [ph1 (path-of x search-key)]
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
  "Is path in code the location of a defined symbol?
   I.e. the path to 'x in '(let [x 1])."
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

;;;;;;;; Text reducing functions ;;;;;;;;;
 
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

;;;;;;;; Macro fns ;;;;;;;;

(defn macro-expand-hilite-track [ns code code-with-hilite]
  "Finds the path on the expanded code of the macro of the 'hilight'."
  (let [ns (if (symbol? ns) (find-ns ns) ns)
        code-ex (binding [*ns* ns] 
                  (walk/macroexpand-all code))
        code-hx (binding [*ns* ns]
                  (walk/macroexpand-all code-with-hilite))]
    (tree-diff code-ex code-hx)))

(defn macro-expand-path [ns code path]
  "Where path goes on the macroexpanded code.
   Not gaurenteed to work in all cases (should return nil when fails)."
  (let [sy (gensym "Mark")
        f #(cond (collections/listy? %) (list sy)
             (vector? %) [sy]
             (set? %) #{sy}
             (map? %) {sy sy}
             (number? %) (Math/random)
             (symbol? %) sy
             :else (str sy)) 
        code-marked (collections/cupdate-in code path f)]
    (macro-expand-hilite-track 
      ns code code-marked)))

;;;;;;;; Code navigaton fns ;;;;;;;;
;symqual-to-fstr-ixs
(defn symqual-to-fstr-ixs [sym-qual]
  "Filename and. what (cursor) indexes on the string correspond to sym-qual.
   This function isn't fool-proof though, but should work except in unusual cases.
   TODO: move this function go into coder.clojure as it is language dependent, or be combined with the massive unwieldy reversible parser.?"
  (let [filename (cbase/ns2file (cbase/ns-of sym-qual))
        codes-txt (jfile/open filename)
        ; TODO: polyglot use a specialized reads-string function.
        reads #(try (read-string (str "[\n" % "\n]")) (catch Exception e false))
        N (count codes-txt)
        lookfor (str (cbase/unqual sym-qual))]
    (loop [ix 0]
      (if (>= ix N) (let [ix0 (string/index-of codes-txt lookfor) ix0 (if ix0 ix0 0)] 
                      [filename ix0 (+ ix0 (count lookfor))]) ; fail
        (let [ix0 (string/index-of codes-txt lookfor ix)]
          (if ix0
            (let [ix1 (+ ix0 (count lookfor)) tsym (symbol (str "TESTSYM" "24"))
                  codes-txt1 (str (subs codes-txt 0 ix0) " " tsym " " (subs codes-txt ix1))
                  sym-set (set (sunshine/all-syms (reads codes-txt1)))]
              (if (contains? sym-set tsym) [filename ix0 ix1]
               (recur ix1)))
            (recur 1e100)))))))

(defn local-downhills [cpath]
  "Returns the where-it-is-used paths of a variable addressed by path in code.
   [] if it can't find anything or no symbol is defined."
  (let [sym-qual (first cpath)
        path (into [] (rest cpath))
        code (:source (cbase/var-info sym-qual true))
        codeu (sunshine/pipeline code false identity)]
    (if (sym-def? code path)
      (let [paths (collections/paths codeu)
            target (collections/cget-in codeu path)
            path-ix (first (filter #(= (nth paths %) path) (range)))]
        (filterv #(= (collections/cget-in codeu %) target) (subvec paths (inc path-ix))))
      [])))

(defn local-uphill [cpath]
  "Returns the path to the local variable that defined us, false if we can't find anything."
  (let [sym-qual (first cpath)
        path (into [] (rest cpath))
        code (:source (cbase/var-info sym-qual true))
        codeu (sunshine/pipeline code false identity)
        target (collections/cget-in codeu path)]
    (if (symbol? target)
      (let [first-use (first (filter #(= (collections/cget-in codeu %) target) (collections/paths codeu)))]
        (if (and (sym-def? codeu first-use) (and (not= path first-use))) 
          (collections/vcat [sym-qual] first-use) false))
      false)))

;cbase/uses-of
;cbase/used-by
;