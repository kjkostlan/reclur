; MACro NAVigation

(ns coder.macnav
  (:require [clojure.walk :as walk]
    [clojure.string :as string]))

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

(defn tree-diff [x y]
  "Finds a difference between x and y.
   Did this get written b4?
   Should this go in collections?"
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
  "Finds the path of search-key in code. False when nothing found."
  (cond (= code search-key) []
    (not (coll? code)) false
    :else
    (let [kys (into [] (collections/ckeys code)) vals (into [] (collections/cvals code))
          n (count vals)]
      (loop [ix 0]
        (if (= ix n) false
          (if-let [p (path-of (nth vals ix) search-key)]
            (vcons (nth kys ix) p) (recur (inc ix))))))))

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

;;;;;;;; Main fns ;;;;;;;;

(defn macro-expand-hilite-track [ns code code-with-hilite]
  "Finds the path on the expanded code of the macro of the 'hilight'."
  (let [ns (if (symbol? ns) (find-ns ns) ns)
        code-ex (binding [*ns* ns] 
                  (walk/macroexpand-all code))
        code-hx (binding [*ns* ns]
                  (walk/macroexpand-all code-with-hilite))]
    (tree-diff code-ex code-hx)))

(defn macro-expand-path [ns code path]
  "Where path goest on the macroexpanded code.
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

