; MACro NAVigation

(ns coder.macnav
  (:require [clojure.walk :as walk]))

;;;;;;;; Support fns ;;;;;;;;;

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

;;;;;;;; Main fns ;;;;;;;;

(defn macro-expand-path [ns code code-with-hilite]
  "Finds the path on the expanded code of the macro of the 'hilight'."
  (let [ns (if (symbol? ns) (find-ns ns) ns)
        code-ex (binding [*ns* ns] 
                  (walk/macroexpand-all code))
        code-hx (binding [*ns* ns]
                  (walk/macroexpand-all code-with-hilite))]
    (tree-diff code-ex code-hx)))

