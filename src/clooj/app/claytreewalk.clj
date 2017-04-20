; Simple walking functions that recursivly look at :children.
(ns clooj.app.claytreewalk)

(defn vis-walk [tree f]
 "walks through all visible stuff recursivly. :children-visible? decides whether we go recursivly.
  We assume the root is always visible. Breadth-first"
  (assoc (f tree) :children 
    (if (:children-visible? tree) (mapv #(vis-walk % f) (:children tree)) (:children tree))))

(defn vis-walk-d [tree f]
  "Depth first walk on all visible nodes."
  (f (assoc tree :children 
       (if (:children-visible? tree) (mapv #(vis-walk-d % f) (:children tree)) (:children tree)))))

(defn _assign-paths [tree path vis-only?]
  (apply concat [(dissoc (assoc tree :path path) :children)]
    (if (or (not vis-only?) (:children-visible? tree))
      (mapv #(_assign-paths %1 (conj path :children %2) vis-only?) (:children tree) (range)) [])))

(defn assign-paths [tree vis-only?] ; assigns paths, breadth first.
  (into [] (_assign-paths tree [] vis-only?)))

(defn all-walk [tree f] 
  "Like vis-walk but goes through all nodes."
  (assoc (f tree) :children (mapv #(all-walk % f) (:children tree))))

(defn vis-tree-unwrap [tree]
  "unwraps the visible elements in the tree as a vector, including ourselves.
   Used for the physics simulation. Adds a :path to each element and removes :children"
  (into [] (_assign-paths tree [] true)))

(defn tree-wrap [treeu]
  "Uses the :path of each node (i.e. from vis-tree-unwrap) to build a tree.
   Leaf-nodes of treeu are allowed to have :children."
  (let [f-add (fn [x nu] (cond (and (:children x) (:children nu)) 
                           (throw (Exception. "Non-leaf-node has extra :children, there is a conflict."))
                           (:children x) (assoc nu :children (:children x)) ; don't wipe-out deeper levels.
                           :else nu))]
    (reduce (fn [acc nu] (let [p (:path nu)] ((if (= p []) #(%3 %1 %2) update-in) acc p f-add))) 
      {} treeu)))

(defn all-tree-unwrap [tree]
  "unwrapes all elements in the tree as a vector, including ourselves.
   Used for checking if any child changed. Adds a :path to each element and removes :children"
  (into [] (_assign-paths tree [] false)))

(defn vis-walk-extract [tree f]
  "Like vis-walk but extracts something from the tree in vector form."
  (let [treeu (vis-tree-unwrap (vis-walk tree #(assoc % :_vis-walk-xtract (f %))))]
    (mapv :_vis-walk-xtract treeu)))

(defn common-root [paths]
  "If each path starts with [:foo :bar] but some paths differ in the third element 
   the common root is [:foo :bar].
   Paths is a sequential of sequentials but we don't support laziness."
  (if (= (count paths) 0) []
    (let [paths (mapv #(into [] %) paths) ; convert to vector form.
          agree? (fn [ix] (let [xis (mapv #(get % ix) paths)] 
                            (and (apply = xis) (= (count (filterv nil? xis)) 0))))
          n-agree (first (filter #(not (agree? %)) (range)))]
      (into [] (subvec (first paths) 0 n-agree)))))