; Handling files and tree-navigation-stuff.
(ns clooj.app.claytreetree
  (:require [clooj.java.file :as jfile]
    [clooj.app.claytreephysics :as claphy]
    [clooj.coder.blitcode :as blitcode]
    [clooj.app.claytreewalk :as clwalk]
    [clooj.app.claytreetext :as cltext]))

(def empty-node {:type :empty 
                 :children [] :children-visible? false
                 :physics claphy/placeholder-physics :tbox cltext/place-holder-text})

(defn get-text [node]
  "It properly compiles any children's text for nodes that are split up."
  (apply str (butlast (interleave (:pieces (:tbox node)) (conj (mapv get-text (:children node)) [])))))

(defn make-node [ty text]
  (let [node0 (assoc empty-node :type ty :tbox (assoc cltext/place-holder-text :pieces [text])) ; start with one piece.
        node1 (claphy/update-physical-properties node0)]
    (cltext/on-text-change node1)))

(defn position-relative-to [node child dx dy vel-opt set-size-to-size0?]
  "Returns the child node positioned relative to the parent node.
   vel-opt is :zero = make the velocity 0, :ident = don't change, :parent = match parent (zeros expansion and contraction)"
  (let [p0 (:physics node) p1 (:physics child)
        x0 (* 0.5 (+ (:x0 p0) (:x1 p0))) y0 (* 0.5 (+ (:y0 p0) (:y1 p0)))
        x1 (* 0.5 (+ (:x0 p1) (:x1 p1))) y1 (* 0.5 (+ (:y0 p1) (:y1 p1)))
        w (if set-size-to-size0? (:width0 p1) (- (:x1 p1) (:x0 p1)))
        h (if set-size-to-size0? (:height0 p1) (- (:y1 p1) (:y0 p1)))
        p11 (assoc p1 :x0 (+ dx x0 (* -0.5 w)) :x1 (+ dx x0 (* 0.5 w))
             :y0 (+ dy y0 (* -0.5 h)) :y1 (+ dy y0 (* 0.5 h)))
        p11 (cond (= vel-opt :zero) (assoc p11 :vx0 0 :vy0 0 :vx1 0 :vy1 0)
              (= vel-opt :ident) p11
              (= vel-opt :parent) 
              (let [vx (* 0.5 (+ (:vx0 p0) (:vx1 p0))) vy (* 0.5 (+ (:vy0 p0) (:vy1 p0)))]
                (assoc p11 :vx0 vx :vx1 vx :vy0 vy :vy1 vy))
              :else (throw (Exception. "Unrecognized velocity option.")))]
    (assoc child :physics p11)))

(defn toggle-children [node]
  "Shows or hides children."
  (if (> (count (:children node)) 0) 
    (let [chvis? (:children-visible? node)
          pcl (:parent-child-spring-length claphy/*phy-params*)
          ch1 (if chvis? (:children node) 
                ; Start with a relaxed spring, same velocity as parent, and width equal to width0.:
                (let [n1 (inc (count (:children node))) angles (mapv #(* % 2.0 Math/PI (/ n1)) (range (dec n1)))
                      xx (* 0.5 (+ (:x0 (:physics node)) (:x1 (:physics node)))) ;midpoint of parent.
                      yy (* 0.5 (+ (:y0 (:physics node)) (:y1 (:physics node))))
                      vx (* 0.5 (+ (:vx0 (:physics node)) (:vx1 (:physics node))))
                      vy (* 0.5 (+ (:vy0 (:physics node)) (:vy1 (:physics node))))
                      standoffs (mapv #(claphy/parent-child-l0 node %) (:children node))]
                  (mapv #(position-relative-to node %1 (* %3 (Math/cos %2)) (* %3 (Math/sin %2)) :zero true) 
                    (:children node) angles standoffs)))]
      (assoc node :children-visible? (not chvis?) :children ch1)) node))

(defn pull-text [node piece-ix cursor-within-piece]
  "Pulls text out of a node at at a given piece-ix and location within piece, if possible."
  (if (not= (:type node) :text) node ; Only do that to text nodes.
    (let [pieces (:pieces (:tbox node)) ; only consider indentation levels within a piece (otherwise we would need to do tricky family re-org).
          piece (nth pieces piece-ix)
          ^ints idepth (:inter-depth (blitcode/basic-parse piece))
          lev (aget ^ints idepth (int cursor-within-piece))
          ; Range of indexes to slice the string with subs:
          ix0 (loop [i (int cursor-within-piece)]
                (if (< i 0) 0
                  (if (< (aget ^ints idepth i) lev) i (recur (dec i)))))
          n1 (count idepth) ; one more element than the string is.
          ix1 (loop [i (int cursor-within-piece)]
                (if (>= i n1) (dec n1)
                  (if (< (aget ^ints idepth i) lev) i (recur (inc i)))))]
      (if (or (> ix0 0) (< ix1 (dec n1))) ; only do anything if a smaller subset than the piece was selected.
        (let [s0 (subs piece 0 ix0) s1 (subs piece ix0 ix1) s2 (subs piece ix1) ; break into three pieces.
              piece1 [s0 s2] ; pieces can be empty. That's perfectly fine, it indicates back-to-back children.
              pieces1 (into [] (concat (subvec pieces 0 piece-ix) piece1 (subvec pieces (inc piece-ix))))
              pcl (:parent-child-spring-length claphy/*phy-params*)
              new-node (make-node :text s1)
              l0 (claphy/parent-child-l0 node new-node) 
              new-node (position-relative-to node new-node l0 0 :zero true)
              children1 (into [] (concat (subvec (:children node) 0 piece-ix) 
                                   [new-node] (subvec (:children node) piece-ix)))]
          ; Children are between two pieces.
          (assoc node :tbox (assoc (:tbox node) :pieces pieces1) :children children1 :children-visible? true))
        node))))

(defn push-text [node child-ix]
  "Removes the child node that corresponds to child-ix and puts it back into the parent, combining the text.
   It's OK to unbalance the text when editing children, it will be fixed by pushing in and then pulling out again."
  (let [pieces (:pieces (:tbox node)) t-ch (get-text (nth (:children node) child-ix))
        b4 (subvec pieces 0 (inc child-ix)) afr (subvec pieces (inc child-ix)) 
        ; merge the text of the child with the last element of b4 and first element of afr:
        b41 (update b4 (dec (count b4)) #(str % t-ch (first afr)))
        pieces1 (into [] (concat b41 (rest afr)))]
    (assoc node :tbox (assoc (:tbox node) :pieces pieces1) 
      :children (into [] (concat (subvec (:children node) 0 child-ix) (subvec (:children node) (inc child-ix)))))))