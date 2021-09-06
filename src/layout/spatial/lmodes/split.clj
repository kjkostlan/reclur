; Split mode: adding stuff shares space with the components of the matching type within the screen.
(ns layout.spatial.lmodes.split
  (:require
    [layout.spatial.xform :as xform]
    [layout.spatial.collide :as collide]
    [layout.spatial.lmodes.stack :as stack]))

(defn split-h [xxyy]
  "Returns the two xxyys."
  (let [x0 (nth xxyy 0) x1 (nth xxyy 1) y0 (nth xxyy 2) y1 (nth xxyy 3)
        xm (* 0.5 (+ x0 x1))]
    [[x0 xm y0 y1] [xm x1 y0 y1]]))

(defn split-v [xxyy]
  "Returns the two xxyys."
  (let [x0 (nth xxyy 0) x1 (nth xxyy 1) y0 (nth xxyy 2) y1 (nth xxyy 3)
        ym (* 0.5 (+ y0 y1))]
    [[x0 x1 y0 ym] [x0 x1 ym y1]]))

(defn split [comp-to-split comp-to-add-in-space]
  "Returns the two components."
  (let [xxyy (xform/box-xxyy comp-to-split)
        x0 (nth xxyy 0) x1 (nth xxyy 1) y0 (nth xxyy 2) y1 (nth xxyy 3)
        actual-ratio (/ (- x1 x0) (- y1 y0))
        threshold-ratio (* 1.414 1.618)
        xxyy-pair (if (< actual-ratio threshold-ratio) (split-v xxyy) (split-h xxyy))]
    [(xform/set-xxyy comp-to-split (first xxyy-pair))
     (xform/set-xxyy comp-to-add-in-space (second xxyy-pair))]))

(defn add-component [s comp kwd]
  "Uses s to position the new comp."
  (let [ty (:type comp) k-screen (collide/most-on-screen s #(= (:type %) ty))]
    (if k-screen
      (let [comp-pair (split (get (:components s) k-screen) comp)
            comp0 (first comp-pair) comp1 (assoc (second comp-pair) :z (inc (get comp0 :z 0)))
            comps (:components s)
            comps1 (assoc comps k-screen comp0 kwd comp1)]
        (assoc s :components comps1))
      (stack/add-component s comp kwd)))) ; nothing to split.

(defn close-component [comps close-k]
  (let [comp (get comps close-k)
        xxyy0 (xform/box-xxyy comp)
        x0 (first xxyy0) x1 (second xxyy0) y0 (nth xxyy0 2) y1 (nth xxyy0 3)
        xxyys (mapv xform/box-xxyy (vals comps))
        close? #(< (Math/abs (- %1 %2)) 10)
        align? (fn [c] (let [xxyyc (xform/box-xxyy c)
                             x0c (first xxyyc) x1c (second xxyyc) y0c (nth xxyyc 2) y1c (nth xxyyc 3)
                             halign? (and (close? y0c y0) (close? y1c y1))
                             valign? (and (close? x0c x0) (close? x1c x1))
                             hnextto? (or (close? x0c x1) (close? x1c x0))
                             vnextto? (or (close? y0c y1) (close? y1c y0))]
                         (or (and halign? hnextto?) (and valign? vnextto?))))
        align-k (first (filter #(and (= (:type comp) (:type (get comps %)))
                                  (align? (get comps %)))
                         (keys comps)))
        comps1 (if align-k (let [compa (get comps align-k)
                                 xxyy1 (collide/union-xxyy xxyy0 (xform/box-xxyy compa))]
                             (assoc comps align-k
                               (xform/set-xxyy compa xxyy1)))
                  comps)]
    (dissoc comps1 close-k)))

(defn close-components [s kys]
  (update s :components #(reduce close-component % kys)))

(defn layout []
  {:initial-position (fn [& args] (apply stack/initial-position args))
   :add-component (fn [& args] (apply add-component args))
   :goto (fn [s filename char-ix0 char-ix1] (stack/goto-code s filename char-ix0 char-ix1 0.75 add-component))
   :gotos (fn [s filenames char-ix0s char-ix1s] (stack/goto-codes s filenames char-ix0s char-ix1s add-component))
   :close-components (fn [s kys] (close-components s kys))
   :name "split"})