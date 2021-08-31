; Dogpile mode: adding stuff finds the least compressed space and goes there.
(ns layout.lmodes.dogpile
  (:require [c]
    [layout.layoutcore :as lay]
    [layout.lmodes.stack :as stack]))

(defn add-component [s comp kwd]
  "Uses s to position the new comp."
  (let [ty (:type comp) k-screen (lay/most-on-screen s #(= (:type %) ty))
        halfwidth 0.15
        cam-xxyy (lay/visible-xxyy (:camera s))
        query-range-xxyy (lay/scale-xxyy cam-xxyy (- 1.0 (* halfwidth 2.0))) ; smaller so comp is 100% on screen.

        xxyys (mapv #(lay/xxyy %) (vals (:components s)))
        resolution 20
        xs-ys-ds (lay/boxed-density-measure xxyys query-range-xxyy resolution)
        ix (c/argmin (last xs-ys-ds))
        center-x (nth (first xs-ys-ds) ix)
        center-y (nth (second xs-ys-ds) ix)

        sx (- (nth cam-xxyy 1) (nth cam-xxyy 0))
        sy (- (nth cam-xxyy 3) (nth cam-xxyy 2))

        xxyy1 [(- center-x (* halfwidth sx)) (+ center-x (* halfwidth sx))
               (- center-y (* halfwidth sy)) (+ center-y (* halfwidth sy))]
        comp1 (assoc (lay/set-xxyy comp xxyy1) :z (inc (lay/max-z s)))]
    (if k-screen
      (assoc-in s [:components kwd] comp1)
      (stack/add-component s comp1 kwd)))) ; New type of comp => no need to dogpile.

(defn layout []
  {:initial-position (fn [& args] (apply stack/initial-position args))
   :add-component (fn [& args] (apply add-component args))
   :goto (fn [s filename char-ix0 char-ix1] (stack/goto-code s filename char-ix0 char-ix1 0.75 add-component))
   :gotos (fn [s filenames char-ix0s char-ix1s] (stack/goto-codes s filenames char-ix0s char-ix1s add-component))
   :name "dogpile"})