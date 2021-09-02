; Teleport mode: adding stuff copies the screen but with the new component replacing
; the type of the old component, if necessary.
(ns layout.lmodes.teleport
  (:require [np]
    [layout.layoutcore :as lay]
    [layout.xform :as xform]
    [layout.lmodes.stack :as stack]
    [layout.selectmovesize :as selectmovesize]))

(defn add-component [s comp kwd]
  "Uses s to position the new comp."
  (let [ty (:type comp) k-screen (lay/most-on-screen s #(= (:type %) ty))]
    (if k-screen
      (let [; Everything else on the screen.
            screen-xxyy (lay/visible-xxyy (:camera s))
            screen-kys (filterv #(apply lay/overlap? screen-xxyy (lay/xxyy (get-in s [:components %])))
                          (keys (:components s)))
            screen-kys0 (disj (set screen-kys) k-screen)

            all-comp-xxyys (mapv lay/xxyy (vals (:components s)))
            total-xxyy (apply lay/union-xxyy [0 0 0 0] all-comp-xxyys)
            search-xxyy (mapv + [-1000 1000 -1000 1000] total-xxyy)
            search-len (+ (- (second search-xxyy) (first search-xxyy)) (- (nth search-xxyy 3) (nth search-xxyy 2)))

            resolution 50
            xs-ys-ds (lay/boxed-density-measure all-comp-xxyys search-xxyy resolution)
            pnorm 2.0
            dist-penalties (mapv #(/ (lay/pnorm-dist 0.0 %1 0.0 %2 pnorm) search-len 5.0)
                             (first xs-ys-ds) (second xs-ys-ds))
            ix (np/argmin (mapv + dist-penalties (last xs-ys-ds)))

            ; The old and new centers:
            center-x0 (* 0.5 (+ (nth screen-xxyy 0) (nth screen-xxyy 1)))
            center-y0 (* 0.5 (+ (nth screen-xxyy 2) (nth screen-xxyy 3)))
            center-x1 (nth (first xs-ys-ds) ix)
            center-y1 (nth (second xs-ys-ds) ix)
            move-x (- center-x1 center-x0) move-y (- center-y1 center-y0)

            move-comp (fn [c] (update c :position #(mapv + % [move-x move-y])))
            camera1 (xform/xx (xform/x-1 [move-x move-y 1.0 1.0]) (:camera s)) ;xforms are [x y scalex scaley]

            z1 (inc (lay/max-z s))
            comp-copies (zipmap (mapv selectmovesize/derive-key screen-kys0)
                          (mapv #(assoc (move-comp (get (:components s) %1)) :z (+ z1 %2)) screen-kys0 (range)))
            ghost (move-comp (get (:components s) k-screen))
            comp1 (assoc (merge comp (select-keys ghost [:position :size])) :z z1)]
        (assoc s :components (merge (:components s) comp-copies {kwd comp1})
          :camera camera1))
      (stack/add-component s comp kwd)))) ; no need to teleport as no matching type on screen.

(defn layout []
  {:initial-position (fn [& args] (apply stack/initial-position args))
   :add-component (fn [& args] (apply add-component args))
   :goto (fn [s filename char-ix0 char-ix1] (stack/goto-code s filename char-ix0 char-ix1 0.75 add-component))
   :gotos (fn [s filenames char-ix0s char-ix1s] (stack/goto-codes s filenames char-ix0s char-ix1s add-component))
   :name "teleport"})