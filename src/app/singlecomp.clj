; Stuff involving single components.
(ns app.singlecomp
  (:require [app.xform :as xform]
    [app.orepl :as orepl]
    ))

(defn click? [x y comp]
  (if (or (nil? x) (nil? y)) (throw (Exception. "Nil coords")))
  (let [x0 (first (:position comp))
        y0 (second (:position comp))
        sx (first (:size comp))
        sy (second (:size comp))]
    (and (>= x x0) (>= y y0) (<= x (+ x0 sx)) (<= y (+ y0 sy)))))

(defn pos-xform [pos] ; for now they always have size 1 (resizing cgances how many chars are drawn) but the camera can zoom.
  [(if-let [x (first pos)] x 0) (if-let [y (second pos)] y 0) 1 1])

(defn draw-component-l [comp focused?]
  (if (nil? comp) (throw (Exception. "nil component")))
  (if (not (map? comp)) (throw (Exception. "Non-map component")))
  (let [ifns (:interact-fns comp) 
        gfx (try ((:render ifns) (dissoc comp :position) focused?)
              (catch Exception e [[:drawString [(orepl/pr-error e) 10 10] {:Color [1 1 1 1]}]]))] gfx))

(defn gfx-l2g [gfx cam position]
  (let [xform (xform/xx cam (pos-xform position))]
    (mapv #(xform/xgfx xform % true) gfx)))