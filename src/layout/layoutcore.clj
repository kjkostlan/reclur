; Core tools that are useful in general. 
(ns layout.layoutcore
  (:require [globals]
    [app.xform :as xform]))

(defn screen-pixels []
  "[x y] pixels. The Retnia displays pretend to be 1/2 the resolution."
  (if-let [sz (:window-size (:external-state @globals/one-atom))]
    sz [800 600]))

(defn visible-xxyy [cam]
  "x1 x2 y1 y2 of the screen in pixels based on the camera."
  (let [xy (screen-pixels) cam-1 (xform/x-1 cam)
        corner0 (xform/xv cam-1 0 0) corner1 (xform/xv cam-1 (first xy) (second xy))]
    [(first corner0) (first corner1) (second corner0) (second corner1)]))

(defn world-xxyy [vis-xxyy unit-xxyy]
  (let [abs-x0 (+ (nth vis-xxyy 0) (* (- (nth vis-xxyy 1) (nth vis-xxyy 0)) (nth unit-xxyy 0)))
        abs-x1 (+ (nth vis-xxyy 0) (* (- (nth vis-xxyy 1) (nth vis-xxyy 0)) (nth unit-xxyy 1)))
        abs-y0 (+ (nth vis-xxyy 2) (* (- (nth vis-xxyy 3) (nth vis-xxyy 2)) (nth unit-xxyy 2)))
        abs-y1 (+ (nth vis-xxyy 2) (* (- (nth vis-xxyy 3) (nth vis-xxyy 2)) (nth unit-xxyy 3)))]
    [abs-x0 abs-x1 abs-y0 abs-y1]))

(defn unitscreen-xxyy [vis-xxyy world-xxyy]
  (let [x0 (first vis-xxyy) y0 (nth vis-xxyy 2)
        dx (- (second vis-xxyy) (first vis-xxyy))
        dy (- (nth vis-xxyy 3) (nth vis-xxyy 2))
        ux0 (/ (- (first world-xxyy) x0) dx) uy0 (/ (- (nth world-xxyy 2) y0) dy)
        ux1 (/ (- (second world-xxyy) x0) dx) uy1 (/ (- (nth world-xxyy 3) y0) dy)]
    [ux0 ux1 uy0 uy1]))

(defn xxyy [component]
  (let [xy (:position component) sz (:size component)]
    [(xy 0) (+ (xy 0) (sz 0)) (xy 1) (+ (xy 1) (sz 1))]))

(defn set-xxyy [component xxyy]
  (assoc component :position [(first xxyy) (nth xxyy 2)] :size [(- (second xxyy) (first xxyy)) (- (nth xxyy 3) (nth xxyy 2))]))

(defn set-unitscreen-xxyy [component visible-xxyy unit-xxyy]
  (set-xxyy component (world-xxyy visible-xxyy unit-xxyy)))

(defn center-cam-on [cam component]
  "Keeps the zoom the same."
  (let [mid-x (+ (first (:position component)) (* 0.5 (first (:size component))))
        mid-y (+ (second (:position component)) (* 0.5 (second (:size component))))
        vxxyy (visible-xxyy cam) dx (- (vxxyy 1) (vxxyy 0)) dy (- (vxxyy 3) (vxxyy 2))]
    [(- (- mid-x (* dx 0.5))) (- (- mid-y (* dy 0.5))) (cam 2) (cam 3)]))

(defn max-z [s] (apply max 0 (mapv #(if (:z %) (:z %) 0) (vals (:components s)))))

(defn unique-z [components]
  "Assigns each component a unique z-value."
  (loop [acc (zipmap (keys components) (mapv #(if (:z %) % (assoc % :z 1)) (vals components)))]
    (if (= (count (apply hash-set (mapv :z (vals acc)))) (count acc)) acc
      (recur (zipmap (keys acc) (mapv (fn [c] (update c :z #(+ (* % (+ 1 (* (Math/random) 1e-10))) 1e-100))) (vals acc)))))))

(defn y- [unit-xxyy] 
  "When it's more intuitive to work in a system where +y is up."
 [(first unit-xxyy) (second unit-xxyy) (- 1.0 (nth unit-xxyy 3)) (- 1.0 (nth unit-xxyy 2))])

(defn dont-move [comp cam0 cam1]
  "Keeps the screen-position the same, by changing comp's position."
  (let [u-xxyy (unitscreen-xxyy (visible-xxyy cam0) (xxyy comp))]
    (set-unitscreen-xxyy comp (visible-xxyy cam1) u-xxyy)))

(defn overlap? [xxyy x0 x1 y0 y1]
  (and (< (nth xxyy 0) x1)
       (< x0 (nth xxyy 1))
       (< (nth xxyy 2) y1)
       (< y0 (nth xxyy 3))))

(defn _split-rect [xxyy x0 x1 y0 y1] ; May make duplicates.
  (if (overlap? xxyy x0 x1 y0 y1)
    (filterv #(and (> (- (second %) (first %)) 0) (> (- (nth % 3) (nth % 2)) 0))
             [[(max (nth xxyy 0) x1) (nth xxyy 1) (nth xxyy 2) (nth xxyy 3)]
              [(nth xxyy 0) (min (nth xxyy 1) x0) (nth xxyy 2) (nth xxyy 3)]
              [(nth xxyy 0) (nth xxyy 1) (max (nth xxyy 2) y1) (nth xxyy 3)]
              [(nth xxyy 0) (nth xxyy 1) (nth xxyy 2) (min (nth xxyy 3) y0)]])
    [xxyy]))

(defn screenunitfree-rects [unit-xxyys]
  "Set of unique rects that are maximally extended that border unit-xxyys."
  (let [n (count unit-xxyys)
        x0s (mapv first unit-xxyys) x1s (mapv second unit-xxyys)
        y0s (mapv #(nth % 2) unit-xxyys) y1s (mapv #(nth % 3) unit-xxyys)]
    (loop [acc #{[0.0 1.0 0.0 1.0]} ix 0]
      ; The hash-set step prevents the exponential increase, since each rect must border 4 boundaries
      ; and there is no arithmetic => no rounding error.
      ; it is easy to show there are at most O(n^4) rects, for O(n^5) complexity.
      ; Still scary but I think in practice the actual complexity will be much lower.
      (if (= ix n) acc
          (let [x0 (x0s ix) x1 (x1s ix) y0 (y0s ix) y1 (y1s ix)
                acc1 (into [] (apply concat (mapv #(_split-rect % x0 x1 y0 y1) acc)))]
            (recur (apply hash-set acc1) (inc ix)))))))

(defn maxarea-free-screenunitxxyy [s]
  "The largest free xxyy on the screen. 
   x0 = x1 an y0 = y1 if there is no free space."
  (let [vis-xxyy (visible-xxyy (:camera s)) x (- (vis-xxyy 1) (vis-xxyy 0)) y (- (vis-xxyy 3) (vis-xxyy 2))
        uxxyys (mapv #(unitscreen-xxyy vis-xxyy %) (mapv xxyy (vals (:components s))))
        uxxyys (filterv #(and (> (+ (% 0) (% 1)) 0) (< (% 0) x) (> (+ (% 2) (% 3)) 0) (< (% 3) y)) uxxyys)
        
        rects (into [] (screenunitfree-rects uxxyys))
        areas (mapv #(* (- (% 1) (% 0)) (- (% 3) (% 2))) rects)
        max-area (apply max 0 areas)]
    (if (= rects []) [(vis-xxyy 0) (vis-xxyy 0) (vis-xxyy 2) (vis-xxyy 2)] ; can happen once in a while when there is no space.
      (nth rects (first (filter #(= (nth areas %) max-area) (range)))))))
