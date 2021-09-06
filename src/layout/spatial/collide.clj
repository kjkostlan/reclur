; Collision detection and related functions.
(ns layout.spatial.collide
  (:require [layout.spatial.xform :as xform]
    [c] [np]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Component position ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn max-z "Always at least 0" [s] (apply max 0 (mapv #(if (and (:z %) (not= (:type %) :hintbox)) (:z %) 0) (vals (:components s)))))

(defn get-xxyys [s] (mapv xform/box-xxyy (vals (:components s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Working with xxyys ;;;;;;;;;;;;;;;;;;;;;;;;;

(defn union-xxyy [xxyy & xxyys]
  "The xxyy that encloses everything."
  [(apply min (nth xxyy 0) (mapv #(nth % 0) xxyys))
   (apply max (nth xxyy 1) (mapv #(nth % 1) xxyys))
   (apply min (nth xxyy 2) (mapv #(nth % 2) xxyys))
   (apply max (nth xxyy 3) (mapv #(nth % 3) xxyys))])

(defn zero-xxyy [xxyy]
  "Center at origin."
  (let [mx (* 0.5 (+ (nth xxyy 0) (nth xxyy 1))) my (* 0.5 (+ (nth xxyy 2) (nth xxyy 3)))]
    (mapv - xxyy [mx mx my my])))

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

;;;;;;;;;;; Collision detection ;;;;;;;;;;;

(defn hit-rect? [x y x0 x1 y0 y1]
  (and (>= x x0) (<= x x1) (>= y y0) (<= y y1)))

(defn engulfs? [xxyy-big xxyy-small]
  (and (< (xxyy-big 0) (xxyy-small 0)) (> (xxyy-big 1) (xxyy-small 1))
    (< (xxyy-big 2) (xxyy-small 2)) (> (xxyy-big 3) (xxyy-small 3))))

(defn xxyy-union [& xxyys]
  "Similar to bounding boxes of compund objects in physics engines."
  [(apply min (mapv #(nth % 0) xxyys))
   (apply max (mapv #(nth % 1) xxyys))
   (apply min (mapv #(nth % 2) xxyys))
   (apply max (mapv #(nth % 3) xxyys))])

(defn bounding-xxyy [comps]
  (if (= (count comps) 0) [-1e100 -1e100 -1e100 -1e100]
    (apply xxyy-union (mapv xform/box-xxyy comps))))

(defn click? [x y comp]
  (if (or (nil? x) (nil? y)) (throw (Exception. "Nil coords")))
  (let [x0 (first (:position comp))
        y0 (second (:position comp))
        sx (first (:size comp))
        sy (second (:size comp))]
    (and (>= x x0) (>= y y0) (<= x (+ x0 sx)) (<= y (+ y0 sy)))))

(defn unders-cursor [x y comps]
  (filterv #(click? x y (get comps %)) (keys comps)))

(defn under-cursor [x y comps]
  "Returns the key to the highest z-valued comp under the mouse, nil if nothing is under the mouse."
  (let [clickks (unders-cursor x y comps)
        clickzs (mapv #(if-let [z (:z (get comps %))] z 0) clickks)]
    (second (last (sort-by first (mapv vector clickzs clickks))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Complex metrics, exact ;;;;;;;;;;;;;;;;;;;;;;;;

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
  (let [vis-xxyy (xform/visible-xxyy (:camera s)) x (- (vis-xxyy 1) (vis-xxyy 0)) y (- (vis-xxyy 3) (vis-xxyy 2))
        uxxyys (mapv #(xform/unitscreen-xxyy vis-xxyy %) (mapv xform/box-xxyy (vals (:components s))))
        uxxyys (filterv #(and (> (+ (% 0) (% 1)) 0) (< (% 0) x) (> (+ (% 2) (% 3)) 0) (< (% 3) y)) uxxyys)

        rects (into [] (screenunitfree-rects uxxyys))
        areas (mapv #(* (- (% 1) (% 0)) (- (% 3) (% 2))) rects)
        max-area (apply max 0 areas)]
    (if (= rects []) [(vis-xxyy 0) (vis-xxyy 0) (vis-xxyy 2) (vis-xxyy 2)] ; can happen once in a while when there is no space.
      (nth rects (first (filter #(= (nth areas %) max-area) (range)))))))

(defn most-on-screen [s & comp-filter]
  "Most not-offscreen component's key for which (comp-filter comp) is true, comp-filter is optional.
   If there aren't any components (matching comp-filter if used, otherwise no restrictions) on the screen it returns false"
  (let [vis-xxyy (xform/visible-xxyy (:camera s))
        maybe-f (if (first comp-filter) (first comp-filter))
        on-ness #(let [uxxyy (xform/unitscreen-xxyy vis-xxyy (xform/box-xxyy %))
                       x0 (first uxxyy) x1 (second uxxyy)
                       y0 (nth uxxyy 2) y1 (nth uxxyy 3)
                       dx (- x1 x0) dy (- y1 y0)
                       left (min dx (max 0 (- x0)))
                       horiz (min dx (+ left (max 0 (- x1 1))))
                       top (min dy (max 0 (- y0)))
                       vert (min dy (+ top (max 0 (- y1 1))))]
                   (* (- dx horiz) (- dy vert)));(/ (* (- dx horiz) (- dy vert)) (* dx dy)))
        comps (:components s)
        ks (if maybe-f (filterv #(maybe-f (get comps %)) (keys comps)) (keys comps))]
     (if (> (count ks) 0)
       (let [scores (mapv #(on-ness (get comps %)) ks)
             max-sc (apply max scores)]
         (if (> max-sc 0)
           (nth ks (first (filter #(= (nth scores %) max-sc) (range)))) false)) false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Complex metrics, heuristic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn boxed-density-measure [xxyys xxyy-box resolution]
   "Measures how much crap is at or near each point. Maybe components could go where there is less stuff?
    Returns [xs ys density-at-xys]"
    (let [box0 (nth xxyy-box 0) box1 (nth xxyy-box 1)
          boy0 (nth xxyy-box 2) boy1 (nth xxyy-box 3)
          total-size (+ (- box1 box0) (- boy1 boy0))
          ; Collision: center 1.0, edges 0.0
          proximity-wt (* 0.05 total-size) ; Nearness: total distance. This is scale invariant.
          den-fn1 (fn [xxyy x y]
                    (let [x0 (first xxyy) y0 (nth xxyy 2)
                          x1 (second xxyy) y1 (nth xxyy 3)
                          dx (- x1 x0) dy (- y1 y0)
                          wt (* total-size total-size (/ 1.0 (+ 1e-10 (* dx dy))))
                          xm (+ (* 0.5 x0) (* 0.5 x1)) ym (+ (* 0.5 y0) (* 0.5 y1))]
                      (let [x-score (cond (or (<= x x0) (>= x x1)) 0.0
                                      (<= x xm) (- 1.0 (* 2.0 (/ (- xm x) (+ 1e-10 dx))))
                                      (>= x xm) (- 1.0 (* 2.0 (/ (- x xm) (+ 1e-10 dx))))
                                      :else 0.0)
                            y-score (cond (or (<= y y0) (>= y y1)) 0.0
                                      (<= y ym) (- 1.0 (* 2.0 (/ (- ym y) (+ 1e-10 dy))))
                                      (>= y ym) (- 1.0 (* 2.0 (/ (- y ym) (+ 1e-10 dy))))
                                      :else 0.0)
                            proximity (/ proximity-wt (+ (Math/abs (- x x0)) dx (Math/abs (- y y0)) dy))]
                        (+ proximity (* wt x-score y-score)))))
          den-fn (fn [x y] (apply + (mapv #(den-fn1 % x y) xxyys)))
          xs (np/linspace box0 box1 resolution)
          ys (np/linspace boy0 boy1 resolution)
          get-row (fn [y] [xs (into [] (repeat resolution y)) (mapv #(den-fn % y) xs)])
          xs-ys-densitys (mapv get-row ys)
          xs (apply c/vcat (mapv first xs-ys-densitys))
          ys (apply c/vcat (mapv second xs-ys-densitys))
          densities (apply c/vcat (mapv last xs-ys-densitys))]
    [xs ys densities]))