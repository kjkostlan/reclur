; Tools that are useful for making layouts.
(ns layout.layoutcore
  (:require [globals]
    [layout.xform :as xform]
    [c] [np]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn pnorm-dist [x1 x2 y1 y2 pnorm]
  "1-norm vs 2-norms battle it out."
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (cond (<= pnorm 1e-6)
      (+ (Math/abs dx) (Math/abs dy))
      (= pnorm 2.0)
      (Math/sqrt (+ (* dx dx) (* dy dy)))
      (< pnorm 1e6)
      (Math/pow (+ (Math/pow dx pnorm) (Math/pow dy pnorm)) (/ pnorm))
      :else (max (Math/abs dx) (Math/abs dy)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Component position ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn xxyy [component]
  (let [xy (:position component) sz (:size component)]
    [(xy 0) (+ (xy 0) (sz 0)) (xy 1) (+ (xy 1) (sz 1))]))

(defn set-xxyy [component xxyy]
  (assoc component :position [(first xxyy) (nth xxyy 2)] :size [(- (second xxyy) (first xxyy)) (- (nth xxyy 3) (nth xxyy 2))]))

(defn max-z "Always at least 0" [s] (apply max 0 (mapv #(if (and (:z %) (not= (:type %) :hintbox)) (:z %) 0) (vals (:components s)))))

(defn get-xxyys [s] (mapv xxyy (vals (:components s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Working with xxyys ;;;;;;;;;;;;;;;;;;;;;;;;;

(defn scale-xxyy
  "Scale the xxyy keeping the center unchanged."
  ([xxyy0 scalexy]
    (scale-xxyy xxyy0 scalexy scalexy))
  ([xxyy0 scalex scaley]
    (let [x0 (nth xxyy0 0) x1 (nth xxyy0 1) y0 (nth xxyy0 2) y1 (nth xxyy0 3)
          shx (* (- x1 x0) (* 0.5 (- scalex 1.0)))
          shy (* (- y1 y0) (* 0.5 (- scaley 1.0)))]
      [(- x0 shx) (+ x1 shx) (- y0 shy) (+ y1 shy)])))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Screen and camera conversion stuff ;;;;;;;;;;;;;;;;;;;;;;;;;

(defn y- [unit-xxyy]
  "When it's more intuitive to work in a system where +y is up."
 [(first unit-xxyy) (second unit-xxyy) (- 1.0 (nth unit-xxyy 3)) (- 1.0 (nth unit-xxyy 2))])

(defn screen-pixels []
  "[x y] pixels. The Retnia displays pretend to be 1/2 the resolution."
  (if-let [sz (:window-size @globals/external-state-atom)]
    sz [800 600]))

(defn visible-xxyy [cam]
  "x1 x2 y1 y2 of the screen in pixels based on the camera."
  (let [xy (screen-pixels) cam-1 (xform/x-1 cam)
        corner0 (xform/xv cam-1 0 0) corner1 (xform/xv cam-1 (first xy) (second xy))]
    [(first corner0) (first corner1) (second corner0) (second corner1)]))

(defn visible-xxyy-to-cam [xxyy]
  "Visible region -> camera. Should functions like these belong in xform?"
  (let [xy (screen-pixels)
        x0 (first xxyy) x1 (second xxyy) y0 (nth xxyy 2) y1 (nth xxyy 3)
        zoomx (/ (first xy) (- x1 x0)) zoomy (/ (second xy) (- y1 y0))
        zoom (* 0.5 (+ zoomx zoomy))

        sh-x (- (* x0 zoom)) ;x0*zoom + sh-x = 0
        sh-y (- (* y0 zoom))]
    [sh-x sh-y zoom zoom]))

(defn world-xxyy [vis-xxyy unit-xxyy]
  "Visible region, location within region => world location."
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

(defn set-unitscreen-xxyy [component visible-xxyy unit-xxyy]
  "Sets a component within the screen on a 0-1 scale."
  (set-xxyy component (world-xxyy visible-xxyy unit-xxyy)))

(defn center-cam-on [cam component]
  "Keeps the zoom the same."
  (let [mid-x (+ (first (:position component)) (* 0.5 (first (:size component))))
        mid-y (+ (second (:position component)) (* 0.5 (second (:size component))))
        vxxyy (visible-xxyy cam) dx (- (vxxyy 1) (vxxyy 0)) dy (- (vxxyy 3) (vxxyy 2))]
    [(- (- mid-x (* dx 0.5))) (- (- mid-y (* dy 0.5))) (cam 2) (cam 3)]))

(defn unique-z [components]
  "Assigns each component a unique z-value."
  (loop [acc (zipmap (keys components) (mapv #(if (:z %) % (assoc % :z 1)) (vals components)))]
    (if (= (count (apply hash-set (mapv :z (vals acc)))) (count acc)) acc
      (recur (zipmap (keys acc) (mapv (fn [c] (update c :z #(+ (* % (+ 1 (* (Math/random) 1e-10))) 1e-100))) (vals acc)))))))

(defn dont-move [comp cam0 cam1]
  "Keeps the screen-position the same, by changing comp's position."
  (let [u-xxyy (unitscreen-xxyy (visible-xxyy cam0) (xxyy comp))]
    (set-unitscreen-xxyy comp (visible-xxyy cam1) u-xxyy)))

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
  (let [vis-xxyy (visible-xxyy (:camera s)) x (- (vis-xxyy 1) (vis-xxyy 0)) y (- (vis-xxyy 3) (vis-xxyy 2))
        uxxyys (mapv #(unitscreen-xxyy vis-xxyy %) (mapv xxyy (vals (:components s))))
        uxxyys (filterv #(and (> (+ (% 0) (% 1)) 0) (< (% 0) x) (> (+ (% 2) (% 3)) 0) (< (% 3) y)) uxxyys)

        rects (into [] (screenunitfree-rects uxxyys))
        areas (mapv #(* (- (% 1) (% 0)) (- (% 3) (% 2))) rects)
        max-area (apply max 0 areas)]
    (if (= rects []) [(vis-xxyy 0) (vis-xxyy 0) (vis-xxyy 2) (vis-xxyy 2)] ; can happen once in a while when there is no space.
      (nth rects (first (filter #(= (nth areas %) max-area) (range)))))))

(defn most-on-screen [s & comp-filter]
  "Most not-offscreen component's key for which (comp-filter comp) is true, comp-filter is optional.
   If there aren't any components (matching comp-filter if used, otherwise no restrictions) on the screen it returns false"
  (let [vis-xxyy (visible-xxyy (:camera s))
        maybe-f (if (first comp-filter) (first comp-filter))
        on-ness #(let [uxxyy (unitscreen-xxyy vis-xxyy (xxyy %))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Layout functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn snap-rects [xxyys snap-threshold-nextto snap-threshold-distanced allow-resize?]
  "Snaps nearby values together. Thresholds are absolute."
 (/ 0))

(defn make-grid [comps x0 x1 y0 y1]
  "As square as possible."
  (let [n (count comps)
        nx (cond (<= n 3) 1
             (<= n 6) 2
             (<= n 12) 3
             (<= n 20) 4
             (<= n 30) 5
             (<= n 40) 6
             :else (Math/round (Math/sqrt n)))
        ny (int (+ (/ n nx) 1 -1e-9))
        x-values (np/linspace x0 x1 (inc nx))
        y-values (np/linspace y0 y1 (inc ny))
        x0-values (into [] (butlast x-values))
        x1-values (into [] (rest x-values))
        y0-values (into [] (butlast y-values))
        y1-values (into [] (rest y-values))
        xxyys (loop [acc [] ix 0 ix-x 0 ix-y 0]
                (if (= ix n) acc
                  (let [wrap? (= ix-x (dec nx))]
                    (recur (conj acc [(nth x0-values ix-x)
                                      (nth x1-values ix-x)
                                      (nth y0-values ix-y)
                                      (nth y1-values ix-y)])
                      (inc ix)
                      (if wrap? 0 (inc ix-x))
                      (if wrap? (inc ix-y) ix-y)))))
        aply (fn [c ix] (let [xxyy (nth xxyys ix)]
                          (assoc c :position [(first xxyy) (nth xxyy 2)]
                            :size [(- (second xxyy) (first xxyy))
                                   (- (nth xxyy 3) (nth xxyy 2))])))]
    (if (map? comps) (zipmap (keys comps) (mapv aply (vals comps) (range)))
      (mapv aply comps (range)))))