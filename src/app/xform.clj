; transforms are [x y scalex scaley] with scale applied first.
; WARNING: the java interop is very incomplete.

(ns app.xform)

;;;;;;;;;;;;;;;;; Transform multiplication ;;;;;;;;;;;;;;;;;;

(defn xv [xform x y]
  "xforms scale first then move the vector x y"
  [(+ (* x (nth xform 2)) (first xform)) (+ (* y (nth xform 3)) (second xform))])

(defn xx [xform-2 xform-1] ; apply 1 and then apply 2, like matrixes applied to vectors.
  [(+ (* (first xform-1) (nth xform-2 2)) (first xform-2))
   (+ (* (second xform-1) (nth xform-2 3)) (second xform-2))
   (* (nth xform-1 2) (nth xform-2 2))
   (* (nth xform-1 3) (nth xform-2 3))])

(defn x-1 [xform]
  "Inverts the transform."
  (let [x (first xform) y (second xform) sx (nth xform 2) sy (nth xform 3)]
    [(/ (- x) sx) (/ (- y) sy) (/ 1.0 sx) (/ 1.0 sy)]))

;;;;;;;;;;;;;;;;; applying xforms to events and graphics ;;;;;;;;;;;;;;;;;;

(defn xevt [xform evt]
  "xforms the points within the event (i.e. X and Y in mouse events).
   Inverting the camera's xform and applying it to an evt gives the 'real' evt location."
  ; Don't convert the screenX, screenY, they are supposed to be not converted.
  (let [x (first xform) y (second xform) sx (nth xform 2) sy (nth xform 3)
        evt (if (:X evt) (assoc evt :X (+ (* sx (:X evt)) x)) evt)
        evt (if (:X0 evt) (assoc evt :X0 (+ (* sx (:X0 evt)) x)) evt)
        evt (if (:X1 evt) (assoc evt :X1 (+ (* sx (:X1 evt)) x)) evt)
        evt (if (:Y evt) (assoc evt :Y (+ (* sy (:Y evt)) y)) evt)
        evt (if (:Y0 evt) (assoc evt :Y0 (+ (* sy (:Y0 evt)) y)) evt)
        evt (if (:Y1 evt) (assoc evt :Y1 (+ (* sy (:Y1 evt)) y)) evt)] evt))

(defn new-location [xform name-kwd old-loc]
  "calculates the new location (which is the second argument of the graphics, in vector form).
   what we call 'location' is really the list of arguments passed to the java graphics call.
   but it acts like location to a large extent."
  ; only numbers are converted.
  ; the default pattern is [x y width height width height width height].
  ; dievations are handled in the default.
  ; changes to linewidth are NOT handled here.
  ; The code could be made shorter by having functions that perform each elementary transform.
  (let [x (first xform) y (second xform) sx (nth xform 2) sy (nth xform 3)]
    (cond (and (= x 0) (= y 0) (= sx 1) (= sy 1)) old-loc ; no change if not bieng transformed.
      (or (= name-kwd :drawBytes) (= name-kwd :drawChars)) ; don't know how to size these.
      [(first old-loc) (second old-loc) (nth old-loc 2) 
       (+ (* (nth old-loc 3) sx) x) (+ (* (nth old-loc 4) sy) y)]
      (= name-kwd :drawImage)
      (cond (< (count old-loc) 6) (throw (Exception. ":drawImage must use a form that specifies the width and height."))
        (< (count old-loc) 8) ; unscaled form, must scale it.
        (into [] 
          (concat
            [(first old-loc) (+ (* (second old-loc) sx) x) (+ (* (nth old-loc 2) sy) y)
             (+ (* (+ (second old-loc) (nth old-loc 3)) sx) x)
             (+ (* (+ (nth old-loc 2) (nth old-loc 4)) sy) y)] (subvec old-loc 5)))
        :else
        (into [] 
          (concat [(first old-loc) (+ (* (second old-loc) sx) x) (+ (* (nth old-loc 2) sy) y)
                   (+ (* (nth old-loc 3) sx) x) (+ (* (nth old-loc 4) sy) y)]
           (subvec old-loc 5))))
      (= name-kwd :drawLine)
      [(+ (* (first old-loc) sx) x) (+ (* (second old-loc) sy) y)
       (+ (* (nth old-loc 2) sx) x) (+ (* (nth old-loc 3) sy) y)]
      (or (= name-kwd :drawPolygon) (= name-kwd :drawPolyline))
      (throw (Exception. "TODO: support polygons and polylines"))
      (or (= name-kwd :drawRect) (= name-kwd :fillRect) (= name-kwd :drawOval) (= name-kwd :fillOval))
      [(+ (* (first old-loc) sx) x) (+ (* (second old-loc) sy) y)
       (* (nth old-loc 2) sx) (* (nth old-loc 3) sy)] ; x y w h
      (= name-kwd :drawString)
      [(first old-loc) (+ (* (second old-loc) sx) x) (+ (* (nth old-loc 2) sy) y)]
      :else ; guessing, this list isn't complete.
      (mapv #(cond (not (number? %1)) %1 
              (even? %2) (+ (* %1 sx) x)
              :else (+ (* %1 sy) y)) old-loc (range)))))

(defn xy-keypoints [g-cmd]
  "Keypoints that enclose everything. Sometimes a guess. [[x0 x1 x2] [y0 y1 y2]] format."
  (let [name-kwd (first g-cmd) args (second g-cmd) na (count args)]
    (cond
      (or (= name-kwd :drawBytes) (= name-kwd :drawChars) (= name-kwd :drawString)) ; guess size these. Newlines dont wrap
      (let [sz (if-let [sz (:FontSize (get g-cmd 2))] sz 11) s? (= name-kwd :drawString)
            szx (* sz 0.75) szy sz x0 (nth args (if s? 1 3)) y0 (nth args (if s? 2 4))
            nc (if s? (count (first args)) (nth args 2))]
        [[x0 (+ x0 (* szx nc))] [y0 (+ y0 szy)]])
      (= name-kwd :drawImage)
      (if (> na 8) [[(nth args 1) (nth args 3)] [(nth args 2) (nth args 4)]] ; rectangle specified.
        (let [^java.awt.Image im (first args) ^java.awt.image.ImageObserver nil-obs nil
              x0 (nth args 1) y0 (nth args 2) wh? (number? (get args 3))
              w (if wh? (nth args 3) (.getWidth im)) h (if wh? (nth args 4) (.getHeight im))]
          [[x0 (+ x0 w)] [(y0 (+ y0 h))]]))
      (= name-kwd :drawLine)
      [[(nth args 0) (nth args 2)] [(nth args 1) (nth args 3)]]
      (or (= name-kwd :drawPolygon) (= name-kwd :drawPolyline))
      (throw (Exception. "TODO: support polygons and polylines"))
      (or (= name-kwd :drawRect) (= name-kwd :fillRect) (= name-kwd :drawOval) (= name-kwd :fillOval))
      [[(first args) (+ (first args) (nth args 2))] [(second args) (+ (second args) (nth args 3))]]
      :else ; guessing, this list isn't complete.
      (let [x?s (mapv #(and (number? %1) (even? %2)) args (range))
            y?s (mapv #(and (number? %1) (odd? %2)) args (range))]
        [(mapv #(nth args %) (filterv #(nth x?s %) (range na))) 
         (mapv #(nth args %) (filterv #(nth y?s %) (range na)))]))))

(defn xxyy-gfx-bound [g-cmds]
  "Computes bounds of the graphics, xxyy format"
  (let [kpts (mapv xy-keypoints g-cmds) kpx (conj (into [] (apply concat (mapv first kpts))) 0)
        kpy (conj (into [] (apply concat (mapv second kpts))) 0)]
    [(apply min kpx) (apply max kpx) (apply min kpy) (apply max kpy)]))

(defn xgfx [xform g-cmd keep-width?]
  "Transforms a single graphics command.
   xform is how the component is transformed; use this to draw transformed components.
   TODO: implement this keep-width? false mean lines get thinner/thicker (using transparancy when < 1 pixel).
   true will keep lines at the same width, so a 1-pixel line stays 1 pixel."
  (if (= (first g-cmd) :java) g-cmd ; custom commands, no way to try to parse them.
    (let [g-cmdl (assoc g-cmd 1 (new-location xform (first g-cmd) (second g-cmd)))]
      (cond (= (first g-cmd) :drawString)
        (let [sz (* (if-let [sz (:FontSize (get g-cmdl 2))] sz 11) (nth xform 2))]
          (assoc-in g-cmdl [2 :FontSize] sz))
        :else g-cmdl))))

(defn xlisten-map [lfns]
  "Convience fn to work on a map of listener fns."
  (zipmap (keys lfns) 
    (mapv (fn [k v] #(v (xevt %3 %1) %2))
       (keys lfns) (vals lfns))))
