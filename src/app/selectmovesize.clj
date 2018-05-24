; Handles selection (single or multible), moving and resizing.

(ns app.selectmovesize
  (:require [app.xform :as xform]
    [app.singlecomp :as singlecomp]
    [app.multicomp :as multicomp]
    [clojure.string :as string]))
; has been required by the multicomp.

(def ^:dynamic *handle-size* 25)

(defn add-defaults [tool-state] ; nil check. ;TODO: code can be simplified with collections.
  (let [addy (fn [def x] (reduce #(if (get x %2) (assoc %1 %2 (get x %2)) %1) def (keys x)))
        def {:xxyy [0 0 0 0]
             :corner-mode :miss
             :copy-mx 0 :copy-my 0}] (addy def tool-state)))

(defn hit-rect? [x y x0 x1 y0 y1]
  (and (>= x x0) (<= x x1) (>= y y0) (<= y y1)))

(defn unders-cursor [x y comps]
  (filterv #(singlecomp/click? x y (get comps %)) (keys comps)))

(defn under-cursor [x y comps] 
  "Returns the key to the highest z-valued comp under the mouse, nil if nothing is under the mouse."
  (let [clickks (unders-cursor x y comps)
        clickzs (mapv #(if-let [z (:z (get comps %))] z 0) clickks)]
    (second (last (sort-by first (mapv vector clickzs clickks))))))

(defn order [x0 x1 y0 y1]
  (let [v1 (if (<= x0 x1) [x0 x1] [x1 x0])
        v2 (if (<= y0 y1) [y0 y1] [y1 y0])] (into [] (concat v1 v2))))

(defn draw-box-handle [handle-sz ctest x0 x1 y0 y1] 
  ; Draws the selection box and handle.
  (if (>= (Math/abs (* (- x1 x0) (- y1 y0))) 1e-10) 
    (let [xxyy (order x0 x1 y0 y1) x0 (first xxyy) x1 (second xxyy) y0 (nth xxyy 2) y1 (nth xxyy 3)
          a (if (= ctest :miss) 0.6 1.0) ; stronger draw when activly using.
          box [[:fillRect [x0 y0 (- x1 x0) (- y1 y0)] {:Color [0.7 1 0.9 (if (= ctest :miss) 0.05 0.2)]}]
               [:drawRect [x0 y0 (- x1 x0) (- y1 y0)] {:Color [0.7 1 0.9 a]}]
               [:drawRect [(dec x0) (dec y0) (+ (- x1 x0) 2) (+ (- y1 y0) 2)] {:Color [0.4 1 0.5 a]}]]
          -h #(- % handle-sz)
          handles (mapv #(vector :drawRect [(first %) (second %) handle-sz handle-sz] {:Color [0.4 0.7 0.4 a]})
                    [[x0 y0] [x0 (-h y1)] [(-h x1) y0] [(-h x1) (-h y1)]])] 
        (into [] (concat box handles))) []))

(defn click-test [mevt-c ts zoom]
  "Which part of the selection rectangle we are in."
  (let [x (:X mevt-c) y (:Y mevt-c) sz (/ *handle-size* zoom)
        xxyy (apply order (:xxyy ts))
        x0 (first xxyy) x1 (second xxyy) y0 (nth xxyy 2) y1 (nth xxyy 3)]
    (cond
      (hit-rect? x y x0 (+ x0 sz) y0 (+ y0 sz)) :corner-nw
      (hit-rect? x y (- x1 sz) x1 y0 (+ y0 sz)) :corner-ne
      (hit-rect? x y (- x1 sz) x1 (- y1 sz) y1) :corner-se
      (hit-rect? x y x0 (+ x0 sz) (- y1 sz) y1) :corner-sw
      (hit-rect? x y x0 x1 y0 y1) :main-rect
      :else :miss)))

(defn _sel-hit? [x0 y0 x1 y1 comp]
  (let [cx0 (first (:position comp))
        cy0 (second (:position comp))
        cx1 (+ cx0 (first (:size comp)))
        cy1 (+ cy0 (second (:size comp)))
        overlap-frac 0.3] ; needed to trigger a selection.
    (and (<= x0 (+ (* cx0 overlap-frac) (* cx1 (- 1 overlap-frac))))
      (>= x1 (+ (* cx1 overlap-frac) (* cx0 (- 1 overlap-frac))))
      (<= y0 (+ (* cy0 overlap-frac) (* cy1 (- 1 overlap-frac))))
      (>= y1 (+ (* cy1 overlap-frac) (* cy0 (- 1 overlap-frac)))))))
(defn selected-comp-keys [comps x0 x1 y0 y1]
  (let [xxyy (order x0 x1 y0 y1)
        x0 (first xxyy) x1 (second xxyy) y0 (nth xxyy 2) y1 (nth xxyy 3)]
    (filterv #(_sel-hit? x0 y0 x1 y1 (get comps %)) (keys comps))))

(defn _xform-from-rect [bx0-old bx1-old by0-old by1-old bx0-new bx1-new by0-new by1-new]
  (let [sx (/ (- bx1-new bx0-new) (- bx1-old bx0-old))
        sy (/ (- by1-new by0-new) (- by1-old by0-old))]
    [(- bx0-new bx0-old (* bx0-old (- sx 1))) (- by0-new by0-old (* by0-old (- sy 1))) sx sy]))
(defn _new-rect [dmx dmy mode bx0 bx1 by0 by1]
  (cond (= mode :main-rect) [(+ bx0 dmx) (+ bx1 dmx) (+ by0 dmy) (+ by1 dmy)]
    (= mode :corner-nw) [(+ bx0 dmx) bx1 (+ by0 dmy) by1]
    (= mode :corner-ne) [bx0 (+ bx1 dmx) (+ by0 dmy) by1]
    (= mode :corner-se) [bx0 (+ bx1 dmx) by0 (+ by1 dmy)]
    (= mode :corner-sw) [(+ bx0 dmx) bx1 by0 (+ by1 dmy)]
    :else [bx0 bx1 by0 by1]))

(defn _apply-xform [comp xform]
  (let [not1? #(or (< % 0.99999) (> % 1.00001)) ; don't let rounding errors force re-draws.
        resize? (or (not1? (nth xform 2)) (not1? (nth xform 3)))
        comp1 (if resize? (update comp :size #(vector (max 1e-6 (* (first %) (nth xform 2))) (max 1e-6 (* (second %) (nth xform 3))))) comp)]
    (update comp1 :position #(apply xform/xv xform %))))
(defn apply-xform [comps keys xform]
  (reduce (fn [acc k] (update acc k #(_apply-xform % xform))) comps keys))

(defn bounding-xxyy [comps]
  (if (= (count comps) 0) [0 0 0 0]
    [(apply min (mapv #(first (:position %)) comps)) 
     (apply max (mapv #(+ (first (:position %)) (first (:size %))) comps))
     (apply min (mapv #(second (:position %)) comps)) 
     (apply max (mapv #(+ (second (:position %)) (second (:size %))) comps))]))

(defn derive-key [kwd]
  (let [s (subs (str kwd) 1) r #"copy\d+"
        cn (re-find r s)
        n (if cn (Integer. (string/replace cn #"copy" "")))]
    (keyword
      (if cn (str (string/replace s r "") "copy" (inc n)) (str s "copy1")))))

(defn gts [s] (add-defaults (:selectmovesize (:tool-state s))))
(defn sts [s ts] (assoc-in s [:tool-state :selectmovesize] ts))
(defn uts [s f] (update-in s [:tool-state :selectmovesize] f))

; Inkscape-like tool:
(defn get-tool []
 {:render (fn [s] (let [ts (gts s) zoom (last (:camera s))] 
                    (apply draw-box-handle (/ *handle-size* zoom) (:corner-mode s) (:xxyy ts))))
  :mousePressed (fn [mevt-c s] 
                  (let [ts (gts s) x (:X mevt-c) y (:Y mevt-c)
                        comps (:components s) target (click-test mevt-c ts (last (:camera s)))
                        khit (under-cursor x y comps)
                        insta-drag? (and (= target :miss) khit)   
                        ts (assoc ts :corner-mode (if insta-drag? :main-rect target))]
                    (cond insta-drag?
                      (let [comp (get comps khit) pos (:position comp) sz (:size comp)
                            ts (assoc ts :xxyy [(first pos) (+ (first pos) (first sz)) (second pos) (+ (second pos) (second sz))])] 
                        (assoc (sts s ts) :selected-comp-keys (hash-set khit)))
                      (= target :miss) (uts (assoc s :selected-comp-keys #{}) #(assoc % :xxyy [x x y y] :corner-mode :miss)) ; zero-size rectangle.
                      :else (sts s ts))))
  ; Snap to whatever is selected.
  :mouseReleased (fn [mevt-c s] 
                   (let [ts (gts s) xxyy-loose (:xxyy ts)
                         sel-kys (if (= (:corner-mode ts) :miss) (apply selected-comp-keys (:components s) xxyy-loose)
                                   (:selected-comp-keys s))
                         xxyy-tight (bounding-xxyy (mapv #(get (:components s) %) sel-kys))
                         ts (assoc ts :xxyy xxyy-tight :corner-mode :miss)] 
                     (assoc (sts s ts) :selected-comp-keys sel-kys)))
  :mouseDragged (fn [mevt-c s] 
                  (let [ts (gts s)
                        xxyy (:xxyy ts) mx1 (:X1 mevt-c) my1 (:Y1 mevt-c) 
                        mx (:X mevt-c) my (:Y mevt-c) target0 (:corner-mode ts)]
                    (if (= target0 :miss) 
                      (sts s (update ts :xxyy #(assoc % 1 mx 3 my))) ; creating a rectangle, no selection.
                      (let [dx (- mx mx1) dy (- my my1)
                            new-xxyy (if (:single-drag? ts) (mapv + xxyy [dx dx dy dy])
                                       (apply _new-rect dx dy target0 xxyy))
                            new-xxyy [(first new-xxyy) (max (+ (first new-xxyy) 1e-6) (second new-xxyy))
                                      (nth new-xxyy 2) (max (+ (nth new-xxyy 2) 1e-6) (nth new-xxyy 3))]
                            xform (if (> (count (:selected-comp-keys s)) 0) 
                                    (apply _xform-from-rect (first xxyy) (second xxyy) (nth xxyy 2) (nth xxyy 3) new-xxyy))
                            ts-new (assoc ts :xxyy new-xxyy)]
                        (update (sts s ts-new) :components #(apply-xform % (:selected-comp-keys s) xform))))))
  :keyPressed (fn [k-evt s]
                (let [mx (first (:mouse-pos s)) my (second (:mouse-pos s))] 
                  (cond (and (or (:ControlDown k-evt) (:MetaDown k-evt)) (= (str (:KeyChar k-evt)) (str "c")))
                    (let [ts (gts s) 
                          sel (:selected-comp-keys s)]
                      (sts s (assoc ts :copied-comps (zipmap sel (map #(get (:components s) %) sel))
                               :copy-mx mx :copy-my my))) 
                    (and (or (:ControlDown k-evt) (:MetaDown k-evt)) (= (str (:KeyChar k-evt)) (str "v")))
                    (let [ts (gts s) dx (- mx (:copy-mx ts))
                          dy (- my (:copy-my ts))
                          comp-kys (mapv derive-key (keys (:copied-comps ts))) 
                          comp-vals (mapv (fn [c] (update c :position #(vector (+ (first %) dx) (+ (second %) dy))))
                                      (vals (:copied-comps ts)))]
                      (sts (update s :components
                          #(merge % (zipmap comp-kys comp-vals)))
                        (assoc ts :copied-comps (zipmap comp-kys (vals (:copied-comps ts))))))
                    :else s)))})

; How to seralize functions?
;(require '[clojure.repl :as repl])
;(println (read-string (pr-str (:mousePressed (get-tool)))))

(defn clear-selecion [s]
  (let [mp (:mousePressed (get-tool)) mr (:mouseReleased (get-tool))
        s1 (mr (assoc {} :X 0 :Y 0) s) ; not the best code here...
        s2 (mp (assoc {} :X 1e100 :Y 1e100) s1)
        s3 (mr (assoc {} :X 1e100 :Y 1e100) s2)] s3))

(defn swap-on-top [s] 
  "Rotates the :z of components under the cursor. Also clears the selection (that was confusing)."
  (let [s (assoc-in s [:precompute :desync-safe-mod?] true)
        x (first (:mouse-pos s)) y (second (:mouse-pos s))
        comps (:components s)
        kys (unders-cursor x y comps)
      
        zs (mapv #(double (if-let [z (:z (get comps %1))] z %2)) kys (range))
        min-z (apply min 1e100 zs)
        ix-min (first (filter #(= (nth zs %) min-z) (range (count zs)))) ; non-unique min is uniquieified.

        zs (mapv #(if (and (= %1 min-z) (not= %2 ix-min)) (+ %1 1e-9) %1) zs (range))
        max-z (apply max -1e100 zs)
        second-min-z (apply min 1e50 (filter #(not= % min-z) zs))
        drop (- second-min-z min-z)
        zs1 (mapv #(if (= % min-z) max-z (- % drop)) zs)
        comps1 (reduce #(assoc-in %1 [(nth kys %2) :z] (nth zs1 %2)) comps (range (count kys)))]
    (assoc (clear-selecion s) :components comps1)))


; Why not put moving and sizing of the camera here as well?
(defn get-camera-tool []
  {:keyPressed (fn [k-evt s] ; w and s to zoom in and out by an increment.
                 (let [zoom0 (nth (:camera s) 2)
                       zoom-in? (= (:KeyCode k-evt) 38)
                       zoom-out? (= (:KeyCode k-evt) 40)
                       step (cond (:AltDown k-evt) 2 (:ShiftDown k-evt) 1.03 :else 1.1)
                       rzoom (cond zoom-in? step zoom-out? (/ step) :else 1)
                       xf [0 0 rzoom rzoom]]
                   (update s :camera #(xform/xx % xf))))
   :mouseDragged
    (fn [mevt-c s]
      (let [dx (- (:X mevt-c) (:X1 mevt-c)) dy (- (:Y mevt-c) (:Y1 mevt-c))
            xf [dx dy 1 1]]
        (update s :camera #(xform/xx % xf))))})