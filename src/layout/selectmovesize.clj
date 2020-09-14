; Handles selection (single or multible), moving and resizing.

(ns layout.selectmovesize
  (:require [layout.xform :as xform]
    [app.multicomp :as multicomp]
    [clojure.string :as string]
    [layout.keyanal :as ka]
    [layout.layoutcore :as layoutcore]
    [clojure.set :as set]
    [globals]))

(def ^:dynamic *handle-size* 25)

(defn is-sh? [] (:ShiftDown (:external-state @globals/one-atom)))
(defn is-alt? [] (:AltDown (:external-state @globals/one-atom)))
(defn is-mouse? [] (let [ext (:external-state @globals/one-atom)]
                     (or (:Button0 ext) (:Button1 ext) (:Button2 ext))))

(defn add-defaults [tool-state] ; nil check. ;TODO: code can be simplified with collections.
  (let [addy (fn [def x] (reduce #(if (get x %2) (assoc %1 %2 (get x %2)) %1) def (keys x)))
        def {:xxyy [-1e100 -1e100 -1e100 -1e100]
             :corner-mode :miss
             :copy-mx 0 :copy-my 0}] (addy def tool-state)))

(defn comp-xxyy [comp]
  (let [p (:position comp) sz (:size comp)] 
    [(p 0) (+ (p 0) (sz 0)) (p 1) (+ (p 1) (sz 1))]))

(defn hit-rect? [x y x0 x1 y0 y1]
  (and (>= x x0) (<= x x1) (>= y y0) (<= y y1)))

(defn engulfs? [xxyy-big xxyy-small]
  (and (< (xxyy-big 0) (xxyy-small 0)) (> (xxyy-big 1) (xxyy-small 1))
    (< (xxyy-big 2) (xxyy-small 2)) (> (xxyy-big 3) (xxyy-small 3))))

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
  (if (= (count comps) 0) [-1e100 -1e100 -1e100 -1e100]
    [(apply min (mapv #(first (:position %)) comps)) 
     (apply max (mapv #(+ (first (:position %)) (first (:size %))) comps))
     (apply min (mapv #(second (:position %)) comps)) 
     (apply max (mapv #(+ (second (:position %)) (second (:size %))) comps))]))

(defn fit-to-screen [s comp]
  "Moves the comp, and resizes it if necessary, to make it fit in the screen."
(let [screen-pix (layoutcore/screen-pixels)
      i-cam (xform/x-1 (:camera s))
      corner-nw (xform/xv i-cam 0 0) corner-se (apply xform/xv i-cam screen-pix)
      pos (:position comp) sz (:size comp)
      sz (mapv min sz (mapv - corner-se corner-nw))
      pos (mapv max pos corner-nw)
      pos (mapv min pos (mapv - corner-se sz))]
  (assoc comp :position pos :size sz)))

(defn derive-key [kwd]
  (keyword (gensym 'copy)))

(defn gts [s] (add-defaults (:selectmovesize (:tool-state s))))
(defn sts [s ts] (assoc-in s [:tool-state :selectmovesize] ts))
(defn uts [s f] (update-in s [:tool-state :selectmovesize] f))

; Inkscape-like selection tool:
(defn get-tool []
  {:render (fn [s] (let [ts (gts s) zoom (last (:camera s))
                         _ (if (< zoom 1e-10) (throw (Exception. "The zoom got set to zero somehow.")))
                         sh? (is-sh?) ; shifting disables seeing the selection (until the mouse gets going), as it disables being to drag the selection.
                         alt? (is-alt?) ; alt is the same idea and it removes selections.
                         mouse? (is-mouse?) ts (gts s)] 
                     (if (and (or sh? alt?) (or (not mouse?) (:insta-drag? ts)))
                       [] (apply draw-box-handle (/ *handle-size* zoom) (:corner-mode s) (:xxyy ts)))))
   :mousePressed (fn [mevt-c s]
                   (let [ts (gts s) x (:X mevt-c) y (:Y mevt-c)
                        sh? (:ShiftDown mevt-c) ; shift adds to the selection.
                        alt? (:AltDown mevt-c) ; alt subtracts
                        comps (:components s) target (click-test mevt-c ts (last (:camera s)))
                        target (if (or sh? alt?) :miss target) ; shift/alt means don't drag the selection box.
                        khit (under-cursor x y comps)
                        insta-drag? (and (= target :miss) khit)   
                        ts (assoc ts :fresh-press? true :insta-drag? insta-drag?
                             :corner-mode (if (and insta-drag? (not sh?) (not alt?)) :main-rect target))]
                    (cond insta-drag?
                      (let [comp (get comps khit) pos (:position comp) sz (:size comp)
                            ts (assoc ts :xxyy [(first pos) (+ (first pos) (first sz)) (second pos) (+ (second pos) (second sz))])] 
                        (assoc (sts s ts) :selected-comp-keys 
                          (cond sh? (set/union (set (:selected-comp-keys s)) (hash-set khit))
                            alt? (set/difference (set (:selected-comp-keys s)) (hash-set khit))
                            :else (hash-set khit))))
                      (= target :miss) (sts (if (or sh? alt?) s (assoc s :selected-comp-keys #{})) (assoc ts :xxyy [x x y y] :corner-mode :miss)) ; zero-size rectangle.
                      :else (sts s ts))))
  ; Snap to whatever is selected.
  :mouseReleased (fn [mevt-c s] 
                   (let [ts (gts s)]
                     (if (:fresh-press? ts) 
                       (let [xxyy-loose (:xxyy ts)
                             sel-kys (if (= (:corner-mode ts) :miss) 
                                        ((if (is-alt?) set/difference set/union)
                                          (set (:selected-comp-keys s))
                                          (set (apply selected-comp-keys (:components s) xxyy-loose)))
                                         (set (:selected-comp-keys s)))
                             xxyy-tight (bounding-xxyy (mapv #(get (:components s) %) sel-kys))
                             ts (assoc ts :fresh-press? false :xxyy xxyy-tight :corner-mode :miss)] 
                         (assoc (sts s ts) :selected-comp-keys sel-kys))
                       s)))
  :mouseDragged (fn [mevt-c s] 
                  (let [ts (gts s) insta-drag? (:insta-drag? ts)
                        xxyy (:xxyy ts) mx1 (:X1 mevt-c) my1 (:Y1 mevt-c) 
                        mx (:X mevt-c) my (:Y mevt-c) target0 (:corner-mode ts)]
                    (if (= target0 :miss) 
                      (cond (and (is-sh?) insta-drag?)
                        (let [khit (under-cursor mx my (:components s))]
                          (if khit (update s :selected-comp-keys #(conj % khit)) s))
                        (and (is-alt?) insta-drag?) 
                        (let [khit (under-cursor mx my (:components s))]
                          (if khit (update s :selected-comp-keys #(disj % khit)) s))
                        :else (sts s (update ts :xxyy #(assoc % 1 mx 3 my)))) ; creating a rectangle, no selection.
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
                (let [mx (first (:mouse-pos-world s)) my (second (:mouse-pos-world s))] 
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

(defn clear-selecion [s]
  (let [mp (:mousePressed (get-tool)) mr (:mouseReleased (get-tool))
        s1 (mr (assoc {} :X 0 :Y 0) s) ; not the best code here...
        s2 (mp (assoc {} :X 1e100 :Y 1e100) s1)
        s3 (mr (assoc {} :X 1e100 :Y 1e100) s2)] s3))

(defn swap-on-top [s] 
  "Rotates the :z of components under the cursor. Also clears the selection (that was confusing)."
  (let [s (assoc-in s [:precompute :desync-safe-mod?] true)
        x (first (:mouse-pos-world s)) y (second (:mouse-pos-world s))
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

(defn wheel-zoom [m-evt s] ; w and s to zoom in and out by an increment.
                      (let [zoom0 (nth (:camera s) 2)
                            mX (:X m-evt) mY (:Y m-evt)
                            rotation (:PreciseWheelRotation m-evt)
                            step 0.020; (cond (:AltDown m-evt) 0.5 (:ShiftDown m-evt) 0.01 :else 0.05)
                            rzoom (Math/exp (* step rotation))
                            xf [(* mX (- 1 rzoom)) (* mY (- 1 rzoom)) rzoom rzoom]]
                        (update s :camera #(xform/xx % xf))))

(defn insta-zoom [k-evt s]
  (let [radius-range 1.75 ; for counting as "nearby".
        screen-sz (layoutcore/screen-pixels) 
        srx (* (first screen-sz) 0.5) sry (* (second screen-sz) 0.5)
        target [(first (:mouse-pos-world s)) (second (:mouse-pos-world s))]
        target-xxyy [(- (target 0) (* srx radius-range)) 
                     (+ (target 0) (* srx radius-range)) 
                     (- (target 1) (* sry radius-range)) 
                     (+ (target 1) (* sry radius-range))]
         
        comp-xxyys (mapv comp-xxyy (vals (:components s)))
        
        ; All nearby xxyys:
        near-xxyys (filterv #(engulfs? target-xxyy %) comp-xxyys)
               
        ; Bounding box thereof:
        bound-xxyy [(apply min 1e10 (mapv first near-xxyys))
                    (apply max -1e10 (mapv second near-xxyys))
                    (apply min 1e10 (mapv #(nth % 2) near-xxyys))
                    (apply max -1e10 (mapv #(nth % 3) near-xxyys))]
                         
        target1 [(+ (* (bound-xxyy 0) 0.5) (* (bound-xxyy 1) 0.5))
                 (+ (* (bound-xxyy 2) 0.5) (* (bound-xxyy 3) 0.5))] 
        target1-corner [(- (target1 0) srx) (- (target1 1) sry)]]
    (assoc s :camera [(- (target1-corner 0)) (- (target1-corner 1)) 1 1])))

; Why not put moving and sizing of the camera here as well?
(defn get-camera-tool []
  {:mouseWheelMoved wheel-zoom
   :keyPressed (fn [k-evt s]
                 (if (ka/emacs-hit? "C-1" k-evt) ; Zoom where (or very near where) the mouse currently is to zoom 1, best fitting to componens.
                   (insta-zoom k-evt s) s))
   :mouseDragged
    (fn [mevt-c s]
      (let [dx (- (:X mevt-c) (:X1 mevt-c)) dy (- (:Y mevt-c) (:Y1 mevt-c))
            xf [dx dy 1 1]]
        (update s :camera #(xform/xx % xf))))})
