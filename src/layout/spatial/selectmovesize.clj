; Handles selection (single or multible), moving and resizing.

(ns layout.spatial.selectmovesize
  (:require [globals]
    [layout.spatial.xform :as xform]
    [layout.spatial.collide :as collide]
    [layout.spatial.lmodes.stack :as lstack]
    [layout.spatial.lmodes.teleport :as lteleport]
    [layout.keyanal :as ka] [layout.mouseanal :as ma]
    [app.multicomp :as multicomp]
    [app.tabgroup :as tabgroup]
    [clojure.string :as string]
    [clojure.set :as set]))

(def ^:dynamic *handle-pixels* 25)
(def ^:dynamic *overlap-frac-selectionbox* 0.3)
(def ^:dynamic *instazoom-tol* 0.333)

;;;;;;;;;;; Support functions ;;;;;;;;;;;

(defn is-sh? [] (:ShiftDown @globals/external-state-atom))
(defn is-alt? [] (:AltDown @globals/external-state-atom))
(defn is-mouse? [] (let [ext @globals/external-state-atom]
                     (or (:Button0 ext) (:Button1 ext) (:Button2 ext))))

(defn order [x0 x1 y0 y1]
  (let [v1 (if (<= x0 x1) [x0 x1] [x1 x0])
        v2 (if (<= y0 y1) [y0 y1] [y1 y0])] (into [] (concat v1 v2))))

(defn add-defaults [tool-state] ; nil check. ;TODO: code can be simplified with collections.
  (let [addy (fn [def x] (reduce #(if (get x %2) (assoc %1 %2 (get x %2)) %1) def (keys x)))
        def {:xxyy [-1e100 -1e100 -1e100 -1e100]
             :corner-mode :miss
             :copy-mx 0 :copy-my 0}] (addy def tool-state)))

(defn gts [s] (add-defaults (:selectmovesize (:tool-state s))))
(defn sts [s ts] (assoc-in s [:tool-state :selectmovesize] ts))
(defn uts [s f] (update-in s [:tool-state :selectmovesize] f))

(defn _sel-hit? [x0 y0 x1 y1 comp]
  (let [cx0 (first (:position comp))
        cy0 (second (:position comp))
        cx1 (+ cx0 (first (:size comp)))
        cy1 (+ cy0 (second (:size comp)))
        overlap-frac *overlap-frac-selectionbox*] ; needed to trigger a selection.
    (and (<= x0 (+ (* cx0 overlap-frac) (* cx1 (- 1 overlap-frac))))
      (>= x1 (+ (* cx1 overlap-frac) (* cx0 (- 1 overlap-frac))))
      (<= y0 (+ (* cy0 overlap-frac) (* cy1 (- 1 overlap-frac))))
      (>= y1 (+ (* cy1 overlap-frac) (* cy0 (- 1 overlap-frac)))))))
(defn trapped-box-keys [comps x0 x1 y0 y1]
  (let [xxyy (order x0 x1 y0 y1)
        x0 (first xxyy) x1 (second xxyy) y0 (nth xxyy 2) y1 (nth xxyy 3)]
    (filterv #(_sel-hit? x0 y0 x1 y1 (get comps %)) (keys comps))))

(defn selection-rect-click-test [mevt-c ts zoom]
  "Which part of the selection rectangle we are in."
  (let [x (:X mevt-c) y (:Y mevt-c) sz (/ *handle-pixels* zoom)
        xxyy (apply order (:xxyy ts))
        x0 (first xxyy) x1 (second xxyy) y0 (nth xxyy 2) y1 (nth xxyy 3)]
    (cond
      (collide/hit-rect? x y x0 (+ x0 sz) y0 (+ y0 sz)) :corner-nw
      (collide/hit-rect? x y (- x1 sz) x1 y0 (+ y0 sz)) :corner-ne
      (collide/hit-rect? x y (- x1 sz) x1 (- y1 sz) y1) :corner-se
      (collide/hit-rect? x y x0 (+ x0 sz) (- y1 sz) y1) :corner-sw
      (collide/hit-rect? x y x0 x1 y0 y1) :main-rect
      :else :miss)))

(defn tab-sync [s0 s1]
  "The tabgroup may need synching even if multisync is not called."
  (assoc s1 :components (tabgroup/sync-on-update (:components s0) (:components s1))))

;;;;;;;;;;;;; Moving components around ;;;;;;;;;;;;;;;

(defn _apply-xform [comp xform] "TODO: rename x-box and put into xform"
  (let [not1? #(or (< % 0.99999) (> % 1.00001)) ; don't let rounding errors force re-draws.
        resize? (or (not1? (nth xform 2)) (not1? (nth xform 3)))
        comp1 (if resize? (update comp :size #(vector (max 1e-6 (* (first %) (nth xform 2))) (max 1e-6 (* (second %) (nth xform 3))))) comp)]
    (update comp1 :position #(apply xform/xv xform %))))
(defn apply-xform [comps keys xform] "TODO: rename as apply-xforms"
  (reduce (fn [acc k] (update acc k #(_apply-xform % xform))) comps keys))

(defn dont-move [box cam0 cam1]
  "Keeps the screen-position the same, by changing comp's position."
  (let [u-xxyy (xform/unitscreen-xxyy (xform/visible-xxyy cam0) (xform/box-xxyy box))]
    (lstack/set-unitscreen-xxyy box (xform/visible-xxyy cam1) u-xxyy)))

(defn fit-to-screen [s comp]
  "Moves the comp, and resizes it if necessary, to make it fit in the screen."
  (let [screen-pix (xform/screen-pixels)
        i-cam (xform/x-1 (:camera s))
        corner-nw (xform/xv i-cam 0 0) corner-se (apply xform/xv i-cam screen-pix)
        pos (:position comp) sz (:size comp)
        sz (mapv min sz (mapv - corner-se corner-nw))
        pos (mapv max pos corner-nw)
        pos (mapv min pos (mapv - corner-se sz))]
    (assoc comp :position pos :size sz)))

(defn unique-z [components]
  "Assigns each component a unique z-value."
  (zipmap (keys components) (tabgroup/unique-z-vec (vals components))))

;;;;;;;;;;;;; Rendering ;;;;;;;;;;;;;;;

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

(defn seltool-render [s]
  (let [ts (gts s) zoom (last (:camera s))
        _ (if (< zoom 1e-10) (throw (Exception. "The zoom got set to zero somehow.")))
        sh? (is-sh?) ; shifting disables seeing the selection (until the mouse gets going), as it disables being to drag the selection.
        alt? (is-alt?) ; alt is the same idea and it removes selections.
        mouse? (is-mouse?) ts (gts s)]
    (if (and (or sh? alt?) (or (not mouse?) (:insta-drag? ts)))
      [] (apply draw-box-handle (/ *handle-pixels* zoom) (:corner-mode s) (:xxyy ts)))))

;;;;;;;;;;;;; Specific UI functions ;;;;;;;;;;;;;;;

(defn seltool-mousepress [mevt-c s]
  (let [ts (gts s) x (:X mevt-c) y (:Y mevt-c)
        sh? (:ShiftDown mevt-c) ; shift adds to the selection.
        alt? (:AltDown mevt-c) ; alt subtracts
        comps (:components s) target (selection-rect-click-test mevt-c ts (last (:camera s)))
        target (if (or sh? alt?) :miss target) ; shift/alt means don't drag the selection box.
        khit (collide/under-cursor x y comps)
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

(defn seltool-mouserelease [mevt-c s] ; Snap to whatever is selected.
  (let [ts (gts s)]
     (if (:fresh-press? ts)
       (let [xxyy-loose (:xxyy ts)
             sel-kys (if (= (:corner-mode ts) :miss)
                        ((if (is-alt?) set/difference set/union)
                          (set (:selected-comp-keys s))
                          (set (apply trapped-box-keys (:components s) xxyy-loose)))
                         (set (:selected-comp-keys s)))
             xxyy-tight (collide/bounding-xxyy (mapv #(get (:components s) %) sel-kys))
             ts (assoc ts :fresh-press? false :xxyy xxyy-tight :corner-mode :miss)]
         (assoc (sts s ts) :selected-comp-keys sel-kys))
       s)))

(defn seltool-mousedrag [mevt-c s]
  (#(tab-sync s %)
    (let [ts (gts s) insta-drag? (:insta-drag? ts)
          xxyy (:xxyy ts) mx1 (:X1 mevt-c) my1 (:Y1 mevt-c)
          mx (:X mevt-c) my (:Y mevt-c) target0 (:corner-mode ts)]
      (if (= target0 :miss)
        (cond (and (is-sh?) insta-drag?)
          (let [khit (collide/under-cursor mx my (:components s))]
            (if khit (update s :selected-comp-keys #(conj % khit)) s))
          (and (is-alt?) insta-drag?)
          (let [khit (collide/under-cursor mx my (:components s))]
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
          (update (sts s ts-new) :components #(apply-xform % (:selected-comp-keys s) xform)))))))

(defn seltool-keypress [k-evt s]
  (#(tab-sync s %)
    (let [mx (first (:mouse-pos-world s)) my (second (:mouse-pos-world s))]
      (cond (and (ka/c? k-evt) (= (str (ka/lowercase-letter k-evt)) (str "c")))
        (let [ts (gts s)
              sel (:selected-comp-keys s)]
          (sts s (assoc ts :copied-comps (zipmap sel (map #(get (:components s) %) sel))
                   :copy-mx mx :copy-my my)))
        (and (ka/c? k-evt) (= (str (ka/lowercase-letter k-evt)) (str "v"))) ; paste.
        (let [ts (gts s) dx (- mx (:copy-mx ts))
              dy (- my (:copy-my ts))
              raise-z (inc (- (apply max (mapv :z (vals (:components s))))
                              (apply min (mapv :z (vals (:copied-comps ts))))))
              comp-kys (mapv lteleport/derive-key (keys (:copied-comps ts)))
              comp-vals (mapv (fn [c] (update (update c :position #(vector (+ (first %) dx) (+ (second %) dy)))
                                        :z #(+ % raise-z)))
                          (vals (:copied-comps ts)))]
          (sts (update s :components
              #(merge % (zipmap comp-kys comp-vals)))
            (assoc ts :copied-comps (zipmap comp-kys (vals (:copied-comps ts))))))
        :else s))))

;(defn clear-selection [s] (assoc s :selected-comp-keys #{})) ; doesn't work.
(defn clear-selection [s]
  (let [mp seltool-mousepress mr seltool-mouserelease
        s1 (mr (assoc {} :X 0 :Y 0) s) ; not the best code here...
        s2 (mp (assoc {} :X 1e100 :Y 1e100) s1)
        s3 (mr (assoc {} :X 1e100 :Y 1e100) s2)] s3))

(defn swap-on-top [s]
  "Rotates the :z of components under the cursor. Also clears the selection (that was confusing)."
  (let [s (assoc-in s [:precompute :desync-safe-mod?] true)
        x (first (:mouse-pos-world s)) y (second (:mouse-pos-world s))
        comps (:components s)
        kys (collide/unders-cursor x y comps)

        zs (mapv #(double (if-let [z (:z (get comps %1))] z %2)) kys (range))
        min-z (apply min 1e100 zs)
        ix-min (first (filter #(= (nth zs %) min-z) (range (count zs)))) ; non-unique min is uniquieified.

        zs (mapv #(if (and (= %1 min-z) (not= %2 ix-min)) (+ %1 1e-9) %1) zs (range))
        max-z (apply max -1e100 zs)
        second-min-z (apply min 1e50 (filter #(not= % min-z) zs))
        drop (- second-min-z min-z)
        zs1 (mapv #(if (= % min-z) max-z (- % drop)) zs)
        comps1 (reduce #(assoc-in %1 [(nth kys %2) :z] (nth zs1 %2)) comps (range (count kys)))]
    (assoc (clear-selection s) :components comps1)))

(defn wheel-zoom [m-evt s] ; w and s to zoom in and out by an increment.
                      (let [zoom0 (nth (:camera s) 2)
                            mX (:X m-evt) mY (:Y m-evt)
                            rotation (second (ma/get-scroll-xy m-evt true))
                            step 0.005; (cond (:AltDown m-evt) 0.5 (:ShiftDown m-evt) 0.01 :else 0.05)
                            rzoom (Math/exp (* step rotation))
                            xf [(* mX (- 1 rzoom)) (* mY (- 1 rzoom)) rzoom rzoom]]
                        (update s :camera #(xform/xx % xf))))

(defn insta-zoom-region [target-xxyy comp-xxyys]
  "Heuristic that tries to match target-xxyy, but will move or zoom out slightly
   if it would avoid clipping off stuff.
   Returns the target xxyy."
  (let [dx (- (second target-xxyy) (first target-xxyy))
        dy (- (nth target-xxyy 3) (nth target-xxyy 2))
        r (* 0.5 (+ (- (nth target-xxyy 3) (nth target-xxyy 2)) (- (nth target-xxyy 1) (nth target-xxyy 0))))
        tol *instazoom-tol*
        if< #(if (< %1 %2) %1 0.0)
        pull (fn [sxxyy cxxyy tol-pix] ; returns [dx dx dy dy]
               (let [-x (max 0 (- (nth sxxyy 0) (nth cxxyy 0)))
                     +x (max 0 (- (nth cxxyy 1) (nth sxxyy 1)))
                     -y (max 0 (- (nth sxxyy 2) (nth cxxyy 2)))
                     +y (max 0 (- (nth cxxyy 3) (nth sxxyy 3)))]
                 [(- (if< -x tol-pix)) (+ (if< +x tol-pix))
                  (- (if< -y tol-pix)) (+ (if< +y tol-pix))]))
        max-pull (fn [sxxyy tol-pix]
                   (let [pulls (mapv #(pull sxxyy % tol-pix) comp-xxyys)]
                     (apply collide/xxyy-union pulls)))

        the-pull (max-pull target-xxyy (* tol r))
        enclosing (mapv + target-xxyy the-pull [-1 1 -1 1])
        xxyys-hot (filterv #(collide/engulfs? enclosing %) comp-xxyys)]
    (if (> (count xxyys-hot) 0) (apply collide/xxyy-union xxyys-hot)
      target-xxyy)))

(defn insta-zoom [k-evt s]
  (let [screen-sz (xform/screen-pixels)
        srx (* (first screen-sz) 0.5) sry (* (second screen-sz) 0.5)
        target [(first (:mouse-pos-world s)) (second (:mouse-pos-world s))]
        target-xxyy [(- (target 0) (* 0.5 (first screen-sz)))
                     (+ (target 0) (* 0.5 (first screen-sz)))
                     (- (target 1) (* 0.5 (second screen-sz)))
                     (+ (target 1) (* 0.5 (second screen-sz)))]
        comp-xxyys (mapv xform/box-xxyy (vals (:components s)))
        zoom-xxyy (insta-zoom-region target-xxyy comp-xxyys)
        cam (xform/xxyy2camera zoom-xxyy screen-sz)]
    (assoc s :camera cam)))

;;;;;;;;;;; Major tools with multiple operating modes ;;;;;;;;;;;

(defn get-selection-tool [] ; Inkscape-like selection tool.
  {:render seltool-render
   :mousePressed seltool-mousepress
   :mouseReleased seltool-mouserelease
   :mouseDragged seltool-mousedrag
   :keyPressed seltool-keypress})

(defn get-camera-tool [] ; Simple move camera around, as well as the "sudden zoom in" mode.
  {:mouseWheelMoved wheel-zoom
   :keyPressed (fn [k-evt s]
                 (if (ka/emacs-hit? "C-1" k-evt) ; Zoom where (or very near where) the mouse currently is to zoom 1, best fitting to componens.
                   (insta-zoom k-evt s) s))
   :mouseDragged
    (fn [mevt-c s]
      (let [dx (- (:X mevt-c) (:X1 mevt-c)) dy (- (:Y mevt-c) (:Y1 mevt-c))
            xf [dx dy 1 1]]
        (update s :camera #(xform/xx % xf))))})
