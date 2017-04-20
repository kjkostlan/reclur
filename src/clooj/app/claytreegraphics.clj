; Graphics
(ns clooj.app.claytreegraphics
  (:require [clooj.app.claytreewalk :as clwalk]
    [clooj.app.claytreetext :as cltext]
    [clooj.app.claytreephysics :as claphy]
    [clooj.app.colorful :as colorful]
    [clooj.coder.blitcode :as blitcode]))

(defn render-order-arrows [tree]
  "Gets the graphics commands for rendering arrows that show order (i.e. earlier functions are before later functions).
   Order is only shown for children."
  (apply concat 
    (clwalk/vis-walk-extract tree 
      (fn [nd] ;lines is [x1 y1 x2 y2].
        (if (= (:type nd) :text)
          (let [lines (if (:children-visible? nd) 
                        (mapv #(vector (* 0.5 (+ (:x0 (:physics %1)) (:x1 (:physics %1)))) (* 0.5 (+ (:y0 (:physics %1)) (:y1 (:physics %1))))
                                        (* 0.5 (+ (:x0 (:physics %2)) (:x1 (:physics %2)))) (* 0.5 (+ (:y0 (:physics %2)) (:y1 (:physics %2)))))
                          (:children nd) (rest (:children nd))) [])
                opts {:Color [0.8 0.2 0.3 1]}]
            (apply concat (mapv #(vector [:drawLine % opts] [:drawLine (mapv + % [10 0 0 0]) opts] [:drawLine (mapv + % [0 10 0 0]) opts]) lines))) [])))))

(defn render-inheritence [tree]
  (apply concat
    (clwalk/vis-walk-extract tree
      (fn [nd] 
        (let [par-pts (mapv #(claphy/parent-child-xy0 nd %) (range (count (:children nd))))
              ch-pts (mapv claphy/center (:children nd))]
          (if (:children-visible? nd) 
            (mapv #(vector :drawLine [(first %1) (second %1) (first %2) (second %2)] {:Color [0 1 0 0.4]}) 
               par-pts ch-pts) []))))))

(defn render-scroll [node]
  "Renders scrollbars if need be."
  (let [sc (cltext/scroll-amounts node) ;[top bottom left right]
        t (first sc) b (second sc) l (nth sc 2) r (nth sc 3)
        vr (cltext/view-range node) ;[top bottom left right]
        nv (max 1 (inc (- (second vr) (first vr))))
        nh (max 1 (inc (- (nth vr 3) (nth vr 2))))
        m 1 ; margin so that the line is visible.
        x0 (+ (:x0 (:physics node)) m) x1 (- (:x1 (:physics node)) m)
        y0 (+ (:y0 (:physics node)) m) y1 (- (:y1 (:physics node)) m)
        opts {:Color [1 1 0 1]}
        h-cmd (if (> (+ l r) 0) [(+ x0 (* (- x1 x0) l (/ (+ l nh r)))) y1 (- x1 (* (- x1 x0) r (/ (+ l nh r)))) y1])
        v-cmd (if (> (+ t b) 0) [x0 (+ y0 (* (- y1 y0) t (/ (+ t nv b)))) x0 (- y1 (* (- y1 y0) b (/ (+ t nv b))))])]
    (mapv #(vector :drawLine % opts) (filterv identity [h-cmd v-cmd]))))

(defn render-box [node]
  "Renders a box around each node, not selected.
   Renders an extra box for locked nodes."
  (let [phy (:physics node) ty (:type node)
        c0 [:drawRect [(:x0 phy) (:y0 phy) (- (:x1 phy) (:x0 phy)) (- (:y1 phy) (:y0 phy))] 
             {:Color (cond (= ty :folder) [0.7 0.4 0.2 1] (= ty :file) [0.2 0.5 0.6 1] (= ty :text) [0.6 0.2 0.7 1]
                       (= ty :root) [0.4 0.4 0.4 1] (= ty :namespace) [1.0 0.6 0.3 1] (= ty :repl) [0.8 0.0 0.0 1]
                       (= ty :arbor) [0.8 0.2 0.3 1])}]
        l? (:locked? phy)
        c1 (if l? [:drawRect [(- (:x0 phy) 2) (- (:y0 phy) 2) (+ (- (:x1 phy) (:x0 phy)) 4) (+ (- (:y1 phy) (:y0 phy)) 4)] 
                 {:Color [0.4 0.4 0.4 1]}])
        c2 (if l? [:drawRect [(- (:x0 phy) 4) (- (:y0 phy) 4) (+ (- (:x1 phy) (:x0 phy)) 8) (+ (- (:y1 phy) (:y0 phy)) 8)] 
                 {:Color [0.4 0.4 0.4 1]}])]
    (if l? [c0 c1 c2] [c0])))

(defn render-text-sel [panel]
  "Gfx commands to render the cursor and text selection."
  (if-let [path (first (:selected-paths (:edit-state panel)))]
    (let [node (get-in panel path)
          xy (cltext/cursor-ix-to-world node)
          rects (cltext/get-selected-char-rects node)]
     {:below (mapv #(vector :fillRect % {:Color [0.6 0.6 1.0 1]}) rects) 
      :above (if xy (mapv (fn [sx c] [:drawLine [(+ (first xy) sx) (- (second xy) 8) (+ (first xy) sx) (+ (second xy) 8)] {:Color c}])
                      [-1 0 1] [[0 0 0 1] [1 1 1 1] [0 0 0 1]]) [])}) {:below [] :above []}))

(defn render-edit-state [panel]
  "Returns the :below and :above renderings of the edit-state.
   The tree is rendered between the below and above stuff."
  (let [edit-state (:edit-state panel) edit-mode (:edit-mode panel)
        ; Selected adds a slight modification to the rect:
        mod-rect (fn [rc] ;(println rc)
                   (update (assoc rc 2 {:Color [0.2 0.8 0.7 1]})
                     1 #(vector (- (first %) 1) (- (second %) 1) (+ (nth % 2) 2) (+ (nth % 3) 2))))
        selected-rects {:below [] :above (mapv #(mod-rect (first (render-box (get-in panel %)))) (:selected-paths edit-state))}
        x0 (:X (:last-mouse-down-evt edit-state)) y0 (:Y (:last-mouse-down-evt edit-state))
        xy (if (:mouse-drag edit-state) (:mouse-drag edit-state) [x0 y0])
        hint {\1 "Node move (# keys to change mode)" \2 "Node size (# keys to change mode)" \3 "Typing (esc to leave this mode)" 
              \4 "Tree manipulation (# keys to change mode)" \5 "Camera (# keys to change mode)"}
        state-show [[:drawString [(str "Edit mode: " edit-mode ": " (get hint edit-mode)) 12 12] 
                      {:FontSize 14 :Color [0.4 0.7 0.5 1]}]]
        rect-selection
          (cond (and (not= edit-mode \5) (:mousing? edit-state) (not (:sel-path-last-mouse-down edit-state))) ; for now always show it.
            {:below [] :above [] :hud [[:drawRect [x0 y0 (- (first xy) x0) (- (second xy) y0)] {:Color [0.5 1 0.5 1]}]]}
            :else {:below [] :above [] :hud []})
        text-selection (if (= edit-mode \3) (render-text-sel panel))
        scroll (if-let [x (first (:selected-paths (:edit-state panel)))] (render-scroll (get-in panel x)))]
    {:below (into [] (concat (:below rect-selection) (:below selected-rects) (:below text-selection)))
     :above (into [] (concat (:above rect-selection) (:above selected-rects) (:above text-selection) scroll))
     :hud (into [] (concat (:hud rect-selection) state-show))}))

(defn render-text [node]
  "Renders the characters of a node" ; TODO: some color-coding of characters at various levels.
  (let [gr (cltext/string-grid node false) ; [ix iy] => i.
        s (str (cltext/rendered-string node) " ") ; space at end to not overflow one char.
        chars (mapv #(subs s % (inc %)) (vals gr)) ; TODO: other languages besides clojure.
        ^ints idepth (:inter-depth (blitcode/basic-parse s)) ; paranthesis nesting level.
        char-lev (mapv #(max (aget ^ints idepth %) (aget ^ints idepth (inc %))) (vals gr)) ; Nesting level of characters. 
        cols (mapv #(conj % 1) (mapv colorful/level2rgb char-lev))
        x (cltext/font-size node true) fzy (second x)
        char-left-mids (mapv #(cltext/cursor-screengrid-to-world node (first %) (second %)) (keys gr))         
        char-left-bottoms (mapv #(vector (first %) (+ (second %) (* 0.5 fzy))) char-left-mids)
        shx (* (:font-xshift-to-size cltext/*text-params*) (last x))
        shy (* (:font-yshift-to-size cltext/*text-params*) (last x))]
    (mapv #(vector :drawString [%2 (+ (first %1) shx) (+ (second %1) shy)] {:Color %3 :FontSize (last x)}) char-left-bottoms chars cols)))

(defn render-node [node]
  "Gfx commands to render a node."
  (let [ty (:type node)
        box-cmds (render-box node)
        txt-cmds (render-text node)]
    (into [] (concat box-cmds txt-cmds))))

(defn render-tree [tree]
  (into [] (apply concat (clwalk/vis-walk-extract tree render-node))))

;;;;;;;;;;;;; Camera operations.

(def default-camera {:x -300 :y -300 :zoom 1}) ; default camera settings, since x and y are of the corners, 0 is close to the middle point.

(defn fil [x]
  (let [f #(if % % [])] {:below (f (:below x)) :above (f (:above x)) :hud (f (:hud x))}))

(defn camera-screen-to-global [camera pt]
  "pt is [x y]"
  [(+ (/ (first pt) (:zoom camera)) (:x camera)) (+ (/ (second pt) (:zoom camera)) (:y camera))])

(defn camera-global-to-screen [camera pt]
  "pt is [x y]"
  [(* (- (first pt) (:x camera)) (:zoom camera)) (* (- (second pt) (:y camera)) (:zoom camera))])

(defn s2g [cam vs ks]
  "Screen to global. Input = output = vector. The key can be :x,:y,:w,:h."
  (mapv #(cond (= %2 :x) (+ (/ %1 (:zoom cam)) (:x cam))
           (= %2 :y) (+ (/ %1 (:zoom cam)) (:y cam))
           (or (= %2 :w) (= %2 :h)) (/ %1 (:zoom cam))
           :else (throw (Exception. (str "Unrecognized option:" %2)))) vs ks))

(defn g2s [cam vs ks]
  "Global to screen (used for rendering). Input = output = vector. The key can be :x,:y,:w,:h,:nil"
  (mapv #(cond (= %2 :x) (* (- %1 (:x cam)) (:zoom cam))
           (= %2 :y) (* (- %1 (:y cam)) (:zoom cam))
           (or (= %2 :w) (= %2 :h)) (* %1 (:zoom cam))
           (= %2 :nil) %1
           :else (throw (Exception. (str "Unrecognized option:" %2)))) vs ks))

(defn apply-camera [gfx cam]
  "Transforms all graphics commands so that they are what the camera sees."
  (let [gs #(g2s cam %1 %2) gs2 (fn [gc ks] (update gc 1 #(gs % ks)))
        mappy {:drawLine [:x :y :x :y] ; TODO: eventually have the graphics function do this and a much more complete list.
               :drawRect [:x :y :w :h] :fillRect [:x :y :w :h]
               :drawString [:nil :x :y]}]
    (mapv 
      (fn [gc]
        (let [gc (if-let [x (:FontSize (get gc 2))] (assoc-in gc [2 :FontSize] (* x (:zoom cam))) gc) ; fontsize option.
              ty (first gc)]
          (gs2 gc (if-let [x (get mappy ty)] x (throw (Exception. (str "Oops need the camera-transform rule for this type: " ty))))))) 
      gfx)))
  
(defn _render-whole-scene [panel]
  "The global rendering function."
  (let [eds (fil (render-edit-state panel))]
    (into []
     (concat [[:java (fn [g] (.setFont g (java.awt.Font. "Monospaced" 0 10)))]
              [:fillRect [0 0 3000 3000] {:Color [0.01 0 0 1]}]]
      (apply-camera 
        (into [] (concat [[:drawRect [-5 -5 10 10] {:Color [0.4 0.4 0.4 1]}]] (:below eds) 
                   (render-order-arrows (:tree panel))
                   (render-inheritence (:tree panel))
                   (render-tree (:tree panel))
                   (:above eds))) (:camera panel)) (:hud eds)))))

(defn render-whole-scene [panel]
; Debug.
(_render-whole-scene panel))