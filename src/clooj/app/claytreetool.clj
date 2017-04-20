; Tools for what happens when the mouse gets pressed.
(ns clooj.app.claytreetool
  (:require [clooj.app.claytreewalk :as clwalk]
    [clooj.app.claytreegraphics :as clagfx]
    [clooj.app.claytreetree :as clatre]
    [clooj.app.claytreerepl :as clrepl]
    [clooj.app.claytreeevt :as clevt]
    [clooj.app.claytreearbor :as clarbor]
    [clooj.app.claytreetext :as cltext]))

;;;;;;;;;;;;;;;;;;;; Supporting functions ;;;;;;;;;;;;;;;;;;;;

(defn depth-last-mod [panel modify-paths f]
  "Depth-last modifciation so that changed to :children don't get nullified by subsequent modifications higher up."
  (reduce #(assoc-in %1 %2 (f (get-in %1 %2))) panel (sort-by count modify-paths)))

(defn click-n-drag [panel modify-paths pt0-g pt1-g f]
  "Handles clicking and dragging, where (f x0 x1 y0 y1 obj-0 obj) and should only use a couple of fields from obj-0.
   The panel-at-mouse-down was stored when the mous was originally pressed."
  (let [edit-state (:edit-state panel)
        modify-paths (sort-by count modify-paths) ; depth-last so that lower-level modifications don't get crushed by upper level modifications.
        panel0 (:panel-at-mouse-down edit-state)
        x0 (first pt0-g) y0 (second pt0-g)
        x1 (first pt1-g) y1 (second pt1-g)]
  (reduce #(assoc-in %1 %2 (f x0 x1 y0 y1 (get-in panel0 %2) (get-in panel %2))) panel modify-paths)))

(defn single-select [tree x-g y-g]
  "Gets a single selection path in the tree for the mouse bieng at x and y.
   Small objects inside of large objects will take priority, etc.
   Returns nil for nothing."
 (let [treeu (clwalk/vis-tree-unwrap tree)
       sz #(let [p (:physics %)] (* 0.5 (+ (- (:x1 p) (:x0 p)) (- (:y1 p) (:y0 p)))))
       over #(let [p (:physics %)] (min (- (:x1 p) x-g) (- (:y1 p) y-g) (- x-g (:x0 p)) (- y-g (:y0 p))))
       scores (mapv #(let [o (over %) s (sz %)] (* (+ (over %) 3) (/ (+ s 10)))) treeu)]
   (if (not (or (= (count treeu) 0) (<= (apply max scores) 0)))
     (:path (nth treeu (apply max-key #(nth scores %) (range (count scores))))))))

(defn rect-multi-select [tree x0 y0 x1 y1 overlap-fraction-threshold]
  "Selects everything that overlaps the rectangle by the threshold.
   Paths are relative to the tree."
  (let [treeu (clwalk/vis-tree-unwrap tree)
        over #(let [p (:physics %)] (min (- (:x1 p) x0) (- (:y1 p) y0) (- x1 (:x0 p)) (- y1 (:y0 p))))]
    (mapv :path (filterv #(> (over %) 0) treeu))))

(defn size-ratio [x0 x1 y0 y1]
  "A nice click and drag size ratio."
  (Math/exp (* 0.007 (+ (- y0 y1) (- x1 x0)))))

(defn loc2globe [panel pt]
  (clagfx/camera-screen-to-global (:camera panel) pt))

;;;;;;;;;;;;;;;;;;;; The tools themselves ;;;;;;;;;;;;;;;;;;;;
; Tools are agnostic, click, drag, or key. 

(defn select-none [panel & _]
  "Clear any selection."
  (assoc-in panel [:edit-state :selected-paths] []))

(defn click-select-single [panel pt]
  "Click a node so it is selected. Usually part of other functions.
   Will unselect all other selected paths if we select a non-selected node or select nothing.
   If we don't unselect all other nodes we will be first."
  (let [pt (loc2globe panel pt) a [:edit-state :selected-paths]
        paths (into [] (get-in panel a))
        path (into [] (concat [:tree] (single-select (:tree panel) (first pt) (second pt))))]
    (assoc-in panel a
      (cond (not path) [] (not (first (filterv #(= % path) paths))) [path] 
        :else (into [] (concat [path] (apply vector (disj (apply hash-set paths) path)))))))) ; make sure there are no duplicates.

(defn xclick-select-single [panel pt]
  "The exclusive version that ensures there is at most 0 or 1 paths."
  (let [pt (loc2globe panel pt) a [:edit-state :selected-paths]
        path (into [] (concat [:tree] (single-select (:tree panel) (first pt) (second pt))))]
    (assoc-in panel a (if path [path] []))))

(defn click-unlock [panel pt]
  "Unlocks selected nodes and the node that is clicked on.
   Removes all selection."
  (let [paths (:selected-paths (:edit-state panel)) 
        path (if-let [x (apply single-select (:tree panel) (loc2globe panel pt))] (into [] (concat [:tree] x)))
        paths (if path (into [] (concat [path] paths)) paths)]
    (depth-last-mod (assoc-in panel [:edit-state :selected-paths] []) paths
      #(assoc-in % [:physics :locked?] false))))

(defn click-type [panel pt]
  "A click in typing mode."
  (let [panel (xclick-select-single panel pt)
        pt (loc2globe panel pt)
        path (first (:selected-paths (:edit-state panel)))]
    (if path
      (update-in panel path
        (fn [node]
          (let [tbox (:tbox node)]
            (assoc node :tbox (assoc tbox :cursor-ix (apply cltext/cursor-world-to-ix node pt) 
              :selection-start 0 :selection-end -1))))) panel)))

(defn click-toggle-children-visible [panel0 pt]
  (let [panel (click-select-single panel0 pt)
        path (into [] (first (get-in panel [:edit-state :selected-paths])))
        ty (if path (:type (get-in panel path)))]
    (cond (not path) panel ; nothing selected.
      (= ty :namespace) (update-in panel path #(assoc % :children (conj (:children %) (clrepl/new-repl-node %)))) ; add a new node.
      (= ty :repl) (let [ix (last path)]
                     (assoc-in ; remove repl cmd.
                       (update-in panel (butlast path)
                         #(into [] (concat (subvec % 0 ix) (subvec % (inc ix)))))
                       [:edit-state :selected-paths] []))
      :else (update-in panel path clatre/toggle-children))))

(defn click-toggle-pulling-code-piece [panel pt]
  (let [panel (click-type panel pt) ; simulate a click to set the cursor.
        path (first (get-in panel [:edit-state :selected-paths]))]
    (if path 
      (let [node (get-in panel path) x (cltext/cursor-ix-to-piece node)]
          (assoc-in panel path (if (last x) (clatre/push-text node (first x))
            (clatre/pull-text node (first x) (second x)))))
     panel)))

(defn inflate-toggle [panel pt]
  "click-toggle-pulling-code-piece or click-toggle-children-visible
   depending on our type of node."
  (let [panel (click-type panel pt)
        path (first (get-in panel [:edit-state :selected-paths]))
        ty (:type (get-in panel path))]
    (cond (not path) panel
     (= ty :text) (click-toggle-pulling-code-piece panel pt) 
     (= ty :arbor) (clarbor/override-inflate-toggle panel pt)
     :else (click-toggle-children-visible panel pt))))

(defn drag-type [panel pt0 pt1]
  "Click and drag selection when typing."
  (let [path (first (:selected-paths (:edit-state panel)))]
    (if path
      (let [pt0 (loc2globe panel pt0) pt1 (loc2globe panel pt1)
            node (get-in panel path)
            cursor1 (:cursor-ix (:tbox node));(cltext/cursor-world-to-ix node x0 y0) 
            cursor2 (apply cltext/cursor-world-to-ix node pt1)
            node1 (update node :tbox #(assoc % :selection-start cursor1 :selection-end (dec cursor2)))]
        (assoc-in panel path node1)) panel)))

(defn drag-select-multi [panel pt0 pt1]
  "Region selection."
  (let [pt0 (loc2globe panel pt0) pt1 (loc2globe panel pt1)]
    (assoc-in panel [:edit-state :selected-paths]
      (mapv #(into [] (concat [:tree] %))
        (rect-multi-select (:tree panel) (first pt0) (second pt0) (first pt1) (second pt1) 0.25)))))

(defn drag-camera-move [panel pt0 pt1]
  "Drag move the camera."
  (click-n-drag panel [[:camera]] [0 0]
    (mapv #(* % (/ -1.0 (:zoom (:camera panel)))) (mapv - pt1 pt0))
    #(assoc %6 :x (+ (:x %5) (- %2 %1)) :y (+ (:y %5) (- %4 %3)))))

(defn drag-node-move [panel pt0 pt1]
  "Drag move a node."
  (let [pt0 (loc2globe panel pt0) pt1 (loc2globe panel pt1)]
    (click-n-drag panel (:selected-paths (:edit-state panel)) pt0 pt1
      #(assoc %6 :physics
         (let [phy (:physics %5) x0 (:x0 phy) x1 (:x1 phy) y0 (:y0 phy) y1 (:y1 phy)
               x0-new (+ x0 (- %2 %1)) x1-new (+ x1 (- %2 %1))
               y0-new (+ y0 (- %4 %3)) y1-new (+ y1 (- %4 %3))]
         (assoc (:physics %6) :locked? true ;:vx0 0 :vy0 0 :vx1 0 :vy1 0 
           ;:x0 x0-new :y0 y0-new :x1 x1-new :y1 y1-new
           :lock-x (* 0.5 (+ x0-new x1-new)) :lock-y (* 0.5 (+ y0-new y1-new)))))))) 

(defn drag-camera-zoom [panel pt0 pt1]
  "Zoom in or out."
  (click-n-drag panel [[:camera]] pt0 pt1
    #(let [ratio (size-ratio %1 %2 %3 %4)] 
	  (assoc %6 :zoom (* (:zoom %5) ratio)))))

(defn drag-node-size [panel pt0 pt1]
  "Click and drag to resize all selected nodes."
  (click-n-drag panel (:selected-paths (:edit-state panel)) pt0 pt1
    #(let [ratio-h (size-ratio %1 %2 0 0)
           ratio-v (size-ratio 0 0 %3 %4)]
      (assoc %6 :physics
        (assoc (:physics %6) :width0 (* (:width0 (:physics %5)) ratio-h) :height0 (* (:height0 (:physics %5)) ratio-v))))))

(defn drag-node-font-size [panel pt0 pt1]
  "Click and drag to font-size all selected nodes."
  (click-n-drag panel (:selected-paths (:edit-state panel)) pt0 pt1
    #(let [ratio (size-ratio %1 %2 %3 %4)] 
             (assoc-in %6 [:tbox :font-size0] (* (:font-size0 (:tbox %5)) ratio)))))

(defn save-main-arbor-disk!!! [panel]
 "Saves the panel to the disk and returns the modified panel (with updated file-saving)."
  (let [ch (:children (:tree panel))
        ix (first (filter identity (map #(= (:type (nth ch %)) :arbor) (range))))] ; which one is the :arbor.
    (update-in panel [:tree :children ix] clarbor/save-to-disk!!!)))

(defn key-type [panel ky-evt]
  "Key event for typing on the panel."
  (let [path (first (:selected-paths (:edit-state panel)))
        ty (:type (get-in panel path))]
    (cond 
      (= (clevt/ctrl+ ky-evt) \s) (save-main-arbor-disk!!! panel) ; save works from anywhere.
      (not path) panel
      (and (= ty :repl) (clrepl/shift-enter? ky-evt)) (update-in panel path clrepl/eval!)
      (= ty :arbor) (update-in panel path #(clarbor/override-keytype % ky-evt))
      :else (update-in panel path #(cltext/key-press % ky-evt)))))

(defn key-select-all-visnodes [panel ky-evt]
  "Selects all visible nodes if the key event is ctrl + a"
  (if (not= (clevt/ctrl+ ky-evt) \a) panel
    (assoc-in panel [:edit-state :selected-paths] 
      (mapv #(into [] (concat [:tree] (:path %)))
        (clwalk/vis-tree-unwrap (:tree panel))))))

(defn everyframe-scroll [panel frame-e]
  "Scrolling is click and drag but continous over many frames."
  (let [path (first (:selected-paths (:edit-state panel)))]
    (if path 
      (let [node (get-in panel path)
            pt0 (let [e (:last-mouse-down-evt (:edit-state panel))] [(:X e) (:Y e)])
            pt (:mouse-drag (:edit-state panel))
            gain 0.1 dx (* gain (- (first pt) (first pt0))) 
            dy (* gain (- (second pt) (second pt0)))]
        (update-in panel path #(cltext/scroll % dx dy))) panel)))