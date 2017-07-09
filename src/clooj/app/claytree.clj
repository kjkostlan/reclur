; The code is shown as a tree and the user can manipulate it and expand pieces of it, etc.
; TODO: refactor the text-editing and physics sim code into other files.
; TODO: make clooj.java.file have more complete features because we have to use file objects from time to time.
  ; Components can be passed text-editing commands like "press this key", etc.
(ns clooj.app.claytree
  (:require [clooj.java.clipboard :as clipboard]
   [clooj.app.claytreephysics :as claphy]
   [clooj.app.claytreegraphics :as clagfx]
   [clooj.app.claytreeevt :as clevt]
   [clooj.app.claytreetree :as clatre]
   [clooj.app.claytreerepl :as clrepl]
   [clooj.app.claytreearbor :as clarbor]
   [clooj.app.claytreewalk :as clwalk]
   [clooj.app.claytreetool :as cltool]))

; Node datastructure:
; :physics stores the physical properties above.
; :type = :folder, :file, or :text (of code).
  ; :file nodes have one child: the code.
  ; :text only nests when the user drags something out. At that point, the () levels are used.
; :tbox = stuff related to the textbox.

; :children = vector of child nodes. A folder has sub files/folders as children, etc.
; :children-visible? = are the children are visible or not. All children are either visible or not.

; Editing modes are more persistant than edit-states.
; What is in edit-state:
  ; :selected-paths = the path from the panel to the selected objects (this means the path for tree objects is prepended with :tree)
  ; :panel-at-mouse-down = the panel's state when the mouse was pressed, excluding the edit-state.
  ; :last-mouse-down-evt = the last mouse-down event.
  ; :mouse-drag = the current point [x y] that the mouse is dragged to (including when the mouse is pressed).
; Eventual TODO: use futures to avoid clogging up the main thread.
; These return the modified panel.

(defn initial-state []
  "Initial state of panel. Needs to be a function since there is disk interaction."
  (let [p0 {:edit-state {:selected-paths [] :last-mouse-down-evt {:X 0 :Y 0}}
            :tree (let [l #(assoc %1 :children-visible? true :physics (assoc (:physics %) :lock-x %2 :lock-y %3 :locked? true))
                        s #(assoc-in %1 [:tbox :pieces] [%2])
                        nd #(l (clatre/make-node %1 (str %1)) %2 %3) ; :type x y
                        root (nd :root 0 -70) fileroot-node (l (clarbor/load-from-disk) -100 -50)
                        ns-node (l (clrepl/new-ns-node) 100 -50)
                        repl-node (l (clrepl/new-repl-node ns-node) 100 50)]
                   (assoc root :children [(assoc ns-node :children [repl-node]) fileroot-node]))
            :edit-mode \1
            :camera clagfx/default-camera}]
    (assoc-in p0 [:edit-state :panel-at-mouse-down] (dissoc p0 :edit-state))))

(defmacro tw [sym] ;Wrapper to aboid storing the old tool functions.
  `(fn [& args#] (apply ~sym args#)))

(def tool-packs
  "Map from hotkeys to :leftdown :rightdown :leftdrag :rightdrag :keydown :everyframe.
   The mouse functions are broken down further into e.g. :open-leftdown and :node-leftdown.
   Everyframe is broken down over all combinations: :everyframe-node-leftdown, etc.
   Omitted keys means do nothing."
  (let [selpack {:open-leftdown (tw cltool/select-none)  ; A common set of how to work with nodes.
                 :node-leftdown (tw cltool/click-select-single)
                 :keydown (tw cltool/key-select-all-visnodes)
                 :open-leftdrag (tw cltool/drag-select-multi)}]
  {\1 (assoc selpack :node-leftdrag (tw cltool/drag-node-move) ; Moving and (un)locking nodes.
        :open-rightdown (tw cltool/click-unlock) :node-rightdown (tw cltool/click-unlock))
  \2 (assoc selpack :node-leftdrag (tw cltool/drag-node-size) ; node sizing.
       :node-rightdrag (tw cltool/drag-node-font-size))
  \3 {:node-leftdown (tw cltool/click-type) ; Typing and scrolling.
      :keydown (tw cltool/key-type)
      :node-leftdrag (tw cltool/drag-type)
      :everyframe-node-rightdrag (tw cltool/everyframe-scroll)}
  \4 (assoc selpack ; Basic tree manipulations. TODO: Node creation and destruction.
       :node-leftdown (tw cltool/inflate-toggle))
  \5 {:open-leftdrag (tw cltool/drag-camera-move) :node-leftdrag (tw cltool/drag-camera-move)
      :open-rightdrag (tw cltool/drag-camera-zoom) :node-rightdrag (tw cltool/drag-camera-zoom)}}))

(defn key6 [x sel? button y] "Makes keywords to pull out of tool-packs."
  (keyword (str x (if sel? "node" "open") "-" (subs (str button) 1) y)))

(defn on-mouse-move [mouse-evt panel]
  (let [pt [(:X mouse-evt) (:Y mouse-evt)]
        edit-pack (get tool-packs (:edit-mode panel))
        sel? (boolean (get-in panel [:edit-state :sel-path-last-mouse-down]))]
    (if sel? (if-let [x (get edit-pack :node-mousemove)] (x pt panel) panel)
      (if-let [x (get edit-pack :open-mousemove)] (x pt panel) panel))))

(defn on-mouse-press [mouse-evt panel]
  "Mouse pressed have several variables that need to be stored."
  (let [pt [(:X mouse-evt) (:Y mouse-evt)] 
        panel (assoc-in panel [:edit-state :last-mouse-down-evt] mouse-evt)
        panel (assoc-in panel [:edit-state :mouse-drag] pt)
        panel (assoc-in panel [:edit-state :mousing?] true)
        panel (assoc-in panel [:edit-state :sel-path-last-mouse-down] 
                (if-let [p (apply cltool/single-select (:tree panel) (clagfx/camera-screen-to-global (:camera panel) pt))]
                  (into [] (concat [:tree] p)) nil))
        panel (assoc-in panel [:edit-state :panel-at-mouse-down] (dissoc panel :edit-state))
        edit-pack (get tool-packs (:edit-mode panel))
        button (clevt/mouse-button mouse-evt) sel? (boolean (get-in panel [:edit-state :sel-path-last-mouse-down]))
        ky (key6 "" sel? button "down")]
    (if-let [x (get edit-pack ky)] (x panel pt) panel)))

(defn on-mouse-drag [mouse-evt panel]
  "There is currently LOTS of click-and-drag interactions."
  (let [pt [(:X mouse-evt) (:Y mouse-evt)] 
        evt0 (get-in panel [:edit-state :last-mouse-down-evt])
        panel (assoc-in panel [:edit-state :mouse-drag] pt) ; update the mouse drag.
        edit-pack (get tool-packs (:edit-mode panel))
        button (clevt/mouse-button mouse-evt) sel? (boolean (get-in panel [:edit-state :sel-path-last-mouse-down]))
        ky (key6 "" sel? button "drag")]
    (if-let [x (get edit-pack ky)] 
      (x panel [(:X evt0) (:Y evt0)] pt) panel)))

(defn on-mouse-release [mouse-evt panel] 
  (let [pt [(:X mouse-evt) (:Y mouse-evt)]
        panel (assoc-in panel [:edit-state :mousing?] false)
        edit-pack (get tool-packs (:edit-mode panel))
        button (clevt/mouse-button mouse-evt) sel? (boolean (get-in panel [:edit-state :sel-path-last-mouse-down]))
        ky (key6 "" sel? button "up")]
    (if-let [x (get edit-pack ky)] (x panel pt) panel)))

(defn on-key-press [key-evt panel]
  "Run the function or change the edit mode."
  (let [edit-pack (get tool-packs (:edit-mode panel))
        ck (clevt/ctrl+ key-evt) esc? (clevt/esc? key-evt) 
        tk (let [c (clevt/typed-key key-evt)] (if (string? c) (first c) c))]
    (cond 
      esc? (assoc panel :edit-mode \1) ; Get back to a mode where we don't eat keypresses.
      (and (not= (:edit-mode panel) \3) (not ck) (get tool-packs tk)) ; toggle edit modes unless typing.
      (assoc panel :edit-mode tk)
      (get edit-pack :keydown) ((get edit-pack :keydown) panel key-evt) ; run the key function.
      :else panel)))

(def ^{:dynamic true} *block-frames* false) ; debug emergency stop.
(defn _on-every-frame [clock-evt panel]
  "Run a few sim steps every frame and update the graphics. No other function affects the graphics directly."
(if *block-frames* panel
  (let [panel (update panel :tree (fn [t] (clwalk/vis-walk t #(if (= (:type %) :repl) (clrepl/update-if-done %) %)))) ; Checking the repl futures.
        panel (update panel :tree #(claphy/run-for % 0.03)) ; physics simulation
        edit-mode (:edit-mode panel) edit-pack (get tool-packs (:edit-mode panel))
        sel? (boolean (get-in panel [:edit-state :sel-path-last-mouse-down]))
        button (if (not (:mousing? (:edit-state panel))) :none
                 (if-let [x (get-in panel [:edit-state :last-mouse-down-evt :Button])] (clevt/mouse-button {:Button x}) :none))
        kys (mapv #(key6 "everyframe-" sel? button %) ["down" "drag" "up"])
        panel (cond (get edit-pack (nth kys 0)) ((get edit-pack (nth kys 0)) panel clock-evt)
                (get edit-pack (nth kys 1)) ((get edit-pack (nth kys 1)) panel clock-evt)
                (get edit-pack (nth kys 2)) ((get edit-pack (nth kys 2)) panel clock-evt)
                  :else panel)]
    (assoc panel :Graphics (clagfx/render-whole-scene panel))))) ; graphics doesn't seem to cause a memory leak.

(defn on-every-frame [clock-evt panel]
  "Allows us to change the every-frame function in case we end up making massive amounts of crap."
  (_on-every-frame clock-evt panel))

(defn listenerify [f]
  "Turns one of the above on functions into a listener function."
  #(vector [:object :panel (f %1 %2)]))

(defn starting-commands [] ; commands to set up the editor.
  [[:object :window {:Type :JFrame :Size [700 700] :DefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE}] ; evantually it will exit when closed after finializaitons.
  [:object :panel (assoc (initial-state) :Type :JPanel)] ; the (initial-state) is an involved call that reads from the disk.
  [:relation :window :panel]
  [:listener :Clock :everyFrame :xyz [(listenerify _on-every-frame) :panel]]
  [:listener :panel :mouseMoved :xyz [(listenerify on-mouse-move) :panel]]
  [:listener :panel :mouseDragged :xyz [(listenerify on-mouse-drag) :panel]]
  [:listener :panel :mousePressed :xyz [(listenerify on-mouse-press) :panel]]
  [:listener :panel :mouseReleased :xyz [(listenerify on-mouse-release) :panel]]
  [:listener :window :keyPressed :xyz [(listenerify on-key-press) :panel]] ; the window seems to eat the key presses.
  ])
