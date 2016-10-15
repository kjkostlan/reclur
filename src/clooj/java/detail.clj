; Individual component-by-component "details" that don't fit properly into the behavior.
(ns clooj.java.detail
  (:import [java.awt Point] [javax.swing JPanel]
           [java.awt.event ComponentEvent]
           [javax.swing.event DocumentEvent]
           [javax.swing.tree DefaultMutableTreeNode]
           [javax.swing JTree SwingUtilities JFrame] [jcode JFrameClient]
           [javax.swing.tree TreePath]
           [javax.swing.text DefaultCaret])
  (:require [clooj.java.clojurize :as clojurize] [clooj.java.thread :as thread]
    [clooj.collections :as collections] [clooj.coder.grammer :as grammer]
    [clojure.set :as set]
    [clooj.coder.history :as history]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Optional widget fieldnames to override default behavior ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Brief description:
; :childs-with-listeners = which listeners go to which subsidiary objects.
; :upkeep = builtin changing the state due to events.
; :construct = constructs the java object.
; :extra-update! = other things that must be updated, not captured in clojurize/get-single-level.
; :add-child!, :remove-child! = override changing the children.
; :use-sized-proxy? = do we need to also override getPreferredSize.
; :vital-fields = list of fields that are specified in the default so that the components look resonable or behave consistantly.
; :max-children = maximum # of children allowed.

;; Detailed description:
; :childs-with-listeners
;    Objects like the JScrollBar are subsidiary to the JScrollPane. 
;    subsidiary is different than adding our children because those levels are never exposed to the user.
;    Map from listener-type-as-keyword to function that gets the subsidiary given the main object.
;    Defaults: None (no subsidiaries are assumed to exist).
; :upkeep
;   Events cause the object to change, and the  clojure-datastructure state must stay updated.
;   Upkeep is a map from listener dispatches as keywords, such as :keyPressed to
;   functions that take the state, event (in clojurized form) and object to return the new state.
;   the object itself isn't modified.
;   Each of these functions is added as a listener automatically in addition to any user-defined listeners.
;   Defaults: None (no listeners are added).
; :construct is called when creating the java object for the state before any children are added, etc.
;   The arguments are [state sub-state root-atom path to-string-fn], where path gets from state to sub-state.
;   Defaults: Reflection is used to look for the zero-argument construction function.
;   Note: we always add the needed client properties regardless of the detail.
; :extra-update! allows us to modify the object's state beyond any modifications that are
;   performed by clojurize/set-single-level.
;   The function takes in [obj old-substate new-substate] and modifies obj in place.
;   obj is always the main obj and never the subsidiary. 
;   Default: no extra modifications are done beyond set-single-level.
;   Note: it occurs once at startup after the object is created and before any children have been added, and
;     whenever the state is modified (excluding the upkeep).
; :add-child! takes in [parent-obj child-obj state path-to-parent] and modifies
;   the objects in place, adding the child-obj. There are several different functions depending on 
;   the component, for example .setViewportView for JScrollPane.
;   The parent type determines the :add-child! function, but it can check the type of the child.
;   Default: (.add parent-obj child-obj)
; :remove-child! takes in [parent-obj child-obj] and is the opposite of add-child!
; :use-sized-proxy?
;   This seems to be mostly a broken and unneeded feature. Default false.
; :vital-fields 
;   These fields must be specified in order to show a reasonable object on the screen.
;   If they are not specified defaults are used.
; :max-children:
;   Scrollpanes only have one child, the viewport, and splitpanes at most have 2.
;   Exceeding this throws errors. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Details of how to encode events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-leaf-path-from-jtree-event [e array?]
  (let [;fx #(.getUserObject (last (.getPath %))) x (mapv fx (.getPaths e))
        ;_ (println "Tree event: types:" (mapv type x) " values:" (mapv x))
        tpath-to-out #(.getClientProperty (.getUserObject (last (.getPath %))) "path")]  ; trust me, this works.
    (if array? (mapv tpath-to-out (.getPaths e)) (tpath-to-out (.getPath e)))))

(def event-encode ; Mostly not dependent on the the obj but sometimes needs it.
{java.awt.event.AdjustmentEvent 
     (fn [e obj]
        (let [bar (.getSource e) pane obj] ; obj = (.getParent bar)
          ;(println "scroll adjust:" (let [pt (.getViewPosition (.getViewport pane))] [(int (.getX pt)) (int (.getY pt))]))
          {:mode (.getAdjustmentType e) ; unit's blocks or track.
           :direction (cond
                        (= bar (.getHorizontalScrollBar pane)) :x
                        (= bar (.getVerticalScrollBar pane)) :y
                        :else (throw (Exception. "Scrollbar scroll event (AdjustmentEvent) neither triggered on horizontal nor vertical bar.")))
           ; the view-position of where our keyhole is on the sub-component. This is not the :Location of the component.
           :View (let [pt (.getViewPosition (.getViewport pane))] [(int (.getX pt)) (int (.getY pt))])
           :value (.getValue e)}))
java.beans.PropertyChangeEvent
  (fn [e obj] 
    (let [x (clojurize/get-single-level e true false)]
      (if (instance? javax.swing.JSplitPane obj) (assoc x :DividerLocation (.getDividerLocation obj)) x)))
java.awt.event.ItemEvent 
  (fn [e obj] (assoc (clojurize/get-single-level e true false) :Selected (.isSelected obj)))
javax.swing.event.CaretEvent (fn [e obj] (assoc (clojurize/get-single-level e true false) 
                                              :CaretPosition (.getCaretPosition obj)
                                              :SelectionStart (.getSelectionStart obj)
                                              :SelectionEnd (.getSelectionEnd obj)))
java.awt.event.KeyEvent
  (fn [e obj] {:KeyCode (long (.getKeyCode e)) :AltDown (boolean (.isAltDown e)) 
               :MetaDown (boolean (.isMetaDown e)) :ShiftDown (boolean (.isShiftDown e))})
javax.swing.event.DocumentEvent
  (fn [e obj] (assoc (clojurize/get-single-level e true false) :Text (.getText obj)))
java.awt.event.ComponentEvent
     (fn [e obj]
       (let [id (.getID e)]
           (cond
             (or (= id (ComponentEvent/COMPONENT_RESIZED)) (= id (ComponentEvent/COMPONENT_MOVED))); need to know the new position and size.
             (let [pos (.getLocation obj) sz (.getSize obj)]
               ; the :Size immediatly is put into the :PreferredSize
               {:Location [(int (.getX pos)) (int (.getY pos))] :Size [(int (.getWidth sz)) (int (.getHeight sz))]})
             :else {})))
javax.swing.event.TreeSelectionEvent
    (fn [e obj] 
      (let [leaf-paths (get-leaf-path-from-jtree-event e true)
            root-path (.getClientProperty obj "path")
            paths-added? (mapv #(.isAddedPath e %) (range (count leaf-paths)))
            jtreedescs (mapv #(subvec (into [] %) (count root-path)) leaf-paths)]
        {:jtree-descendents jtreedescs :jtree-added? paths-added?
         :jtree-descendents-selected (reduce #(if (nth paths-added? %2) (conj %1 (nth jtreedescs %2)) %1) [] (range (count paths-added?)))
         :jtree-descendents-unselected (reduce #(if (not (nth paths-added? %2)) (conj %1 (nth jtreedescs %2)) %1) [] (range (count paths-added?)))}))
javax.swing.event.TreeExpansionEvent 
     (fn [e obj] 
       (let [leaf-path (get-leaf-path-from-jtree-event e false)
             root-path (.getClientProperty obj "path")]
         {:jtree-descendent (subvec (into [] leaf-path) (count root-path))
          :treePath (.getPath (.getPath e))}))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; Subsidiary components ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-main-obj [e obj-source]
  "Gets the object that contains the root-atom and handles most of the changes.
   Most objects that come from the event are the main object.
   But there are cases Swing adds more levels than necessary.
   It's easier to simply specify scrollbar properties at the JScrollPane level but the JScrollBar is a child object and events get
     triggered on it. This function will return the JScrollPane instead of the bar for bar events."
  (cond (instance? javax.swing.JScrollBar obj-source) (.getParent obj-source) ; scrollbar -> scrollpane.
        (instance? javax.swing.text.Document obj-source) (.getProperty obj-source "parent") ; Use our custom "parent" property.
        :else obj-source))

(defn get-source [e]
  "99% of the time just .getSource, except for documentEvents and maybe other obscure things we didn't use."
  (if (instance? DocumentEvent e) (.getDocument e) (.getSource e)))        

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Details of each component ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def _JScrollPane
 {:use-sized-proxy? true
  :vital-fields [:View]
  :max-children 1
  :upkeep {:adjustmentValueChanged (fn [s e o] (assoc s :View (:View e)))}
  :add-child! (fn [parent-obj child-obj state path-to-parent]
               ;(println "Type of child object: " (type child-obj))
                (let [view (:View (get-in state path-to-parent)) root-atom (.getClientProperty parent-obj "root")
                      vu-pt (Point. (first view) (second view))
                      vu-port (.getViewport parent-obj)]
                  (history/java-update (.setViewportView parent-obj child-obj) root-atom)
                  (history/java-update (.setViewPosition vu-port vu-pt) root-atom)))
  :remove-child! (fn [parent-obj child-obj]
                   (let [mt-panel (JPanel.) root-atom (.getClientProperty parent-obj "root")] ; empty panel, don't know if nil will throw a NullPointer
                     (history/java-update (.setViewportView parent-obj mt-panel) root-atom)))
  :childs-with-listeners 
   {:AdjustmentListener [#(.getHorizontalScrollBar %) #(.getVerticalScrollBar %)]}   
  :construct (fn [state sub-state root-atom path to-string-fn mk-ob-fn]
               (let [obj (history/java-update (mk-ob-fn) root-atom)]
                 (history/java-update (.putClientProperty (.getHorizontalScrollBar obj) "root" root-atom) root-atom)
                 (history/java-update (.putClientProperty (.getVerticalScrollBar obj) "root" root-atom) root-atom)
                 (history/java-update (.putClientProperty (.getHorizontalScrollBar obj) "path" path) root-atom)
                 (history/java-update (.putClientProperty (.getVerticalScrollBar obj) "path" path) root-atom) obj))
  :extra-update!
    (fn [obj old new] ; We bipass the scrollbar.
      (let [root-atom (.getClientProperty obj "root")
            viewport (history/java-update (.getViewport obj) root-atom)
            view-old (:View old)
            view-new (:View new)]
        (if (and view-new (not= view-old view-new))
          (history/java-update (.setViewPosition viewport (Point. (first view-new) (second view-new))) root-atom))
        (if (and (:Horizontal-policy new) (not (= (:Horizontal-policy old) (:Horizontal-policy new))))
            (history/java-update (.setHorizontalScrollBarPolicy obj (:Horizontal-policy new)) root-atom))
        (if (and (:Vertical-policy new) (not (= (:Vertical-policy old) (:Vertical-policy new))))
            (history/java-update (.setVerticalScrollBarPolicy obj (:Vertical-policy new)) root-atom))
        ; Shuffling the children around tends to reset the scrollbar's position. Here we block that:
        (if (:View new) 
          (let [pt (Point. (first (:View new)) (second (:View new)))]
            (history/java-update (.setViewPosition (.getViewport obj) pt) root-atom)))))})

(def _JSplitPane
  {:max-children 2
   :upkeep {:propertyChange (fn [s e o] (assoc s :DividerLocation (:DividerLocation e)))}
   ; Keep track of which children are open with the "openChildren" client property.
   :add-child! (fn [parent-obj child-obj state path-to-parent]
     (let [root-atom (.getClientProperty parent-obj "root")
           open (history/java-update (.getClientProperty parent-obj "openChildren") root-atom)]
       (if (:left open)
           (do (history/java-update (.putClientProperty parent-obj "openChildren" (disj open :left)) root-atom)
               (history/java-update (.setLeftComponent parent-obj child-obj) root-atom))
           (do (history/java-update (.putClientProperty parent-obj "openChildren" (disj open :right)) root-atom)
               (history/java-update (.setRightComponent parent-obj child-obj) root-atom)))))
   :remove-child! (fn [parent-obj child-obj]
           (let [root-atom (.getClientProperty parent-obj "root")
                 left (.getLeftComponent parent-obj)
                 open (history/java-update (.getClientProperty parent-obj "openChildren") root-atom)]
             (if (= left child-obj)
               (let [pl (JPanel.)] 
                 (history/java-update (.putClientProperty parent-obj "openChildren" (conj open :left)) root-atom)
                 (history/java-update (.setLeftComponent parent-obj pl) root-atom))
               (let [pl (JPanel.)]
                 (history/java-update (.putClientProperty parent-obj "openChildren" (conj open :right)) root-atom)
                 (history/java-update (.setRightComponent parent-obj pl) root-atom)))))
   :construct (fn [state sub-state root-atom path to-string-fn mk-ob-fn]
                (let [obj (history/java-update (mk-ob-fn) root-atom)
                      pl (JPanel.) pl1 (JPanel.)] ; TODO: have an empty split-pane at least show the splitpane.
                  (history/java-update (.setLeftComponent obj pl) root-atom) ; empty
                  (history/java-update (.setRightComponent obj pl1) root-atom)
                  (history/java-update (.putClientProperty obj "openChildren" #{:left :right}) root-atom) obj))})

(def _JCheckBox
 {:vital-fields [:Selected]
  :upkeep {:itemStateChanged (fn [s e o] (assoc s :Selected (:Selected e)))}})

(defn text-obj-stuff [obj sub-state root-atom path]
  (let [doc (.getDocument obj)]
	; The .putProperty sets the parent that is accessed in .getProperty above.
	(history/java-update (.putProperty doc "parent" obj) root-atom)
	; Request focus if we are focused, but only on startup.
	(if (:Focus? sub-state) 
	  (SwingUtilities/invokeLater #(history/java-update (.requestFocus obj) root-atom))) obj))
(def texty-stuff
 {:childs-with-listeners
   {:DocumentListener [#(.getDocument %)]}
  :upkeep {:insertUpdate (fn [s e o] (assoc s :Text (:Text e)))
           :removeUpdate (fn [s e o] (assoc s :Text (:Text e)))
           ; These are not too useful:
           :focusGained (fn [s e o] (assoc s :Focus? true))
           :focusLost (fn [s e o] (assoc s :Focus? false))
           ; Selection goes hand-in-hand with carets:
           ; http://stackoverflow.com/questions/15147016/which-event-a-selection-of-text-trigger-in-java-jtextarea
           :caretUpdate (fn [s e o] (assoc s :CaretPosition (:CaretPosition e)
                                             :SelectionStart (:SelectionStart e)
                                             :SelectionEnd (:SelectionEnd e)))
           }
  :extra-update! 
    (fn [obj old new]
      (if (and (:SelectionStart new) (:SelectionEnd new)
           (or (not= (:SelectionStart old) (:SelectionStart new))
               (not= (:SelectionEnd old) (:SelectionEnd new))))
        (do ;(.requestFocus obj) ; doesn't seem that useful.
          (.select obj (:SelectionStart new) (:SelectionEnd new)))))})
(def _JTextArea (assoc texty-stuff :construct (fn [state sub-state root-atom path to-string-fn mk-ob-fn] 
                                                (text-obj-stuff (history/java-update (mk-ob-fn) root-atom) sub-state root-atom path))))
(def _JTextField (assoc texty-stuff :construct (fn [state sub-state root-atom path to-string-fn mk-ob-fn] 
                                                (text-obj-stuff (history/java-update (mk-ob-fn) root-atom) sub-state root-atom path))))
(def _JTextPane (assoc texty-stuff :construct (fn [state sub-state root-atom path to-string-fn mk-ob-fn] 
                                                (text-obj-stuff (history/java-update (mk-ob-fn) root-atom) sub-state root-atom path))))

(def _JPanel
  {:use-sized-proxy? true})

; :Keep-if-headless? (WARNING: only used on setup: Do we keep the frame alive even when we close it)
; :Prevent-user-close? (WARNING: only used on setup: Do we block the frame from closing when we press the x)
     ; We still can make the frame 
(def _JFrame 
  {:vital-fields [:Location :PreferredSize]
  :extra-update! (fn [obj old-substate new-substate]   
              (if (or (not= (:MinimumSize old-substate) (:MinimumSize new-substate))
                      (not= (:PreferredSize old-substate) (:PreferredSize new-substate))
                      (not= (:MaximumSize old-substate) (:MaximumSize new-substate))
                      (not= (:Size old-substate) (:Size new-substate))) (.pack obj)))
  :upkeep {:componentResized (fn [s e o] (assoc s :PreferredSize (:Size e)))
           :componentMoved (fn [s e o] (assoc s :Location (:Location e)))}
  ; Technically a menu can be added to a frame, but in practice it will always be with setMenuBar
  :add-child! (fn [parent-obj child-obj state path-to-parent]
                (let [root-atom (.getClientProperty parent-obj "root")]
                  (if (instance? javax.swing.JMenuBar child-obj)
                      (history/java-update (.setJMenuBar parent-obj child-obj) root-atom)
                      (history/java-update (.add parent-obj child-obj) root-atom))))
  :remove-child! (fn [parent-obj child-obj]
                (let [root-atom (.getClientProperty parent-obj "root")]
                  (if (instance? javax.swing.JMenuBar child-obj)
                      (history/java-update (.setJMenuBar parent-obj nil) root-atom)
                      (history/java-update (.remove parent-obj child-obj) root-atom))))
  :construct (fn [state sub-state root-atom path to-string-fn mk-ob-fn]
               ; See http://stackoverflow.com/questions/16372241/run-function-on-jframe-close
               (let [obj (history/java-update (JFrameClient.) root-atom)] ; Don't use our proxies here.
				 (.setDefaultCloseOperation obj JFrame/DO_NOTHING_ON_CLOSE)
				 (let [delete!! #(let [ns (find-ns 'clooj.java.updater) ; dynamic require to avoid dependency loop error
									   delete-fcn! (ns-resolve ns 'delete!)]
								   (delete-fcn! root-atom))
					   close!! #(if (not (history/java-update (.getClientProperty obj "closing") root-atom)) 
								  (do (history/java-update (.putClientProperty obj "closing" true) root-atom)
								      (if % (delete!!)) (history/java-update (.dispose obj) root-atom))) ; .dispose last.
					   headless? (:Keep-if-headless? sub-state)
					   nox? (:Prevent-user-close? sub-state)
					   wl (cond (and (not headless?) (not nox?))
								(proxy [java.awt.event.WindowAdapter] [] (windowClosing [event] (close!! true)))
								(or (and (not headless?) nox?)) nil
								(and headless? (not nox?))
								(proxy [java.awt.event.WindowAdapter] [] (windowClosing [event] (close!! false)))
								(and (headless? nox?)) nil)]
				   (if wl (history/java-update (.addWindowListener obj wl) root-atom)) obj)))})

;; The JTree is exceptionally ill-behaved. This is the best I can do so far:
(defn jnode-lineage [tree-node-obj]
  ; Converts a treenode into a an array of ancestors including ourselves.
  ;http://www.java2s.com/Code/Java/Swing-JFC/GettreepathfromTreeNode.htm
  (let [nodes (loop [acc [tree-node-obj] x tree-node-obj]
                (let [xp (.getParent x)]
                  (if (nil? xp) acc (recur (conj acc xp) xp))))]
    (into-array (reverse nodes))))

(defonce jtree-adjustement-queue (atom #{})) ; Set of jtree objects (note: only the root is actually used in the gui).
(defonce jtree-refresh-ix (atom 0)) ; For now this is bieng set but isn't bieng used.

(defn jstate-set!! [jtree]
  "Sets the expansion and selection states IF we have access to the root tree. 
      the value (i.e. text) is handled locally like most properties.
    Returns true if we can access the root tree, otherwise returns false.
   like any function that deals with the java objects, call this on the EDT."
  (let [node (.getClientProperty jtree "JTreeNode")
        lin (jnode-lineage node)
        root-jtree (.getClientProperty (.getUserObject (first lin)) "jtree")]
    (if root-jtree
      (let [tpath (TreePath. ^"[Ljava.lang.Object;" lin) expanded? (.isExpanded root-jtree tpath) ; expanded = this node wants to be expanded.
            selected? (.isPathSelected root-jtree tpath)
            cpath (into [] (concat [:state] (.getClientProperty jtree "path")))
            want-expanded? (get-in @(.getClientProperty jtree "root") (conj cpath :Expanded?))
            want-selected? (get-in @(.getClientProperty jtree "root") (conj cpath :Selected?))]
        (let [cad (.getClientProperty jtree "childrenAdded")]
          (if (> (count cad) 0)
            (do (.putClientProperty jtree "childrenAdded" [])
              (.nodesWereInserted (.getModel root-jtree) node (into-array Integer/TYPE cad)))))
        (let [crm (.getClientProperty jtree "childrenRemoved")
              rmix (into-array Integer/TYPE (mapv :ix crm))
              rmob (into-array Object (mapv :obj crm))]
          (if (> (count crm) 0)
            (do (.putClientProperty jtree "childrenRemoved" [])
              (.nodesWereRemoved (.getModel root-jtree) node rmix rmob))))
        (cond (and expanded? (not want-expanded?)) (try (.collapsePath root-jtree tpath) (catch Exception e (println "JTREE ERROR: " e)))
              (and (not expanded?) want-expanded?) (try  (.expandPath root-jtree tpath) (catch Exception e (println "JTREE ERROR: " e))))
        (cond (and selected? (not want-selected?)) (try (.removeSelectionPath root-jtree tpath) (catch Exception e (println "JTREE ERROR: " e)))
              (and (not selected?) want-selected?) (try (.addSelectionPath root-jtree tpath) (catch Exception e (println "JTREE ERROR: " e))))))))

(defn tree-pulse!! [] ; always runs every 30 ms.
  (swap! jtree-refresh-ix inc)
  (let [x (collections/get-and-reset! jtree-adjustement-queue #{})]
    (if (> (count x) 0) ; if there is stuff to do.
      (SwingUtilities/invokeLater
        (fn [] (let [x1 (reduce (fn [acc xi] (if (jstate-set!! xi) acc (conj acc xi))) #{} x)
                     deleted-stuff (set/difference x x1)]
                 (swap! jtree-adjustement-queue #(set/union % x1))))))))

(defonce ___ (thread/pulse!! #(tree-pulse!!) 30 (str "clooj/lava/detail/" "tree/pulse")))

(defn schedule-adjustment!! [jtree] 
  "Schedules a node adjustement or refresh. The node adjustement will only occur if it is needed and 
   wait for when the tree-root can be accessed."
  (.putClientProperty jtree "refresh-ix" @jtree-refresh-ix)
  (swap! jtree-adjustement-queue #(conj % jtree)))

(defn str-with-path [s path] ;Holds a string but also with a path for us to be able to store it.
  (let [x (proxy [javax.swing.JPanel] [] ; We need to put the client property as "path"
            (toString [] (str (.getClientProperty this "str"))))]
    (.putClientProperty x "str" s)
    (.putClientProperty x "path" path) x))

(def _JTree
{:construct (fn [state sub-state root-atom path to-string-fn mk-ob-fn]
                (let [node-userObj (str-with-path "nody" path) 
                      node (DefaultMutableTreeNode. node-userObj)

                      ; The path includes non-tree stuff at higher levels and tree stuff at lower levels.
                      ; We want to extract the tree part of the path, inclusive of the root.
                      tree? (fn [ix] (= (keyword (:Type (collections/gett-in state (subvec path 0 (* ix 2))))) :JTree))
                      ix-first-tree (first (filterv tree? (range (inc (/ (count path) 2)))))
                      ;_ (if (nil? ix-first-tree) (println "Nill tree pathy:" path "Type: " (:Type sub-state)))
                      green-path (subvec path (* ix-first-tree 2))
                      brown-path (subvec path 0 (* ix-first-tree 2))
      
                      ; We have a JTree for every node. However, only the top JTree is added to scrollpane, etc.
                      ; The other JTrees do no tree-related stuff.
                      out (if (= (count green-path) 0) ; root of tree status.
                             (proxy [JTree] [node] (toString [] (to-string-fn this)))
                             (proxy [JTree] [] (toString [] (to-string-fn this))))]
                  (.putClientProperty out "green-path" green-path)
                  (.putClientProperty out "brown-path" brown-path)
                  (.putClientProperty out "JTreeNode" node)
                  (.putClientProperty out "childrenAdded" [])
                  (if (= (count green-path) 0) (.putClientProperty node-userObj "jtree" out))
                  out))
  ; the parents and children are JTrees, but it's their getTreeNode that is the buisness end.
  :add-child!
  (fn [parent-obj child-obj state path-to-parent]
    (.add (.getClientProperty parent-obj "JTreeNode") (.getClientProperty child-obj "JTreeNode"))  ; add the node to the node.
    ;(.putClientProperty parent-obj "childrenAdded"
    ;  (conj (.getClientProperty parent-obj "childrenAdded") 
    ;    (dec (.getChildCount (.getClientProperty parent-obj "JTreeNode")))))
    (let [lin (jnode-lineage (.getClientProperty parent-obj "JTreeNode"))
          root-jtree (.getClientProperty (.getUserObject (first lin)) "jtree")]
      (if (and root-jtree (= (count (.getClientProperty root-jtree "green-path")) 0))
        (let [par (.getClientProperty parent-obj "JTreeNode")
              chi (.getClientProperty child-obj "JTreeNode")
              ix (.getIndexOfChild (.getModel root-jtree) par chi)]
          ;(println "add: " (.getClientProperty parent-obj "green-path") (.getClientProperty child-obj "green-path")
          ;  "Root path: " (.getClientProperty root-jtree "green-path"))
          (.putClientProperty parent-obj "childrenAdded"
            (conj (.getClientProperty parent-obj "childrenAdded") ix)))))
     (schedule-adjustment!! parent-obj))
  :remove-child!
  (fn [parent-obj child-obj]
    (let [lin (jnode-lineage (.getClientProperty parent-obj "JTreeNode"))
          root-jtree (.getClientProperty (.getUserObject (first lin)) "jtree")]
      (if (and root-jtree (= (count (.getClientProperty root-jtree "green-path")) 0))  ; We only need this fancy updating stuff if we already attached the root tree.
        (let [par (.getClientProperty parent-obj "JTreeNode")
              chi (.getClientProperty child-obj "JTreeNode")
              ix (.getIndexOfChild (.getModel root-jtree) par chi)]
          (.putClientProperty parent-obj "childrenRemoved" 
            (conj (.getClientProperty parent-obj "childrenRemoved") {:ix ix :obj chi}))
          (schedule-adjustment!! parent-obj))))
    (.remove (.getClientProperty parent-obj "JTreeNode") (.getClientProperty child-obj "JTreeNode")))  
  :extra-update!
  (fn [obj old new]
    (let [node-ob (.getClientProperty obj "JTreeNode")]
      (if (not= (:Text old) (:Text new))
        (.putClientProperty (.getUserObject node-ob) "str" (str (:Text new))))
      (if (or (not= (boolean (:Expanded? old)) (boolean (:Expanded? new)))
              (not= (boolean (:Selected? old)) (boolean (:Selected? new)))) (schedule-adjustment!! obj))))
  ; All events come at the root node.
  :upkeep {:valueChanged (fn [s e o]
                           (let [paths (:jtree-descendents e) add? (:jtree-added? e)]
                             (reduce ;#(assoc-in %1 (concat (nth paths %2) [:Selected?]) (nth add? %2))
                               #(let [p (into [] (nth paths %2))]
                                  (if (:Type (get-in s p))
                                    (assoc-in %1 (conj p :Selected?) (nth add? %2)) %1))
                               s (range (count paths)))))
           :treeWillCollapse (fn [s e o] 
                               (collections/updayte-in s (:jtree-descendent e) assoc :Expanded? false))
           :treeWillExpand (fn [s e o] 
                             (collections/updayte-in s (:jtree-descendent e) assoc :Expanded? true))}})

; Trees have a VERY unuasual setup, one JTree at the top and endless nodes.
; :Selected? = is the current branch selected.
; :Expanded? = is the current branch expanded. Meaningless for a leaf node.
; :ExpertOnCollapse? = let the user update the state dynamically when collapsing. 
; :ExpertOnExpand? = let the user update the state dynamically when collapsing.
; We simply treat 'JTree as either a branch node or a leaf node.
; This is similar to menus but menuItems, unlike treenodes, are actually components.
; TODO: graphics simply won't work in the current incarnation.
; TODO: more than just strings.
; TODO: throw errors if the non-root nodes attempt to change propertys that are only once-per-tree, instead of "why is this not working"?
; TODO: throw errors if the non-root nodes have listeners, ALL events are on the root node with :tree-descendent setting where the descendent goes to.
; DANGER: THIS CODE IS A SUPER MESS, and it doesn't perfectly set the state.

(defn obj-to-root-jtree [obj] ; gets the :obj and :state, nil :obj if not built yet.
  (let [root-atom (.getClientProperty obj "root")
        root-state @root-atom
        root-tree-path (.getClientProperty obj "getJTreeRootPath") ; NOT a TreePath.
        jtree (get-in root-state (concat [:java] root-tree-path [:obj]))
        ] {:obj jtree :state (get-in root-state (concat [:state] root-tree-path))
           :leaf-state (get-in root-state (concat [:state] (.getClientProperty obj "path")))}))
(defn block-for-a-while! [jtree time-ms strng] ; immediatly adds the event block, then removes it after some time.
  (.putClientProperty jtree strng true)
  (future (do (Thread/sleep (long time-ms)) (SwingUtilities/invokeLater 
                                       #(.putClientProperty jtree strng false)))))
(defn node-to-treePath [node]
  (let [^"[Ljavax.swing.tree.TreeNode;" tmpPath (.getPath node)] ; type hint so the right constructor is used.
    (TreePath. tmpPath)))
(def sms 150) ; is there a need to block for a longer time?
(defn colexpand [obj old new spin-atom invoke-later?] 
  (let [node-ob (.getClientProperty obj "getJTreeNode")
        old-expanded? (:Expanded? old)   
        mainf (fn [] (let [
                 jtree-st (obj-to-root-jtree obj) 
                 jtree (:obj jtree-st) ;jtreestate (:state jtree-st)
                 tree-path (node-to-treePath node-ob) ; IS a TreePath.
                 ; Make sure we still disagree:
                 new-expanded? (boolean (get (:leaf-state jtree-st) :Expanded?))
                 ;_ (println "expanded? " new-expanded? (mapv str (.getPath tree-path)))
                 fn!! (if new-expanded? #(.expandPath jtree tree-path) #(.collapsePath jtree tree-path))
                 blocked? (if jtree (.getClientProperty jtree "JTree-expand-collapse-blocked?") false)
                 ]
             ; Validity check: we aren't atomistic, so we may have the wrong kind of object:
             (if (and (not= (boolean old-expanded?) new-expanded?) ; do we need to change?
                   (not blocked?))
                 (if (instance? JTree jtree) ; are we even set up yet?
                   ; Wrap in a try-catch just in case something funky made something else invalid.
                   (try (reset! spin-atom false) (block-for-a-while! jtree sms "JTree-events-blocked?") (fn!!)
                     ;(println "new-expanded?" new-expanded? "path: " (.getClientProperty obj "path"))
                     (catch Exception e []))) ;(println "Error with expansion setting: " e)
                 (reset! spin-atom false) ; no longer valid.    
                 )))]  
  (if invoke-later? (fn [] (SwingUtilities/invokeLater mainf)) mainf)))
(defn selecty [obj selected? spin-atom]
  (fn [] (SwingUtilities/invokeLater (fn []
    (let [node-ob (.getClientProperty obj "getJTreeNode")
          treePath (if node-ob (node-to-treePath node-ob))
          jtree (:obj (obj-to-root-jtree obj))]
      (if (and treePath jtree)
        (do (if selected? (.addSelectionPath jtree treePath) (.removeSelectionPath jtree treePath))
          (reset! spin-atom false))))))))
(defn _deepc [subsub]
  (assoc (if (:Children subsub) (assoc subsub :Children (collections/cmap :vals _deepc (:Children subsub))) subsub) :Expanded? false))
(defn deep-collapse [substate tree-desc]
  "Collapsing nodes => all :Children levels need to be collapsed."
  (let [sub-sub (get-in substate tree-desc)]
    (collections/asoc-in substate tree-desc (_deepc sub-sub))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Put widget versions in development below ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def _JPanel-DEV ; just an example of using -DEV.
  {:use-sized-proxy? true :construct
    (fn [state sub-state root-atom path to-string-fn mk-ob-fn] 
      (println "We made a :JPanel in DEV mode. Creating in detail a _JFoo-DEV and specifying this type 
lets us safely develop and test a new JFoo detail object while still using the stable JFoo detail for the rest of the app.
One way we try to allow building new parts of the app while standing on the old ones.") (mk-ob-fn))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; The occasional function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def widgets
  (let [no_ #(.replace ^String (str %) "_" "")
        vars (ns-interns *ns*) kys (filter #(= (first (no_ %)) \J) (keys vars))]
    (zipmap (mapv #(keyword (no_ %)) kys) (mapv #(deref (get vars %)) kys))))

(defn remove-DEV [x]
  "Removes the -DEV ending (works for strings, symbols, and keywords).
   Use :JFoo-DEV instead of JFoo to safely test your own details without messing up code that uses the other details.
   The :JFoo-DEV tells us to use the :JFoo-DEV key from details but to keep :JFoo everywhere else."
  (if (.contains ^String (str x) "-DEV")
    (let [rp #(.replace (str %) "-DEV" "")]
      (cond (string? x) (rp x) (symbol? x) (symbol (rp x))
        (keyword? x) (keyword (subs (rp x) 1))
        :else (throw (Exception. (str "Not recognized type to remove for remove dev: " (type x)))))) x))
