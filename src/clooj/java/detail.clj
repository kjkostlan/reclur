; Individual component-by-component "details" that don't fit properly into the behavior.
(ns clooj.java.detail
  (:import [java.awt Point] [javax.swing JPanel]
           [java.awt.event ComponentEvent]
           [javax.swing.event DocumentEvent]
           [javax.swing.tree DefaultMutableTreeNode]
           [javax.swing JTree SwingUtilities JFrame]
           [javax.swing.tree TreePath]
           [javax.swing.text DefaultCaret])
  (:require [clooj.java.clojurize :as clojurize] [clooj.java.thread :as thread]
    [clooj.collections :as collections] [clooj.coder.grammer :as grammer]))

; These fieldnames can all be omitted (we have defaults).
; :get-main = how to get the main object from a child object.
; :childs-with-listeners = which listeners, rather then bieng bound to the main object, go to the child objects.
;     to still add the listener to the parent include the identity function.
; :event-encode = how to represent the event. This uasually means adding information about the obj as it was changed.
;      addressed by the event type (as well as addressing everything by the type-keword).
; :upkeep = map of listener -> functions that keep the state up-to-date when user actions change it.
;   Uasually :upkeep uasually ferrys a property to the state which is encoded by :event-encode
; :construct = when constructing this object.
; :extra-make! = other things that must be done when making the component
; :extra-update! = other things that must be updated, not captured in clojurize/get-single-level.
; :add-child!, :remove-child! = override changing the children.
;       This is for the PARENT. To override it for the child we would need to change every one of widgets.
; :daycare! = functions that help maintain the state of the object.
; :use-sized-proxy? = do we need to also override getPreferredSize.
; :use-user-class = have a user-class rather than a proxy override it.
;     The argument should be a symbol ending in a dot.
; :vital-fields = list of fields that are specified in the default so that the components look resonable or behave consistantly.
; :max-children = maximum # of children allowed.
; :blocked? = sometimes we need to block events.

(def scroll-pane
; TODO: list of listener fn -> object getter fn.
 {:get-main #(.getParent %) ; scrollbar -> scrollpane.
  :use-sized-proxy? true
  :vital-fields [:View]
  :max-children 1
  :upkeep {:adjustmentValueChanged (fn [s e o] (assoc s :View (:View e)))}
  ; Shuffling the children around tends to reset the scrollbar's position. Here we block that:
  :daycare! (fn [obj old-substate new-substate]
              (if (:View new-substate) 
               (clojurize/on-java-change (.setViewPosition (.getViewport obj) 
                   (Point. (first (:View new-substate)) (second (:View new-substate)))) obj)))
  :add-child! (fn [parent-obj child-obj state path-to-parent]
                (let [view (:View (get-in state path-to-parent))]
                  (clojurize/on-java-change (.setViewportView parent-obj child-obj) parent-obj)
                  (clojurize/on-java-change (.setViewPosition (.getViewport parent-obj) (Point. (first view) (second view))) parent-obj)))
  :remove-child! (fn [parent-obj child-obj]
                   (clojurize/on-java-change (.setViewportView parent-obj (JPanel.)) parent-obj)) ; empty panel, don't know if nil will throw a NullPointer
  :childs-with-listeners 
   {:AdjustmentListener [#(.getHorizontalScrollBar %) #(.getVerticalScrollBar %)]}
  :event-encode
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
           :value (.getValue e)}))}
  :extra-make! (fn [obj sub-state root-atom path]
                 (clojurize/on-java-change (.putClientProperty (.getHorizontalScrollBar obj) "root" root-atom) root-atom)
                 (clojurize/on-java-change (.putClientProperty (.getVerticalScrollBar obj) "root" root-atom) root-atom)
                 (clojurize/on-java-change (.putClientProperty (.getHorizontalScrollBar obj) "path" path) root-atom)
                 (clojurize/on-java-change (.putClientProperty (.getVerticalScrollBar obj) "path" path) root-atom))
  :extra-update!
    (fn [obj old new] ; We bipass the scrollbar.
      (let [viewport (.getViewport obj)
            view-old (:View old)
            view-new (:View new)
            root-atom (.getClientProperty obj "root")]
        (if (and view-new (not= view-old view-new))
          (clojurize/on-java-change (.setViewPosition viewport (Point. (first view-new) (second view-new))) root-atom))
        (if (and (:Horizontal-policy new) (not (= (:Horizontal-policy old) (:Horizontal-policy new))))
            (clojurize/on-java-change (.setHorizontalScrollBarPolicy obj (:Horizontal-policy new)) root-atom))
        (if (and (:Vertical-policy new) (not (= (:Vertical-policy old) (:Vertical-policy new))))
            (clojurize/on-java-change (.setVerticalScrollBarPolicy obj (:Vertical-policy new)) root-atom))))})

(def split-pane
  {:max-children 2
   :event-encode {java.beans.PropertyChangeEvent
                   (fn [e obj]
                     (assoc (clojurize/get-single-level e true false) :DividerLocation (.getDividerLocation obj)))}
   :upkeep {:propertyChange (fn [s e o] (assoc s :DividerLocation (:DividerLocation e)))}
   :daycare! (fn [obj old-substate new-substate]
               (if (:DividerLocation new-substate)
                 (clojurize/on-java-change (.setDividerLocation obj 
                     (:DividerLocation new-substate)) obj)))
   ; Keep track of which children are open with the "openChildren" client property.
   :add-child! (fn [parent-obj child-obj state path-to-parent]
     (let [open (.getClientProperty parent-obj "openChildren")]
       (if (:left open)
           (do (.putClientProperty parent-obj "openChildren" (disj open :left))
               (.setLeftComponent parent-obj child-obj))
           (do (.putClientProperty parent-obj "openChildren" (disj open :right))
               (.setRightComponent parent-obj child-obj)))))
   :remove-child! (fn [parent-obj child-obj]
           (let [left (.getLeftComponent parent-obj)
                 open (.getClientProperty parent-obj "openChildren")]
             (if (= left child-obj)
               (do (.putClientProperty parent-obj "openChildren" (conj open :left))
                   (.setLeftComponent parent-obj (JPanel.)))
               (do (.putClientProperty parent-obj "openChildren" (conj open :right))
                   (.setRightComponent parent-obj (JPanel.))))))
   :extra-make! (fn [obj sub-state root-atom path]
     ; TODO: have an empty split-pane at least show the splitpane.
     (.setLeftComponent obj (JPanel.)) ; empty
     (.setRightComponent obj (JPanel.))
     (.putClientProperty obj "openChildren" #{:left :right}))
    })

(def check-box
 {:vital-fields [:Selected]
  :upkeep {:itemStateChanged (fn [s e o] (assoc s :Selected (:Selected e)))}
  :event-encode
    {java.awt.event.ItemEvent (fn [e obj] (assoc (clojurize/get-single-level e true false) :Selected (.isSelected obj)))}
  })

(def texty-stuff
 {:get-main #(.getProperty % "parent") ; document -> the text area.
  :extra-make! (fn [obj sub-state root-atom path]
                 (let [doc (.getDocument obj)]
                   ; The .putProperty sets the parent that is accessed in .getProperty above.
                   (clojurize/on-java-change (.putProperty doc "parent" obj) root-atom)
                   ; Request focus if we are focused, but only on startup.
                   (if (:Focus? sub-state) 
                     (SwingUtilities/invokeLater #(clojurize/on-java-change (.requestFocus obj) obj)))))
  :childs-with-listeners
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
  ; Is this nessessary?
  :daycare! (fn [obj old-substate new-substate]
               (if (and (:SelectionStart new-substate) (:SelectionEnd new-substate))
                 (do (.select obj (:SelectionStart new-substate) (:SelectionEnd new-substate)))))
  :event-encode
   {javax.swing.event.CaretEvent (fn [e obj] (assoc (clojurize/get-single-level e true false) 
                                              :CaretPosition (.getCaretPosition obj)
                                              :SelectionStart (.getSelectionStart obj)
                                              :SelectionEnd (.getSelectionEnd obj)))
    javax.swing.event.DocumentEvent
     (fn [e obj] (assoc (clojurize/get-single-level e true false) :Text (.getText obj)))}})

(def panel
  {:use-sized-proxy? true})

; :Keep-if-headless? (WARNING: only used on setup: Do we keep the frame alive even when we close it)
; :Prevent-user-close? (WARNING: only used on setup: Do we block the frame from closing when we press the x)
     ; We still can make the frame 
(def frame 
  {:use-user-class 'JFrameClient.
  :vital-fields [:Location :PreferredSize]
  :daycare! (fn [obj old-substate new-substate]           
              (if (or (not= (:MinimumSize old-substate) (:MinimumSize new-substate))
                      (not= (:PreferredSize old-substate) (:PreferredSize new-substate))
                      (not= (:MaximumSize old-substate) (:MaximumSize new-substate))
                      (not= (:Size old-substate) (:Size new-substate))) (.pack obj)))
  :upkeep {:componentResized (fn [s e o] (assoc s :PreferredSize (:Size e)))
           :componentMoved (fn [s e o] (assoc s :Location (:Location e)))}
  ; Technically a menu can be added to a frame, but in practice it will always be with setMenuBar
  :add-child! (fn [parent-obj child-obj state path-to-parent]
                (if (instance? javax.swing.JMenuBar child-obj)
                    (.setJMenuBar parent-obj child-obj)
                    (.add parent-obj child-obj)))
  :remove-child! (fn [parent-obj child-obj]
                (if (instance? javax.swing.JMenuBar child-obj)
                    (.setJMenuBar parent-obj nil)
                    (.remove parent-obj child-obj)))
  :extra-make! (fn [obj sub-state root-atom path]
                 ; See http://stackoverflow.com/questions/16372241/run-function-on-jframe-close
                 (.setDefaultCloseOperation obj JFrame/DO_NOTHING_ON_CLOSE)
                 (let [delete!! #(let [ns (find-ns 'clooj.java.updater) ; dynamic require to avoid dependency loop error
                                       delete-fcn! (ns-resolve ns 'delete!)]
                                   (delete-fcn! root-atom))
                       close!! #(if (not (.getClientProperty obj "closing")) 
                                  (do (.putClientProperty obj "closing" true) (if % (delete!!)) (.dispose obj))) ; .dispose last.
                       headless? (:Keep-if-headless? sub-state)
                       nox? (:Prevent-user-close? sub-state)
                       wl (cond (and (not headless?) (not nox?))
                                (proxy [java.awt.event.WindowAdapter] [] (windowClosing [event] (close!! true)))
                                (or (and (not headless?) nox?)) nil
                                (and headless? (not nox?))
                                (proxy [java.awt.event.WindowAdapter] [] (windowClosing [event] (close!! false)))
                                (and (headless? nox?)) nil)]
                   (if wl (.addWindowListener obj wl))))
  :event-encode
   {java.awt.event.ComponentEvent
     (fn [e obj]
       (let [id (.getID e)]
           (cond
             (or (= id (ComponentEvent/COMPONENT_RESIZED)) (= id (ComponentEvent/COMPONENT_MOVED))); need to know the new position and size.
             (let [pos (.getLocation obj) sz (.getSize obj)]
               ; the :Size immediatly is put into the :PreferredSize
               {:Location [(int (.getX pos)) (int (.getY pos))] :Size [(int (.getWidth sz)) (int (.getHeight sz))]})
             :else {})))}})


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
(defn str-with-path [s path] ;Holds a string but also with a path for us to be able to store it.
  (let [x (proxy [javax.swing.JPanel] [] ; We need to put the client property as "path"
            (toString [] s))]
    (.putClientProperty x "path" path) x))
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
  (assoc (if (:Children subsub) (assoc subsub :Children (grammer/cmap :vals _deepc (:Children subsub))) subsub) :Expanded? false))
(defn deep-collapse [substate tree-desc]
  "Collapsing nodes => all :Children levels need to be collapsed."
  (let [sub-sub (get-in substate tree-desc)]
    (collections/asoc-in substate tree-desc (_deepc sub-sub))))
(defn get-leaf-path-from-event [e array?]
  (let [tpath-to-out #(.getClientProperty (.getUserObject (last (.getPath %))) "path")]  ; trust me, this works.
    (if array? (mapv tpath-to-out (.getPaths e)) (tpath-to-out (.getPath e)))))
(def tree
  {:construct (fn [state root-atom path to-string-fn] 
                (let [sub-state (get-in state path)
                      node (DefaultMutableTreeNode. (str-with-path "nody" path))
                      ; We have a JTree for every node. However, only the top JTree is added to scrollpane, etc.
                      ; The other JTrees do no tree-related stuff.
                      ;out (JTree. node)
                      out (proxy [JTree] [node] (toString [] (to-string-fn this)))
                      stubs (mapv #(subvec path 0 %) (range 0 (inc (count path)) 2))
                      tree? (fn [ix] (= (keyword (:Type (get-in state (nth stubs ix)))) :JTree))
                      
                      last-not-tree (let [x (last (filterv #(not (tree? %)) (range (count stubs))))] (if x x -1))
                      root-jtree-path (get stubs (inc last-not-tree))] ; the JTree that actually does stuff.
                  (.putClientProperty out "getJTreeRootPath" root-jtree-path)
                  (.putClientProperty out "getJTreeNode" node) ; no, there is no getTreeNode() in JTree.
                  out))
  :blocked? (fn [obj e]
             ; TODO: we currently block all events in one fell swoop when we modify things. Is this best?
             (let [jtree (:obj (obj-to-root-jtree obj))
                   out (if jtree (boolean (.getClientProperty jtree "JTree-events-blocked?")) false)]
                out))
  ; the parents and children are JTrees, but it's theier getTreeNode that is the buisness end.
  :add-child!
  (fn [parent-obj child-obj state path-to-parent]
    (if (instance? JTree child-obj) ; add the node to the node.
        (.add (.getClientProperty parent-obj "getJTreeNode") (.getClientProperty child-obj "getJTreeNode"))
        (.add (.getClientProperty parent-obj "getJTreeNode") child-obj))) ; does this make much sense? 
  :remove-child!
  (fn [parent-obj child-obj]
    (if (instance? JTree child-obj) ; remove the node from the node.
        (.remove (.getClientProperty parent-obj "getJTreeNode") (.getClientProperty child-obj "getJTreeNode"))
        (.remove (.getClientProperty parent-obj "getJTreeNode") child-obj))) ; does this make much sense?   
  ; Changing the structure has to go on daycare!
  :daycare!
  (fn [obj old new]
    (let [colexpand-spin-atom (atom true) colexpand-spin-fcn!! (colexpand obj old new colexpand-spin-atom true)
          root-tree-path (.getClientProperty obj "getJTreeRootPath")
          node-ob (.getClientProperty obj "getJTreeNode")
          select-spin-atom (atom true)]
      ;((colexpand obj old new spin-atom false))
      (if (not= (:Selected? old) (:Selected? new))
        (thread/pulse!! (selecty obj (:Selected? new) select-spin-atom) #(* (inc %) 30) (str "tree selection is a modest pain:" root-tree-path)))
      (thread/pulse!! colexpand-spin-fcn!! #(* (inc %) 30) (str "tree expansion is a big pain:" root-tree-path) colexpand-spin-atom)
      )
    )
  ; We basically are replacing all of the builtin updater here:
  :extra-update! 
  (fn [obj old new]
  
    (let [node-ob (.getClientProperty obj "getJTreeNode")
          root-tree-path (.getClientProperty obj "getJTreeRootPath")
          spin-atom (atom true)
          spin-fcn!! (colexpand obj old new spin-atom true)
          ]
      ;(thread/pulse!! spin-fcn!! #(* (inc %) 30) (str "detail/trees are a big pain:" root-tree-path) spin-atom)
      (if (not= (:Text old) (:Text new))
        (.setUserObject node-ob (str-with-path (:Text new) (.getClientProperty obj "path"))))))
  :event-encode
   {javax.swing.event.TreeSelectionEvent
    (fn [e obj] 
      (let [leaf-paths (get-leaf-path-from-event e true)
            root-path (.getClientProperty obj "path")
            paths-added? (mapv #(.isAddedPath e %) (range (count leaf-paths)))
            jtreedescs (mapv #(subvec (into [] %) (count root-path)) leaf-paths)]
        ;(println "tree selection:" leaf-paths path-added?) 
        {:jtree-descendents jtreedescs :jtree-added? paths-added?
         :jtree-descendents-selected (reduce #(if (nth paths-added? %2) (conj %1 (nth jtreedescs %2)) %1) [] (range (count paths-added?)))
         :jtree-descendents-unselected (reduce #(if (not (nth paths-added? %2)) (conj %1 (nth jtreedescs %2)) %1) [] (range (count paths-added?)))}))
    javax.swing.event.TreeExpansionEvent 
     (fn [e obj] 
       (let [leaf-path (get-leaf-path-from-event e false)
             root-path (.getClientProperty obj "path")]
         {:jtree-descendent (subvec (into [] leaf-path) (count root-path))
          :treePath (.getPath (.getPath e))}))}
  ; All events come at the root node.
  :upkeep {:valueChanged (fn [s e o]
                           (let [paths (:jtree-descendents e) add? (:jtree-added? e)]
                             (reduce #(assoc-in %1 (concat (nth paths %2) [:Selected?]) (nth add? %2))
                               s (range (count paths)))))
           :treeWillCollapse (fn [s e o] ;(println "collapse: " (:jtree-descendent e))
                               (if (not (:ExpertOnCollapse? s))
                                 (block-for-a-while! (:obj (obj-to-root-jtree o)) sms "JTree-expand-collapse-blocked?"))
                               (deep-collapse s (:jtree-descendent e)))
           :treeWillExpand (fn [s e o] ;(println "expand: " (:jtree-descendent e)) 
                             (if (not (:ExpertOnExpand? s))
                               (block-for-a-while! (:obj (obj-to-root-jtree o)) sms "JTree-expand-collapse-blocked?"))
                             (collections/updayte-in s (:jtree-descendent e) assoc :Expanded? true))}
  }
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def widgets {:JScrollPane scroll-pane :JTextArea texty-stuff :JTextField texty-stuff :JTextPane texty-stuff
              :JFrame frame :JPanel panel :JCheckBox check-box :JSplitPane split-pane :JTree tree})

; TODO: this event handling code is messy because it includes details.
(defn get-main-obj [e obj-source]
  "Gets the main object. Some events are bound to accessorys like scrolbar. This gets the main object."
  (cond (instance? javax.swing.JScrollBar obj-source) ((:get-main (:JScrollPane widgets)) obj-source)
        (instance? javax.swing.text.Document obj-source) ((:get-main (:JTextArea widgets)) obj-source) ; JTextField et al also would work.
        :else obj-source))  

(defn get-source [e]
  "99% of the time just .getSource"
  (if (instance? DocumentEvent e)
    (.getDocument e)
    (.getSource e)))