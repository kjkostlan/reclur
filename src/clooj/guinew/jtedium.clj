; The JTrees are unique in that they are way harder to manage than other stuff.
(ns clooj.guinew.jtedium)

javax.swing.event.TreeSelectionEvent
      (let [leaf-paths (get-leaf-path-from-jtree-event e true)
            root-path (.getClientProperty obj "path")
            paths-added? (mapv #(.isAddedPath e %) (range (count leaf-paths)))
            jtreedescs (mapv #(subvec (into [] %) (count root-path)) leaf-paths)]
        {:jtree-descendents jtreedescs :jtree-added? paths-added?
         :jtree-descendents-selected (reduce #(if (nth paths-added? %2) (conj %1 (nth jtreedescs %2)) %1) [] (range (count paths-added?)))
         :jtree-descendents-unselected (reduce #(if (not (nth paths-added? %2)) (conj %1 (nth jtreedescs %2)) %1) [] (range (count paths-added?)))})

javax.swing.event.TreeExpansionEvent 
     (fn [e obj] 
       (let [leaf-path (get-leaf-path-from-jtree-event e false)
             root-path (.getClientProperty obj "path")]
         {:jtree-descendent (subvec (into [] leaf-path) (count root-path))
          :treePath (.getPath (.getPath e))}))

(defonce jtree-adjustement-queue (atom #{})) ; Set of jtree objects (note: only the root is actually used in the gui).
(defonce jtree-refresh-ix (atom 0)) ; For now this is bieng set but isn't bieng used.

(defn tree-pulse!! [] ; runs every frame.
  (swap! jtree-refresh-ix inc)
  (let [x (collections/get-and-reset! jtree-adjustement-queue #{})]
    (if (> (count x) 0) ; if there is stuff to do.
      (SwingUtilities/invokeLater
        (fn [] (let [x1 (reduce (fn [acc xi] (if (jstate-set!! xi) acc (conj acc xi))) #{} x)
                     deleted-stuff (set/difference x x1)]
                 (swap! jtree-adjustement-queue #(set/union % x1))))))))
                 
(defonce ___ (thread/pulse!! #(tree-pulse!!) 30 (str "clooj.java.detail/" "tree-pulse")))

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

(construct [state sub-state root-atom path to-string-fn mk-ob-fn]
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

(add-child! [parent-obj child-obj state path-to-parent]
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

(remove-child! [parent-obj child-obj]
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

(extra-update! [obj old new]
    (let [node-ob (.getClientProperty obj "JTreeNode")]
      (if (not= (:Text old) (:Text new))
        (.putClientProperty (.getUserObject node-ob) "str" (str (:Text new))))
      (if (or (not= (boolean (:Expanded? old)) (boolean (:Expanded? new)))
              (not= (boolean (:Selected? old)) (boolean (:Selected? new)))) (schedule-adjustment!! obj))))

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
                             (collections/updayte-in s (:jtree-descendent e) assoc :Expanded? true))}
                             

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