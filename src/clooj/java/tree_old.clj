; A simplified abstraction interface for working with Jtrees (graphical tree components for file browsing, etc).
; Most functions work with strings here.

(ns clooj.java.tree_old
  (:require [clooj.java.file :as jfile] [clojure.string :as string])
  (:import (javax.swing.event TreeSelectionListener)
           (javax.swing JTree JPanel JLabel JScrollPane SpringLayout)
           (javax.swing.tree TreePath)
           (javax.swing.tree DefaultMutableTreeNode)
           (jcode Treej)))

(defn _make-node [nickname name]
  (proxy [DefaultMutableTreeNode] []
    (getUserObject [] name) ; gets the full-path.
    (toString [] nickname))) ; show as the leaf but store the full-path.

; makes a new tree object with a single node (the root).
(defn _new-tree [nickname name] 
  (let [out (JTree. (_make-node nickname name))]
    (.setExpandsSelectedPaths out true) out))

(defn boxed-new [title root-nickname root-name]
  "Creates a new tree inside a nice scrollpane (all arguments are strings)"
  (let [tree (_new-tree root-nickname root-name)
        panel (JPanel.)
        label (JLabel. title)
        scrollpane (JScrollPane. tree)] 
     (doto panel (.setLayout (SpringLayout.)) (.add label) (.add scrollpane))
       {:tree tree :panel panel :label label :scrollpane scrollpane
         :springs [[:N label -3 :N panel]
                   [:N scrollpane 0 :S label]
                   [:S scrollpane 0 :S panel]
                   [:W label 0 :W panel]
                   [:W scrollpane 0 :W panel]
                   [:E scrollpane 0 :E panel]]
         :root panel}))

; you can call our get methods for the event that the selection listener gives back.
(defn add-tree-select-listener! [atomtree f]
  (.addTreeSelectionListener (:tree @atomtree) (reify TreeSelectionListener (valueChanged [this evt] (f evt)))))

(defn requestFocusInWindow! [atomtree]
  (.requestFocusInWindow (:tree @atomtree)))

(defn _treepath2str [tpath]
  (if (not (nil? tpath))
      (let [filepath (.getPath tpath) sep (jfile/sep)]
        (str sep (apply str (interpose sep (map #(.toString %) filepath))))) nil))

(defn selection-to-string [e]
  "The path.to.selection (each level we use a .toString() representation)"
  (_treepath2str (.getNewLeadSelectionPath e)))

(defn selection-to-string-non-event [atomtree]
  "The path.to.selection (each level we use a .toString() representation). blocklisteners to stop event listening."
  (_treepath2str (.getSelectionPath (:tree @atomtree))))

(defn _str-child [model node str] ; like star child but clunkier b/c of java's inherent clunkiness.
  (let [nch (.getChildCount model node)
        children (map #(.getChild model node %) (range nch))
        out (filter #(= (.toString %) str) children)]
    (if (= (count out) 0) nil (nth out 0))))

(defn string-to-selection! [atomtree pathy blocklisteners]
  "inverse of selection-to-string, except this is not event driven."
  (let [boxedtree @atomtree
        tree (:tree boxedtree)
        pathynodes_ (map string/trim (string/split pathy #"[\\\/]"))
        pathynodes (rest (if (= (count (nth pathynodes_ 0)) 0) (rest pathynodes_) pathynodes_))
        model (.getModel tree)
        root (.getRoot model)
        path_ (loop [grow [root] n 0]
          (if (or (= n (count pathynodes)) (nil? (last grow))) grow
            (recur (conj grow (_str-child model (last grow) (nth pathynodes n))) (inc n))))
        path (if (nil? (last path_)) (rest path_) path_)
        tpath_ (into-array DefaultMutableTreeNode (.getPath (last path)))
        tpath (Treej/fromArray tpath_);(TreePath. tpath_)
        f #(do (.setSelectionPath tree tpath) (.scrollPathToVisible tree tpath))]
    (if blocklisteners 
      (let [sellects (.getTreeSelectionListeners tree)]
        (doall (map #(.removeTreeSelectionListener tree %) sellects))
        (f)
        (doall (map #(.addTreeSelectionListener tree %) sellects))) (f))
      (f)))


(defn _make-mutnode-obj [leaf full]
  (proxy [DefaultMutableTreeNode] []
    (getUserObject [] full)
    (toString [] leaf))) ; show as the leaf but store the full-path.
(defn _populate-jtree [jnode tree]
  (let [ch-nodes (map _make-mutnode-obj (map #(:leaf %) (:children tree)) (map #(:full %) (:children tree)))]
    (dorun (map #(.add jnode (_make-mutnode-obj %1 %2)) (:ch-leaf tree) (:ch-local tree) )) ; add .clj files
    (dorun (map #(.add jnode %) ch-nodes)) ; add children folders to us.
    (dorun (map _populate-jtree ch-nodes (:children tree))))) ; recursivly on children.

(defn load-src-files-in-tree! [atomtree]
  ; TODO: is O(n) performance cost for very large projects an issue?
  (let [boxedtree (:tree @atomtree)
        model (.getModel boxedtree) root (.getRoot model)
        oldsel (selection-to-string-non-event atomtree)]
    (if (nil? root) (println "NULL ROOT FOR TREE!"))
    (if (nil? model) (println "NULL MODEL FOR TREE!"))
    (.removeAllChildren root)
    (_populate-jtree root (jfile/get-texty-tree))
    (.reload model root)
    ;(if (not (nil? oldsel)) (string-to-selection! tree oldsel))
    ))