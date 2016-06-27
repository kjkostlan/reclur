; The overall shape of the window.

(ns clooj.java.window_old
  (:require [clooj.java.prefs :as jprefs] [clooj.utils :as utils])
  (:import (javax.swing.event TreeSelectionListener)
           (javax.swing AbstractAction JButton JFileChooser JMenu JMenuBar JMenuItem BorderFactory
             JOptionPane JSplitPane KeyStroke SpringLayout SwingUtilities)))

;; tree seq on widgets (awt or swing)

(defn widget-seq [^java.awt.Component comp]
  (tree-seq #(instance? java.awt.Container %)
            #(seq (.getComponents %))
            comp))

(defn persist-window-shape!!! [name ^java.awt.Window window]
  (let [components (widget-seq window)
        shape-persister (agent nil)]
    (jprefs/restore-shape! name components)
    (jprefs/watch-shape! components
                 #(send-off shape-persister
                            (fn [old-shape]
                              (let [shape (jprefs/get-shape components)]
                                (when (not= old-shape shape)
                                  (jprefs/write-value-to-prefs!!! name shape))
                                shape))))))
;; Setting up the layout.

(defn snug-fit! [component]
  "Makes the component fit snugly compared to it's parent, so stuff expands."
;(utils/constrain-to-parent! component :n 0 :w 0 :n 15 :w 15 :e 0 :e 15 :s 0 :s 15)
(utils/constrain-to-parent! component :n 5 :w 5 :s -5 :e -5))

(defn _single-spring! [boxed sp root] ; applies a single spring.
  (let [obA (nth sp 1) obB (nth sp 4) dist (nth sp 2)
        layout (.getLayout root)
        lookup {:N SpringLayout/NORTH :S SpringLayout/SOUTH :E SpringLayout/EAST :W SpringLayout/WEST}]
     (.putConstraint layout ((nth sp 0) lookup) obA dist ((nth sp 3) lookup) obB)))

(defn apply-springs! [boxed]
"Applies any spring (constraints) for a given boxed component. The format for each constraint:
   example [:N label dist :E panel]."
  (doall (map #(_single-spring! boxed % (:root boxed)) (:springs boxed))))
