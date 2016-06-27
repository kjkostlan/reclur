; A simplified abstraction interface for working with popup windows (warnings, user inputs, etc).
; This includes java's builtin selection of files, etc.

(ns clooj.java.popup
  (:require [clooj.java.prefs :as jprefs])
  (:import (javax.swing.event TreeSelectionListener)
           (javax.swing AbstractAction JButton JFileChooser
             JOptionPane JSplitPane KeyStroke SpringLayout SwingUtilities)
           (java.awt FileDialog)
           (java.io FilenameFilter File)))

; Files are always reperesnted as strings dot-local from the root.
; i.e. "./src/clooj/core.clj"
(defn choose-file [parent title suffix load]
  (let [dialog
    (doto (FileDialog. parent title
            (if load FileDialog/LOAD FileDialog/SAVE))
      (.setFilenameFilter
        (reify FilenameFilter
          (accept [this _ name] (. name endsWith suffix))))
      (.setVisible true))
    d (.getDirectory dialog)
    n (.getFile dialog)]
    (if (and d n)
      (println (str "choosen file: " (File. d n)))
      (println (/ 1 0)))))

(defn choose-directory [parent title]
  (let [fc (JFileChooser.)
        last-open-dir (jprefs/read-value-from-prefs "last-open-dir")]
    (doto fc (.setFileSelectionMode JFileChooser/DIRECTORIES_ONLY)
      (.setDialogTitle title)
      (.setCurrentDirectory (if last-open-dir (File. last-open-dir) nil)))
    (if (= JFileChooser/APPROVE_OPTION (.showOptionDialog fc parent))
      (println (str "choosen directory: " (.getSelectedFile fc)))
      (println (/ 1 0)))))

(defn yes-no [text] 
  "Opens up a warning box that lets the user click on yes or no. Default no."
  (let [opts (into-array ["no" "yes"])
        n (JOptionPane/showOptionDialog nil text "Warning" 
             (JOptionPane/DEFAULT_OPTION) (JOptionPane/WARNING_MESSAGE) nil opts (nth opts 0))]
    (if (= n 1) true false)))