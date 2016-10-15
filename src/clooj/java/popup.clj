; Opens up blocking 
; Must be on the event dispatch thread.
; TODO: Clojurize these and make a gui/popup command that creates the state.
;   It will be a much simpler version of the main one because it is once-through.

(ns clooj.java.popup
  (:import (javax.swing.event TreeSelectionListener)
           (javax.swing AbstractAction JButton JFileChooser
             JOptionPane JSplitPane KeyStroke SpringLayout SwingUtilities)))

(defn yes-no [text] 
  "Opens up a warning box that lets the user click on yes or no. Default no."
  (let [opts (into-array ["no" "yes"])
        n (JOptionPane/showOptionDialog nil text "Warning" 
             (JOptionPane/DEFAULT_OPTION) (JOptionPane/WARNING_MESSAGE) nil opts (nth opts 0))]
    (if (= n 1) true false)))
    
(defn user-input [instruction] ; returns what the user says.
  (JOptionPane/showInputDialog instruction))