; Annoyed about having to work with stateful apps in functional programming?
; Better to be annoyed once with almost all our state stored in one place.

(ns clooj.app.state_old
  (:import (javax.swing AbstractListModel BorderFactory JDialog
                        JFrame JLabel JList JMenuBar JOptionPane
                        JPanel JScrollPane JSplitPane JTextArea
                        JTextField JTree KeyStroke SpringLayout
                        JTextPane JCheckBox JButton
                        ListSelectionModel
                        UIManager)
           (javax.swing.event TreeSelectionListener
                              TreeExpansionListener)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel)
           (java.awt Insets Rectangle Window)
           (java.awt.event AWTEventListener FocusAdapter 
                           MouseAdapter WindowAdapter 
                           ActionListener KeyAdapter)
           (java.awt AWTEvent Color Font GridLayout Toolkit)
           (java.net URL)
           (java.util.concurrent LinkedBlockingQueue)
           (java.util Map)
           (java.io File FileReader StringReader
                    BufferedWriter OutputStreamWriter FileOutputStream)
           (org.fife.ui.rsyntaxtextarea RSyntaxTextArea SyntaxConstants
                                        TokenMakerFactory)    
           (org.fife.ui.rtextarea RTextScrollPane))
  (:require [clojure.set]))

;; the many mutable things of app:

(defonce settings (atom nil)) ; saved to disk via Java preference api.

;; Repl command history, debugging, etc.

(defonce repl-history (atom []))
(defonce repl-history-ind (atom 0))
(defonce repl-state (atom {})) ; everything about the repl's state both inside and outside of debug mode.

;; File information:

(defonce current-file (atom nil)) ; The file with text displayed in the editor.
(defonce files-visited (atom [])) ; the history of files+caret positions visited.
(defonce files-visited-ind (atom 0)) ; where we are.
(defonce places-of-files (atom {})) ; the places we are on each file.
(defonce changing-file (atom false))
(defonce settings (atom nil)) ; settings that help store where our stuff goes.

;; GUI pieces:

(defonce frame (atom nil)) ; the WHOLE window.
(defonce split-pane (atom nil)) ; the main way the groups are organized.
(defonce src-split-pane (atom nil))
(defonce repl-split-pane (atom nil))
(defonce embedded (atom false))

(defonce src-tree (atom nil)) ; tree browser of the src files.

(defonce src-text-area (atom nil)) ; you edit files here.
(defonce repl-in-text-area (atom nil)) ; type repl (both in and out of debug mode) here.
(defonce repl-out-text-area (atom nil)) ; repl results go here.
(defonce help-text-area (atom nil)); Don't know what this does.
