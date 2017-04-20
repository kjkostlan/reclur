; The main code that sets up the app.
(ns clooj.app.framernew
  (:import [javax.swing JSplitPane] [java.awt Font Color]
           [javax.swing.text DefaultHighlighter DefaultHighlighter$DefaultHighlightPainter])
  (:require [clooj.java.widget :as widget]
            [clooj.app.hilighter :as hilighter]
            [clooj.app.editbox :as editbox]
            [clooj.java.popup :as jpopup]
            [clooj.java.file :as jfile]
            [clooj.coder.repl :as repl]
            [clooj.collections :as collections]
            [clooj.app.filetree :as filetree]
            [clooj.app.colorful :as colorful]
            [clooj.app.editframe :as editframe]
            [clooj.coder.blitcode :as blitcode]
            [clooj.flow :as flow]))

(repl/add-to-repl!! "REPL output" false)

; TODO: editing text in an empty file and then it may not be saved.
; TODO: a command system.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Repl functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn code-to-repl!! [s]
  (repl/add-to-repl!! (:Text (get-in s (get-in s [:id-paths :input])))) 
  (assoc s :one-frame-repl-update-block? true)) ; repl change.

(defn bind-repl-text-as-s!! [s]
  (repl/bind-as-str!! "s" (:Text (get-in s (get-in s [:id-paths :input])))) s)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;Building the GUI (leaf first);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn output [] ; Here instead of the terminal.
  {:Type 'JScrollPane
   :Children 
   [(flow/flag-leaf {:Type 'JTextArea :Font (Font. "monospaced" Font/PLAIN 12)} :output)]})

(defn input [] ; REPL
  {:Type 'JScrollPane
   :Children 
   [(flow/flag-leaf
      {:Type 'JTextArea :Text "REPL input" :Font (Font. "monospaced" Font/PLAIN 12)
       :insertUpdate (fn [s e o] (hilighter/hilight-update! s e o) s)
       :removeUpdate (fn [s e o] (hilighter/hilight-update! s e o) s)
       :caretUpdate (fn [s e o] (hilighter/hilight-update! s e o) s)} :input)]})

(declare reset-app) ; reset app -> menu -> window -> reset-app circle.
(declare reload-fn) ; Same idea.

; Menus:
(defonce trash (atom "")) ; The last file deleted is stored here to undo a single mistake.
(defn menu [] 
  {:Type 'JMenuBar
   :Children
   [{:Type 'JMenu :Text "file" 
     :Children
     ; :callback only takes in the state.
    [{:Type 'JMenuItem :Text "load" :callback
       (fn [s] (let [choice (jpopup/user-input "type in a filename (i.e. ./src/foo/bar.clj)")]
         (if choice  ; save old, load new.
           (if (jfile/file? choice) (editframe/save-current-file!!! s :editframe :editbox)
             (do (repl/add-to-repl!! (str "Can't find file: " choice) false) s))
           (do (repl/add-to-repl!! "No filename choosen." false) s))))}
     {:Type 'JMenuItem :Text "new" :callback
       (fn [s] (let [choice (jpopup/user-input "type in a filename (i.e. ./src/foo/bar.clj)")]
         (if choice  ; save old, load new.
           (if (not (jfile/file? choice)) (editframe/save-current-file!!! s :editframe :editbox)
             (do (repl/add-to-repl!! (str "Already a file: " choice) false) s))
           (do (repl/add-to-repl!! "No filename choosen." false) s))))}
     {:Type 'JMenuItem :Text "save (C+S)" :callback #(editframe/save-current-file!!! % :editframe :editbox)}
     {:Type 'JMenuItem :Text "delete (non-folders only)" 
      :callback (fn [s] 
                   (if (jpopup/yes-no "Delete current file?")
                       (let [file (editframe/get-filename s :editframe)]
                         (if (= (count file) 0)
                           (do (repl/add-to-repl!! "No file open to delete." false) s)
                           (do (repl/add-to-repl!! (str "Deleted " file " (the most recently deleted file's text is in framer/trash)."))
                             (editframe/delete-current-file!!! s :editframe :editbox :filetree))) s)))}]}
    {:Type 'JMenu :Text "window"
     :Children
     [{:Type 'JMenuItem :Text "reload fcns" :callback reload-fn}
      {:Type 'JMenuItem :Text "reset app" :callback reset-app}]}
    {:Type 'JMenu :Text "repl"
     :Children
     [{:Type 'JMenuItem :Text "eval input (S + enter)" :callback code-to-repl!!}
      {:Type 'JMenuItem :Text "Quote as s (C + enter)" :callback bind-repl-text-as-s!!}
      {:Type 'JMenuItem :Text "clc done tasks" :callback #(do (repl/clear-done-cmds!!) %)}
      {:Type 'JMenuItem :Text "try 2 nicely stop tasks" :callback #(do (repl/clear-all-cmds!!) %)}
      {:Type 'JMenuItem :Text "force abort tasks" :callback #(do (repl/abort-all-cmds!!) %)}
      {:Type 'JMenuItem :Text "reset namespace" :callback #(do (repl/reset-ns!!) %)}]}]})


; The main window with everything:
(defn _window []
 {:Type 'JFrame
  :Title "Key Missing features: find-replace, debugging, refactoring, (un)binding projects."
  :windowClosing (fn [s e o] ; save on close:
                   (editframe/save-current-file!!! s :editframe :editbox) 
                   );(System/exit 0) ; Don't exit when we are in a testing phase.               
  :editframe (flow/flag-leaf (editframe/build) :editframe)
  ; Checking if the files change:
  :frame-ix 0
  :Every-frame (fn [s e o] 
                 (let [s1 (update s :frame-ix inc)
                       ; Very fast b/c it's cached:
                       ; :one-frame-repl-update-block? reduces the <in progress> flicker for very short calculations.
                       s1 (if (:one-frame-repl-update-block? s1) (assoc s1 :one-frame-repl-update-block? false)
                           (assoc-in s1 (concat (:output (:id-paths s1)) [:Text])
                             (repl/get-repl-output)))]
                   ; periodically check for changes:
				   (if (= (mod (:frame-ix s1) 25) 0) (editframe/alert-changes s1 :filetree) s1)))
  ; Mainly menues here:
  :below-actionPerformed
  (fn [s e o]
    (let [from (:origin-state e)] 
      (if (:callback from) ((:callback from) s) s)))
  ; This event runs the repl:
  :below-keyPressed
  (fn [s e o]
    (let [shifting? (boolean (:ShiftDown e)) ; TODO: why do we need a boolean cast? Need to fix the event clojurizer...
          ctrl? (boolean (:MetaDown e))]
      (cond (and (= (:descendent e) (get-in s [:id-paths :input])) ; repl input...
              shifting? (= (:KeyCode e) 10)) (code-to-repl!! s) ; ...and shift + enter = repl 
        ; saving files:
        (and ctrl? (= (:KeyCode e) 83)) (editframe/save-current-file!!! s :editframe :editbox) 
        ; s is now a string corresponding to whatever value the stuff was:
        (and ctrl? (= (:KeyCode e) 10)) (bind-repl-text-as-s!! s)
        :else s))); no hotkey
  :Children
 {:menu (menu)
  :panes
  {:Type 'JSplitPane :Orientation JSplitPane/HORIZONTAL_SPLIT
  :Children
  {:filessource 
     {:Type 'JSplitPane :Orientation JSplitPane/HORIZONTAL_SPLIT
       :Children
       {:files (flow/flag-leaf (filetree/build) :filetree) :source (flow/flag-leaf (editbox/build) :editbox)}}
    :inout
   {:Type 'JSplitPane :Orientation JSplitPane/VERTICAL_SPLIT
     :Children
     {:out (output) :in (input)}}}}}})
(defn kf [x]
  (cond (vector? x) (range (count x))
        (not (map? x)) []
        (and (:Type x) (= (keyword (:Type x)) :JFrame)) (keys x) ; Outer level
        (and (:Type x) (:Children x)) [:Children]
        (:Type x) []
        :else (keys x)))

(defn window [] (let [w (_window)
                      wtag (assoc w :id-paths (flow/deep-find-flags w kf))
                      wlisten (-> wtag (editframe/add-events :editframe :editbox :filetree))] 
                  wlisten))

(defn reset-app [s]
   "Almost Completely resets the application.
    TODO: the almost is some bug with the JTrees maybe?"
  (let [src-text ((:get-src-text s) s)
        w-new (window)]
    ((:set-src-text w-new) w-new src-text)))
  
(defn _reload-fn [s r]
  (let [ca (widget/align-children s r)
        kys (filterv #(first (get ca %)) (keys ca)) ; don't add r children to nil s children.
        s1 (if (:Children s) (assoc s :Children (mapv #(_reload-fn (first (get ca %)) (second (get ca %))) kys)) s)
        fks (filterv #(and (fn? (get s %)) (fn? (get r %))) (keys s))]
    ; the actual replace step:
    (reduce #(assoc %1 %2 (get r %2)) s fks)))
(defn reload-fn [s]
  "Reloads everything that is a function, but keeps the rest of the app.
   Makes it easier because there is no need to store the application.
   (saving this file reloads the namespace and this reload the functions).
   Note: only internal editing will do this (TODO external edits should update)."
   (_reload-fn s (window)))