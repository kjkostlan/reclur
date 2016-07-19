; The main code that sets up the app.
(ns clooj.app.framer
  (:import [javax.swing JSplitPane] [java.awt Font Color]
           [javax.swing.text DefaultHighlighter DefaultHighlighter$DefaultHighlightPainter])
  (:require [clooj.java.gui :as gui]
            [clooj.java.widget :as widget]
            [clooj.java.popup :as jpopup]
            [clooj.java.file :as jfile]
            [clooj.coder.repl :as repl]
            [clooj.utils :as utils]
            [clooj.collections :as collections]
            [clooj.coder.grammer :as grammer]))

; (require '[clooj.app.framer :as framer] '[clooj.java.gui :as gui])

(repl/add-to-repl!! "REPL output" false)

;;;;;;;;;;;;;;;;;;;;;;;;;;; Keeping track of paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _id-paths [acc sub-state path]
  (reduce #(_id-paths %1 (get-in sub-state [:Children %2]) (concat path [:Children %2]))
    (if (:id sub-state) (assoc acc (:id sub-state) (into [] path)) acc) 
    (grammer/ckeys (:Children sub-state))))
(defn id-paths [state]
  ; Creates a map from :id keys to paths.
  ; id keys allow us to not worry about the particular path.
  ; This captures an oo advantage: a reference is stored w/o keeping track of where it is.
  ; Duplicate :id's will shadow.
  (_id-paths {} state []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-filename-from-tree [jtree value-changed-e]
  "Gets the filename from a value-changed event."
  (let [p (first (:jtree-descendents-selected value-changed-e))
        filepath (get-in jtree (concat p [:filepath]))] filepath))

(defn hilite-same-level! [text caret obj]
 "Simply gets the graphics to draw. Our graphics for now are spartan:
  outlining the region at the current selection level."
  ; TODO: optimize both the parse code (reflection?) and look at differences in the text.
  ; This IS noticably slow as is, but still usable most of the time.
  ; TODO: we can centralize the side effects (bury them in the changes in state) if we change the details of the text. 
  (if (> (count text) 0)
	(if (and text caret obj)
          (let [parse (grammer/basic-parse text)
			^ints levels (:level parse)
			^ints tokens (:token parse)
            ^ints modes (:mode parse)
			^ints breaks (:break parse)
            ^ints boosts (:boost parse)
            ^ints escapes (:escape parse)
            ^chars cs (.toCharArray text)
			n (int (count levels)) ; all three arrays are 1:1.
                        
			; Wedges mean that the cursor is actually between stuff but there is no space:
			; Since we are between tokens we should behave like we are.
			crt? (and (> caret 0) (< caret n))
			wc? (if crt? (grammer/cclose? (aget ^chars cs (dec caret))))
			wo? (if crt? (and (grammer/copen? (aget ^chars cs caret)) (= (aget ^ints escapes (dec caret)) 0)))
			wedge-paren? (and wc? wo?); ()|()
			wedge-pmacro? (and wc? (grammer/creader? (aget ^chars cs caret))  ; ()|'foo
                                        (or (= caret (dec n)) (not= (aget ^chars cs caret) \#) ; Avoid #{ and #" macros.
                                          (and (not= (aget ^chars cs (inc caret)) \{) (not= (aget ^chars cs (inc caret)) \"))))
			wedge-macrop? (or (and wo? (> (aget ^ints boosts (dec caret)) (aget ^ints boosts caret))) ; 'foo|() '(foo)|()
                                          (and wedge-pmacro? (= (aget ^ints boosts (dec caret)) (aget ^ints boosts caret)))) ; 'foo|'() and '(foo)|'()
                                        

			wedge? (or wedge-paren? wedge-pmacro? wedge-macrop?)
			dual-m-wedge? (and wedge-pmacro? wedge-macrop?)
;_ (if (= (first text) \X) (println "caret: " caret "wedge?" wedge-paren? wedge-pmacro? wedge-macrop?))
			; Hilighter ends: the first char that end the hilighting at the same location as we do.
			lev+ (fn [ix] (grammer/level-boost-end cs modes levels boosts ix (and wedge? (not dual-m-wedge?)) dual-m-wedge?))
			lev- (fn [ix] (grammer/level-boost-beginning cs modes levels boosts ix (and wedge? (not dual-m-wedge?)) dual-m-wedge?))
			tok+ (fn [ix] (grammer/token-end tokens breaks ix))
			tok- (fn [ix] (grammer/token-beginning tokens breaks ix))

			; level-based hilite. inclusive ix for chars to be hilighted.
			soft0 (min (lev- caret) (lev- (dec caret)))
			soft0 (if (and (> soft0 0) (= (aget ^chars cs soft0) \{) (= (aget ^chars cs (dec soft0)) \#))
			         (dec soft0) soft0) ; include the # for hash-sets. 
			soft1 (max (lev+ caret) (lev+ (dec caret)))
;_ (if (< (count text) 100) (println "lev:" (into [] levels) "boost:" (into [] boosts) "mode:" (into [] modes) "break: " (into [] breaks)  "soft:" soft0 soft1 "caret:" caret))
            in-token? (and (> caret 0) (< caret n) (not= (aget ^ints breaks caret) 1)
                        (= (aget ^ints tokens (dec caret)) 1) (= (aget ^ints tokens caret) 1))
			hard0 (if in-token? (tok- (dec caret)))
			hard1 (if in-token? (tok+ caret))
			hltr (.getHighlighter obj)
			hpainter-soft (DefaultHighlighter$DefaultHighlightPainter. (Color. (float 1.0) (float 0.9) (float 0.85)))
			hpainter-hard (DefaultHighlighter$DefaultHighlightPainter. Color/orange)]
	  (.removeAllHighlights hltr)
	  (.setDrawsLayeredHighlights hltr false)
	  (.addHighlight hltr soft0 (inc soft1) hpainter-soft)
	  (if in-token? (.addHighlight hltr hard0 (inc hard1) hpainter-hard))))))
          
(defn hilight-update! [s e o] 
  "Selection hilight superscedes the syntax-level hilight" 
  (let [s0 (:SelectionStart s) s1 (:SelectionEnd s)]
	(if (or (not s0) (not s1) (= s0 s1)) ; selection => don't hilight the level.
	  (try (hilite-same-level! (:Text s) (:CaretPosition s) o)
	    (catch Exception e (println "Syntax hilight error: " e)))
	  (let [hltr (.getHighlighter o)
			hpainter (DefaultHighlighter$DefaultHighlightPainter. (Color. (float 0.75) (float 0.75) (float 1.0)))]
		(.removeAllHighlights hltr) (.addHighlight hltr s0 s1 hpainter)))) s)


(defn update-tree-keep-settings [tree-old tree-new]
  "Updates tree-old's structure to reflect tree-new, but tries to keep the settings of tree-new."
  (let [cha (widget/align-children tree-old tree-new)
        ; the new children:
        ch1 (reduce #(let [c0 (get-in cha [%2 0]) c1 (get-in cha [%2 1])]
                        (cond (or (= c0 c1) (and c1 (not c0))) (assoc %1 %2 c1)
                              (and c0 c1) (assoc %1 %2 (update-tree-keep-settings c0 c1))
                              (not c1) %1 :else (throw (Exception. "This shouldn't happen..."))))
              {} (keys cha))
        this-aligned (if (= (:filepath tree-old) (:filepath tree-new)) tree-old tree-new)]
   (if (:Children tree-new) (assoc this-aligned :Children ch1) this-aligned)))

(defn ensure-file-open [jtree file]
  "Ensures that a given file is open and selected. Do this every time the tree changes."
  (if file
    (let [this-level (if (= file (:filepath jtree))
                       ; Java's trees are extremly hard to get behaving properly, for now this println is a proxy to 'it would have worked':
                       (do ;(println "expanded and selected: " file)
                          (assoc jtree :Expanded? true :Selected? true))
                       (assoc jtree :Selected? false))]
      (if (:Children this-level) 
         (assoc this-level :Children (grammer/cmap :vals #(ensure-file-open % file) (:Children this-level)))
          this-level))) ; no children to recursively operate on.
    jtree) ; nil file -> no change.

(defn get-diff [t-old t-new]
  "List of differences between the two trees. It is string form and stops at the first change."
  (let [fo (:filepath t-old) fn (:filepath t-new)
        this-change (not= fo fn)]
    ;(if (not= fo fn) (println "fo fn: " fo fn))
    (if this-change 
      [(cond (and fo fn) (str "rename: " fo "->" fn) ; our current tree should not capture renames though.
           (not fo) (str "add: " fn)
           (not fn) (str "remove: " fo)
           :else "bizzaraxtioc")]
     (into [] (apply concat (mapv #(apply get-diff %) (vals (widget/align-children t-old t-new))))))))

(defn get-wantsedit-file [s]
  "Gets the current filename from the (ns) macro. Only works for clojure files."
  (let [txt ((:get-src-text s) s)
        namesp (second (try (into [] (read-string txt)) (catch Exception e nil)))]
    (if namesp (jfile/namespace2file (str namesp)))))

(defn save-file!!! [s]
  "Saves a the text of the editor into (:cur-file s), creates :cur-file if it doesn't exist on the disk.
   Does nothing if there is no :cur-file yet."
  (if (:cur-file s)
    (let [cur-file (:cur-file s)
          is-file? (jfile/is-file cur-file)
          ; clj files must match between namespaces and java.
          clj? (jfile/clj? cur-file)
          ns-conflict (if clj? (not= (get-wantsedit-file s) cur-file) false)
          text ((:get-src-text s) s)
          _ (jfile/save-textfile!!! cur-file text)
          s (if is-file? s (update-in s (:file-tree (:id-paths s)) #((:update %) % cur-file)))
          t0 (if is-file? "Saved: " "Created: ")
          msg (if ns-conflict (str t0 cur-file " ERROR: Namespace declared in file is missing, invalid, or doesn't match the filename.")
                (try (if clj? (do (repl/reload-file!! cur-file) (str "Saved: " cur-file " (no error)."))
                       (str "Saved: " cur-file " [not a .clj file, no compilation done]."))
                         (catch Exception e (str "Saved: " cur-file " Compile error: " e  " (Error prevented downstream (re)definitions)."))))]
      (repl/add-to-repl!! msg false) s) s))

(defn loadfile [s]
  "Uses the :cur-file to load. Does not check for validity."
  (update-in ((:set-src-text s) s (jfile/load-textfile (:cur-file s)))
     (:file-tree (:id-paths s)) #(ensure-file-open % (:cur-file s))))

(defn alert-changes [s]
   "Updates s if the files changed and updates the GUI."   
  (let [tpath (:file-tree (:id-paths s))
		s-new (update-in s tpath #((:update %) % (:cur-file s))) ; update the JTree.
		changes (get-diff (get-in s tpath) (get-in s-new tpath))]
	; Every fyi text alert must use the repl:
	(if (> (count changes) 0)
	   (repl/add-to-repl!! (apply str "External files changed (rename = remove+add), these namespaces are NOT reloaded: " 
						     (mapv #(str % "\n") changes)) false)) ; false = don't evaluate.
	s-new))

(defn code-to-repl!! [s]
  (do (repl/add-to-repl!! (:Text (get-in s (get-in s [:id-paths :input])))) 
    (assoc s :one-frame-repl-update-block? true))) ; repl change.

(defn line-nos [s] 
  "Graphics commands that will draw the line numbers."
  (let [g0 (.getGraphics (java.awt.image.BufferedImage. 2 2 java.awt.image.BufferedImage/TYPE_INT_RGB))
        t (:Text s)
		metrics (.getFontMetrics g0 (:Font s))
		height (.getHeight metrics)
		width (.charWidth metrics \space)
		^chars text (chars (.toCharArray (str (:Text s) "\n")))
		n (int (count text))
		c-p-l (loop [ix (int 0) lines [] numthis (int 0)] ; chars per line.
				(cond (= ix n) lines
				  (= (aget ^chars text ix) \newline) (recur (inc ix) (conj lines numthis) 0)
				  :else (recur (inc ix) lines (if (= (aget ^chars text ix) \tab) (+ numthis 8) (inc numthis)))))
		g (mapv #(vector :drawString [(str (inc %)) (int (+ 50 (* (nth c-p-l %) width)))
									   (int (* height (+ % 0.8)))]
				   {:Color (Color. (float 0.4) (float 0.6) (float 0.99))}) (range (count c-p-l)))] g))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;Building the GUI (leaf first);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-tree []
  "Gets the files in JTree form."
  (let [ftree (jfile/filetree "." (fn [_] true))
        convert (fn fc [branch] 
                  (let [x {:Type 'JTree :Text (:leaf branch) :filepath (:local branch)}]
                    (let [folders (if (> (count (:children branch)) 0)
                                    (mapv fc (:children branch)) [])
                          files-and-folders (mapv #(hash-map :Type 'JTree :Text %2 :filepath %1)
                                  (:ch-local branch) (:ch-leaf branch))
                          files (filterv #(jfile/is-file (:filepath %)) files-and-folders)
                          
                          filders-v (into [] (concat folders files))
                          filders-k (into [] (concat (mapv #(str "F" (:Text %)) folders) (mapv #(str "f" (:Text %)) files)))
                          filders (zipmap filders-k filders-v)]
                      (if (> (count filders) 0)
                          (assoc x :Children filders) x))))]
   (assoc (convert ftree) :get-file-selected get-filename-from-tree
      :id :file-tree :update (fn [t-old ensure-this-file-is-open]
                               ; Update for new files, but keep selected status if applicable (apply to the 'JTree level):
                               (ensure-file-open (update-tree-keep-settings t-old (get-tree)) 
                                 ensure-this-file-is-open)))))
(defn file-tree []
   (let [ftree (get-tree)]
     {:Type 'JScrollPane
      :Children 
      [ftree]}))

; Edit code:
(defn editor []
  {:Type 'JScrollPane
   :Children 
   [{:Type 'JTextArea :Text "Editor" :Font (Font. "monospaced" Font/PLAIN 12)
	 :insertUpdate (fn [s e o] (hilight-update! s e o) (assoc s :Graphics (line-nos s)))
	 :removeUpdate (fn [s e o] (hilight-update! s e o) (assoc s :Graphics (line-nos s)))
	 :caretUpdate (fn [s e o] (hilight-update! s e o) (assoc s :Graphics (line-nos s)))
	 :id :editor}]})

(defn output [] ; Here instead of the terminal.
  {:Type 'JScrollPane
   :Children 
   [{:Type 'JTextArea :Font (Font. "monospaced" Font/PLAIN 12)
     :id :output}]})

(defn input [] ; REPL
  {:Type 'JScrollPane
   :Children 
   [{:Type 'JTextArea :Text "REPL input" :Font (Font. "monospaced" Font/PLAIN 12)
     :insertUpdate (fn [s e o] (hilight-update! s e o) s)
     :removeUpdate (fn [s e o] (hilight-update! s e o) s)
     :caretUpdate (fn [s e o] (hilight-update! s e o) s)
     :id :input}]})

(declare reset-app) ; reset app -> menu -> window -> reset-app circle.
(declare reload-fn)

; Menus:
(defn menu [] 
  {:Type 'JMenuBar
   :Children
   [{:Type 'JMenu :Text "file" 
     :Children
     ; :callback only takes in the state.
    [{:Type 'JMenuItem :Text "load" :callback
       (fn [s] (let [choice (utils/user-input "type in a filename (i.e. ./src/foo/bar.clj)")
                     tpath (concat (:output (:id-paths s)) [:Text])]
         (if choice 
           (if (jfile/is-file choice) (loadfile (assoc (save-file!!! s) :Title choice :cur-file choice))
             (assoc-in s tpath (str "Can't find file: " choice)))
           (assoc-in s tpath "No filename choosen."))))}
     {:Type 'JMenuItem :Text "new" :callback
       (fn [s] (let [choice (utils/user-input "type in a filename (i.e. ./src/foo/bar.clj)")
                     tpath (concat (:output (:id-paths s)) [:Text])]
         (if choice 
           (if (jfile/is-file choice) (assoc-in s tpath (str "Already exists: " choice))
               (save-file!!! (assoc s :cur-file choice)))
           (assoc-in s tpath "No filename choosen."))))}
     {:Type 'JMenuItem :Text "save" :callback 
       #(if (:cur-file %) (save-file!!! %)
          (do (repl/add-to-repl!! "No file is open. Use file -> new to save any text in the editor." false) %))}
     {:Type 'JMenuItem :Text "delete (non-folders only)" 
      :callback (fn [s] 
                   (if (jpopup/yes-no "Delete current file?")
                     (let [cf (:cur-file s)]
                       ; IMPORTANT: keep the :cur-file for undoable deletion.
                       (if cf (jfile/delete-file!!! cf))
                       (repl/add-to-repl!! (if cf (str "(UNDOABLE) Deleted: " cf " to UNDO: use file->save before opening another file.") 
                                             (str "No file is open to delete.")) false)
                       (update-in s (:file-tree (:id-paths s)) #((:update %) % cf))) s))}]}
    {:Type 'JMenu :Text "window"
     :Children
     [{:Type 'JMenuItem :Text "reload fcns" :callback reload-fn}
      {:Type 'JMenuItem :Text "reset app" :callback reset-app}]}
    {:Type 'JMenu :Text "repl"
     :Children
     [{:Type 'JMenuItem :Text "eval input (sh + enter)" :callback code-to-repl!!}
      {:Type 'JMenuItem :Text "clc done tasks" :callback #(do (repl/clear-done-cmds!!) %)}
      {:Type 'JMenuItem :Text "try to abort all tasks" :callback #(do (repl/clear-all-cmds!!) %)}
      {:Type 'JMenuItem :Text "reset namespace" :callback #(do (repl/reset-ns!!) %)}]}]})

; The main window with everything:
(defn _window []
 {:Type 'JFrame
  :Title "Key Missing features: find-replace, debugging, refactoring, (un)binding projects."
  ; Gets the editor text:
  :get-src-text #(:Text (get-in % (:editor (:id-paths %))))
  :set-src-text (fn [s txt] (let [tp (:editor (:id-paths s))] 
                   (update-in (assoc-in s (concat tp [:Text]) txt) tp #(assoc % :Graphics (line-nos %)))))
  :cur-file nil
  :windowClosing (fn [s e o] 
                   ; save on close:
                   (if (and (:cur-file s) (jfile/is-file (:cur-file s))) (save-file!!! s))
                   (System/exit 0))
  ; This event does a lot: saving and loading files as well as updating the textarea:
  :below-valueChanged
  (fn [s e o] 
    (let [tree (get-in s (:file-tree (:id-paths s)))
          text ((:get-src-text s) s)
          ;Check if we update the file by saving and loading:
          filename (str (get-filename-from-tree tree e))
          is-leaf-file? (and filename (jfile/is-file filename))]
      (if (and is-leaf-file?)
        (loadfile (assoc (if (:cur-file s) (save-file!!! s) s) :Title filename :cur-file filename)) s)))
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
				   (if (= (mod (:frame-ix s1) 100) 0) (alert-changes s1) s1)))
  ; Mainly menues here:
  :below-actionPerformed
  (fn [s e o]
    (let [from (:origin-state e)] 
      (if (:callback from) ((:callback from) s) s)))
  ; This event runs the repl:
  :below-keyPressed
  (fn [s e o]
    (let [shifting? (boolean (:ShiftDown e)) ; TODO: why do we need a boolean cast?
          ctrl? (boolean (:MetaDown e))]
      (cond (and (= (:descendent e) (get-in s [:id-paths :input])) ; repl input...
              shifting? (= (:KeyCode e) 10)) (code-to-repl!! s) ; ...and shift + enter = repl 
        ; saving files:
        (and ctrl? (= (:KeyCode e) 83)) (if (:cur-file s) (save-file!!! s) (do (repl/add-to-repl!! "No file is open. Use file -> new to save any text in the editor." false) s))
        :else s))); no hotkey
  :Children
 {:menu (menu)
  :panes
  {:Type 'JSplitPane :Orientation JSplitPane/HORIZONTAL_SPLIT
  :Children
  {:filessource 
     {:Type 'JSplitPane :Orientation JSplitPane/HORIZONTAL_SPLIT
       :Children
       {:files (file-tree) :source (editor)}}
    :inout
   {:Type 'JSplitPane :Orientation JSplitPane/VERTICAL_SPLIT
     :Children
     {:out (output) :in (input)}}}}}})
(defn window [] (let [w (_window)] (assoc w :id-paths (id-paths w))))

(defn reset-app [s]
   "Completely resets the application."
  (window))
  
(defn _reload-fn [s r]
   ; Use s unless r has the 
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
   
