; The main code that sets up the app.
(ns clooj.app.framer
  (:import [javax.swing JSplitPane] [java.awt Font Color]
           [javax.swing.text DefaultHighlighter DefaultHighlighter$DefaultHighlightPainter])
  (:require [clooj.java.gui :as gui]
            [clooj.java.widget :as widget]
            [clooj.java.file :as jfile]
            [clooj.repl.main :as repl]
            [clooj.utils :as utils]
            [clooj.coder.grammer :as grammer]
            [clooj.repl.debugger :as debugger]))

; (require '[clooj.app.framer :as framer] '[clooj.java.gui :as gui])

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
  (if (and text caret obj)
    (let [parse (grammer/basic-parse text false false)
          ^ints levels_ (:level parse) levels (into [] levels_)
          caret-level (let [x0 (get levels (dec caret)) x1 (get levels caret)] 
                        (if (and x0 x1) (min x0 x1) 0))
          ; all stuff at the level:
          after (loop [acc [] ix caret]
                   (cond (or (< ix 0) (>= ix (count levels))) acc
                     (>= (nth levels ix) caret-level) (recur (conj acc ix) (inc ix))
                     :else acc))
          ; Stuff below:
          before (loop [acc [] ix caret]
                   (cond (or (< ix 0) (>= ix (count levels))) acc
                     (>= (nth levels ix) caret-level) (recur (conj acc ix) (dec ix))
                     :else acc))
          region (sort (concat before after))          
          begin (first region) end (last region)]
          (if (and begin end)
            (let [hltr (.getHighlighter obj)
                  hpainter (DefaultHighlighter$DefaultHighlightPainter. Color/orange)]
              (.removeAllHighlights hltr)
              (.addHighlight hltr begin (inc end) hpainter))))))

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
  "Saves a file (unless file is nil, for which it does nothing)."
  (if (:cur-file s)
    (let [cur-file (:cur-file s)
          is-file? (jfile/is-file cur-file) 
          ; clj files must match between namespaces and java.
          ns-conflict (if (jfile/clj? cur-file) (not= (get-wantsedit-file s) cur-file) false)
          text ((:get-src-text s) s)]
      (do (jfile/save-textfile!!! cur-file text)
          (let [msg (if ns-conflict (str "Saved: " cur-file " ERROR: Namespace declared in file is missing, invalid, or doesn't match the filename.")
                      (try (do (repl/reload-file cur-file) (str "Saved: " cur-file " (no error)."))
                         (catch Exception e (str "Saved: " cur-file " Compile error: " e  " (Error prevented downstream (re)definitions)."))))]
            ; Update s if we have made a new file:
            (assoc-in (if is-file? s (update-in s (:file-tree (:id-paths s)) #((:update %) % cur-file)))
              (concat (:output (:id-paths s)) [:Text]) msg)))) s))

(defn loadfile [s]
  "Uses the :cur-file to load. Does not check for validity."
  (update-in (assoc-in s (concat (:editor (:id-paths s)) [:Text])
               (jfile/load-textfile (:cur-file s)))
     (:file-tree (:id-paths s)) #(ensure-file-open % (:cur-file s))))

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
                               ;(println "ensuring this is open: " ensure-this-file-is-open)
                               (ensure-file-open (update-tree-keep-settings t-old (get-tree)) 
                                 ensure-this-file-is-open))
      ; Update for new files, but keep selected status if applicable (apply to the 'JTree level):
      )))
(defn file-tree []
   (let [ftree (get-tree)]
     {:Type 'JScrollPane
      :Children 
      [ftree]}))

; Edit code:
(defn editor []
  (let [g0 (.getGraphics (java.awt.image.BufferedImage. 2 2 java.awt.image.BufferedImage/TYPE_INT_RGB))
        h! (fn [s e o] (let [s0 (:SelectionStart s) s1 (:SelectionEnd s)]
             (if (or (not s0) (not s1) (= s0 s1)) (hilite-same-level! (:Text s) (:CaretPosition s) o)
               (let [hltr (.getHighlighter o)
                     hpainter (DefaultHighlighter$DefaultHighlightPainter. (Color. (float 0.75) (float 0.75) (float 1.0)))]
                 (.removeAllHighlights hltr) (.addHighlight hltr s0 s1 hpainter))
             )))
        ; Draw the line numbers:
        line-nos (fn [s] 
                   (let [t (:Text s)
                         metrics (.getFontMetrics g0 (:Font s))
                         height (.getHeight metrics)
                         width (.charWidth metrics \space)
                         ^chars text (chars (.toCharArray (str (:Text s) "\n")))
                         n (int (count text))
                         c-p-l (loop [ix (int 0) lines [] numthis (int 0)] ; chars per line.
                                 (cond (= ix n) lines
                                   (= (aget ^chars text ix) \newline) (recur (inc ix) (conj lines numthis) 0)
                                   :else (recur (inc ix) lines (inc numthis))))
                         g (mapv #(vector :drawString [(str (inc %)) (int (+ 50 (* (nth c-p-l %) width)))
                                                        (int (* height (+ % 0.8)))]
                                    {:Color (Color. (float 0.2) (float 0.3) (float 0.99))}) (range (count c-p-l)))]
                      (assoc s :Graphics g)))]
    {:Type 'JScrollPane
     :Children 
     [{:Type 'JTextArea :Text "Editor" :Font (Font. "monospaced" Font/PLAIN 12)
       :insertUpdate (fn [s e o] (h! s e o) (line-nos s))
       :removeUpdate (fn [s e o] (h! s e o) (line-nos s))
       :caretUpdate (fn [s e o] (h! s e o) (line-nos s))
       :id :editor}]}))

(defn output [] ; Here instead of the terminal.
  {:Type 'JScrollPane
   :Children 
   [{:Type 'JTextArea :Text "REPL output" :Font (Font. "monospaced" Font/PLAIN 12)
     :id :output}]})

(defn input [] ; REPL
  {:Type 'JScrollPane
   :Children 
   [{:Type 'JTextArea :Text "REPL input" :Font (Font. "monospaced" Font/PLAIN 12)
     :id :input}]})

(declare reload)

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
          (assoc-in % (concat (:output (:id-paths %)) [:Text]) "No file is open. Use file -> new to save the text."))}
     {:Type 'JMenuItem :Text "delete (non-folders only)" 
      :callback (fn [s] 
                   (let [cf (:cur-file s)]
                     (if cf (jfile/delete-file!!! cf))
                     (assoc-in (update-in s (:file-tree (:id-paths s)) #((:update %) % cf))
                       (concat (:output (:id-paths s)) [:Text])
                       ; IMPORTANT: keep the :cur-file for undoable deletion.
                       (if cf (str "(UNDOABLE) Deleted: " cf " to UNDO: use file->new/save before opening another file.") 
                         (str "No file is open to delete.")))))}]}
    {:Type 'JMenu :Text "window"
     :Children
     [{:Type 'JMenuItem :Text "reset app" :callback reload}]}]})

; The main window with everything:
(defn _window []
 {:Type 'JFrame
  :Title "Key Missing features: find-replace, debugging, refactoring."
  ; Gets the editor text:
  :get-src-text #(:Text (get-in % (:editor (:id-paths %))))
  :set-src-text #(assoc-in %1 (concat (:editor (:id-paths %1)) [:Text]) %2)
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
                 (let [s1 (update s :frame-ix inc)]
                            (if (= (mod (:frame-ix s1) 100) 0) 
                              (let [tpath (:file-tree (:id-paths s1))
                                    s-new (update-in s1 tpath #((:update %) % (:cur-file s1)))
                                    opath (:output (:id-paths s-new))
                                    changes (get-diff (get-in s1 tpath) (get-in s-new tpath))
                                    ]
                                ; tell the user about external changes:
                                (if (> (count changes) 0)
                                  (assoc-in s-new (concat opath [:Text]) (apply str "External files changed (rename = add+remove): " 
                                                                           (mapv #(str % "\n") changes))) s-new))
                               s1)))
  ; Mainly menues here:
  :below-actionPerformed
  (fn [s e o]
    (let [from (:origin-state e)] 
      (if (:callback from) ((:callback from) s) s)))
  ; This event runs the repl:
  :below-keyPressed
  (fn [s e o]
    (let [inpath (get-in s [:id-paths :input]) outpath (get-in s [:id-paths :output])]
      (if (and (= (:descendent e) inpath) ; right place.
            (:ShiftDown e) (= (:KeyCode e) 10)) ; shift + enter.
        ; TODO: on a seperate thread so we do not freeze if a slow command appears:
        (assoc-in s (concat outpath [:Text]) 
          (try (debugger/non-debug-eval!!! (:Text (get-in s inpath)))
            (catch Exception e (str e))))
        s))); didn't fit the repl criteria, no actual change.
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

(defn reload [s]
   ;ignores the current state.
  (window))