(ns clooj.app.framer
  (:import [javax.swing JSplitPane] [java.awt Font Color]
           [javax.swing.text DefaultHighlighter DefaultHighlighter$DefaultHighlightPainter])
  (:require [clooj.java.gui :as gui]
            [clooj.java.file :as jfile]
            [clooj.repl.main :as repl]
            [clooj.coder.grammer :as grammer]
            [clooj.repl.debugger :as debugger]))

; (require '[clooj.app.framer :as framer] '[clooj.java.gui :as gui])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Sets up the gui components ;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-tree []
  "Gets the files in JTree form."
  (let [ftree (jfile/filetree "./src" (fn [_] true))
        convert (fn fc [branch] 
                  (let [x {:Type 'JTree :Text (:leaf branch) :filepath (:local branch)}]
                    (if (> (count (:children branch)) 0)
                     (assoc x :Children (mapv fc (:children branch)))
                     ; :Children only includes folders, but we need to add files:
                     (assoc x :Children 
                       (mapv #(hash-map :Type 'JTree :Text %2 :filepath %1)
                       (:ch-local branch) (:ch-leaf branch))))))]
   (convert ftree)))
(defn file-tree []
  {:Type 'JScrollPane
   :Children 
   [(get-tree)]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Listener helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
          begin (first region) end (last region)
          ]
          (if (and begin end)
            (let [hltr (.getHighlighter obj)
                  hpainter (DefaultHighlighter$DefaultHighlightPainter. Color/orange)]
              (.removeAllHighlights hltr)
              (.addHighlight hltr begin (inc end) hpainter))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Edit code:
(defn editor []
  (let [h! (fn [s e o] (let [s0 (:SelectionStart s) s1 (:SelectionEnd s)]
             (if (or (not s0) (not s1) (= s0 s1)) (hilite-same-level! (:Text s) (:CaretPosition s) o)
               (let [hltr (.getHighlighter o)
                     hpainter (DefaultHighlighter$DefaultHighlightPainter. (Color. (float 0.75) (float 0.75) (float 1.0)))]
                 (.removeAllHighlights hltr) (.addHighlight hltr s0 s1 hpainter))
             )))]
    {:Type 'JScrollPane
     :Children 
     [{:Type 'JTextArea :Text "Editor" :Font (Font. "monospaced" Font/PLAIN 12)
       :insertUpdate (fn [s e o] (h! s e o) s)
       :removeUpdate (fn [s e o] (h! s e o) s)
       :caretUpdate (fn [s e o] (h! s e o) s)
     }]}))

; See output:
(defn output []
  {:Type 'JScrollPane
   :Children 
   [{:Type 'JTextArea :Text "REPL output" :Font (Font. "monospaced" Font/PLAIN 12)}]})

; REPL:
(defn input []
  {:Type 'JScrollPane
   :Children 
   [{:Type 'JTextArea :Text "REPL input" :Font (Font. "monospaced" Font/PLAIN 12)}]})

; The main window with everything:
(defn window []
 {:Type 'JFrame
  :Title "Key Missing features: Creating deleting and renaming files, line numbers, save current file on close."
  :current-file nil
  ; This event does a lot: saving and loading files as well as updating the textarea:
  :windowClosing (fn [s e o] (System/exit 0)) 
  :below-valueChanged
  (fn [s e o] 
    (let [tree (get-in s [:Children 0 :Children 0 :Children :files :Children 0])
          filename (get-filename-from-tree tree e)
          is-leaf-file? (and filename (not (jfile/is-dir filename)))
          text-path [:Children 0 :Children 0 :Children :source :Children 0 :Text]
          text (get-in s text-path)
          cur-file (:current-file s)
          safe-to-save-files? true] ; debug false to avoid problems. This is an addition to your own backups.
      (if (and is-leaf-file? cur-file (not safe-to-save-files?)) ; double checking.
          (println "wants to save to this file: " cur-file "this text: " text))
      (if (and is-leaf-file? cur-file safe-to-save-files?)
        (do (jfile/save-textfile!!! cur-file text) ; side effects...
            (repl/reload-file cur-file)))
      (if is-leaf-file? (assoc-in (assoc s :Title (str filename) :current-file (str filename))
                         text-path (jfile/load-textfile (str filename))) s)))
  ; This event runs the repl:
  :below-keyPressed
  (fn [s e o]
    (let [inpath [:Children 0 :Children 1 :Children :in :Children 0]
          outpath [:Children 0 :Children 1 :Children :out :Children 0]]
      (if (and (= (:descendent e) inpath) ; right place.
            (:ShiftDown e) (= (:KeyCode e) 10)) ; shift + enter.
        ; TODO: on a seperate thread so we do not freeze if a slow command appears:
        (assoc-in s (concat outpath [:Text]) 
          (try (debugger/non-debug-eval!!! (:Text (get-in s inpath)))
            (catch Exception e (str e))))
        s))); didn't fit the repl criteria, no actual change.
  :Children
 [{:Type 'JSplitPane :Orientation JSplitPane/HORIZONTAL_SPLIT
  :Children
  [{:Type 'JSplitPane :Orientation JSplitPane/HORIZONTAL_SPLIT
     :Children
     {:files (file-tree) :source (editor)}}
   {:Type 'JSplitPane :Orientation JSplitPane/VERTICAL_SPLIT
     :Children
     {:out (output) :in (input)}}]}]})

;(gui/setup (framer/window))