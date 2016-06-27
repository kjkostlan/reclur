; Safemode and initiation code.
; (require '[clooj.core :as clooj])
; (clooj/build-safemode-gui)

(ns clooj.core
  (:import (javax.swing UIManager JFrame SwingUtilities) 
    (java.awt.event WindowEvent ActionListener KeyListener)
    (javax.swing.event CaretListener)
    (java.nio.file Files Paths)
    (java.io ByteArrayOutputStream PrintStream)
    (java.nio.charset StandardCharsets)
    (javax.swing JTextArea JScrollPane JOptionPane JMenu JMenuBar JMenuItem JSplitPane)
    (javax.swing.text DefaultHighlighter$DefaultHighlightPainter)
    (java.io File BufferedWriter OutputStreamWriter FileOutputStream)
    (java.awt Dimension Color Font))
    (:require [clooj.app.state_old :as app_old]
              [clooj.java.gui :as gui]
              [clooj.app.framer :as framer]
              [clojure.string :as string]
              [clooj.java.window_old :as jwindow_old]
              [clooj.app.make_old :as make_old]
              [clooj.repl.main :as repl]
              [clooj.utils :as utils]
              [clooj.java.prefs :as jprefs]
              [clooj.java.tree_old :as jtree_old]
              [clooj.java.file :as jfile]
              [clooj.java.textarea_old :as jtext_old])
    (:gen-class
     :methods [^{:static true} [show [] void]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;; Safe-mode duplicated code copied from other files so we don't need them ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn src-directory [] "./src") ; we simply use locals.

(defn _texty? [^File file] (and (not (.isDirectory file))
  (or (.endsWith (.getName file) ".clj") (.endsWith (.getName file) ".java") (.endsWith (.getName file) ".js")
      (.endsWith (.getName file) ".txt") (.endsWith (.getName file) ".text"))))

(defn sep [] (File/separator))
(defn file2folder [^String file]
  "extracts the folder that the file (which is NOT a folder) is in."
  (let [match (last (utils/re-index (utils/str-to-regex (sep)) file))]
    (if (nil? match) 
      file ; no change.
      (subs file 0 match))))

(defn _dir? [^File file] (.isDirectory file))

(defn visible-children
  "Get a vector of a directory's children, if there are any.
   Omits hidden and temporary files."
  [^File file]
  (->> (.listFiles file)
       (remove #(.startsWith (.getName %) "."))
       (remove #(.endsWith (.getName %) "~"))
       vec))

(defn local-path [^File file]
 "converts a file object into local-path string"
 (str "." (sep) (.getPath (.relativize (.toURI (File. ".")) (.toURI file)))))

(defn load-textfile [^String file]
  (try (let [out (String. (Files/readAllBytes (Paths/get file (into-array [""]))) (StandardCharsets/UTF_8))
             n (count out)] out)
  (catch Exception e
    (println (.getMessage e))
    (JOptionPane/showMessageDialog nil (str "Unable to load file: " file) "Oops" JOptionPane/ERROR_MESSAGE))))

(defn save-textfile!!! [^String file ^String contents] ; three ! means that the disk is mutated.
  (try (do
         ; check for folder: 
         (let [folder (File. (file2folder file))]
           (.mkdirs folder))
         (with-open [writer (BufferedWriter. (OutputStreamWriter. (FileOutputStream. (File. file)) "UTF-8"))]
                    (.write writer contents)))     
  (catch Exception e
    (JOptionPane/showMessageDialog nil (str "Unable to save file: " file) "Oops" JOptionPane/ERROR_MESSAGE))))

(defn file2namespace [^String file]
  "./src/clooj/java/file.clj into clooj.java.file, etc"
  (string/replace (subs file 6 (- (count file) 4)) (sep) "."))

(defn reload-file [file]
  (let [dotpath (file2namespace file) namespace (create-ns '_debugger.namespace)]
    (binding [*ns* namespace] (require (symbol dotpath) :reload))))


(defn delete-file!!! [^String file]
  "Remember to always prompt the user fist."
  (.delete (File. file)))

(defn _filetree [^File folder filt]
  "Makes a tree of folders/files starting from a folder, only including files with a filter function."
  (let [ch (visible-children folder) ft (filter filt ch)] ; File objects.
    { ; doall's so we leave java-object land asap.
      :ch-local (mapv local-path ft)
      :ch-absolute (mapv #(.getAbsolutePath %) ft)
      :ch-leaf (mapv #(.getName %) ft)
      :children (mapv #(_filetree % filt) (filter _dir? ch)) ; recursive.
      :full (.getAbsolutePath folder)
      :leaf (.getName folder)}))
(defn filetree [folder filt] (_filetree (File. folder) filt))

(defn get-texty-tree [] (filetree (src-directory) _texty?))

(defn add-action-listener! [item f use-event?]
  "use-event? is whether f takes in the event as an argument."
  (let [listener (proxy [ActionListener] []
                   (actionPerformed [e]
                     (if use-event? (f e) (f))))]
    (.addActionListener item listener)))

(defn add-caret-listener! [item f use-event?]
  "Moving the caret around."
  (let [listener (proxy [CaretListener] []
                   (caretUpdate [e]
                     (if use-event? (f e) (f))))]
    (.addCaretListener item listener)))

(defn add-keypressed-listener! [item f use-event?]
  "use-event? is whether f takes in the event as an argument."
  (let [listener (proxy [KeyListener] []
                   (keyPressed [e]
                     (if use-event? (f e) (f)))
                   (keyTyped [e])
                   (keyReleased [e]))]
    (.addKeyListener item listener)))

(defn non-debug-eval!!! [cmd]
   "We need to be cogniscient of the namespaces here."
   (let [nss 'u]
     (if (nil? (find-ns nss)) 
       ; bind the namespace and let us use the core:
       (let [n (create-ns nss)]
         (binding [*ns* n] (eval '(clojure.core/use 'clojure.core)))))
     (binding [*ns* (find-ns nss)] 
         (eval (read-string cmd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Safe-mode bare-bones GUI that allows creating editing and deleting files ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def console (System/out))

(defn caret-update! [box]
  "When the caret listener is called."
  (let [^java.lang.String text (str (.getText box))
        limit (int (count text))
        highlighter (.getHighlighter box)
        hilite-old (.getClientProperty box "HilightTag")]
    ;(.removeAllHighlights highlighter)
    (if hilite-old (.removeHighlight highlighter hilite-old))
    (if (> limit 0)
      (let [open (int (.getCaretPosition box))
            copen (char (.charAt text (Math/min (int open) (int (dec limit)))))]
        (if (or (= copen \() (= copen \[) (= copen \{) (= copen \#) (= copen \") (= copen \') )
          (let [close (loop [ix (int (inc open))]
                        (if (>= ix limit) -1 ; no closing found.
                          (let [c (char (.charAt text ix))]
                            (if (or (= c \)) (= c \]) (= c \}) (= c \") (= c \'))
                              (let [^java.lang.String s (subs text open (inc ix))
                                    good? (try (do (read-string s) true)
                                            (catch Exception e false))]
                                (if good? ix (recur (inc ix)))) (recur (inc ix))))))
                hpainter (DefaultHighlighter$DefaultHighlightPainter. (if (> close -1) Color/yellow Color/red))]
             (.putClientProperty box "HilightTag"
               (.addHighlight highlighter open (if (> close -1) (inc close) (inc open)) hpainter))))))))

(defn make-text-box []
  "Makes a text-box that hilights the matching parenthesis if the user clicks on a parenthesis.
   This is a survival tool: it is hard to work without it, and OK to work with it."
  ; Proxy tool that will paint line numbers (using RSyntaxTextArea did not work).
  (let [box (proxy [JTextArea] []
              (paintComponent [g] 
                (proxy-super paintComponent g)
                (let [metrics (.getFontMetrics g (.getFont this))
                      height (.getHeight metrics)
                      width (.charWidth metrics \space)
                      ^chars text (chars (.toCharArray (str (.getText this) "\n")))
                      n (int (count text))
                      c-p-l (loop [ix (int 0) lines [] numthis (int 0)] ; chars per line.
                              (cond (= ix n) lines
                                    (= (aget ^chars text ix) \newline) (recur (inc ix) (conj lines numthis) 0)
                                    :else (recur (inc ix) lines (inc numthis))))]
                  (.setColor g (Color. (float 0.2) (float 0.3) (float 0.99)))
                  (mapv #(.drawString g (str (inc %)) (int (+ 50 (* (nth c-p-l %) width)))
                      (int (* height (+ % 0.8)))) (range (count c-p-l))))))] 
    (.setFont box (Font. "monospaced" Font/PLAIN 12))
    (.setText box "Editor")
    (add-caret-listener! box #(caret-update! box) false) box))

(defn nil-file-check [file window] 
  (if (nil? file) (JOptionPane/showMessageDialog window  "No file has been set (create or load one to set it)." "not yet set" JOptionPane/ERROR_MESSAGE)) (nil? file))

(defn new-command!!! [window text-box menu-update!]
  (let [file (str (JOptionPane/showInputDialog window "Input a filename such as \"./src/foo/bar.clj\"."
                    "New file" JOptionPane/PLAIN_MESSAGE nil nil ""))]
    (try 
      (do (save-textfile!!! file (.getText text-box))
          (.putClientProperty text-box "file" file)
          (println "Created and saved " file " using the text in the window."))
      (catch Exception e (JOptionPane/showMessageDialog window (str "File couldn't be created: " file) (str e) JOptionPane/ERROR_MESSAGE)))
    (menu-update! window text-box)))

(defn load-command! [window text-box menu-update! file] ; a bit different since it creates the file.
  (try (do (.setText text-box (load-textfile file))
           (.putClientProperty text-box "file" file)
           (println "Loaded " file "into the window."))
    (catch Exception e (JOptionPane/showMessageDialog window (str "File couldn't be opened: " file) (str e) JOptionPane/ERROR_MESSAGE))))

(defn save-command!!! [window text-box menu-update!]
  (let [file (.getClientProperty text-box "file") text (.getText text-box)]
    (if (not (nil-file-check file window))
      (do (save-textfile!!! file text) (menu-update! window text-box)
        (println "saved current text in file: " file)
        (try (reload-file file) (catch Exception e (println "error compiling: " e)))))))
        

(defn delete-command!!! [window text-box menu-update!]
  (let [file (.getClientProperty text-box "file") text (.getText text-box)]
    (if (not (nil-file-check file window))
      (if (= (JOptionPane/showOptionDialog window
                 "Delete current file?" "are you sure?" JOptionPane/YES_NO_OPTION
                 JOptionPane/QUESTION_MESSAGE nil (into-array ["Yes" "no"]) "no") 0)
        (do (delete-file!!! file) (menu-update! window text-box)
          (println "deleted file:" file ". To undo this (using the text in the window), save."))))))

(defn _add-load-submenus! [window text-box tree parent-menu menu-update!]
  (let [folder-menus (mapv (fn [ch] (JMenu. (:leaf ch))) (:children tree))
        ; leaf menu items that load the file, this level only.
        leaf-mitems (mapv (fn [local-file] (JMenuItem. local-file)) (:ch-local tree))]
  ; Add submenus to each folder menu recursivly:
  (mapv (fn [subtree menu]
          (_add-load-submenus! window text-box subtree menu menu-update!)) (:children tree) folder-menus)
  ; Add the action listeners to the leaf menuitems:
  (mapv (fn [file mitem] 
          (add-action-listener! mitem 
            #(load-command! window text-box menu-update! file) false)) (:ch-local tree) leaf-mitems)
  ; Add the folder menus leaf items to the parent:
  (mapv #(.add parent-menu %) (concat folder-menus leaf-mitems))))

(defn make-load-menu [window text-box menu-update!]
  ; wrap in one level so the src is included:
  (let [tree_ (get-texty-tree)
       load-menu (JMenu. "Load") tree {:ch-local [] :children [tree_]}]
    (_add-load-submenus! window text-box tree load-menu menu-update!) load-menu))

(defn about-command!!! [window]
  (JOptionPane/showMessageDialog window 
    "This is safe-mode, a stripped-down editor that is used when the main editor is offline.
     It is triggered TODO if a failure is detected in the main code, or TODO manually brought up.
     Editing: Fairly basic load-save interface. Saving a file re-imports it.
     Errors: See the console window.
     Hilighting: Put the cursor before a (, [, {, etc to hilight the code between the ().
     REPL: TODO."))

(defn start-print-stream-loop! [box stream writer]
  "Box will be set to the stream's value every frame."
  (let [loop-fn!! #(loop []
                    (SwingUtilities/invokeLater 
                      (fn [] 
                        (let [stream (.getClientProperty box "stream")
                              writer (.getClientProperty box "writer")
                              text-now (.getText box)
                              text-next (.toString stream)]
                          (.flush stream) ; flushing helps ensure stuff makes it out?
                          (if (not= text-now text-next)
                            (.setText box text-next)))))
                    (try (Thread/sleep 30) (catch InterruptedException iex [])) (recur))]
   (.putClientProperty box "stream" stream)
   (.putClientProperty box "writer" writer)
   (.start (Thread. loop-fn!!))))
(defn set-print-destination!! [box]
 ; Redirects the println to the box instead of the console.
 ; Uncauht exceptions still go to the console.
  (let [os (ByteArrayOutputStream.)
        os-writer (OutputStreamWriter. os)]
    ;(System/setOut ps)
    (alter-var-root (var *out*) (fn [_] os-writer))
    (println "Console")
    ;(set! *out* ps)  <= won't work. Causes error.
    (start-print-stream-loop! box os os-writer)))
(defn clear-console!! [box]
  (let [os (ByteArrayOutputStream.)
        os-writer (OutputStreamWriter. os)]
   (alter-var-root (var *out*) (fn [_] os-writer))
   (.putClientProperty box "stream" os)
   (.putClientProperty box "writer" os-writer)))


(defn update-menus! [window text-box & info-box]
  "Sets the menu options: new, open, save, delete."
  (let [menu-bar (JMenuBar.) new (JMenu. "New") load (make-load-menu window text-box update-menus!) 
        save (JMenu. "Save") delete (JMenu. "Delete") help (JMenu. "Help")
        console (JMenu. "Console") 
        new-item (JMenuItem. "New file") help-item (JMenuItem. "About") 
        save-item (JMenuItem. "Save this file") delete-item (JMenuItem. "Delete this file")
        clear-item (JMenuItem. "Clear")]
    (add-action-listener! new-item #(new-command!!! window text-box update-menus!) false)
    (add-action-listener! save-item #(save-command!!! window text-box update-menus!) false)
    (add-action-listener! delete-item #(delete-command!!! window text-box update-menus!) false)
    (add-action-listener! help-item #(about-command!!! window) false)
    (if (first info-box)
      (add-action-listener! clear-item #(clear-console!! (first info-box)) false))
    (.add new new-item) (.add save save-item) (.add delete delete-item) (.add help help-item)
    (.add console clear-item)
    (doto menu-bar (.add new) (.add load) (.add save) (.add delete) (.add console) (.add help))
    (.setJMenuBar window menu-bar)))

(defn repl!! [e box]
  (let [text (.getText box)
        caret (.getCaretPosition box)]
    (if (and (= (.getKeyChar e) \newline) (= caret (count text)))
      (try (println (non-debug-eval!!! text))
        (catch Exception e (do (println "REPL error: ") (println e)))))))

(defn build-safemode-gui []
  ;http://stackoverflow.com/questions/1052473/scrollbars-in-jtextarea
  "Builds a single jframe with a jtextarea and menus for files.
   Does not use our functional-reactive GUI, as we are in safe mode!"
  (let [task (fn []
                (let [
                      window (JFrame.) edit-box (make-text-box) 
                      edit-scroll (JScrollPane. edit-box);(JScrollPane. edit-box)
                      info-box (JTextArea.) info-scroll (JScrollPane. info-box)
                      repl-box (JTextArea.) repl-scroll (JScrollPane. repl-box)
                      split-pane1 (JSplitPane. JSplitPane/VERTICAL_SPLIT edit-scroll info-scroll)
                      split-pane2 (JSplitPane. JSplitPane/VERTICAL_SPLIT split-pane1 repl-scroll)]
                  (add-keypressed-listener! repl-box #(repl!! % repl-box) true)
                  (.setText repl-box "REPL")
                  ;(.setMinimum (.getHorizontalScrollBar edit-scroll) 100)
                  (.setTitle window "Safe mode barebones GUI, see terminal for exceptions")
                  (.setPreferredSize edit-scroll (Dimension. 800 200))
                  (.setPreferredSize info-scroll (Dimension. 800 200))
                  (.setPreferredSize repl-scroll (Dimension. 800 200))
                  (.setSize window 800 600)
                  (.add (.getContentPane window) split-pane2)
                  ;(.add (.getContentPane window) split-pane2)
                  (.setVisible window true)
                  (update-menus! window edit-box info-box)
                  (.pack window)
                  (.setDividerLocation split-pane1 0.7)
                  (.setDividerLocation split-pane2 0.8)
                  (set-print-destination!! info-box)))]
    (SwingUtilities/invokeLater task)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Standard functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; DONT call save-file, we can't run swing stuff from shutdown hooks reliably.
; Instead we do this on the exit-if-close function.
; TODO: get working.
(defn on-shutdown [] ())

;;  startup

(defn startup_old []
  (Thread/setDefaultUncaughtExceptionHandler
    (proxy [Thread$UncaughtExceptionHandler] []
      (uncaughtException [thread exception]
                       (println thread) (.printStackTrace exception))))
  (UIManager/setLookAndFeel (UIManager/getSystemLookAndFeelClassName))
  (make_old/create-app!!)
  (make_old/make-menus!!)
  (make_old/add-visibility-shortcut!!)
  (repl/add-repl-input-handler!!)
  (let [frame @app_old/frame]
    (jwindow_old/persist-window-shape!!! "main-window" frame)
    (utils/enable-mac-fullscreen! frame)
    (.setVisible frame true)
    ;(make_old/on-window-activation! frame #(make_old/update-for-file-structure!!))
    )
  (jtree_old/add-tree-select-listener! app_old/src-tree #(make_old/tree-select!! %))
  (make_old/apply-settings!!! @app_old/settings)
  (.addShutdownHook (Runtime/getRuntime) (Thread. on-shutdown)))

(defn -show []
  (reset! app_old/embedded true)
  (if (not @app_old/frame)
    (startup_old)
    (.setVisible @app_old/frame true)))

(defn print-to-repl [x]
(jtext_old/append-text! app_old/repl-out-text-area (str x \newline)))

; When upgrading the app there are several different versions we may use:
(def which-version :current) ; :safe, :old, :current

(defn -main [& args]
  (cond 
     (= which-version :safe)
     (build-safemode-gui)
     (= which-version :old)
     (do (reset! app_old/embedded false) (startup_old) (make_old/update-for-file-structure!!)) ; after this we just wait for swing events to be dispatched.
     (= which-version :current)
     (gui/setup (framer/window))
     :else
     (throw (Exception. "Unrecognized version to sue: " which-version)))) 

; Restart application (not pure, will still preserve some things).
; (do (use 'clooj.core) (restart-app!!))
(defn restart-app!! []
  (let [txt (jtext_old/get-text app_old/src-text-area) caret (jtext_old/get-caret-position app_old/src-text-area)
        file @app_old/current-file]
    (.setDefaultCloseOperation @app_old/frame (JFrame/DISPOSE_ON_CLOSE))
    (doall (map #(.removeWindowListener @app_old/frame %) (.getWindowListeners @app_old/frame)))
    ;http://stackoverflow.com/questions/1234912/how-to-programmatically-close-a-jframe.
    (.dispatchEvent @app_old/frame (WindowEvent. @app_old/frame WindowEvent/WINDOW_CLOSING))
    (-main)
    ;reload any text we were using:
    (jtext_old/set-text! app_old/src-text-area txt caret)
    ;(println "file " (subs @app_old/current-file 1))
    (if (not (nil? file)) (SwingUtilities/invokeLater #(jtree_old/string-to-selection! app_old/src-tree (subs file 1) true)))))