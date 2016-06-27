; creates the app, binds functions to it, etc.
; Many functions here modify the global app variable, thus the use of the double !!

(ns clooj.app.make_old
  (:import (javax.swing AbstractListModel BorderFactory JDialog
                        JFrame JLabel JList JMenuBar JOptionPane
                        JPanel JScrollPane JSplitPane JTextArea
                        JTextField JTree KeyStroke
                        JTextPane JCheckBox JButton
                        ListSelectionModel
                        UIManager SwingUtilities)
           (javax.swing.event TreeSelectionListener
                              TreeExpansionListener)
           (javax.swing SpringLayout)
           (javax.swing.tree DefaultMutableTreeNode DefaultTreeModel
                             TreePath TreeSelectionModel)
           (java.awt Insets Rectangle Window)
           (java.awt.event AWTEventListener FocusAdapter 
                           MouseAdapter WindowAdapter 
                           ActionListener KeyAdapter)
           (java.awt AWTEvent Color Font GridLayout Toolkit)
           (java.net URL)
           (java.io File StringReader)
           (java.util.concurrent LinkedBlockingQueue)
           (java.util Map)
           (org.fife.ui.rsyntaxtextarea RSyntaxTextArea SyntaxConstants TokenMakerFactory)
           (org.fife.ui.rtextarea RTextScrollPane))
  (:require [clooj.java.popup :as jpopup]
            [clooj.java.window_old :as jwindow_old]
            [clojure.string :as string]
            [clojure.string :as string]
            [clooj.repl.main :as repl]
            [clooj.repl.debugger :as debugger]
            [clooj.app.state_old :as app_old]
            [clooj.utils :as utils]
            [clooj.navigate :as navigate]
            [clooj.indent :as indent]
            [clooj.brackets :as brackets]
            [clooj.highlighting :as highlighting]
            [clooj.search :as search]
            [clooj.settings :as settings]
            [clooj.java.tree_old :as jtree_old]
            [clooj.coder.namespacer :as namespacer]
            [clooj.coder.grammer :as grammer]
            [clooj.coder.cbase :as cbase]
            [clooj.java.file :as jfile]
            [clooj.coder.io :as cio]
            [clooj.coder.strmap :as strmap]
            [clooj.coder.tracer :as tracer]
            [clooj.java.prefs :as jprefs]
            [clooj.repl.output :as repl-output]
            [clooj.java.textarea_old :as jtext_old]
            [clooj.coder.autofix :as autofix]))

(def gap 5)

(defprotocol DynamicWordHighlighter
  (addWordToHighlight [this word token-type]))

(extend-type RSyntaxTextArea
  DynamicWordHighlighter
  (addWordToHighlight [word token-type]))
    
(defn make-rsyntax-text-area []
  (let [tmf (TokenMakerFactory/getDefaultInstance)
        token-maker (.getTokenMaker tmf "text/clojure")
        token-map (.getWordsToHighlight token-maker)
        rsta (proxy [RSyntaxTextArea] []
               (addWordToHighlight [word token-type]
                                   (do
                                     (.put token-map word token-type)
                                     token-type)))]
      (.. rsta getDocument (setTokenMakerFactory tmf))
    rsta))

(defn get-main-text-areas [] ; the big three that you see.
   [(:textarea @app_old/src-text-area) (:textarea @app_old/repl-in-text-area) (:textarea @app_old/repl-out-text-area)])

;; format of text and fonts.

(defn set-line-wrapping! [text-area mode]
  (.setLineWrap text-area mode))

(defn set-fonts!! ; !! means that there is mutation for on variables that are NOT passed into us (the app)
    [font-name size]
    (let [f (Font. font-name Font/PLAIN size)]
      (utils/awt-event
        (dorun (map #(.setFont % f) (get-main-text-areas))))))

(defn resize-fonts!! [size] (set-fonts!! (:font-name @app_old/settings) size))

(defn grow-font1!! [] (resize-fonts!! (+ (:font-size @app_old/settings) 1)))

(defn shrink-font!! [] (resize-fonts!! (- (:font-size @app_old/settings) 1)))

;; settings are saved/loaded to disk using java's Preference array.

(def default-settings
  (merge 
    (zipmap [:font-name :font-size] 
            (cond (utils/is-mac) ["Monaco" 11]
                  (utils/is-win) ["Courier New" 12]
                  :else    ["Monospaced" 12]))
  {:line-wrap-doc false
   :line-wrap-repl-out false
   :line-wrap-repl-in false
   :show-only-monospaced-fonts true
   }))


(defn apply-settings!!! [_settings] ; !!! means tht the disk is modified (java's prefreneces in this case)
  (let [settings @_settings]
    (map #(set-line-wrapping! % (:line-wrap-doc settings)) (get-main-text-areas))
    (set-fonts!!
            (:font-name settings)
            (:font-size settings))
    (reset! app_old/settings settings)
    (jprefs/save-settings!!! settings)))

;; caret finding

(def highlight-agent (agent nil))

(def arglist-agent (agent nil))

(defn save-caret-position!!! []
  (utils/when-lets [text-area (:textarea @app_old/src-text-area)
                    pos (jtext_old/get-caret-position app_old/src-text-area)
                    file @app_old/current-file]
    (when-not (jfile/dir? file)
      (let [key-str (str "caret_" (.hashCode file))]
        (jprefs/write-value-to-prefs!!! key-str pos)))))

(defn load-caret-position!! []
  (utils/when-lets [text-area (:textarea @app_old/src-text-area)
                    file @app_old/current-file]
    (when-not (jfile/is-dir file)
      (utils/when-lets [key-str (str "caret_" (.hashCode file))
                  pos (jprefs/read-value-from-prefs key-str)]
        (let [length (.. text-area getDocument getLength)
              pos2 (Math/min pos length)]
          (.setCaretPosition text-area pos2)
          (jtext_old/scroll-to-caret! app_old/src-text-area))))))

(defn display-caret-position!! [src-text-area]
  (let [x (jtext_old/get-caret-coords src-text-area)
       row (nth (:keys x) 0) col (nth (:keys x) 1)]
    (println (str " " (inc row) "|" (inc col)))))
   
;; double-click paren to select form

(defn double-click-selector! [text-comp]
  (.addMouseListener text-comp
    (proxy [MouseAdapter] []
      (mouseClicked [e]
        (when (== 2 (.getClickCount e))
          (utils/when-lets [pos (.viewToModel text-comp (.getPoint e))
                            c (.. text-comp getDocument (getText pos 1) (charAt 0))
                            pos (cond (#{\( \[ \{ \"} c) (inc pos)
                                      (#{\) \] \} \"} c) pos)
                            [a b] (brackets/find-enclosing-brackets (utils/get-text-str text-comp) pos)]
            (utils/set-selection! text-comp a (inc b))))))))

(declare restart-doc)
    
(defn text-file? [f]
  (not (some #{(utils/file-suffix f)}
             ["jar" "class" "dll" "jpg" "png" "bmp"])))

;; build gui

(defn make-scroll-pane [text-area]
  (RTextScrollPane. text-area))

(defn create-arglist-label []
  (doto (JLabel.)
    (.setVisible true)))

; Files must be local path.
(defn nav-to-file-no-save!! [file]
  (let [curfile @app_old/current-file]
    (if (not= file curfile)
      (let [contents (jfile/load-textfile file)]
        (if (not (nil? contents)) ; if the user opens an invalid file or moves a file, etc.
          (do
            (if (not (nil? curfile)) (swap! app_old/places-of-files assoc curfile (jtext_old/get-place app_old/src-text-area)))
            (swap! app_old/files-visited #(conj % file))
            (reset! app_old/current-file file)
            (let [contents (jfile/load-textfile file)]
              (jtext_old/set-text! app_old/src-text-area contents 0))
            (reset! app_old/files-visited-ind (- (count @app_old/files-visited) 1))
            (jtree_old/string-to-selection! app_old/src-tree (subs file 1) true)
            (when-let [pl (get @app_old/places-of-files file)] (jtext_old/set-place! app_old/src-text-area pl))
            (.setTitle @app_old/frame file)
            (if (jfile/clj? file)
              (jtext_old/set-parse! app_old/src-text-area jtext_old/parsy)
              (if (jfile/java? file)
                (jtext_old/set-parse! app_old/src-text-area cio/parse-summary-java)
                (jtext_old/set-parse! app_old/src-text-area nil)))
            (jtext_old/clear-undo! app_old/src-text-area)))))))

(defn nav-to-caret!! [caret]
  ; TODO: making a more functional text-area will remove much of the need to change the caret.
  (jtext_old/request-focus! app_old/src-text-area)
  (jtext_old/set-caret! app_old/src-text-area caret))

(defn savefile-err-msg [e file]
  "Generates an error message that's useful for compile-time Exceptions.
   :str is the string. :lo :hi :f is link structure for the user to click on (see Jtext)"
  (let [msg (.getMessage e)
        msg1 (string/replace (string/replace msg "java.lang.RuntimeException: " "") ", compiling:" "")
        re #"\(.+:\d+\)"
        part1 (first (string/split msg1 re)) ; swap these b/c location info is hottest.
        part2 (first (re-seq re msg1))

        ; find the place of the file:
        lineno (try (dec (Integer. (second (string/split part2 #":")))) (catch Exception e (do (println "bad part2 " part2) 0)))
        contents (jfile/load-textfile file)
        caret (first (grammer/lines2caret contents [lineno]))

        ; Build the link: DONT save the file when linked! We already are saving the file.
        lo 1 hi (dec (count part2))
        f #(do (nav-to-file-no-save!! file) (nav-to-caret!! caret))
        ]
    {:str (str part2 " " part1) :lo lo :hi hi :f f}))

(defn save-file!!! [print-info?] ; simple and easy to use.
  "Saves the current file and returns the info. Also prints it to the repl if you want to."
  (let [file @app_old/current-file] (jfile/save-textfile!!! file (jtext_old/get-text app_old/src-text-area))
    (let [info {:str (str "Saved file: " file " ")}
         ; side effect: loading file.
         info (if (jfile/clj? file) 
                (try (do ;(println "before reload") 
                         (repl/reload-file file)
                         ;(println "after reload") 
                         {:str (str (:str info) "(no compile error).\n")})
                  (catch Exception e
                    (let [errhint " [error prevented downstream compiling/updating this namespace]."
                          erinfo (savefile-err-msg e file)
                          pad "\n    "
                          n0 (+ (count pad) (count (:str info)))
                          a #(+ % n0)]
                      {:str (str (:str info) pad (:str erinfo) errhint)
                        :lo (a (:lo erinfo)) :hi (a (:hi erinfo)) :f (:f erinfo)
                        :err e})))
                 {:str (str (:str info) "\n")})
          info (if (jfile/java? file) {:str (str (:str info) " WARNING: restart required to recompile java files.\n")}
                  info)]
     (if print-info? 
       (if (nil? (:lo info))
         (jtext_old/append-text! app_old/repl-out-text-area (:str info)) ;no link.
         (do (jtext_old/append-link-text1! app_old/repl-out-text-area (str (:str info) "\n") (:f info) (:lo info) (:hi info) true))))
     info))) ;link

(defn save-file-with-autocorrect!!! []
  "Saves the file using auto-correct capability. Auto-correct is limited but a nice convenience to have for common errors."
  (let [file @app_old/current-file
        jtxt app_old/src-text-area
        text0 (jtext_old/get-text jtxt)
        ;_ (println text0)
        prnt (fn [& args] (jtext_old/append-text! app_old/repl-out-text-area (apply str "\n" args)))
        result
          (loop [n-correct 0 info (save-file!!! false) text text0]
            
            (if (or (nil? (:err info)) (= n-correct 3)) ; no error.
                {:n-correct n-correct :text text :success true}
                (let [fixed1 (autofix/fix text (:err info))] ; only fixes one error.
                  (if (nil? fixed1) 
                      {:n-correct n-correct :text text :success false} ; can't fix.
                      (do 
                        (jtext_old/set-text! jtxt fixed1 (:caret (jtext_old/get-place jtxt)))
                        (recur (inc n-correct) (save-file!!! false) fixed1)))))); did fix, but is it the last error?
        iters (:n-correct result)
        happy (:success result)
        text (:text result)] 
    (if happy
      (prnt "Saved " file (cond (= iters 0) ", no changes needed." (= iters 1) ", 1 auto-fix.\n" :else (str iters " auto-fixes.\n")))
      (prnt "Saved " file (cond (= iters 0) ", no fixes could be made." (= iters 1) ", 1 auto-fix but still sad.\n" :else (str iters " auto-fixes but still sad.\n"))))))

(defn nav-to-file!! [file]
  (if (not (nil? @app_old/current-file)) (save-file!!! true))
  (nav-to-file-no-save!! file))

(defn nav-to-file-caret!! [file caret] ; convieince funciton.
  (nav-to-file!! file)
  (nav-to-caret!! caret))
(defn exit-if-closed!! [^java.awt.Window f]
  (when-not @app_old/embedded
    (.addWindowListener f
      (proxy [WindowAdapter] []
        (windowClosing [_]
          (save-caret-position!!!)
          (if (not (nil? @app_old/current-file)) (save-file!!! true)) ; save any work in progress.
          (System/exit 0))))))

(def no-project-txt
    "\n Welcome to clooj, a lightweight IDE for clojure\n
     To start coding, you can either\n
       a. create a new project
            (select the Project > New... menu), or
       b. open an existing project
            (select the Project > Open... menu)\n
     and then either\n
       a. create a new file
            (select the File > New menu), or
       b. open an existing file
            (click on it in the tree at left).")
       
(def no-file-txt
    "To edit source code you need to either: <br>
     &nbsp;1. create a new file 
     (select menu <b>File > New...</b>)<br>
     &nbsp;2. edit an existing file by selecting one at left.</html>")

(defn unbind-project!!! []  (println "TODO: unbind project"))

(defn bind-project!!! []
  (when-let [file (jpopup/choose-file @app_old/current-file "Bind to Leiningen project.clj file")]
  (println "TODO: bind project")))
(defn attach-global-action-keys!! [comp]
  (utils/attach-action-keys! comp
    ["cmd1 EQUALS" #(grow-font1!!)]
    ["cmd1 shift EQUALS" #(grow-font1!!)]
    ["cmd1 PLUS" #(grow-font1!!)]
    ["cmd2 MINUS" #(.toBack @app_old/frame)]
    ["cmd2 PLUS" #(.toFront @app_old/frame)]
    ["cmd2 EQUALS" #(.toFront @app_old/frame)]
    ;["cmd1 K"#(jtext_old/set-text! app_old/repl-out-text-area "" 0)]
    ))

(defn on-window-activation! [win fun]
  (.addWindowListener win
    (proxy [WindowAdapter] []
      (windowActivated [_]
        (fun)))))

;;;;;;;;;;;;; MAKE IT

(defn create-app!! []
  (reset! app_old/frame (JFrame.))
  (reset! app_old/src-text-area (jtext_old/boxed-new "Source Editor" true false))
  (search/add-search! app_old/src-text-area)
  (reset! app_old/repl-in-text-area (jtext_old/boxed-new "Clojure REPL input" true false))
  (reset! app_old/repl-out-text-area (jtext_old/boxed-new "Clojure REPL output" false false))
  (search/add-search! app_old/repl-out-text-area)
  (reset! app_old/help-text-area (jtext_old/boxed-new "Invizi-help" true false))
  (reset! app_old/src-tree (jtree_old/boxed-new "Projects" "src" "./src"))
  (reset! app_old/src-split-pane
    (utils/make-split-pane (:panel @app_old/src-tree) (:panel @app_old/src-text-area) true gap 0.25))
  (reset! app_old/repl-split-pane
    (utils/make-split-pane (:panel @app_old/repl-out-text-area) (:panel @app_old/repl-in-text-area) false gap 0.75))
  (reset! app_old/split-pane (utils/make-split-pane @app_old/src-split-pane @app_old/repl-split-pane true gap 0.5))
  (reset! app_old/settings (jprefs/load-settings default-settings))
  (doto @app_old/frame
      (.setBounds 25 50 950 700)
      (.setLayout (SpringLayout.))
      (.add @app_old/split-pane)
      (.setTitle (str "clooj " (utils/get-clooj-version) ", no file open")))

  ;(add-constraints!!)
  (jwindow_old/snug-fit! @app_old/split-pane)
  (jwindow_old/apply-springs! @app_old/src-tree)
  (jwindow_old/apply-springs! @app_old/src-text-area)
  (jwindow_old/apply-springs! @app_old/repl-in-text-area)
  (jwindow_old/apply-springs! @app_old/repl-out-text-area)
  (doto (:textarea @app_old/repl-in-text-area)
    double-click-selector!
    navigate/attach-navigation-keys!)
    (.layoutContainer (SpringLayout.) @app_old/frame)
  (exit-if-closed!! @app_old/frame)
  ;(activate-caret-highlighter!!)
  (utils/attach-action-keys! (:textarea @app_old/src-text-area)
    ["cmd1 ENTER" #(repl/send-selected-to-repl!!!)])
  ;(indent/setup-autoindent! app_old/repl-in-text-area)
  (dorun (map #(attach-global-action-keys!! %)
              [(:tree @app_old/src-tree) (:textarea @app_old/src-text-area) (:textarea @app_old/repl-in-text-area) (:textarea @app_old/repl-out-text-area)
               (.getContentPane @app_old/frame)])))

;; projects.

; call this every time a file is modified (and once at startup).
(defn update-for-file-structure!! []
  (jtree_old/load-src-files-in-tree! app_old/src-tree))

(def project-clj-text (.trim
"
(defproject PROJECTNAME \"1.0.0-SNAPSHOT\"
  :description \"FIXME: write description\"
  :dependencies [[org.clojure/clojure \"1.5.1\"]])
"))

(defn specify-source [project-dir title default-namespace]
  (when-let [namespace (JOptionPane/showInputDialog nil
                         "Please enter a fully-qualified namespace"
                         title
                         JOptionPane/QUESTION_MESSAGE
                         nil
                         nil
                         default-namespace)]
    (let [tokens (map munge (.split namespace "\\."))
          dirs (cons "src" (butlast tokens))
          dirstring (apply str (interpose File/separator dirs))
          name (last tokens)
          the-dir (File. project-dir dirstring)]
      (.mkdirs the-dir)
      [(File. the-dir (str name ".clj")) namespace])))


; the user types in a path and we make the file there.
; TODO: actually go to the file bieng created.
(defn create-file!!! []
   (let [new-ns (utils/user-input "type in a namespace (i.e. foo.bar)")]
     (if (not (nil? new-ns))
       (let [file (str "." (jfile/sep) "src" (jfile/sep) (string/replace new-ns "." (jfile/sep)) ".clj")]
         (if (jfile/exists? file)
           (JOptionPane/showMessageDialog nil (str "File " file " already exists") "Oops" JOptionPane/ERROR_MESSAGE) ; create-file can't overwrite files!
           (do (jfile/save-textfile!!! file "")
             (update-for-file-structure!!)
             (nav-to-file!! file)))))))

(defn new-project-clj!!! [project-dir]
 (println "TODO: make new project feature"))

(defn rename-file!!! []
 (println "TODO: rename file feature")(update-for-file-structure!!))

(defn delete-file!!! []
  ; prompt the user:
  (if (jpopup/yes-no "Delete current file? No take-backs!")
    (when-let [fl @app_old/current-file]
      (jfile/delete-file!!! fl) (reset! app_old/current-file nil)
      (jtext_old/set-text! app_old/src-text-area "" 0) (update-for-file-structure!!)
      (.setTitle @app_old/frame (str "clooj " (utils/get-clooj-version) ", no file open")))))

(defn revert-file!!! []
  (jfile/revert-textfile!!! @app_old/current-file)
  (update-for-file-structure!!)
  (let [contents (jfile/load-textfile @app_old/current-file)]
   (jtext_old/set-text! app_old/src-text-area contents 0)))

(defn- dir-rank [dir]
  (get {"src" 0 "test" 1 "lib" 2} (.getName dir) 100))

(defn- find-file [project-path relative-file-path]
  (let [classpath-dirs (sort-by dir-rank < (utils/get-directories (File. project-path)))
        file-candidates (map 
                          #(File. (str (.getAbsolutePath %) File/separatorChar relative-file-path)) 
                          classpath-dirs)]
    (first (filter #(and (.exists %) (.isFile %)) file-candidates))))

(defn toggle-breakpoint!! [atomtext]
  (let [tc (repl/toggle-breakpoint (jtext_old/get-text atomtext) (jtext_old/get-caret-position atomtext))]
    (jtext_old/set-text! atomtext (:text tc) (:caret tc))))

(defn history-visit!! [jump] 
  ;(println "history visitting " jump)
  (let [visited @app_old/files-visited ind_ (+ @app_old/files-visited-ind jump)
        ind (max 0 (min ind_ (- (count visited) 1)))]
    (nav-to-file!! (nth visited ind)) 
    ; the technological singularity is a decent beginning of history timepoint, 2015 is so primative in comparison its prehistory.
    (if (< ind_ 0) (jtext_old/append-text! app_old/repl-out-text-area "Beginning of history\n"))
    ; The heat death of the universe is the end of history (unless black holes turn out to be survivable):
    (if (> ind_ ind) (jtext_old/append-text! app_old/repl-out-text-area "End of history\n"))
    ;(reset! app_old/files-visited visited) ; visiting history museums is itself history?
    (reset! app_old/files-visited-ind ind))) ; this gets modified by nav-to-file.
  
(defn show-history!! [] 
  (jtext_old/append-text! app_old/repl-out-text-area
    (str "\n" (apply str (interpose "\n" @app_old/files-visited)) "\n")))

; load the file when the user selects a node on the tree
(defn tree-select!! [e]
  (let [file (str "." (jtree_old/selection-to-string e))]
    (if (and (not (nil? file)) (jfile/texty? file)) (nav-to-file!! file))))

; another way of loading a file, that tries to be somewhat smart for the user's name:
(defn open-file!! []
  (let [file (repl/user-decide-file)] 
    (if (not (= file -1)) ; -1 means user does not decide a file. nil means invalid file.
      (if (nil? file)
        (JOptionPane/showMessageDialog nil (str "Unable to recognize your keyword") "Oops" JOptionPane/ERROR_MESSAGE)
        (nav-to-file!! file)))))

; a nice way of importing a file or namespace. idiomatic = uses your conventions of what you call imports, etc.
(defn idiomatic-import!! [hint_]
  (let [hint (if (nil? hint_) (utils/user-input "type the file or how you :as the namespace") hint_)
        importst (repl/idiomatic-require hint true)]
    (if (nil? importst) (JOptionPane/showMessageDialog nil (str "Unable to recognize your keyword") "Oops" JOptionPane/ERROR_MESSAGE)
      (do (jtext_old/append-text! app_old/repl-in-text-area importst)
          (.requestFocusInWindow (:textarea @app_old/repl-in-text-area))))))

; Useful for the REPL:
(defn use-ns!! []
  (jtext_old/append-text! app_old/repl-in-text-area
    (namespacer/use-ns (jtext_old/get-text app_old/src-text-area)))
  (.requestFocusInWindow (:textarea @app_old/repl-in-text-area)))

; forgot what something was called?
(defn list-vars!! []
  (let [file (repl/user-decide-file)]
    (if (not (= file -1))
      (if (nil? file)
        (JOptionPane/showMessageDialog nil (str "Unable to recognize your keyword") "Oops" JOptionPane/ERROR_MESSAGE)
        (let [vars (rest (:obj (strmap/reads-string+ (jfile/load-textfile file)))); exclude the ns part at the top.
              names (mapv #(str (:obj (second (:obj %)))) vars)
              
              ; doc strings as it is called in Python:
              docs (mapv #(if (nil? %) "<no doc>" %) (mapv (fn [v] (:obj (first (filter #(string? (:obj %)) (:obj v))))) vars))
              
              ; Argument names: ("" if var, "[...]" if single arg, "([...], [...],...)" if multi-arg).
              ; TODO: make it work for (def x (fn [...]...)), not just defn.
              fn? (mapv #(= (:obj (first (:obj %))) 'defn) vars)
              single-arg (mapv (fn [v] (str (:obj (first (filter #(vector? (:obj %)) (:obj v)))) vars)))
              multi-arg (mapv (fn [v] (let [groups (mapv #(:obj %) (filterv #(list? (:obj %)) (:obj v)))]
                                        (mapv (fn [g] (str (mapv #(str (:obj %)) (first (:obj g)))) groups)))))
              args (mapv #(if %1 (if (nil? %2) %3 %2) "") fn? single-arg multi-arg)
              
              carets (mapv #(first (:pos %)) vars)
              
              txts (map #(str %1 " " %2 " " %3 "\n") names args docs)
              ato app_old/repl-out-text-area
              sfn #(jtext_old/append-link1! nav-to-file-caret!! ato %1 [0 (count %2)] file %3)]
          
            (jtext_old/append-text! ato "\n\nVar-list for ")
            ;append-link1! gotofn!! atomtext text textix file fx.
            (doall (map sfn txts names carets))
          )))))

(defn list-linked-vars!! [varfulls] 
  "Vars and thier link for you to click on them. Fully-qualified only."
  (let [locs (mapv cbase/get-var-loc varfulls)]
    (mapv #(jtext_old/append-link1! nav-to-file-caret!! app_old/repl-out-text-area (str %1 "\n") [0 (count (str %1))] %2 %3)
      varfulls (mapv #(:file %) locs) (mapv #(:caret %) locs))))

(defn find-vars!! []
  "Finds vars or functions."
  (let [input (utils/user-input "Give a partial or full function or variable name that you want to use.")]
    (if (not (nil? input))
      (let [all-vars (cbase/get-all-vars)
            all-varnames (into [] (apply concat (mapv (fn [n vs] (mapv #(str n "/" %) vs)) (keys all-vars) (vals all-vars)))) ; fully-qualified.
            
            vars (filterv #(.contains (.toLowerCase %) (.toLowerCase input)) all-varnames) ;matchin ones.
            txt (if (= (count vars) 0) (str "\nno var matches for: " input "\n") 
                    (str "\nvar matches for " input ":\n" (apply str (interpose "\n" vars))   "\n"))]
        (jtext_old/append-text! app_old/repl-out-text-area 
          (str (if (empty? vars) "NO " "") "matches for " input (if (empty? vars) "" ":") "\n") true)
        (list-linked-vars!! vars)))))

(defn find-usages!! []
  "Finds usages of a function in the current file"
  (let [input (utils/user-input "Type in a variable name in the CURRENT file.")
        file @app_old/current-file
        txt (str "\"" (jfile/file2namespace file) (if (nil? input) "\"" (str "/" input "\"")) "\n")]
    (if (not (nil? input))
      (let [full (namespacer/resolved-code-ns (jfile/file2namespace @app_old/current-file) input)]
        (if (nil? full) (jtext_old/append-text! app_old/repl-out-text-area (str "Unrecognized var in this file:" input "\n") true)
            (let [vars (get (:reverse (cbase/dependency)) (symbol full))] 
              (jtext_old/append-text! app_old/repl-out-text-area
                (str (if (> (count vars) 0) "Usages of" "NO usages of") txt) true)
              (list-linked-vars!! vars)))))))

(defn clear-repl!! [] 
  ;(println "clearin repl")
  (jtext_old/clear-links! app_old/repl-out-text-area) (jtext_old/set-text! app_old/repl-out-text-area "" 0))

(defn make-menus!! []
  (when (utils/is-mac)
    (System/setProperty "apple.laf.useScreenMenuBar" "true"))
  (let [menu-bar (JMenuBar.)]
    (. @app_old/frame setJMenuBar menu-bar)
    (let [file-menu
          (utils/add-menu! menu-bar "File" "F"
            ["New" "N" "cmd1 N" #(create-file!!!)]
            ["Open" "O" "cmd1 O" #(open-file!!)]
            ["Save" "S" "cmd1 S" #(save-file!!! true)]
            ["Save + auot-fill" nil "cmd1 shift S" #(save-file-with-autocorrect!!!)]
            ["Move/Rename" "M" "cmd1 M" #(rename-file!!!)]
            ["Revert" "R" "cmd1 cmd2 shift R" #(revert-file!!!)]
            ["Delete" nil nil #(delete-file!!!)])]
      (when-not (utils/is-mac)
        (utils/add-menu-item! file-menu "Exit" "X" nil #(System/exit 0))))
    (utils/add-menu! menu-bar "Project" "P"
      ["New project..." "N" "cmd1 cmd2 shift N" #(println "TODO new project creates an empty, bound project.")]
      ["Bind project.clj..." "O" "cmd1 cmd2 shift O" #(bind-project!!!)]
      ["Unbind project.clj..." "U" "cmd1 cmd2 shift U" #(unbind-project!!!)])
    (utils/add-menu! menu-bar "Navigate" "N"
      ["Goto previous file (saves current file)" "P" "cmd1 9" #(history-visit!! -1)]
      ["Goto next file (saves current file)" "N" "cmd1 0" #(history-visit!! 1)]
      ["Show visit file history" "S" "cmd1 shift H" #(show-history!!)])    
    (utils/add-menu! menu-bar "Source" "U"
      ["Comment" "C" "cmd1 SEMICOLON" #(utils/toggle-comment! (:textarea @app_old/src-text-area))]
      ["TODO Auto-indent" "F" "cmd1 BACK_SLASH" #(indent/fix-indent-selected-lines! app_old/src-text-area)]
      ["Indent lines" "I" "cmd1 shift CLOSE_BRACKET" #(utils/indent! (:textarea @app_old/src-text-area))]
      ["Unindent lines" "D" "cmd1 shift OPEN_BRACKET" #(utils/unindent! (:textarea @app_old/src-text-area))]
      ["Goto line num..." "G" "cmd1 L" #(jtext_old/move-caret-to-line! app_old/src-text-area)]
      )
    (utils/add-menu! menu-bar "REPL" "R"
      ["Evaluate here" "E" "cmd1 ENTER" #(repl/send-selected-to-repl!!!)]
      ["Evaluate entire file" "F" "cmd1 E" #(repl/send-alltext-to-repl!!!)]
      ["Clear Output" "C" "cmd1 shift K" clear-repl!!]
      ["Toggle Breakpoint" "B" "cmd1 B" #(toggle-breakpoint!! app_old/src-text-area)]
      ["Print stack trace for last error" "T" "cmd1 T" #(debugger/print-stack-trace! nav-to-file-caret!! app_old/repl-out-text-area true)]
      ["Print raw stack trace 4 last err" "T" "cmd1 shift T" #(debugger/print-stack-trace! nav-to-file-caret!! app_old/repl-out-text-area false)]
      ["Be like this namespace" "U" "cmd1 U" #(use-ns!!)] 
      ["Idiomatic require this file" "I" "cmd1 I" #(idiomatic-import!! @app_old/current-file)]
      ["Idiomatic require a file" "J" "cmd1 shift I" #(idiomatic-import!! nil)])
    (utils/add-menu! menu-bar "Search" "S"
      ["Focus on searchbar" "F" "cmd1 F" #(.requestFocus (:search-box @app_old/src-text-area))]
      ["Find in files" "L" "cmd1 shift F" #(do (.requestFocus (:search-box @app_old/src-text-area)) (search/search-src!! nav-to-file-caret!!))]
      ["List vars in a file" "L" "cmd1 shift L" #(list-vars!!)]
      ["Find var or function definition" "D" "cmd1 shift D" #(find-vars!!)]
      ["Find usages" "U" "cmd1 shift U" #(find-usages!!)])
    (utils/add-menu! menu-bar "Window" "W"
      ["Go to REPL input" "R" "cmd1 3" #(jtext_old/requestFocusInWindow! app_old/repl-in-text-area)]
      ["Go to Editor" "E" "cmd1 2" #(jtext_old/requestFocusInWindow! app_old/src-text-area)]
      ["Go to Project Tree" "P" "cmd1 1" #(jtree_old/requestFocusInWindow! app_old/src-tree)]
      ["Increase font size" nil "cmd1 PLUS" #(grow-font1!!)]
      ["Decrease font size" nil "cmd1 MINUS" #(shrink-font!!)]
      ["Settings" nil nil #(settings/show-settings-window!! apply-settings!!!)]
      ["Refersh GUI" nil "cmd1 shift R" #(repl/send-to-repl!!! "(do (use 'clooj.core) (restart-app!!))")])))

(defn add-visibility-shortcut!! []
  (let [shortcuts [(map utils/get-keystroke ["cmd2 EQUALS" "cmd2 PLUS"])]]
    (.. Toolkit getDefaultToolkit
      (addAWTEventListener
        (proxy [AWTEventListener] []
          (eventDispatched [e]
            (when (some #{(KeyStroke/getKeyStrokeForEvent e)}
                     shortcuts)
              (.toFront @app_old/frame))))
        AWTEvent/KEY_EVENT_MASK))))