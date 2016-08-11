(ns clooj.utils
  (:require [clojure.string :as string :only (join split)])
  (:import (java.util UUID)
           (java.awt FileDialog Point Window)
           (java.awt.event ActionListener MouseAdapter)
           (java.security MessageDigest)
           (java.io ByteArrayInputStream ByteArrayOutputStream
                    File FilenameFilter BufferedReader
                    InputStreamReader
                    ObjectInputStream ObjectOutputStream
                    OutputStream Writer PrintStream)
           (javax.swing AbstractAction JButton JFileChooser JMenu JMenuBar JMenuItem BorderFactory
                        JOptionPane JSplitPane KeyStroke SpringLayout SwingUtilities)
           (javax.swing.event DocumentListener UndoableEditListener TreeSelectionListener)
           (javax.swing.undo UndoManager)
           (java.net URL)
           (java.io File FileReader StringReader BufferedWriter OutputStreamWriter FileOutputStream)
           (java.nio.file Files Paths)
           (java.nio.charset Charset StandardCharsets)))

;; Note: the swing specific code is bieng pahses out as its refactored into other files.
;; TODO: organize.

;; general

;http://stackoverflow.com/questions/16349415/is-there-a-way-to-be-notified-when-a-clojure-future-finishes
; Modify slightly. Note that this is basically only useful for side-effects.
(defn when-done [future-to-watch function-to-call]
  (future (try (do @future-to-watch
                   (try (function-to-call) (catch Exception e (println "error in FUNCTION: " e))))
            (catch Exception e (println "error in FUTURE: " e)))))

(defmacro do-when [f & args] ; not used currently.
  (let [args_ args]
    `(when (and ~@args_)
       (~f ~@args_))))

(defmacro when-lets [bindings & body]
  (assert (vector? bindings))
  (let [n (count bindings)]
    (assert (zero? (mod n 2)))
    (assert (<= 2 n))
  (if (= 2 n)
    `(when-let ~bindings ~@body)
    (let [[a b] (map vec (split-at 2 bindings))]     
      `(when-let ~a (when-lets ~b ~@body))))))

(defn count-while [pred coll]
  (count (take-while pred coll)))

(defn remove-nth [s n]
  (lazy-cat (take n s) (drop (inc n) s)))

(defmacro awt-event [& body]
  `(SwingUtilities/invokeLater
     (fn [] (try ~@body
                 (catch Throwable t# 
                        (.printStackTrace t#))))))

(defmacro gen-map [& args]
  (let [kw (map keyword args)]
    (zipmap kw args)))

(defn ssubvec [v lo0 hi0]
  "stops at the edge instead of overflowing."
  (let [n (count v)
        lo (min n (max lo0 0))
        hi (min n (max hi0 0))]
    (subvec v lo hi)))

; other non-macro functions that are widely useful.

(defn cur-time [] ; time in seconds.
  (/ (System/currentTimeMillis) 1000.0))

(defn set-nth [coll n val]
  "works on sequential? stuff. Lazy will be converted into vectors!
   sorted-set is not sequential?."
  (if (vector? coll) (assoc coll n val)
    (if (list? coll)
      (apply list (assoc (into [] coll) n val)) ; not the most efficient, but should be short lists. 
    (if (not (sequential? coll)) (throw (Exception. "set-nth only works on sequential?"))
      (assoc (into [] coll) n val)))))

(defn set-range [v lo hi val] 
  "sets all values between lo and hi to val. Best performance with vectors.
   Ranges out-of-bounds are clipped to the bounds."
  (reduce #(assoc %1 %2 val) v (range (max lo 0) (min hi (count v)))))

(defn argmax [f coll] 
  "returns the index for which (f coll) is maximized."
  (nth (apply max-key #(nth % 1) (map #(vector %1 %2) (range) (map f coll))) 0))

; from http://rosettacode.org/wiki/Averages/Median.
(defn median [ns]
  (let [ns (sort ns)
        cnt (count ns)
        mid (bit-shift-right cnt 1)]
    (if (odd? cnt)
      (nth ns mid)
      (/ (+ (nth ns mid) (nth ns (dec mid))) 2))))

(defn class-for-name
  "Returns true if a class represented by class-name
   can be found by the class loader."
  [class-name]
  (try (Class/forName class-name)
       (catch Throwable _ nil)))

(defn static-method [class-name method-name & arg-types]
  (let [method
        (some-> class-name
                class-for-name
                (.getMethod method-name
                            (into-array Class arg-types)))]
          (fn [& args]
            (.invoke method nil (object-array args)))))

(defn str-to-regex
  "sometimes you don't need a fancy regular expression. This makes a regexp that is equivalent to a non-regexp match of str.
   http://stackoverflow.com/questions/28342570/how-to-split-a-string-in-clojure-not-in-regular-expression-mode"
  [c]
  (re-pattern (java.util.regex.Pattern/quote (str c))))

(defn re-index [re s]
  "Returns the indexes of where the regular expression occurs in the string s."
  (let [ins (re-seq re s)
        outs (string/split s re)
        cins (map count ins) 
        n (count cins)
        couts (concat (map count outs) (repeat n 0))
        cs (map + cins couts)];
    (if (= n 0)
      []
      (reduce #(conj %1 (+ (last %1) %2)) [(nth couts 0)] (rest cs)))))

;; identify OS

(defn get-os []
  (.. System (getProperty "os.name") toLowerCase))

(def is-win
  (memoize #(not (neg? (.indexOf (get-os) "win")))))

(def is-mac
  (memoize #(not (neg? (.indexOf (get-os) "mac")))))

(def is-unix
  (memoize #(not (and (neg? (.indexOf (get-os) "nix"))
                      (neg? (.indexOf (get-os) "nux"))))))

(def get-clooj-version
  (memoize
    (fn []
      (try
        (-> (Thread/currentThread) .getContextClassLoader
            (.getResource "clooj/core.class") .toString
            (.replace "clooj/core.class" "project.clj")
            URL. slurp read-string (nth 2))
        (catch Exception _ nil)))))

;; swing layout

(defn put-constraint [comp1 edge1 comp2 edge2 dist]
  (let [edges {:n SpringLayout/NORTH
               :w SpringLayout/WEST
               :s SpringLayout/SOUTH
               :e SpringLayout/EAST}]
    (.. comp1 getParent getLayout
        (putConstraint (edges edge1) comp1 
                       dist (edges edge2) comp2))))

(defn put-constraints [comp & args]
  (let [args (partition 3 args)
        edges [:n :w :s :e]]
    (dorun (map #(apply put-constraint comp %1 %2) edges args))))

(defn constrain-to-parent!
  "Distance from edges of parent comp args"
  [comp & args]
  (apply put-constraints comp
         (flatten (map #(cons (.getParent comp) %) (partition 2 args)))))


;; text components

(defn user-input [instruction] ; returns what the user says.
  (JOptionPane/showInputDialog instruction))

(defn get-line-text [text-pane line]
  (let [start (.getLineStartOffset text-pane line)
        length (- (.getLineEndOffset text-pane line) start)]
    (.. text-pane getDocument (getText start length))))

(defn add-text-change-listener! [text-comp f]
  "Executes f whenever text is changed in text component."
  (.addDocumentListener
    (.getDocument text-comp)
    (reify DocumentListener
      (insertUpdate [this evt] #(f evt) text-comp)
      (removeUpdate [this evt] #(f evt) text-comp)
      (changedUpdate [this evt]))))

(defn remove-text-change-listeners! [text-comp]
  (let [d (.getDocument text-comp)]
    (doseq [l (.getDocumentListeners d)]
      (.removeDocumentListener d l))))
                               
(defn get-text-str [text-comp]
  (let [doc (.getDocument text-comp)]
    (.getText doc 0 (.getLength doc))))


(defn set-selection! [text-comp start end]
  (doto text-comp (.setSelectionStart start) (.setSelectionEnd end)))

(defn get-selected-lines [text-comp]
  (let [row1 (.getLineOfOffset text-comp (.getSelectionStart text-comp))
        row2 (inc (.getLineOfOffset text-comp (.getSelectionEnd text-comp)))]
    (doall (range row1 row2))))

(defn get-selected-line-starts [text-comp]
  (map #(.getLineStartOffset text-comp %)
       (reverse (get-selected-lines text-comp))))

(defn insert-in-selected-row-headers! [text-comp txt]
  (awt-event
    (let [starts (get-selected-line-starts text-comp)
          document (.getDocument text-comp)]
      (dorun (map #(.insertString document % txt nil) starts)))))

(defn remove-from-selected-row-headers! [text-comp txt]
  (awt-event
    (let [len (count txt)
          document (.getDocument text-comp)]
      (doseq [start (get-selected-line-starts text-comp)]
        (when (= (.getText (.getDocument text-comp) start len) txt)
          (.remove document start len))))))

(defn comment-out! [text-comp]
  (insert-in-selected-row-headers! text-comp ";"))

(defn uncomment-out! [text-comp]
  (remove-from-selected-row-headers! text-comp ";"))

(defn toggle-comment! [text-comp]
  (if (= (.getText (.getDocument text-comp) 
                   (first (get-selected-line-starts text-comp)) 1)
         ";")
    (uncomment-out! text-comp)
    (comment-out! text-comp)))
    
(defn indent! [text-comp]
  (when (.isFocusOwner text-comp)
    (insert-in-selected-row-headers! text-comp " ")))

(defn unindent! [text-comp]
  (when (.isFocusOwner text-comp)
    (remove-from-selected-row-headers! text-comp " ")))

;; other gui

(defn make-split-pane [comp1 comp2 horizontal divider-size resize-weight]
  (doto (JSplitPane. (if horizontal JSplitPane/HORIZONTAL_SPLIT 
                                    JSplitPane/VERTICAL_SPLIT)
                     true comp1 comp2)
        (.setResizeWeight resize-weight)
        (.setOneTouchExpandable false)
        (.setBorder (BorderFactory/createEmptyBorder))
        (.setDividerSize divider-size)))

;; keys

(defn get-keystroke [key-shortcut]
  (KeyStroke/getKeyStroke
    (-> key-shortcut
      (.replace "cmd1" (if (is-mac) "meta" "ctrl"))
      (.replace "cmd2" (if (is-mac) "ctrl" "alt")))))

(defn fident [f] f)

;; actions

(defn attach-child-action-key!
  "Maps an input-key on a swing component to an action,
  such that action-fn is executed when pred function is
  true, but the parent (default) action when pred returns
  false."
  [component input-key pred f]
  (let [im (.getInputMap component)
        am (.getActionMap component)
        input-event (get-keystroke input-key)
        parent-action (if-let [tag (.get im input-event)]
                        (.get am tag))
        child-action
          (proxy [AbstractAction] []
            (actionPerformed [e]
              (if (pred)
                (f) ; override parent.
                (when parent-action ; don't override parent.
                  (.actionPerformed parent-action e)))))
        uuid (.. UUID randomUUID toString)]
    (.put im input-event uuid)
    (.put am uuid child-action)))


(defn attach-child-action-keys! [comp & items]
  (doall (map #(apply attach-child-action-key! comp %) items)))

(defn attach-action-key!
  "Maps an input-key on a swing component to an action-fn."
  [component input-key f]
  (attach-child-action-key! component input-key
                           (constantly true) f))

(defn attach-action-keys!
  "Maps input keys to action-fns."
  [comp & items]
  (doall (map #(apply attach-action-key! comp %) items)))
  
;; buttons
 
(defn create-button [text f]
  (doto (JButton. text)
    (.addActionListener
      (reify ActionListener
        (actionPerformed [_ _] (f))))))

;; menus

(defn add-menu-item!
  ([menu item-name key-mnemonic key-accelerator f]
    (let [menu-item (JMenuItem. item-name)]  
      (when key-accelerator
        (.setAccelerator menu-item (get-keystroke key-accelerator)))
      (when (and (not (is-mac)) key-mnemonic)
        (.setMnemonic menu-item (.getKeyCode (get-keystroke key-mnemonic))))
      (.addActionListener menu-item
                          (reify ActionListener
                            (actionPerformed [this action-event]
                                             (f))))
      (.add menu menu-item)))
  ([menu item]
    (condp = item
      :sep (.addSeparator menu))))
  
(defn add-menu!
  "Each item-tuple is a vector containing a
  menu item's text, mnemonic key, accelerator key, and the function
  it executes."
  [menu-bar title key-mnemonic & item-tuples]
  (let [menu (JMenu. title)]
    (when (and (not (is-mac)) key-mnemonic)
      (.setMnemonic menu (.getKeyCode (get-keystroke key-mnemonic))))
    (doall (map #(apply add-menu-item! menu %) item-tuples))
    (.add menu-bar menu)
    menu))

;; mouse

(defn on-click [comp num-clicks f]
  (.addMouseListener comp
    (proxy [MouseAdapter] []
      (mouseClicked [event]
        (when (== num-clicks (.getClickCount event))
          (.consume event)
          (f))))))


;; file handling
(defn file-suffix [^File f]
  (when-lets [name (.getName f)
             last-dot (.lastIndexOf name ".")
             suffix (.substring name (inc last-dot))]
    suffix))


(defn get-directories [path]
  (filter #(and (.isDirectory %)
                (not (.startsWith (.getName %) ".")))
          (.listFiles path)))

(defn file-exists? [^String file]
  (and file (.. (File. file) exists)))

    
(defn confirmed? [question title]
  (= JOptionPane/YES_OPTION
     (JOptionPane/showConfirmDialog
       nil question title  JOptionPane/YES_NO_OPTION)))

(defn ask-value [question title]
  (JOptionPane/showInputDialog nil question title JOptionPane/QUESTION_MESSAGE))

(defn sha1-str [obj]
   (let [bytes (.getBytes (with-out-str (pr obj)))] 
     (String. (.digest (MessageDigest/getInstance "MD") bytes))))

;; streams, writers and readers
 
(defn printstream-to-writer [writer]
  (->
    (proxy [OutputStream] []
      (write
        ([^bytes bs offset length]
          (.write writer
                  (.toCharArray (String. ^bytes bs "utf-8"))
                  offset length))
        ([b]
          (.write writer b)))
      (flush [] (.flush writer))
      (close [] (.close writer)))
    (PrintStream. true)))

(defn process-reader
  "Create a buffered reader from the output of a process."
  [process]
  (-> process
      .getInputStream
      InputStreamReader.
      BufferedReader.))

(defn copy-input-stream-to-writer
  "Continuously copies all content from a java InputStream
   to a java Writer. Blocks until InputStream closes."
  [input-stream writer] 
  (let [reader (InputStreamReader. input-stream)]
    (loop []
      (let [c (.read reader)]
        (when (not= c -1)
          (.write writer c)
          (recur))))))

;; .clj file in current jar

(defn local-clj-source
  "Reads a clj source file inside a jar from the current classpath."
  [clj-file]
  (try
    (-> (Thread/currentThread)
        .getContextClassLoader
        (.getResource clj-file)
        .toString
        java.net.URL.
        slurp)
    (catch Exception _ nil)))

;; OS-specific utils

(defn enable-mac-fullscreen!
  "Shows the Mac full-screen double arrow, as introduced in
   OS X Lion, if possible."
  [window]
    (when (is-mac)
      (let [enable (static-method
                     "com.apple.eawt.FullScreenUtilities"
                     "setWindowCanFullScreen"
                     java.awt.Window
                     Boolean/TYPE)]
        (enable window true))))