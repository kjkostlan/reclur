; Handles both the editor box and the file tree. 
; Needs to access several paths from the root state.
; This is done by having the root state hold :id-paths, which
; tells us where each id is. We just have to pass in keys within id-paths.
; Many functions operate on the global gui state using keywords.
  ; In this case s is the state.

(ns clooj.app.editframe
  (:require [clooj.java.file :as jfile]
    [clooj.java.widget :as widget]
    [clooj.app.filetree :as filetree]
    [clooj.app.editbox :as editbox]
    [clooj.collections :as collections]
    [clooj.coder.repl :as repl]
    [clooj.app.uicomp :as uicomp]))

(defn default-frame [text]
  {:View [0 0] :Text text :filename "" :Caret 0})

; TODO: anti-memory leak finite frame storage? 
(defn build []
  {:frame-ix 0 :ix-to-frame {0 (default-frame "Editor")}
   :filename-to-frame {}})

(defn alert-changes [s file-tree-key] 
   "Keeps s up to date with all the files, alerting the repl if there are any changes."
  (let [file-tree-path (get (:id-paths s) file-tree-key)
        t0 (filetree/get-jtree (get-in s file-tree-path))
        t1 (filetree/read-jtree) 
        t11 (filetree/update-tree-keep-settings t0 t1)
        s-new (update-in s file-tree-path #(filetree/set-jtree % t11)) ; update the JTree.
        changes (filetree/get-diff t0 t1)]
	; Every fyi text alert must use the repl:
	(if (> (count changes) 0)
	   (repl/add-to-repl!! (apply str "External files changed (rename = remove+add), these namespaces are NOT reloaded: " 
						     (mapv #(str % "\n") changes)) false)) ; false = don't evaluate.
	s-new))
	
(defn get-frame [s editbox-key filetree-key]
  "extracts the frame from the state."
  (let [editbox-path (get (:id-paths s) editbox-key)
        filetree-path (get (:id-paths s) filetree-key)
        d (default-frame (editbox/get-text (get-in s editbox-path)))
        d (if (:View s) (assoc d :View (:View (get-in s filetree-path))) d)
        d (assoc d :filename (:filename (get-in s filetree-path)))] d))

(defn set-frame [s frame editframe-key editbox-key filetree-key]
  "Sets what frame the state is on. Doesn't affect the frame ix, etc."
    (let [editframe-path (get (:id-paths s) editframe-key)
          editbox-path (get (:id-paths s) editbox-key)
          filetree-path (get (:id-paths s) filetree-key)]
      (-> s 
        (update-in editbox-path 
          #(-> % (editbox/set-text (:Text frame)) (editbox/set-view (:View frame))
             (editbox/set-caret (:Caret frame))))
        (update-in filetree-path 
          #(filetree/set-jtree % (filetree/ensure-file-open (filetree/get-jtree %) (:filename frame))))
        (assoc-in (concat editframe-path [:filename]) (:filename frame)))))

(defn prev-file [s editframe-key editbox-key filetree-key]
  "Go to the previous file if possible (returning the state)."
  (let [editframe-path (get (:id-paths s) editframe-key)
        frix (:frame-ix (get-in s editframe-path))]
    (if (> frix 0)
      (-> s
        (set-frame (get (get-in s editframe-path) (dec frix)) editframe-key editbox-key filetree-key)
        (assoc-in (concat editframe-path [:frame-ix]) (dec frix))) 
      s)))
      
(defn next-file [s editframe-key editbox-key filetree-key]
  "Go to the previous file if possible."
  (let [editframe-path (get (:id-paths s) editframe-key)
        frix (:frame-ix s)]
    (if (< (inc frix) (count (:index-to-frame (get-in s editframe-path))))
      (-> s
        (set-frame (get (get-in s editframe-path) (inc frix)) editframe-key editbox-key filetree-key)
        (assoc-in (concat editframe-path [:frame-ix]) (inc frix)))
      s)))

(defn set-file
  "Sets the state to the file, adding/modifying a frame and the jtree. Text is optional, it loads the file if no text is given."
  ([s fname editframe-key editbox-key filetree-key]
    (set-file s fname editframe-key editbox-key filetree-key (jfile/load-textfile fname)))
  ([s fname editframe-key editbox-key filetree-key text]
    (let [editframe-path (get (:id-paths s) editframe-key)
          editbox-path (get (:id-paths s) editbox-key)
          filetree-path (get (:id-paths s) filetree-key)
          f2f (:file-to-frame s) i2f (:index-to-frame s)
          ; Reuse old frames or create new ones:
          frame (if (get f2f fname) (get f2f fname) (default-frame (if (nil? text) (throw (Exception. "No text for a new file.")) text)))
          frame (assoc (if (nil? text) frame (assoc frame :Text text)) :filename fname)
          ix (:frame-ix s)]
      (-> s
        (set-frame frame editframe-key editbox-key filetree-key)
        (update-in (concat editframe-path [:frame-ix]) inc) ; add one to the index.s
        (update-in (concat editframe-path [:index-to-frame]) #(assoc % ix frame)) 
        (update-in (concat editframe-path [:file-to-frame]) #(assoc % fname frame))
        (update-in filetree-path #(filetree/set-jtree % (filetree/ensure-file-open (filetree/get-jtree %) fname)))))))

(defn get-wantsedit-file [text]
  "Gets the current filename from the (ns) macro. Only works for clojure files."
  (let [namesp (second (try (into [] (read-string text)) (catch Exception e nil)))]
    (if namesp (jfile/namespace2file (str namesp)))))

(defn save-file-with-reload!!! [filename text]
  "Saves the file, reloads the namespace. Returns the message.
   Gives an appropriate error message if no file exists."
  (if (> (count filename) 0) ; filename must be non-empty to save.
    (let [is-file? (jfile/file? filename)
          ; clj files must match between namespaces and java.
          clj? (jfile/clj? filename)
          _ (jfile/save-textfile!!! filename text) ; actually save the file.
          ns-conflict (if clj? (not= (get-wantsedit-file text) filename) false)    
          t0 (if is-file? "Saved: " "Created: ") 
          msg (if ns-conflict (str t0 filename " ERROR: Namespace declared in file is missing, invalid, or doesn't match the filename.")
                  (try (if clj? (do (repl/reload-file!! filename) (str "Saved: " filename " (no error)."))
                         (str "Saved: " filename " [not a .clj file, no compilation done]."))
                           (catch Exception e 
                             (str "Saved: " filename " Compile error: " e "\n (Error prevented downstream (re)definitions)."))))] msg)
    "No file open."))

(defn save-current-file!!! [s editframe-key editbox-key]
  "Saves the text into the current file, with reload.
   If no file is open it will save a new file."
  (let [editframe-path (get (:id-paths s) editframe-key)
        editbox-path (get (:id-paths s) editbox-key)]
    (save-file-with-reload!!! (get-in s (concat editframe-path [:filename])) 
      (editbox/get-text (get-in s editbox-path))) s))

(defn delete-current-file!!! [s editframe-key editbox-key filetree-key]
  "Deletes the file, clears the text, and deletes our frame associated with the file.
   TODO: jtree fixing."
  (let [editframe-path (get (:id-paths s) editframe-key)
        editbox-path (get (:id-paths s) editbox-key)
        filetree-path (get (:id-paths s) filetree-key)]
    (jfile/delete-file!!! (get-in s (concat editframe-path [:filename])))
    (-> s (assoc-in (concat editframe-path [:filename]) "") 
      (update-in editbox-path #(editbox/set-text % ""))
      (update-in filetree-path #(filetree/set-jtree % (filetree/update-for-file-changes (filetree/get-jtree %)))))))

(defn save-old-set-new-file!!! [s fname editframe-key editbox-key filetree-key]
  "Saves the old file so that changes are stored, then sets us to a new file."
  (-> s (save-current-file!!! editframe-key editbox-key)
    (set-file fname editframe-key editbox-key filetree-key)))

(defn get-filename [s editframe-key]
  "empty string = no file open."
  (let [editframe-path (get (:id-paths s) editframe-key)]
    (get-in s (concat editframe-path [:filename]))))

(defn add-events [s editframe-key editbox-key filetree-key]
  "Stuff the top-level needs to listen to."
  (uicomp/add-top-listeners
    {:below-valueChanged ; This event does a lot: saving and loading files as well as updating the textarea:
    (fn [s e o] 
      (let [old-file (get-filename s editframe-key)
            new-file (filetree/get-file-user-selected (filetree/get-jtree (get-in s (get (:id-paths s) filetree-key))) e)
            new-file? (and new-file (jfile/file? new-file))]
        (cond (and (not= old-file "") new-file?) ; change in file to a leaf file.
          (save-old-set-new-file!!! s new-file editframe-key editbox-key filetree-key) 
          (and (not= old-file new-file) new-file?) (set-file s new-file editframe-key editbox-key filetree-key)
          :else s)))}
      s))