; Handles the array of files and folders.
; Most of the functions are designed to operate on the jtree inside the scrollpane.

(ns clooj.app.filetree
  (:require [clooj.java.file :as jfile]
    [clooj.java.widget :as widget]
    [clooj.collections :as collections]
    [clooj.coder.repl :as repl]))

(defn ensure-file-open [jtree file]
  "Ensures that a given file is open and selected. Do this every time the tree changes."
  (if file
    (let [this-level (if (= file (:filepath jtree))
                       (do ;(println "expanded and selected: " file)
                          (assoc jtree :Expanded? true :Selected? true))
                       (assoc jtree :Selected? false))]
      (if (:Children this-level) 
         (assoc this-level :Children (collections/cmap :vals #(ensure-file-open % file) (:Children this-level)))
          this-level))) ; no children to recursively operate on.
    jtree) ; nil file -> no change.

(defn update-tree-keep-settings [jtree-old jtree-new]
  "Updates tree-old's structure to reflect tree-new, but tries to keep the settings of tree-new."
  (let [cha (widget/align-children jtree-old jtree-new)
        ; the new children:
        ch1 (reduce #(let [c0 (get-in cha [%2 0]) c1 (get-in cha [%2 1])]
                        (cond (or (= c0 c1) (and c1 (not c0))) (assoc %1 %2 c1)
                              (and c0 c1) (assoc %1 %2 (update-tree-keep-settings c0 c1))
                              (not c1) %1 :else (throw (Exception. "This shouldn't happen..."))))
              {} (keys cha))
        this-aligned (if (= (:filepath jtree-old) (:filepath jtree-new)) jtree-old jtree-new)]
   (if (:Children jtree-new) (assoc this-aligned :Children ch1) this-aligned)))

(defn get-file-user-selected [jtree value-changed-e]
  "Gets the filename from a value-changed event."
  (let [p (first (:jtree-descendents-selected value-changed-e))
        filepath (get-in jtree (concat p [:filepath]))] (str filepath))) ; str conversion necessary?

(defn read-jtree []
  "Gets the files in JTree form. This function is impure because it uses the disk."
  (let [ftree (jfile/filetree "." (fn [_] true))
        convert (fn fc [branch] 
                  (let [x {:Type 'JTree :Text (:leaf branch) :filepath (:local branch)}]
                    (let [folders (if (> (count (:children branch)) 0)
                                    (mapv fc (:children branch)) [])
                          files-and-folders (mapv #(hash-map :Type 'JTree :Text %2 :filepath %1)
                                  (:ch-local branch) (:ch-leaf branch))
                          files (filterv #(jfile/file? (:filepath %)) files-and-folders)
                          filders-v (into [] (concat folders files))
                          filders-k (into [] (concat (mapv #(str "F" (:Text %)) folders) (mapv #(str "f" (:Text %)) files)))
                          filders (zipmap filders-k filders-v)]
                      (if (> (count filders) 0)
                          (assoc x :Children filders) x))))]
   (convert ftree)))

(defn update-for-file-changes [jtree-old]
  "update-tree-keep-settings on the new file tree.
   This keeps settings (i.e. files selected) on the old tree.
   TODO: is there any way that's better than this slow O(n) step?"
  (update-tree-keep-settings jtree-old (read-jtree)))
   
(defn build []
   "The jtree simply goes into a scrollpane."
   (let [jtree (read-jtree)]
     {:Type 'JScrollPane
      :Children 
      [jtree]}))

(defn get-diff [jt-old jt-new]
  "List of differences between the two trees. Changes are represented a strings.
   This function acts recursively."
  (let [fo (:filepath jt-old) fn (:filepath jt-new)
        this-change (not= fo fn)]
    (if this-change 
      [(cond (and fo fn) (str "rename: " fo "->" fn) ; our current tree should not capture renames though.
           (not fo) (str "add: " fn)
           (not fn) (str "remove: " fo)
           :else "bizzaraxtioc")]
     (into [] (apply concat (mapv #(apply get-diff %) (vals (widget/align-children jt-old jt-new))))))))

(defn get-jtree [s] 
  "Like OO coding with getters and setters."
  (get-in s [:Children 0]))

(defn set-jtree [s jtree] 
  (assoc-in s [:Children 0] jtree))
  
(defn get-view [s] (:View s))

(defn set-view [s view] (assoc s :View view))