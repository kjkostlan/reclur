; A simplified abstraction interface for working with Files.
; Most functions work with strings here.
; Functional files:
   ; There is a *disk* variable which keeps track of the disk.
   ;TODO
; TODO: maybe get rid of the timestamp functions, if they are only called upon when we load.
 ; not sure what happens if the disk is modified during the multicomp/save file, but as long as one save is free of other changes this should correct any errors.

(ns javac.file
  (:require [clojure.java.io :as io]
            [clojure.string :as string])
  (:import (java.io File BufferedWriter OutputStreamWriter FileOutputStream)
           (javax.swing JOptionPane)
           (org.apache.commons.io FileUtils)
           (java.nio.file Files Paths)
           (java.nio.charset StandardCharsets)))

;;;;;;;;;;;;;;;;;;;;; Misc stuff

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


;;;;;;;;;;;;;;;;;;;;;; environment:

(defn sep [] (File/separator))

(defn absolute-project-folder []
  "Gets the absolute project folder, the reclur folder's full path."
  (System/getProperty "user.dir"))

; gets the src directory (string) that should contain both our project and the editor's project:
(defn src-directory [] "./src") ; we simply use locals for the old app.

;;;;;;;;;;;;;;;;;;;;;; conversion functions:

(defn str-to-regex
  "sometimes you don't need a fancy regular expression. This makes a regexp that is equivalent to a non-regexp match of str.
   http://stackoverflow.com/questions/28342570/how-to-split-a-string-in-clojure-not-in-regular-expression-mode"
  [c]
  (re-pattern (java.util.regex.Pattern/quote (str c))))

(defn file2dotpath [^String file]
  "./src/clooj/java/file.clj into clooj.java.file, etc"
  (string/replace (subs file 6 (- (count file) 4)) (sep) "."))

(defn dotpath2file [^String dotpath]
  "inverse of file2dotpath."
  ; TODO: rename and possibly change these two fns since we return a string representation.
  (str "." (sep) "src" (sep) (string/replace dotpath "." (sep)) ".clj"))

(defn file2folder [^String file]
  "extracts the folder that the file (which is NOT a folder) is in."
  (let [match (last (re-index (str-to-regex (sep)) file))]
    (if (nil? match) 
      file ; no change.
      (subs file 0 match))))

(defn full-to-leaf [^String file]
  (last (string/split file (str-to-regex (sep)))))


;;;;;;;;;;;;;;;;;;;; Saving and loading, etc:

; store files in memory that we can revert:
(def _buffer-size 0)
(def _max-buffer-size 50000000) ; maximum buffer size.
(def _buffers []) ; new -> old.
(defn get-buf-size [] (reduce + (map #(count (:contents %)) _buffers)))
(defn get-buffer [file]
  (let [match (filter #(= (:name %) file) _buffers)]
    (if (= (count match) 0) nil (:contents (nth match 0)))))

; Save and load, simplified with strings. Use ./src/foo/bar/etc to save your .clj files.
;http://stackoverflow.com/questions/326390/how-to-create-a-java-string-from-the-contents-of-a-file
; Technically load mutates ourselves but the end-user does not really need to know.
(defn open [^String file]
  "False when the file doesn't exist."
  (try (let [out (String. (Files/readAllBytes (Paths/get file (into-array [""]))) (StandardCharsets/UTF_8))
             n (count out) ntot (get-buf-size)]
         (if (and (<= ntot _max-buffer-size) (nil? (get-buffer file)))
           (do (def _buffers (into [] (concat [{:name file :contents out}] _buffers))) ; add to beginnning.
             ; clean up old files if we get too large:
             (while (> (get-buf-size) _max-buffer-size) (def _buffers (pop _buffers))))) ;remove from end.
             out)
  (catch Exception e
    false ;(println (.getMessage e))
    ; DANGEROUS (can crash computer with super many windows): (JOptionPane/showMessageDialog nil (str "Unable to load file: " file) "Oops" JOptionPane/ERROR_MESSAGE)
    )))

(defn save!!! [^String file ^String contents] ; three ! means that the disk is mutated.
  (try (do
         ; check for folder: 
         (let [folder (File. (file2folder file))]
           (.mkdirs folder))
         (with-open [writer (BufferedWriter. (OutputStreamWriter. (FileOutputStream. (File. file)) "UTF-8"))]
                    (.write writer contents)))     
  (catch Exception e
    (println (.getMessage e))
    ; DANGEROUS (JOptionPane/showMessageDialog nil (str "Unable to save file: " file) "Oops" JOptionPane/ERROR_MESSAGE)
    )))

(defn rename!!! [^String file-old ^String file-new]
  (let [^File f0 (File. file-old) ^File f1 (File. file-new)] (.renameTo f0 f1)))

(defn delete!!! [^String file]
  "File or folder, all contents in folder (if it is a folder) are also deleted just like GUI delete."
  (let [^File f (File. file)]
    (cond (.isDirectory? f) (FileUtils/deleteDirectory f)
      (.isFile f) (.delete f)
      :else (throw (Exception. "File isn't a file or folder.")))))

(defn get-last-modified [^String file]
  (.lastModified (let [^File f (File. file)] f)))

; reverts to a buffered version, if we have one. Also returns the reverted string.
(defn revert!!! [^String file]
  (let [old (get-buffer file)]
    (if (nil? old)
      (do (println "unable to revert file, maybe it fell off the finite buffer.") #_(JOptionPane/showMessageDialog DANGEROUS nil
       (str "Unable to revert file (we limit the old file buffer storage, that may be the issue): " file)
        "Oops" JOptionPane/ERROR_MESSAGE) "")
      (do (save!!! file old) old))))

;;;;;;;;;;;;;;;;;;;;; Various types of files:

; is .clj file and is folder. Yes this could be abstracted to save a couple of lines.
(defn _clj? [^File file] (and (not (.isDirectory file)) (.endsWith (.getName file) ".clj")))
(defn _java? [^File file] (and (not (.isDirectory file)) (.endsWith (.getName file) ".java")))
(defn _texty? [^File file] (and (not (.isDirectory file))
  (or (.endsWith (.getName file) ".clj") (.endsWith (.getName file) ".java") (.endsWith (.getName file) ".js")
      (.endsWith (.getName file) ".txt") (.endsWith (.getName file) ".text"))))
(defn _dir? [^File file] (.isDirectory file))
(defn clj? [^String file] (_clj? (File. file)))
(defn java? [^String file] (_java? (File. file)))
(defn texty? [^String file] (_texty? (File. file)))
(defn dir? [^String file] (_dir? (File. file)))
(defn file? [^String file] (.isFile (File. file)))
(defn exists? [^String file] 
  "Make sure you check for existance before creating new files!"
  (.exists (File. file)))

;;;;;;;;;;;;;;;;;;;;; Trees:

(defn _visible-children-file-obj1 ; an ugly fn that accepts a File obj and returns a vector of strings.
  [^File file]
  (->> (.listFiles file)
       (remove #(.startsWith (.getName %) "."))
       (remove #(.endsWith (.getName %) "~"))
       vec))
(defn _visible-children-file-obj [^String file] ; also ugly in the same way as above.
  (mapv #(let [^File f %] (.getName f)) (_visible-children-file-obj1 (File. file))))   

(defn visible-children [folder full-path?]
  "Get a vector of a directory's children, if there are any.
   Omits hidden and temporary files.
   Works with both absolute and relative paths."
  (mapv #(let [^File f (File. %)] (if full-path? (.getAbsolutePath f) (.getName f))) (_visible-children-file-obj folder)))

(defn _local-path [^File file]
 "converts a file object into local-path string"
 (str "." (sep) (.getPath (.relativize (.toURI (File. ".")) (.toURI file)))))

(defn _filetree [^File folder filt]
  "Makes a tree of folders/files starting from a folder, only including files with a filter function."
  (let [ch (_visible-children-file-obj1 folder) ft (filter filt ch)] ; File objects.
    { ; doall's so we leave java-object land asap.
      :ch-local (mapv _local-path ft)
      :ch-absolute (mapv #(.getAbsolutePath %) ft)
      :ch-leaf (mapv #(.getName %) ft)
      :children (mapv #(_filetree % filt) (filter _dir? ch)) ; recursive.
      :full (.getAbsolutePath folder)
      :local (_local-path folder)
      :leaf (.getName folder)}))

; TODO: this is not the most convienent format because of the leaf.
(defn filetree [^String folder filt] (_filetree (File. folder) filt))

; sets up the tree of .clj files for the current project.
; Format: :clj (.clj files as strings), :children (more src-trees) :full full-path filename :leaf leaf-filename.
(defn get-clj-tree [] (filetree (src-directory) _clj?))
(defn get-texty-tree [] (filetree (src-directory) _texty?))

(defn _get-file-list [grow tree local?]
  ; grow keeps track of the growing list.
  (let [thislevel (reduce #(conj %1 %2) grow (if local? (:ch-local tree) (:ch-absolute tree)))]
    (concat thislevel (apply concat (map #(_get-file-list grow % local?) (:children tree))))))
(defn get-clj-files []
  "gets a list of src-files, local paths."
  (into [] (_get-file-list [] (get-clj-tree) true)))

(defn get-texty-files []
  "gets a list of src-files, local paths."
  (into [] (_get-file-list [] (get-texty-tree) true)))

(defn get-all-subfiles [absolute-path]
  "All sub-file leafs from this path, absolute paths."
  (into [] (_get-file-list [] (filetree absolute-path (fn [_] true)) false)))

;;;;;;; Timestamp functions ;;;;;;

(defn load-timestamp [^String file]
  "Loads a textfile, returning {:text :last-modified}.
   Keeps loading the file until the :last-modified date doesn't change before vs after.
   I think this is concurrent safe, in that it can't give back an old version without an updated modification date."
  (let [a (atom {})]
    (while (let [d0 (get-last-modified file) txt (load file) d1 (get-last-modified file)]
             (reset! a {:text txt :last-modified d1}) (not= d0 d1))) @a))

(defn rename-timestamp!!! [^String file-old ^String file-new]
  "Renames the file OR folder, returning the new date modified.
   TODO: how to handle concurrency ???"
  (let [^File f0 (File. file-old) ^File f1 (File. file-new)] (.renameTo f0 f1)
    (.get-last-modified f1)))

(defn save-timestamp!!! [^String file ^String contents]
  "Keeps saving a file until it is up-to-date, and returns the modification timestamp.
   Again, concurrancy-safe to make sure that if it can't have an up-to-date stamp without bieng the up-to-date file."
  (let [a (atom 0)]
    (while (let [_ (save!!! file contents)
                 t0 (get-last-modified file) _ (reset! a t0)
                 val (load file)] (not= val contents))) @a))