; A simplified abstraction interface for working with Files.
; Most functions work with strings here.

(ns clooj.java.file
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clooj.utils :as utils])
  (:import (java.io File BufferedWriter OutputStreamWriter FileOutputStream)
           (javax.swing JOptionPane)
           (java.nio.file Files Paths)
           (java.nio.charset StandardCharsets)
           (java.util.prefs Preferences)))

; stupid OS difference:
(defn sep [] (File/separator))

(defn is-dir [^String file] ; No, type hints are not always evil, these are strings.
  (.isDirectory (File. file)))

(defn is-file [^String file]
  (.isFile (File. file)))

; conversion functions:

(defn file2namespace [^String file]
  "./src/clooj/java/file.clj into clooj.java.file, etc"
  (string/replace (subs file 6 (- (count file) 4)) (sep) "."))

(defn namespace2file [^String namesp]
  "inverse of file2namespace."
  (str "." (sep) "src" (sep) (string/replace namesp "." (sep)) ".clj"))

(defn file2folder [^String file]
  "extracts the folder that the file (which is NOT a folder) is in."
  (let [match (last (utils/re-index (utils/str-to-regex (sep)) file))]
    (if (nil? match) 
      file ; no change.
      (subs file 0 match))))

; store files in memory that we can revert:
(def _buffer-size 0)
(def _max-buffer-size 50000000) ; maximum buffer size.
(def _buffers []) ; new -> old.
(defn get-buf-size [] (reduce + (map #(count (:contents %)) _buffers)))
(defn get-buffer [file]
  (let [match (filter #(= (:name %) file) _buffers)]
    (if (= (count match) 0) nil (:contents (nth match 0)))))

(defn exists? [^String file] 
  "Make sure you check for existance before creating new files!"
  (.exists (File. file)))

; Save and load, simplified with strings. Use ./src/foo/bar/etc to save your .clj files.
;http://stackoverflow.com/questions/326390/how-to-create-a-java-string-from-the-contents-of-a-file
; Technically load mutates ourselves but the end-user does not really need to know.
(defn load-textfile [^String file]
  (try (let [out (String. (Files/readAllBytes (Paths/get file (into-array [""]))) (StandardCharsets/UTF_8))
             n (count out) ntot (get-buf-size)]
         (if (and (<= ntot _max-buffer-size) (nil? (get-buffer file)))
           (do (def _buffers (into [] (concat [{:name file :contents out}] _buffers))) ; add to beginnning.
             ; clean up old files if we get too large:
             (while (> (get-buf-size) _max-buffer-size) (def _buffers (pop _buffers))))) ;remove from end.
             out)
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

(defn delete-file!!! [^String file]
  "Remember to always prompt the user fist."
  (.delete (File. file)))

; reverts to a buffered version, if we have one. Also returns the reverted string.
(defn revert-textfile!!! [^String file]
  (let [old (get-buffer file)]
    (if (nil? old)
      (do (JOptionPane/showMessageDialog nil
       (str "Unable to revert file (we limit the old file buffer storage, that may be the issue): " file)
        "Oops" JOptionPane/ERROR_MESSAGE) "")
      (do (save-textfile!!! file old) old))))

; gets the src directory (string) that should contain both our project and the editor's project:
(defn src-directory [] "./src") ; we simply use locals.

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

(defn _filetree [^File folder filt]
  "Makes a tree of folders/files starting from a folder, only including files with a filter function."
  (let [ch (visible-children folder) ft (filter filt ch)] ; File objects.
    { ; doall's so we leave java-object land asap.
      :ch-local (mapv local-path ft)
      :ch-absolute (mapv #(.getAbsolutePath %) ft)
      :ch-leaf (mapv #(.getName %) ft)
      :children (mapv #(_filetree % filt) (filter _dir? ch)) ; recursive.
      :full (.getAbsolutePath folder)
      :local (local-path folder)
      :leaf (.getName folder)}))

; TODO: this is not the most convienent format because of the leaf.
(defn filetree [folder filt] (_filetree (File. folder) filt))

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
