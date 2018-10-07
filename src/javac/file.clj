; A simplified abstraction interface for working with Files.
; Most functions work with strings here.
; TODO: not sure about concurrency.
; TODO: is there a better way to handle disk mutations?
; TODO: normalize the api as to whether to give an error on missing files, etc.

(ns javac.file
  (:require [clojure.set :as set] [clojure.string :as string]
            [app.chfile :as chfile])
  (:import (java.io File BufferedWriter OutputStreamWriter FileOutputStream)
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

;;;;;;;;;;;;;;;;;;;;; Various types of files:

; is .clj file and is folder. Yes this could be abstracted to save a couple of lines.
(defn _clj? [^File file] (and (not (.isDirectory file)) (.endsWith (.getName file) ".clj")))
(defn _java? [^File file] (and (not (.isDirectory file)) (.endsWith (.getName file) ".java")))
(defn _texty? [^File file] (and (not (.isDirectory file))
  (or (.endsWith (.getName file) ".clj") (.endsWith (.getName file) ".java") (.endsWith (.getName file) ".js")
      (.endsWith (.getName file) ".txt") (.endsWith (.getName file) ".text"))))
(defn _folder? [^File file] (.isDirectory file))
(defn clj? [^String file] (_clj? (File. file)))
(defn java? [^String file] (_java? (File. file)))
(defn texty? [^String file] (_texty? (File. file)))
(defn folder? [^String file] (_folder? (File. file)))
(defn file? [^String file] (.isFile (File. file)))
(defn exists? [^String file] (.exists (File. file)))

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

;;;;;;;;;;;;;;;;;;;;;; environment:

(defn sep [] (File/separator))

(def ^:dynamic *file-safety?* true)

(defn absolute-project-folder []
  "Gets the absolute project folder, the reclur folder's full path."
  (System/getProperty "user.dir"))

(defn absolute-path [^String file]
  "Simplifies it as much as possible."
  (.getCanonicalPath ^File (File. file)))

(defn assert-in-our-folders [file]
  "Safety feature to ensure we only modify our own directories, which is either our reclur directory or the child iteration thereof."
  (if *file-safety?*
    (let [^String fullpath (absolute-path file)
          us-folder (absolute-path chfile/us-folder)
          ch-folder (absolute-path chfile/child-folder)
          contained-in? (fn [^String folder] (.startsWith fullpath folder))]
      (if (or (contained-in? us-folder) (contained-in? ch-folder)) true
        (throw (Exception. (str file " = " fullpath " is not in either us or the child reclur folder. It may be safe to do so, but be careful before removing this error!")))))))

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

; store files in memory that we can revert (we currently don't use this feature but may be useful):
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
    )))

(defn save!!! [^String file ^String contents] ; three ! means that the disk is mutated.
  (assert-in-our-folders file)
  (try (do
         ; check for folder: 
         (let [folder (File. (file2folder file))]
           (.mkdirs folder))
         (with-open [writer (BufferedWriter. (OutputStreamWriter. (FileOutputStream. (File. file)) "UTF-8"))]
                    (.write writer contents)))     
  (catch Exception e
    (println (.getMessage e))
    )))

(defn rename!!! [^String file-old ^String file-new]
  (assert-in-our-folders file-old) (assert-in-our-folders file-new)
  (let [^File f0 (File. file-old) ^File f1 (File. file-new)] (.renameTo f0 f1)))

(defn delete!!! [^String file]
  "File or folder, all contents in folder (if it is a folder) are also deleted just like GUI delete."
  (assert-in-our-folders file)
  (let [^File f (File. file)]
    (cond (.isDirectory f) (FileUtils/deleteDirectory f)
      (.isFile f) (.delete f)
      :else (throw (Exception. (str "File isn't a file or folder. " file))))))

(defn get-last-modified [^String file]
  (.lastModified (let [^File f (File. file)] f)))

; reverts to a buffered version, if we have one. Also returns the reverted string.
(defn revert!!! [^String file]
  (assert-in-our-folders file)
  (let [old (get-buffer file)]
    (if (nil? old)
      (do (println "unable to revert file, maybe it fell off the finite buffer.") "")
      (do (save!!! file old) old))))

(defn _delete-missing!!! [ref-folder target-folder ignore-git?]
  "Deletes files that are in the ref-folder but not in the target-folder. Acts recursively."
  (let [ch-ref (apply hash-set (visible-children ref-folder false))
        ch-tgt (apply hash-set (visible-children target-folder false))
        ff (fn [x] (if ignore-git? (filterv #(not= % ".git") x) x))
        common (ff (set/intersection ch-ref ch-tgt))
        deletes (ff (set/difference ch-tgt ch-ref))]
    (mapv #(delete!!! (str target-folder (sep) %)) deletes)
    (mapv #(let [file-ref (str ref-folder (sep) %) folder-r? (folder? file-ref)
                 file-tgt (str target-folder (sep) %) folder-t? (folder? file-tgt)]
             (if (and folder-r? folder-t?)
               (_delete-missing!!! file-ref file-tgt ignore-git?))) common)))

(defn copy!!! [^String orig ^String dest & dot-git-kludge]
  "Copies a file/folder from origin to destination, overwriting any data. 
   For folders, removes files/folders in the dest that aren't in orig.
  dot-git-kludge fixes a strange not file-not-found error in the .git that I don't understand."
  (assert-in-our-folders orig) (assert-in-our-folders dest)
  (let [^File origf (File. orig) ^File destf (File. dest) ignore-git? (boolean (first dot-git-kludge))]
    (if (and ignore-git? (not= orig chfile/us-folder))
      (let [f1 (str orig "/.git")] (if (exists? f1) (delete!!! f1))))
    (cond (.isDirectory origf) (do (FileUtils/copyDirectory origf destf) (_delete-missing!!! orig dest ignore-git?))
      (.isFile origf) (FileUtils/copyFile origf destf)
      :else (throw (Exception. "Original file isn't a file or folder ... somehow.")))))