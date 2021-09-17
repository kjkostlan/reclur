; A simplified abstraction interface for working with Files.
; Most functions work with strings here.
; TODO: not sure about concurrency.
; TODO: is there a better way to handle disk mutations?
; TODO: normalize the api as to whether to give an error on missing files, etc.

(ns javac.file
  (:require [clojure.set :as set] [clojure.string :as string] [globals])
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
        cs (map + cins couts)]
    (if (= n 0)
      []
      (reduce #(conj %1 (+ (last %1) %2)) [(nth couts 0)] (rest cs)))))

(defn unwindoze [str-or-strs]
  (cond (string? str-or-strs)
    (string/replace str-or-strs "\\" "/")
    (vector? str-or-strs) (mapv unwindoze str-or-strs)
    (sequential? str-or-strs) (map unwindoze str-or-strs)
    :else (throw (Exception. "This fn needs a str or vector of strs."))))

;;;;;;;;;;;;;;;;;;;;; Various types of files:

; is .clj file and is folder. Yes this could be abstracted to save a couple of lines.
(defn _clj? [^File file] (and (not (.isDirectory file)) (.endsWith (.getName file) ".clj")))
(defn _java? [^File file] (and (not (.isDirectory file)) (.endsWith (.getName file) ".java")))
(defn _texty? [^File file] (and (not (.isDirectory file))
  (or (.endsWith (.getName file) ".clj") (.endsWith (.getName file) ".java") (.endsWith (.getName file) ".js")
      (.endsWith (.getName file) ".txt") (.endsWith (.getName file) ".text"))))
(defn _folder? [^File file] (.isDirectory file))
(defn clj? [file] (_clj? (File. file)))
(defn java? [file] (_java? (File. file)))
(defn texty? [file] (_texty? (File. file)))
(defn folder? [file] (_folder? (File. file)))
(defn file? [file] (.isFile (File. file)))
(defn exists? [file] (.exists (File. file)))

;;;;;;;;;;;;;;;;;;;;; Trees:

(defn _visible-children-file-obj ; accepts a File obj and returns a vector of File objs.
  [^File file]
  (->> (.listFiles file)
       (remove #(.startsWith (.getName %) "."))
       (remove #(.endsWith (.getName %) "~"))
       vec))

(defn visible-children-leaf-names [file] ; Returns the leaf name.
  (mapv #(let [^File f %] (unwindoze (.getName f))) (_visible-children-file-obj (File. file))))

(defn visible-children-full-names [file] ; Returns the full path.
  (mapv #(let [^File f %] (unwindoze (.getCanonicalPath f))) (_visible-children-file-obj (File. file))))

(defn visible-children [folder full-path?]
  "Get a vector of a directory's children, if there are any.
   Omits hidden and temporary files.
   Works with both absolute and relative paths."
  (let [files (_visible-children-file-obj (File. folder))]
    (mapv #(let [^File f %] (unwindoze (if full-path? (.getCanonicalPath %) (.getName %))))
      files)))

(defn all-files-inside [folder]
  "Every file inside a given folder."
  (let [ch (visible-children folder true)
        pieces (mapv #(if (folder? %) (all-files-inside %) [%]) ch)]
    (into [] (apply concat pieces))))

;;;;;;;;;;;;;;;;;;;;;; environment:

(def ^:dynamic *file-safety?* true)

(defn absolute-project-folder []
  "Gets the absolute project folder, the reclur folder's full path."
  (unwindoze (System/getProperty "user.dir")))

(defn absolute-path [file]
  "Simplifies it as much as possible."
  (unwindoze (.getCanonicalPath ^File (File. file))))

(defn assert-in-our-folders [file]
  "Safety feature to ensure we only modify our own directories, which is either our reclur directory or the child iteration thereof."
  (if *file-safety?*
    (let [fullpath (absolute-path file)
          us-folder (absolute-path (globals/get-working-folder))
          contained-in? (fn [folder] (.startsWith fullpath folder))]
      (if (contained-in? us-folder) true
        (throw (Exception. (str file " = " fullpath " is not in our project folder. It may be safe/desirable to do so, but be careful before removing this error!")))))))

;;;;;;;;;;;;;;;;;;;;;; conversion functions:

(defn str-to-regex
  "sometimes you don't need a fancy regular expression. This makes a regexp that is equivalent to a non-regexp match of str.
   http://stackoverflow.com/questions/28342570/how-to-split-a-string-in-clojure-not-in-regular-expression-mode"
  [c]
  (re-pattern (java.util.regex.Pattern/quote (str c))))

(defn file2dotpath [file]
  "./src/clooj/java/file.clj into clooj.java.file, etc"
  (string/replace (subs file 6 (- (count file) 4)) "/" "."))

(defn dotpath2file [dotpath]
  "inverse of file2dotpath."
  ; TODO: rename and possibly change these two fns since we return a string representation.
  (str "./" "src/" (string/replace dotpath "." "/") ".clj"))

(defn file2folder [file]
  "extracts the folder that the file (which is NOT a folder) is in."
  (let [match (last (re-index (str-to-regex "/") file))]
    (if (nil? match)
      file ; no change.
      (subs file 0 match))))

(defn full-to-leaf [file]
  (last (string/split file (str-to-regex "/"))))

(defn full-to-local [file]
  "Does nothing for files already local apart from ./ formatting."
  (let [file (unwindoze (.getCanonicalPath ^File (File. file)))
        root (unwindoze (.getCanonicalPath ^File (File. ".")))
        _ (if (or (< (count file) (count root))
                (not= (subs file 0 (count root)) root))
            (throw (Exception. "Need to use .. for local TODO")))]
    (str "." (subs file (count root)))))

(defn local-to-full [file]
  "Does nothing for files already fullpath execpt for formatting."
  (unwindoze (.getCanonicalPath ^File (File. file))))

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
(defn open [file]
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

(defn rename!! [file-old file-new]
  (assert-in-our-folders file-old) (assert-in-our-folders file-new)
  (let [^File f0 (File. file-old) ^File f1 (File. file-new)] (.renameTo f0 f1)))

(defn delete!! [file]
  "File or folder, all contents in folder (if it is a folder) are also deleted just like GUI delete."
  (assert-in-our-folders file)
  (let [^File f (File. file)]
    (cond (.isDirectory f) (FileUtils/deleteDirectory f)
      (.isFile f) (.delete f)
      :else (throw (Exception. (str "File isn't a file or folder. " file))))))

(defn save!! [file contents & is-folder?]
  "UTF-8 txt and option to make folder instead of file (ignoring the contents)."
  (assert-in-our-folders file)
  (try (if (first is-folder?)
         (.mkdirs (File. file))
         (let [folder-str (file2folder file)
               _ (if (and (exists? folder-str) (not (folder? folder-str)))
                   (delete!! folder-str))
               ^File folder (File. folder-str)]
           (.mkdirs folder)
           (with-open [writer (BufferedWriter. (OutputStreamWriter. (FileOutputStream. (File. file)) "UTF-8"))]
                      (.write writer (str contents)))))
  (catch Exception e
    (println "Save failed:" (.getMessage e)))))

(defn get-last-modified [file]
  (.lastModified (let [^File f (File. file)] f)))

; reverts to a buffered version, if we have one. Also returns the reverted string.
(defn revert!! [file]
  (assert-in-our-folders file)
  (let [old (get-buffer file)]
    (if (nil? old)
      (do (println "unable to revert file, maybe it fell off the finite buffer.") "")
      (do (save!! file old) old))))

(defn _delete-missing!! [ref-folder target-folder ignore-git?]
  "Deletes files that are in the ref-folder but not in the target-folder. Acts recursively."
  (let [ch-ref (apply hash-set (visible-children ref-folder false))
        ch-tgt (apply hash-set (visible-children target-folder false))
        ff (fn [x] (if ignore-git? (filterv #(not= % ".git") x) x))
        common (ff (set/intersection ch-ref ch-tgt))
        deletes (ff (set/difference ch-tgt ch-ref))]
    (mapv #(delete!! (str target-folder "/" %)) deletes)
    (mapv #(let [file-ref (str ref-folder "/" %) folder-r? (folder? file-ref)
                 file-tgt (str target-folder "/" %) folder-t? (folder? file-tgt)]
             (if (and folder-r? folder-t?)
               (_delete-missing!! file-ref file-tgt ignore-git?))) common)))

(defn copy!! [orig dest & dot-git-kludge]
  "Copies a file/folder from origin to destination, overwriting any data.
   For folders, removes files/folders in the dest that aren't in orig.
  dot-git-kludge fixes a strange not file-not-found error in the .git that I don't understand."
  (assert-in-our-folders orig) (assert-in-our-folders dest)
  (let [^File origf (File. orig) ^File destf (File. dest) ignore-git? (boolean (first dot-git-kludge))]
    (if (and ignore-git? (not= orig (globals/get-working-folder)))
      (let [f1 (str orig "/.git")] (if (exists? f1) (delete!! f1))))
    (cond (.isDirectory origf) (do (FileUtils/copyDirectory origf destf) (_delete-missing!! orig dest ignore-git?))
      (.isFile origf) (FileUtils/copyFile origf destf)
      :else (throw (Exception. "Original file isn't a file or folder ... somehow.")))))