; Conversion tools to work with two file systems.
(ns app.chfile
  (:require [javac.file :as jfile]))

(def child-folder "./../reclurChildIteration") ; DONT put this inside our folder, since we copy between them.
(def us-folder ".")
(defn child2us [child-file] (str us-folder (subs child-file (count child-folder)))) ; global to local in a sense.
(defn us2child [us-file] (str child-folder (subs us-file (count us-folder))))

(def we-are-child? false) ; is set to true when copying us to the child, and false otherwise. A kludge.

(defn uptodate-folder [] (if we-are-child? us-folder child-folder)) ; the child folder it the most uptodate.