; Conversion tools to work with two file systems.
(ns app.chfile)

(def child-folder "./../reclurChildIteration") ; DONT put this inside our folder, since we copy between them.
(def us-folder ".")

(defn child2us [child-file] (str us-folder (subs child-file (count child-folder)))) ; global to local in a sense.
(defn us2child [us-file] (str child-folder (subs us-file (count us-folder))))