; Code database functions, for reading the code, finding out what calls what, etc.
; The functions work with standard clojure stuff, this means symbols instead of vars or namespace objects, etc.
(ns coder.cbase
  (:require [clojure.repl] [clojure.string :as string]
    [javac.file :as jfile]
    [clojure.set :as set]))

;; Simple conversion fns.

(defn ns-of [qual-sym]
  "Splits the symbol-as-a-string."
  (if (.contains ^String (str qual-sym) "/")
    (symbol (re-find #"[A-Za-z\.*+!\-'?=:\_]+" (str qual-sym)))))

(defn unqual [qual-sym]
  "Unqualified symbol."
  (symbol (subs (str qual-sym) (inc (count (str (ns-of qual-sym)))))))

(defn resolved [ns-sym code-sym]
  "Resolves code-sym in ns-sym, which depends on the :requires, :imports, etc.
   Returns a fully qualified symbol."
  (let [ns-ob (find-ns ns-sym)
        _ (if (not ns-ob) (throw (Exception. (str "Namespace: " (pr-str ns-sym) " not found."))))
        var-ob (ns-resolve ns-ob code-sym)
        native-ns-ob (:ns (meta var-ob))]
    (if native-ns-ob
      (let [native-ns-sym (ns-name native-ns-ob)
            sym2var (ns-map native-ns-sym) 
            var2sym (zipmap (vals sym2var) (keys sym2var))
            unqualed (get var2sym var-ob)] 
        (if unqualed (symbol (str native-ns-sym "/" unqualed)))))))

(defn file2ns [file]
  "Converts a local filepath to a namespace symbol."
  (let [pieces (rest (rest (string/split file #"/")))
        st (string/join "." pieces)]
    (symbol (subs st 0 (- (count st) 4)))))

;; Extraction fns.

(defn defs [ns-sym]
  (into [] (keys (ns-interns ns-sym))))

(defn valid-symbols [ns-sym]
  "All recognized symbols of the namespace, with qualifications, etc.
   TODO: java imports."
 (let [x (ns-aliases ns-sym)
       as2ns (zipmap (keys x) (mapv ns-name (vals x)))
       standard (keys (ns-map 'clojure.core))
       native (defs ns-sym)
       foreign (apply concat
                 (mapv (fn [stub nms] (mapv #(symbol (str stub "/" %)) 
                                       (defs nms))) 
                   (keys as2ns) (vals as2ns)))] 
    (into [] (concat native foreign standard))))

(defn auto-complete [ns-sym code-partial-sym]
  "Provides a list of symbols which contain code-partial-sym."
  (let [valids (valid-symbols ns-sym)
        ^String s (str code-partial-sym)
        matches (filterv #(.contains ^String (str %) s) valids)]
    (into [] (sort matches))))

(defn get-code [file line col assert?]
  "Returns the code using the disk, if the file exists, nil otherwise.
   Reader macros are expanded but not vanilla macros."
  (let [file (str "./src/" file) txt (jfile/open file)
        _ (if (and assert? (not txt)) (throw (Exception. (str file " not found"))))]
    (if txt
      (let [lines (string/split-lines txt)
            clip-txt (subs (apply str (interpose "\n" (subvec lines (dec line)))) (dec col))]
        (read-string clip-txt)))))

(defn var-info [qual-sym source?]
  "Gets information about a var in the form of clojure datastructures."
  (let [ns-sym (ns-of qual-sym) ns-obj (find-ns ns-sym)
        sym2var (ns-map ns-obj)
        var-obj (get sym2var (unqual qual-sym))
        out (meta var-obj) out (assoc out :ns ns-sym)
        out (if source? (let [src (get-code (:file out) (:line out) (:column out) false)] 
                          (if src (assoc out :source src) out)) out)]
    out))

;(println "Source test:" (pr-str (var-info 'app.codebox/real-strings true)))