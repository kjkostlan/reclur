; Code database functions, for reading the code, finding out what calls what, etc.
; The functions work with standard clojure stuff, this means symbols instead of vars or namespace objects, etc.
(ns coder.cbase
  (:require [clojure.repl] [clojure.set :as set]
    [clojure.string :as string]
    [javac.file :as jfile]
    [clojure.set :as set]
    [coder.clojure :as cljparse] [coder.sunshine :as sunshine]))

;; Entering and exiting clojure land

(defn ns-ob2sym [ns-ob]
  "Also works for Java classes."
  (let [txt (str ns-ob)
        coffee \u2615
        class? (and (> (count txt) 5) (= (subs txt 0 6) "class "))
        txt (if class? (symbol (str coffee "." (subs txt 6))) txt)]
    (symbol txt)))

(defn ns-sym2ob [ns-sym]
  "Also works for Java classes."
  (let [coffee \u2615
        txt (str ns-sym)
        class? (= (first txt) coffee)]
    (if class? (try (eval (symbol (subs txt 2))) 
                 (catch Exception e)) (find-ns ns-sym))))

;(print (ns-ob2sym java.lang.String))

;; Simple conversion fns.

(defn ns-of [qual-sym]
  "Splits the symbol-as-a-string. Returns a symbol."
  (if (.contains ^String (str qual-sym) "/")
    (symbol (re-find #"[A-Za-z\.*+!\-'?=:\_]+" (str qual-sym)))))

(defn unqual [qual-sym] 
  "Unqualified symbol."
  (symbol (subs (str qual-sym) (inc (count (str (ns-of qual-sym)))))))
 
(defn qual [ns-sym code-sym]
  "Similar but different to resolved."
  (symbol (str ns-sym "/" code-sym)))

(defn resolved [ns-sym code-sym]
  "Resolves code-sym in ns-sym, which depends on the :requires, :imports, etc.
   Returns a fully qualified symbol."
  (let [ns-ob (find-ns ns-sym)
        _ (if (not ns-ob) (throw (Exception. (str "Namespace: " (pr-str ns-sym) " not found."))))
        var-ob (ns-resolve ns-ob code-sym)]
    (if var-ob (symbol (subs (str var-ob) 2)))))

(defn file2ns [file]
  "Converts a local filepath to a namespace symbol."
  (let [file (jfile/full-to-local file)
        pieces (rest (rest (string/split file #"/")))
        st (string/join "." pieces)]
    (symbol (subs st 0 (- (count st) 4)))))

(defn ns2file [ns-sym]
  "Converts a namespace representation to a file representation."
  (let [txt (.replace ^String (str ns-sym) "." "/")]
    (str "./src/" txt ".clj")))

(defn var2sym [v]
  "Fully qualified."
  (let [st (str v)]
    (symbol (subs st 2))))

(defn strip-name [code]
  "Remove named def or functions."
  (if (and (sunshine/listy? code)
        (symbol? (second code)))
    (apply list (first code) [] (nthrest code 2)) code))

;; Extraction fns.

(defn resolve+ [sym-qual]
  "Like resolve but loads the namespace if possible. Same behavior as resolve."
  (let [ns-sym (ns-of sym-qual)]
    (throw (Exception. "TODO: needs lots of debugging here."))))

(defn defs [ns-sym]
  (into [] (keys (ns-interns ns-sym))))

(defn valid-symbols [ns-sym]
  "All recognized symbols of the namespace, with :as qualifications, etc.
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
  "Provides a list of qualified symbols which contain code-partial-sym."
  (let [ns-sym (if (find-ns ns-sym) ns-sym 'clojure.core)
        valids (valid-symbols ns-sym)
        valid-quals (mapv #(resolved ns-sym %) valids)
        ^String s (str code-partial-sym)
        matches (filterv #(.contains ^String (str %) s) 
                  (concat valids valid-quals))
        matches-qual (mapv #(resolved ns-sym %) matches)]
    (into [] (sort (set matches-qual)))))

(defn get-code [file line col assert?]
  "Returns the code using the disk, if the file exists, nil otherwise.
   Reader macros are expanded but not vanilla macros."
  (let [file (str "./src/" file) txt (jfile/open file)
        _ (if (and assert? (not txt)) (throw (Exception. (str file " not found"))))]
    (if txt
      (let [lines (string/split-lines txt)
            clip-txt (subs (apply str (interpose "\n" (subvec lines (dec line)))) (dec col))]
        (read-string clip-txt)))))

(defn get-codes [file]
  "All functions directly defined with a def, etc, in a file, map from qualified symbols to names."
  (let [txt (jfile/open file)]
    (if txt
      (let [ns-sym (file2ns file)
            code (binding [*read-eval* false] (cljparse/reads-string txt))
            def-syms #{'def 'definline 'defmacro 'defmethod 'defmulti 'defn 'defn- 'defonce 'defprotocol 'defrecord 'defstruct 'deftype}
            defs (filterv #(and (list? %) (get def-syms (first %))) code)]
        (zipmap (mapv #(qual ns-sym (second %)) defs) defs)))))

(defn var-info [qual-sym source?]
  "Gets information about a var in the form of clojure datastructures."
  (let [ns-sym (ns-of qual-sym) ns-obj (find-ns ns-sym)
        _ (if (nil? ns-obj) (throw (Exception. (str "Namespace not found:" ns-sym))))
        sym2var (ns-map ns-obj)
        var-obj (get sym2var (unqual qual-sym))
        out (meta var-obj) out (assoc out :ns ns-sym)
        out (if source? (let [src (get-code (:file out) (:line out) (:column out) false)] 
                          (if src (assoc out :source src) out)) out)]
    out))

;; Navigating the code graph, TODO: performance (not yet a problem, but when we scale up...)

(defn nonlocal-syms [code ns-sym]
  "Set of external symbols, i.e. symbols that point outside of the code, all qualified."
  (binding [*ns* (find-ns ns-sym)] 
    (let [syms (sunshine/all-syms (sunshine/pipeline code false))]
      (filterv sunshine/qual? syms))))

(defn varaverse []
  "Maps qualified symbols to source code, but only those where it can find the source."
  (let [nms (mapv ns-name (all-ns))]
    (apply merge (mapv #(get-codes (ns2file %)) nms))))

(defn uses-of [qual-sym]
  "Expensive function that scans through all namespaces looking for qualified syms that use our sym."
  (let [qual-sym (if (sunshine/qual? qual-sym) qual-sym (qual 'clojure.core qual-sym))
        vv (varaverse)
        leaf-sym (unqual qual-sym)
        
        ; First pass optimization: must contain the unqualed version (except in esoteric cases).
        leaf-syms (str leaf-sym)
        keep-k (filterv #(.contains ^String (str (get vv %)) leaf-syms) (keys vv))
        vv1 (zipmap keep-k (mapv #(get vv %) keep-k))
        non-locals-quals (mapv #(set (nonlocal-syms (strip-name %2) (ns-of %1))) (keys vv1) (vals vv1))
        ixs (filterv #(get (nth non-locals-quals %) qual-sym) (range (count non-locals-quals)))
        ks (keys vv1)
        usesof (set (mapv #(nth ks %) ixs))]
    (into [] usesof)))

(defn used-by [qual-sym]
  "What is used by this function."
  (let [qual-sym (if (sunshine/qual? qual-sym) qual-sym (qual 'clojure.core qual-sym))
        nms (ns-of qual-sym)
        code-file (ns2file nms)
        vv-lite (get-codes code-file) ; only need to check a single file.
        code (get vv-lite qual-sym)]
    (nonlocal-syms (strip-name code) nms)))

;;; Startup load everything optional (does this add cost?).

(defn ensure-src-ns-loaded! []
  (let [t0 (System/nanoTime)
        loaded-ns-syms (set (mapv ns-name (all-ns)))
        all-clj-files (filterv jfile/clj? (jfile/all-files-inside "./src"))
        src-ns-syms (set (mapv file2ns all-clj-files))
        needed-ns-syms (set/difference src-ns-syms loaded-ns-syms)
        _ (println "# namespaces loaded:" (count loaded-ns-syms) 
            "# namespaces in src" (count src-ns-syms)
             " # namespaces need to load for coder/cbase:" 
            (count needed-ns-syms))
        _ (binding [*ns* (create-ns (gensym 'tmploadnscbase))]
            (mapv #(try (require %)
              (catch Exception e
                (println "Failed to load ns (bad clj file?):" %)))
              needed-ns-syms))
        t1 (System/nanoTime)]
    (println "Elapsed time (s) to make sure all files in ./src are loaded:" (/ (- t1 t0) 1.0e9))))

(def ^:dynamic *load-all-ns?* true)

(defonce ns-load-atom (atom false))

(if (and *load-all-ns?* (not @ns-load-atom))
  (future (do (Thread/sleep 3000) (ensure-src-ns-loaded!))))

