; Code database functions, for reading the code, finding out what calls what, etc.
; The functions work with standard clojure data-structures, no vars or namespace objects.
; coder.crosslang.langs handles any objects.
(ns coder.cbase
  (:require [clojure.repl] [clojure.set :as set]
    [clojure.string :as string]
    [collections]
    [javac.file :as jfile]
    [clojure.set :as set]
    [coder.textparse :as textparse]
    [coder.cnav :as cnav]
    [coder.crosslang.langs :as langs] [coder.sunshine :as sunshine]))

;; Simple conversion.

(defn strip-name [code]
  "Remove named def or functions."
  (if (and (collections/listy? code)
        (symbol? (second code)))
    (apply list (first code) [] (nthrest code 2)) code))

;;;;;;;;;; Files and texts ;;;;;;

(defn tokenize [txt langkwd]
  "Creats [strs tys]. It is a different format than tokenize-ints as well as being clojure native datastructures."
  (textparse/tokenize-from-ints txt (langs/tokenize-ints txt langkwd)))

(defn stringlang-to-wpath [txt ix langkwd]
  "Like textparse/string-to-wpath but using the langkwd to look up the function to use."
  (let [tokenize-ints-fn #(langs/tokenize-ints % langkwd)
        reads-string-fn #(langs/reads-string % langkwd)]
   (textparse/string-to-wpath txt ix tokenize-ints-fn reads-string-fn)))

(defn x-at-stringlang [txt ix langkwd]
  "Like textparse/x-at-string but using the langkwd to look up the function to use."
  (let [tokenize-ints-fn #(langs/tokenize-ints % langkwd)
        reads-string-fn #(langs/reads-string % langkwd)]
    (textparse/x-at-string txt ix tokenize-ints-fn reads-string-fn)))

(defn file2langkwd [fname]
  (let [ext (last (string/split fname #"\."))]
    (langs/fileext2langkwd ext)))

(defn file2codes [fname]
  "Reads the file into a clojure datastructure so long as it is a supported language.
   Uses the extension.
   Returns false if it fails, which is different than [].
   For each def, the second element in each of codes if it exists gives the name no matter the language as we read-string that way.
   flatten-classes? flattens java classes and makes each def include the class-name."
  (let [txt (jfile/open fname)
        _ (if (or (not txt) (not (string? txt))) (throw (Exception. (str "Can't load file:" fname)))) 
        langkwd (file2langkwd fname)]
    (langs/reads-string txt langkwd)))

(defn file2defmap [fname]
  "Returns qual-sym -> source code."
  (let [txt (jfile/open fname)
        _ (if (or (not txt) (not (string? txt))) (throw (Exception. (str "Can't load file:" fname)))) 
        langkwd (file2langkwd fname)
        codes (langs/reads-string txt langkwd)
        defpaths (cnav/all-defpaths codes)
        ns-sym (langs/file2ns fname)
        def-syms (mapv #(second (collections/cget-in codes %)) defpaths)
        def-codes (mapv #(collections/cget-in codes %) defpaths)
        def-syms-qual (mapv #(langs/resolved ns-sym %) def-syms)]
    (zipmap def-syms-qual def-codes)))

(defn subdefpath-fstr-ixs [sym-qual path-within-def]
  "Filename and what (cursor) indexes on the string correspond to the object in cpath.
   Will throw an error if the read-string for said file fails or it can't find the symbol."
  (let [filename (langs/ns2file (textparse/sym2ns sym-qual))
        codes (file2codes filename)
        def-wpath (cnav/symbol2defpath-qual codes sym-qual)
        _ (if (not def-wpath) (throw (Exception. (str "Can't find symbol: " sym-qual " in a top or class-level def in " filename))))
        wpath (into [] (concat def-wpath path-within-def))
        langkwd (file2langkwd filename)
        tokenize-ints-fn #(langs/tokenize-ints % langkwd)
        reads-string-fn #(langs/reads-string % langkwd)
        txt (jfile/open filename)
        _ (if (not txt) (throw (Exception. (str "Cant load file:" filename))))
        ixs (textparse/wpath-to-string-ixs txt wpath 
              tokenize-ints-fn reads-string-fn)]
    [filename (first ixs) (second ixs)]))

(defn defpath-fstr-ixs [sym-qual]
  "like get-fstr-ixs but to the symbol in the def."
  (subdefpath-fstr-ixs sym-qual [1]))

;; Extraction fns.

#_(defn resolve+ [sym-qual]
  "Like resolve but loads the namespace if possible. Same behavior as resolve."
  (let [ns-sym (textparse/sym2ns sym-qual)]
    (throw (Exception. "TODO: needs lots of debugging here."))))

(defn auto-complete [ns-sym code-partial-sym]
  "Provides a list of qualified symbols which contain code-partial-sym."
  (let [ns-sym (if (find-ns ns-sym) ns-sym 'clojure.core)
        valids (langs/valid-symbols ns-sym)
        valid-quals (mapv #(langs/resolved ns-sym %) valids)
        ;_ (println valid-quals)
        partial-ns (textparse/sym2ns code-partial-sym)
        partial-natives (if (and partial-ns (langs/findable-ns? partial-ns))
                          (mapv #(textparse/qual partial-ns %) (langs/defs partial-ns)) [])
        ^String s (str code-partial-sym)
        matches (filterv #(.contains ^String (str %) s) 
                  (concat valids valid-quals partial-natives))
        matches-qual (mapv #(langs/resolved ns-sym %) matches)]
    (into [] (sort (set matches-qual)))))

;; Navigating the code graph, TODO: performance (not yet a problem, but when we scale up...)

(defn varaverse []
  "Maps qualified symbols to source code, but only those where it can find the source.
   The source-code isn't qualified."
  (println "calling varaverse!")
  (let [nms (into [] (langs/get-all-loaded-ns))
        files (mapv langs/ns2file nms)
        good-ixs (filterv #(jfile/exists? (nth files %)) (range (count files)))
        nms1 (mapv #(nth nms %) good-ixs)]
    (apply merge (mapv #(file2defmap (langs/ns2file %)) nms1))))

(defn nonlocal-syms [code ns-sym]
  "Set of external symbols, i.e. symbols that point outside of the code, all qualified."
  (let [syms (cnav/all-syms (sunshine/pipeline ns-sym code false))]
    (filterv sunshine/qual? syms)))

(defn uses-of [qual-sym]
  "Expensive (TODO: precompute) function that scans through all namespaces looking for qualified syms that use our sym."
  (let [qual-sym (if (sunshine/qual? qual-sym) qual-sym (textparse/qual 'clojure.core qual-sym))
        vv (varaverse)
        leaf-sym (textparse/unqual qual-sym)
        
        ; First pass optimization: must contain the unqualed version (except in esoteric cases).
        leaf-syms (str leaf-sym)
        keep-k (filterv #(.contains ^String (str (get vv %)) leaf-syms) (keys vv))
        vv1 (zipmap keep-k (mapv #(get vv %) keep-k))
        non-locals-quals (mapv #(set (nonlocal-syms (strip-name %2) (textparse/sym2ns %1))) (keys vv1) (vals vv1))
        ixs (filterv #(get (nth non-locals-quals %) qual-sym) (range (count non-locals-quals)))
        ks (keys vv1)
        usesof (set (mapv #(nth ks %) ixs))]
    (into [] usesof)))

#_(defn cpath-uses-of [qual-sym]
  "Like uses-of but with the codepaths that dig into each variable. 
   Can return multible paths per symbol. This function is also expensive to run."
  (let [syms (uses-of qual-sym)
        vv (varaverse)
        source-quals (mapv #(binding [*ns* (find-ns (textparse/sym2ns %))] 
                              (sunshine/symbol-qual (get vv %))) syms)
        pathss (mapv #(cnav/paths-of % qual-sym) source-quals)
        cpathss (mapv #(mapv collections/vcat (repeat [%1]) %2) syms pathss)]
    (apply collections/vcat cpathss)))

(defn used-by [qual-sym]
  "What is used by this function."
  (let [qual-sym (if (sunshine/qual? qual-sym) qual-sym (textparse/qual 'clojure.core qual-sym))
        nms (textparse/sym2ns qual-sym)
        code-file (langs/ns2file nms)
        vv-lite (file2codes code-file) ; only need to check a single file.
        code (get vv-lite qual-sym)]
    (nonlocal-syms (strip-name code) nms)))

;;; Startup load everything optional (does this add cost?).

(def ^:dynamic *load-all-ns?* true)

(defonce ns-load-atom (atom false))

(if (and *load-all-ns?* (not @ns-load-atom))
  (future (do (Thread/sleep 3000) (langs/ensure-src-ns-loaded!)
            (reset! ns-load-atom true))))

