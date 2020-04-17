; Code database functions, for reading the code, finding out what calls what, etc.
; The functions work with standard clojure stuff, this means symbols instead of vars or namespace objects, etc.
(ns coder.cbase
  (:require [clojure.repl] [clojure.set :as set]
    [clojure.string :as string]
    [javac.file :as jfile]
    [clojure.set :as set]
    [coder.clojure :as cljparse]))

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

(defn qual [ns-sym code-sym]
  "Similar but different to resolved."
  (symbol (str ns-sym "/" code-sym)))

(defn unqual [qual-sym]
  "Unqualified symbol."
  (symbol (subs (str qual-sym) (inc (count (str (ns-of qual-sym)))))))
 
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
  "Provides a list of symbols which contain code-partial-sym."
  (let [valids (valid-symbols ns-sym)
        valid-quals (mapv #(resolved ns-sym %) valids)
        ^String s (str code-partial-sym)
        matches (filterv #(.contains ^String (str %) s) 
                  (concat valids valid-quals))]
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
        _ (if (nil? ns-obj) (throw (Exception. (str "Namespace not found:" qual-sym))))
        sym2var (ns-map ns-obj)
        var-obj (get sym2var (unqual qual-sym))
        out (meta var-obj) out (assoc out :ns ns-sym)
        out (if source? (let [src (get-code (:file out) (:line out) (:column out) false)] 
                          (if src (assoc out :source src) out)) out)]
    out))

;; Navigating the code graph, TODO: performance (not yet a problem, but when we scale up...)

(defn nonlocal-syms [code locals]
  "Set of external symbols, i.e. symbols that point outside of the code.
   Doesn't qualify anything.
  POLYGLOT: either shoehorn i.e java/python code into a format that works with this fn, or
            have unique symbols that we can add to this fn (if the behavior is different)."
  (let [locals (if (set? locals) locals (set locals))
        l? (and (sequential? code) (not (vector? code)))
        c0 (if (coll? code) (first code)) c1 (if (coll? code) (second code))
        c2 (if (coll? code) (get code 2))
        macros #{'definline 'definterface 'defmacro 'defmethod 'defn 'defn- 'defmulti 'let 'letfn}]
    ; Limited macro expansion vs full macro expansion (which needs the qualifications)?
    (cond (symbol? code) (if (get locals code) #{} #{code})
      (not (coll? code)) #{}
      (and l? (get macros c0)) (nonlocal-syms (macroexpand code) locals)
      (and l? (= c0 'quote)) #{} ; dig into nested quote unquotes?
      (and l? (not (get locals c0)) (or (= c0 'fn) (= c0 `fn)))
      (let [name? (symbol? (second code)) v-ix (if name? 2 1)
            packed? (list? (get (into [] code) v-ix))
            code-packed (if packed? code 
                          (if name? (list (first code) (second code) (rest (rest code)))
                            (list (first code) (rest code))))
            pieces (if name? (rest (rest code-packed)) (rest code-packed))
            locals1 (if name? (conj locals (second code)) locals)
            piece-globals (mapv #(let [s-args (set (first %)) body (rest %)]
                                   (nonlocal-syms body (set/union locals1 s-args))) pieces)]
        (apply set/union piece-globals))
      (and l? (= c0 'let*)) ; The stuff in the binding as well as the stuff after.
      (let [var-syms (into [] (take-nth 2 c1))
            targets (into [] (take-nth 2 (rest c1)))
            n (count targets)
            lg (loop [l locals g #{} ix 0]
                 (if (= ix n) [l g]
                   (recur (conj l (nth var-syms ix))
                     (set/union g (nonlocal-syms (nth targets ix) l)) (inc ix))))
            locals1 (first lg) globals1 (second lg)]
         (set/union #{'let*} globals1 (nonlocal-syms (rest (rest code)) locals1)))
       (and l? (= c0 'def) (not (get locals 'def)))
       (conj (nonlocal-syms (rest (rest code)) (conj locals (second code))) 'def)
       (or (sequential? code) (set? code)) (apply set/union (mapv #(nonlocal-syms % locals) code))
       (map? code) (set/union (nonlocal-syms (keys code) locals) (nonlocal-syms (vals code) locals)) 
      :else #{})))


(defn varaverse []
  "Maps qualified variables to source code, but only those where it can find the source."
  (let [nms (mapv ns-name (all-ns))]
    (apply merge (mapv #(get-codes (ns2file %)) nms))))


(defn uses-of [qual-sym]
  "Expensive function that scans through all namespaces looking for qualified syms that use our sym."
  (let [vv (varaverse)
        leaf-sym (unqual qual-sym)
        
        ; First pass optimization: must contain the unqualed version (except in esoteric cases).
        leaf-syms (str leaf-sym)
        keep-k (filterv #(.contains ^String (str (get vv %)) leaf-syms) (keys vv))
        vv1 (zipmap keep-k (mapv #(get vv %) keep-k))
       
        non-locals (mapv #(nonlocal-syms % (hash-set)) (vals vv1))
        non-locals-qual (mapv (fn [k nl] 
                                (let [nm (ns-of k)] (set (mapv #(resolved nm %) nl)))) 
                          (keys vv1) non-locals)
        ixs (filterv #(get (nth non-locals-qual %) qual-sym) (range (count non-locals-qual)))
        ks (keys vv1)]
    (mapv #(nth ks %) ixs)))

(defn used-by [qual-sym]
  "What is used by this function."
  (let [nms (ns-of qual-sym)
        code-file (ns2file nms)
        vv-lite (get-codes code-file) ; only need to check a single file.
        code (get vv-lite qual-sym)
        _ (if (not code) (throw (Exception. (str qual-sym " not found in file: " code-file))))
        xtracted-symbols (nonlocal-syms code (hash-set))]
    (mapv #(qual nms %) xtracted-symbols)))

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

