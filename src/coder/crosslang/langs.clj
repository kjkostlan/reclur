; Compiles language-dependent functions into one area.

; Common functions:
; Depth: The interstial indentation depth. "a(b)()"->[0011010].
; Reads-string: read-string but with and outer [], allowing reading the whole file at once.
; tokenize: Returns [token-string, tiken-types]. See below for types.

; Token codes (which can generalize to 95% of languages):
  ; 0 = space and comments and #_(foo), includes delimiters as found in java, python, etc, even if they are necessary, includes MOST python whitespace.
  ; 1 = symbols (basically the same in any language, + and = are symbols in java even though they are treated differently).
  ; 2 = keywords (reserved words like "class" in java, python, etc).
  ; 3 = literals (boolean, number, string, regexp, etc).
  ; 4 = opening ( [ { #{ almost the same in any language, includes opening <tags>, and includes indent whitespace in python.
  ; 5 = closing ) ] } almost the same in any language, includes </tags>, empty for python as there are no chars to be assigned to a dedent.
  ; 6 = punctuation like ending semicolons. Does not exist in lisp.
  ; 7 = reader macros (does not include metadata, sets, or regexp), includes spaces in reader macros which is possible in poorly formatted code, mostly unique to lisps.
  ; 8 = meta tag (java annotations and python decorators).

; Notes about files:
;   Must take linux newlines, the file reader should automatically replace windows newlines with linux newlines.
;   Tabs should be converted to spaces.

; Notes about Java and similar syntaxes:
; if (x>0) {do-stuff}
; Gets converted into:
; (if (> 0 x) (do-stuff))
; Statements:
; x=1;y=2;z=x*y
; Gets converted into a let:
; (let [x 1 y 2 z x*y])
; The let spans the scope of all local statements, so it envelopes the function def.
; Global statements: x=1 => _ (reset! x 1)
; Non-returning calls: x=1; foo.doSomething(); y=2; => (x)
; trailing ; are punctuations if they are mandatory.
; Private tags etc into the metadata.
; Types into :tag in the metadata much like clojure type hinting.
; Vector<String> vec = new Vector<String>(2);
; The tag simply is in (symbol "Vector<String>"), we don't parse this further.

; Notes about python:
; def foo(x=1):
;   if x>1:
;      return x
;   else:
;      print('hit min') # Open token is the last three spaces before the print
;      return 1.0  # No open token on this line, but a closing one at the '\n' at the end.
;   ___  <- above this line are the spaces that indent the if and else. Any other whitespace is an empty token.
; __ <- above this line rae the spaces that indent the foo.
; The \n at the end dedents things.
; Whitespace at the beginning of each line not including is punctuation.
; Special fn calls:
; def foo(x=1, y) => (defn foo ^{:defaults [1 nil]} [x y])
; foo(a,b,*stuff) => (apply foo a b stuff)
; foo(a,b,**dicty) => (apply-kv foo a b dicty)
; foo(a,b,x=5) => (^{:arglist [%1 %2 x]} foo a b 5)
; Decorators into the metadata.

; Notes about C/C++ macros:
; #foo
; The entire thing is a reader macro, no attempt to parse it.

(ns coder.crosslang.langs
  (:require [coder.textparse :as textparse]
            [coder.crosslang.langparsers.clojure :as clojure]
            [coder.crosslang.langparsers.human :as human]
            [c]
            [clojure.string :as string]
            [javac.file :as jfile]
            [clojure.set :as set]
            [clojure.main :as main]
            [clojure.walk :as walk]))

;;;;;;;;;;;;;;;;;;;;;; Support functions that aren't language dependent ;;;;;;;;;;;;;

(defn errlang [fn-name langkwd]
  "One day this function will be removed. One day..."
  (throw (Exception. (str "langs/" fn-name " not yet supported with " langkwd))))

(defn unpacked-fn? [form]
  "Functions without packed arguments. Making all arguments packed can make things easier but changes the path."
  (if (and (c/listy? form) (contains? #{`fn `fn* 'fn} (first form)))
    (cond (vector? (second form)) 1 (vector? (c/third form)) 2 :else false)))

(defn fn-pack1 [code]
  "Like fn-pack but doesn't act recursively."
  (if-let [upk (unpacked-fn? code)]
    (if (= upk 1) (list (first code) (rest code))
      (list (first code) (second code) (rest code))) code))
(defn fn-pack [code]
  "Makes single-arity functions packed like multi-arity fns.
   This reduces the number of cases.
   Clojure's inline functions don't pack up properly even though (fn) does."
  (walk/postwalk fn-pack1 code))

;;;;;;;;;;;;;;;;;;;;;; Language specific fns of medium size ;;;;;;;;;;;;;
; Small = inline. Large = put in dedicated files in coder.crosslang.langparsers.

(defn get-code-clojure [file line col assert?]
  ; TODO: not just clojure.
  "Returns the code using the disk, if the file exists, nil otherwise.
   Reader macros are expanded but not vanilla macros."
  (let [file (str "./src/" file) txt (jfile/open file)
        _ (if (and assert? (not txt)) (throw (Exception. (str file " not found"))))]
    (if txt
      (let [lines (string/split-lines txt)
            clip-txt (subs (apply str (interpose "\n" (subvec lines (dec line)))) (dec col))]
        (read-string clip-txt)))))

(defn clj-resolve-class [sym]
  "Namespace-dependent (I think). Nil if can't be resolved."
  ; TODO: since this depends on namespaces it needs to be incorporated into langs/resolved.
  (if (symbol? sym)
    (try (if (= (type (eval sym)) java.lang.Class)
           (second (string/split (str (eval sym)) #" ")))
      (catch Exception e nil))))

(defn unmacro-static-java1 [x]
  "Combines static java calls into one symbol.
   (. Math sin 1.2) => (java.lang.Math/sin 1.2).
   This can be treated like a qualified symbol to a function for MOST metaprogramming.
  This is a clojure-unique function; for any other lang it will do nothing."
  (cond (not (c/listy? x)) x
    (not (symbol? (first x))) x
    (= (first x) 'new) (apply list (symbol (str (second x) ".")) (nthrest x 2))
    (and (= (first x) '.) (clj-resolve-class (second x)))
    (apply list (symbol (str (clj-resolve-class (second x)) "/" (c/third x))) (nthrest x 3))
    (clj-resolve-class (textparse/sym2ns (first x)))
    (apply list (symbol (str (clj-resolve-class (textparse/sym2ns (first x))) "/" (textparse/unqual (first x)))) (rest x)) ; resolve
    :else x))

;;;;;;;;;;;;;;;;;;;;;; Functions that don't require knowing the language itself, but do depend on language ;;;;;;;;;;;;;

(defn resolved [ns-sym code-sym]
  "Resolves code-sym in ns-sym, which depends on the :requires, :imports, etc.
   Returns a fully qualified symbol."
  (let [langkwd (textparse/ns2langkwd ns-sym)]
    (cond (= langkwd :clojure)
      (let [ns-ob (find-ns ns-sym)
            _ (if (not ns-ob) (throw (Exception. (str "Namespace: " (pr-str ns-sym) " not found; the clj file must first be compiled at least once."))))
            var-ob (ns-resolve ns-ob (symbol (str code-sym)))]
        (if var-ob (symbol (subs (str var-ob) 2))))
      (= langkwd :human) (symbol (str :human (textparse/rm-lang code-sym)))
      :else (errlang "resolved" langkwd))))

(defn fileext2langkwd [ext]
  "Infer the language from the file extension."
  (let [ext (last (string/split ext #"\."))
        priority-order [:clojure :java :javascript :python :haskell :segfault :shell :human]
        ext-map {:clojure #{"clj" "cljs"}
                 :java #{"java"} :javascript #{"js"}
                 :python #{"py"}
                 :haskell #{"hs"}
                 :segfault #{"C" "CPP" "C++"}
                 :shell #{"sh" "bat"}
                 :human #{"txt" "md"}}
        langkwd (first (filter #(contains? (get ext-map %) ext) priority-order))]
    (if langkwd langkwd (throw (Exception. (str "unrecognized file exension:" ext))))))

(defn langkwd2fileext [kwd]
  (let [m {:clojure "clj" :java "java" :javascript "js" :python "py"
           :haskell "hs" :segfault "cpp" :shell "sh" :human "txt"}]
    (get m kwd :unknown)))

(defn file2ns [file]
  "Converts a local filepath to a namespace symbol."
  (let [file-ext (string/split (subs (jfile/full-to-local file) 2) #"\.")
        file0 (first file-ext) ext (second file-ext)
        langkwd (fileext2langkwd ext)]
    (cond (or (= langkwd :human) (= langkwd :clojure))
      (let [pieces (rest (string/split file0 #"\/")) ; exclude the src/
            st (string/join "." pieces)]
        (symbol st))
    :else (errlang "file2ns" langkwd))))

(defn ns2file [ns-sym]
  "Converts a namespace representation to a file representation."
  (let [langkwd (textparse/ns2langkwd ns-sym)]
    (cond (or (= langkwd :clojure) (= langkwd :human))
      (let [ns-sym0 (textparse/rm-lang ns-sym)
            txt (.replace ^String (str ns-sym) "." "/")]
        (str "./src/" txt "." (langkwd2fileext langkwd)))
    :else (errlang "ns2file" langkwd))))

(defn findable-ns? [ns-sym]
  "Maybe it isn't findable."
  (let [langkwd (textparse/ns2langkwd ns-sym)]
    (cond (= langkwd :clojure)
      (boolean (find-ns ns-sym))
      (= langkwd :human) true
      :else (errlang "findable-ns?" langkwd))))

(defn var-info [qual-sym source?]
  "Gets information about a var in the form of clojure datastructures.
   source? means look for the source as well, which can be a little slow."
  (let [langkwd (textparse/ns2langkwd (textparse/sym2ns qual-sym))]
    (if (not (symbol? qual-sym)) (throw (Exception. (str "Qual-sym must be a symbol not a " (type qual-sym)))))
    (if (not= langkwd :clojure)
      (errlang "var-info" langkwd)))
  (let [ns-sym (textparse/sym2ns qual-sym) ns-obj (find-ns ns-sym)
        _ (if (nil? ns-obj) (throw (Exception. (str "Namespace not found:" ns-sym))))
        sym2var (ns-map ns-obj)
        var-obj (get sym2var (textparse/unqual qual-sym))
        out (meta var-obj) out (assoc out :ns ns-sym)
        out (if (and source? (not (:source out)))
              (let [src (get-code-clojure (:file out) (:line out) (:column out) false)]
                (if src (assoc out :source src) out)) out)
        out (if source? out (dissoc out :source))] ; consistancy.
    out))

(defn var-source [qual-sym]
  "Source-code of qual-sym, no macro expansion or symbol qualification."
  (:source (var-info qual-sym true)))

(defn defs [ns-sym]
  "Not qualified."
  (let [langkwd (textparse/ns2langkwd ns-sym)]
    (cond (= langkwd :clojure)
      (into [] (keys (ns-interns ns-sym)))
      (= langkwd :human) []
      :else (errlang "defs" langkwd))))

(defn valid-symbols [ns-sym]
  "All recognized symbols of the namespace, with :as qualifications, etc.
   TODO: java imports for clojure.
   Only includes things that were imported."
 (let [langkwd (textparse/ns2langkwd ns-sym)
       _ (if (not= langkwd :clojure) (errlang "valid-symbols" langkwd))
       x (ns-aliases ns-sym)
       as2ns (zipmap (keys x) (mapv ns-name (vals x)))
       standard (keys (ns-map 'clojure.core))
       native (defs ns-sym)
       foreign (apply concat
                 (mapv (fn [stub nms] (mapv #(symbol (str stub "/" %))
                                       (defs nms)))
                   (keys as2ns) (vals as2ns)))]
    (c/vcat native foreign standard)))

(defn defs [ns-sym]
  "Not qualified."
  (let [langkwd (textparse/ns2langkwd ns-sym)]
    (cond (= langkwd :clojure)
      (into [] (keys (ns-interns ns-sym)))
      (= langkwd :human) []
      :else (errlang "defs" langkwd))))

(defn mexpand [ns-sym code]
  "Performs a macroexpand-all on the function, as well as some other minor steps to make
   the code easier to work with."
  (let [langkwd (textparse/ns2langkwd ns-sym)]
    (#(if (c/listy? %) (apply list %) %) (fn-pack
      (cond (= langkwd :clojure)
        (walk/postwalk unmacro-static-java1
          (if-let [ns-obj (find-ns ns-sym)]
            (binding [*ns* ns-obj] (walk/macroexpand-all code))
              (walk/macroexpand-all code)))
        (= langkwd :human) []
        :else (errlang "mexpand" langkwd))))))

;;;;;;;;;;;;;;;;;;;;;; Functions that need to know which language to use ;;;;;;;;;;;;;;;;;

(defn reads-string [txt langkwd]
  "Reads txt into a clojure datastructure using lang to parse it.
   Returns false if it fails, which is different than []."
  (cond (= langkwd :clojure) (clojure/reads-string txt)
    (= langkwd :human) (human/reads-string txt)
    :else (errlang "reads-string" langkwd)))

(defn interstitial-depth [txt langkwd]
  "The depth at all cursor-ix values, with the cursor going between characters.
   Has one more element than the length of txt.
   It can see zero-wide gaps in stuff like ()()."
  (cond (= langkwd :clojure) (clojure/interstitial-depth txt)
    (= langkwd :human) (human/interstitial-depth txt)
    :else (errlang "interstitial-depth" langkwd)))

(defn tokenize-ints [txt langkwd]
  "Creates ^ints of [st en ty].
   st = start ix on string of token. en = end ix on string of token.
   ty is the type as defined above. It is an int that tells us wether it is a symbol, literal, bracket, whitespace, etc."
  (cond (= langkwd :clojure) (clojure/tokenize-ints txt)
    (= langkwd :human) (human/tokenize-ints txt)
    :else (errlang "tokenize-ints" langkwd)))

(defn convert-stack-trace [stack-trace stop-stack langkwd]
  "Converts a stack trace created in language langkwd into a natural clojure data-structure format.
   Stop-stack (which can be empty []) is a vector of stops for the stacktrace."
  (cond (= langkwd :clojure)
      (let [^"[Ljava.lang.StackTraceElement;" stack-trace stack-trace ;]
            stack (mapv #(str (main/demunge ^String (.getClassName ^java.lang.StackTraceElement %))
                           " " (.getLineNumber ^java.lang.StackTraceElement %)) stack-trace)
            root (fn [elem] (first (string/split (str (if-let [x (textparse/sym2ns elem)] x elem)) #"\.")))
            of-clojure? (fn [elem] (= (root elem) "clojure"))
            of-java? (fn [elem] (= (root elem) "java"))
            stack1 (filterv #(not (or (of-clojure? %) (of-java? %))) stack)
            stop-stack (if (coll? stop-stack) stop-stack [stop-stack])
            stop? (fn [x] (boolean (first (filter #(string/starts-with? (str x) (str %)) stop-stack))))
            stop-ix (first (filter #(stop? (nth stack1 %))
                             (range (count stack1))))
            stack2 (if stop-ix (subvec stack1 0 (inc stop-ix)) stack1)
            stack3 (if stop-ix (conj stack2 {:ClassName "..." :FileName "" :LineNumber "" :MethodName ""}) stack2)]
        stack3)
    :else (errlang "convert-stack-trace" langkwd)))

(defn convert-exception [e stop-stack langkwd]
  "Converts an exception thrown in language langkwd to a readable format.
   Stop-stack (which can be empty []) is a vector of stops for the stacktrace."
  (cond (= langkwd :clojure)
      (let [^Exception e e
            ^"[Ljava.lang.StackTraceElement;" stack-trace (.getStackTrace e)  ;]
            ^String message (.getMessage e)
            stack-clj (convert-stack-trace stack-trace stop-stack langkwd)]
        {:Message message :StackTrace stack-clj})
        :else (errlang "convert-exception" langkwd)))

;;;;;;;;;;;;;;;;;;;;;; Functions that work with multiple languages at once ;;;;;;;;;;;;;

(defn get-all-loaded-ns []
  (let [clojures (mapv ns-name (all-ns))]
    ; Will add more in the future.
    clojures))

(defn ensure-src-ns-loaded! []
  "Our sources. More languages will be added."
  (let [t0 (System/nanoTime)
        loaded-ns-syms (set (mapv ns-name (all-ns)))
        all-clj-files (filterv jfile/clj? (jfile/all-files-inside "./src"))
        src-ns-syms (set (mapv file2ns all-clj-files))
        needed-ns-syms (set/difference src-ns-syms loaded-ns-syms)
        _ (binding [*ns* (create-ns (gensym 'tmploadnscbase))]
            (mapv #(try (require %)
                     (catch Exception e
                       (println "Failed to load ns (bad clj file?):" %)))
              needed-ns-syms))
        t1 (System/nanoTime)]
    (println "Langs/ensure-src-ns-loaded! Loaded: " (count loaded-ns-syms)
      "Elapsed time (s):" (/ (- t1 t0) 1.0e9))))
