; Getters and setters; conversion to/from clj to java formats.
; The translation doesn't have to be pure, for example performance boost in keeping an image as an image
  ; and awareness of mutations.
  
(ns clooj.guinew.getset)

(defonce memoized-getters (atom {})) ; Optimization since reflection is unusually slow.
(defonce memoized-setters (atom {})) ; The builtin memoize fn may save a little code.

; Includes ancestor methods.
(defn methods-starts-with [^java.lang.Class cl pre narg]
  (let [methods (.getMethods (if (string? cl) (eval (read-string cl)) cl))
        n (count pre)
        getter? #(boolean (= (subs (.getName %) 0 n) pre))]
    (filterv #(and (= (count (.getParameterTypes %)) narg) (getter? %)) methods)))
    
(defn _method-memoize [^java.lang.Class cl m-atom slow-fn]
  (let [cache (get @m-atom cl)]
    (if cache cache
      (let [methods (slow-fn)
            method2kwd #(let [name (.getName %)] 
                          (cond (or (.startsWith name "get") (.startsWith name "set")) (keyword (subs name 3))
                                (.startsWith name "is")  (keyword (subs name 2))
                                :else (throw (Exception. (str "Unrecognized method name format." name)))))]
        (let [out (zipmap (mapv method2kwd methods) methods)] 
          (swap! m-atom #(assoc % cl out)) out)))))

(defn getters [^java.lang.Class cl]
  "Getter methods (map from keyword to java.lang.reflect.Method). Includes ancestor methods."
  (_method-memoize cl memoized-getters 
    (fn [] (into [] (concat (methods-starts-with cl "get" 0) (methods-starts-with cl "is" 0))))))

(defn setters [^java.lang.Class cl]
  "Setter methods (map from keyword to java.lang.reflect.Method). Includes ancestor methods."
  (_method-memoize cl memoized-setters
    (fn [] (into [] (concat (methods-starts-with cl "set" 1))))))

(defn filter-by-val ;https://gist.github.com/johnmastro/7224520
  [pred m]
  (into {} (filter (fn [[k v]] (pred v)) m)))

(defn boolean-fix [x]
  "Fixes a strange bug where (and (if x true false) (= x false)) is true
   when we pull from java objects."
  (if (= x false) false x))

(defn java-to-clj [java-obj f-override f-simple]
  "Conversion from java to clojure, one level deep.
   f-override takes the java object and replaces this entire function, nil = use the default behavior.
   the default behavior is NOT pure.
   f-simple can replace individual fields, nil = use the default behavior, :Nil = don't include field.
   nil functions = don't use them."
  (if-let [c0 (if f-override (f-override java-obj))] c0
    (let [x nil];(if f-simple (f-simple java-obj))] ; this seems to be a superfulus code here.
      (if (or x (not java-obj)) x
        (let [methods (getters (.getClass java-obj))
              ks (keys methods)
              vs (mapv #(let [x (if f-simple (f-simple %))] (if x x %)) 
                   (mapv #(.invoke ^java.lang.reflect.Method % java-obj (object-array [])) (vals methods)))]
          (filter-by-val #(not= % :Nil) (zipmap ks (mapv boolean-fix vs))))))))

(defn clj-to-java [^java.lang.Class cl clj f-override f-simple]
  "Converts a clojure object to a java object (the class is specified)."
  (if-let [j0 (if f-override (f-override cl clj))] j0
    (let [j (if f-simple (f-simple cl clj))] 
      (if (or j (not clj)) j
        (let [code (list (symbol (str (re-find #"[a-zA-Z0-9_\.]+" (.getName cl)) ".")))
              j (eval code) methods (setters cl)] ; make a java object and set values.
          (mapv #(if (get clj %) ; only set the java value where specified.
                   (let [^java.lang.reflect.Method m (get methods %)
                         ^java.lang.Class cl (nth (.getParameterTypes m) 0)] 
                     (let [val (f-simple cl (get clj %))]
                       (if (and val (not= val :Nil))
                         (.invoke m j (object-array [val])))))) (keys methods)) j)))))