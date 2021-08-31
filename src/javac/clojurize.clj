; Approximate conversion of clojure objects into java objects.
; Uses reflections and getters.
(ns javac.clojurize
  (:require [clojure.string :as string]
    [crossplatform.cp :as crossp]
    [javac.thread :as jthread])
  (:import [java.awt Point Rectangle Dimension]
    [java.io StringWriter]
    [java.nio Buffer]))

(defn elementary? [t]
  "Is the class t an elementary type or not."
  (or (= t Boolean) (= t Byte) (= t Integer) (= t Long) (= t Short) (= t Float) (= t Double) (= t String)))

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

(defn simple-java-to-clj [obj]
  "Lossy conversion of some simple java obs to clojure, nil = not in the class we convert.
   Conversion between java and clojure forms is more involved so is done at two levels."
  (if (not (nil? obj))
    (let [t (type obj)]
      (cond
        (elementary? t)
        obj ; immutable as-is.
        (= t java.awt.Point)
        [(int (.getX obj)) (int (.getY obj))]
        (= t java.awt.Dimension)
        [(int (.getWidth obj)) (int (.getHeight obj))]
        (= t java.awt.Rectangle)
        [(int (.getX obj)) (int (.getY obj)) (int (.getWidth obj)) (int (.getHeight obj))]
        ; TODO: graphics stroke, etc.
        :else
        nil))))

(defn param-str [java-e]
  (try (.paramString java-e) (catch Exception e "MI_SC")))

(defn get-evt-type [java-e]
  "Java event -> :mousePressed. Parses the paramString (I can't seem to find a better way).
   This shouldn't need overriding."
  (jthread/assert-edt)
  (let [^String s (param-str java-e)
        tokens (re-seq #"[A-Z_]+" s)
        tok (first (filter #(.contains % "_") tokens)) ; they tend to be at the beginning.
        tok-pieces (string/split tok #"_")]
    (keyword (apply str (.toLowerCase (first tok-pieces)) (mapv #(str (.toUpperCase (subs % 0 1)) (.toLowerCase (subs % 1))) (rest tok-pieces))))))

(defn translate-event [java-e java-ob]
  "Translates the event into a clojure form.
   The :Type of the event returned is something like :MousePressed"
  (jthread/assert-edt)
  (let [evt-type (get-evt-type java-e)]
    (let [clean-up #(dissoc % :Source :Component :Class)
          add-sc #(assoc % :ParamString (param-str java-e))
          f0 #(assoc (clean-up (add-sc %)) :Type evt-type)] ; a default function.
      (f0 (java-to-clj java-e nil simple-java-to-clj))))) ; direct java -> clj translation.

(defn translate-generic [java-ob]
  (java-to-clj java-ob nil simple-java-to-clj))

(defn extract! [^StringWriter dump-prints-here]
  (let [out (str dump-prints-here)
        buf ^Buffer (.getBuffer dump-prints-here)]
    (.setLength buf 0) out))

(def default-out *out*)

(def ^:dynamic ^StringWriter *our-out* (StringWriter.))