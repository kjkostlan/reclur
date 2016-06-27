; Approximate conversion of clojure objects into java objects.
; Uses reflections and getters.
(ns clooj.java.clojurize
  (:require [clojure.string :as string] [clojure.set :as set] [clooj.java.thread :as thread])
  (:import [java.awt Point Rectangle Dimension]
   [javax.swing SwingUtilities]))

; TODO: move these memonize functions into a dedicated place:

; Like memonize but more robust (no memory leaks if you are dynamically generating lots of classes, etc).
; Our use of a timeout is also more robust if there is an unexpected change.
; TODO: longer holding times for more computation.
(def _limited-memonize-atom (atom {}))
(def holding-time-ms 10000.0)
(def sweep-interval-ms 2000.0)
(def nuke-sweep-threshold 1e6) ; A rediculously large number of keys = byby.

(defn sweep-step!! [] ; sweeps the atom every so often. Not atomistic on purpose: new values can be added while sweeping and we stay OK.
  (if (>= (count @_limited-memonize-atom) nuke-sweep-threshold)
      (reset! _limited-memonize-atom {})
      (let [time-now (System/nanoTime)
            min-time (- time-now (* holding-time-ms 1e6))]
  (mapv 
    #(let [time-then (:time (get @_limited-memonize-atom %))]
       (if (and time-then (< time-then min-time))
           (swap! _limited-memonize-atom dissoc %)))
    (keys @_limited-memonize-atom)))))

(thread/pulse!! sweep-step!! sweep-interval-ms "clojurize/sweep-step!!")

(defn time-limited-memonize [f]
  (fn [& args]
    (let [x0 (get @_limited-memonize-atom [f args])]
      (if x0 (:val x0) (let [out (apply f args)]
                         (swap! _limited-memonize-atom assoc [f args] 
                           {:time (System/nanoTime) :val out}) out)))))

; Extra debug checking if we are on the event dispatch thread:
(defn edt-assert [] (if (not (SwingUtilities/isEventDispatchThread)) (throw (Exception. "Java modification not from the event dispatch thread."))))
   
(defmacro on-java-change [call root-atom-or-obj]
 "Changes to an object hiererchy (except graphical repaints) will be tracked iff
  :debug-java-changes if non nil (should be set to the empty vector []).
  This bridges part of the gap between Java's mutation and clojure's functions."
  ; TODO: does this quash meta-data for type hints? Don't think so in non-debug mode, should check though.
  (let [arg-vals-sym (gensym 'arg-vals) root-atom-sym (gensym 'root-atom)]
    `(let [~root-atom-sym (if (instance? clojure.lang.Atom ~root-atom-or-obj) 
                            ~root-atom-or-obj (.getClientProperty ~root-atom-or-obj "root"))
           changes# (:debug-java-changes (deref ~root-atom-sym))]
       (if changes# 
         ; Change-tracking:
         (let [_# (edt-assert)
               ~arg-vals-sym ~(apply vector (rest call))]
           (swap! ~root-atom-sym assoc :debug-java-changes 
             (conj changes# (into [] (concat (vector (quote ~(first call))) ~arg-vals-sym)))) 
           (~(first call) ~@(mapv #(list `nth arg-vals-sym %) (range 0 (dec (count call))))))
         ~call)))) ; #normal function call.

(defn reflection-on-java-change! [method obj args]
  "The non-macro version for code that uses reflection's .invoke 
   (if obj is nil where we have static stuff and this simply calls the .invoke)"
  ;(println "Reflection-on-java: " (.getName method))
  (if (nil? obj)
    (.invoke method nil (object-array args)) ;static methods.
    (let [root-atom (.getClientProperty obj "root")
          changes (if root-atom (:debug-java-changes @root-atom) (println "WARNING: no root atom for an obj of type" (type obj)))]
      (if changes
        (do (edt-assert)
            (swap! root-atom assoc :debug-java-changes
              (conj changes (into [] (concat [(.getName method)] [obj] (into [] args)))))
            (.invoke method obj (object-array args)))
        (do ;(println "change reflection invoking: " (.getName method) (type obj) (mapv type args))
        (.invoke method obj (object-array args)))))))

(defn simple? [cl]
  "Is the class a simple type, for which we have a flat clojure structure for? Nil maps to nil."
  (if (not (nil? cl))
    (let [cl (if (= (type cl) java.lang.Class) cl (type cl)) ; can pass a class or an object.
          simples (hash-set Boolean Byte Integer Long Short Float Double String
                   Point Dimension Rectangle)]
      (contains? simples cl))))

; Includes ancestor methods.
(defn methods-starts-with [cl pre narg]
  (let [methods (.getMethods (if (string? cl) (eval (read-string cl)) cl))
        n (count pre)
        getter? #(boolean (= (subs (.getName %) 0 n) pre))]
    (filterv #(and (= (count (.getParameterTypes %)) narg) (getter? %)) methods)))

; TODO: collision detection (where two getters map to the same keyword),  how to even handle this?
; TODO: let the user override these methods in widgets.
(defn getters [cl]
  "Getter methods (map from keyword to java.lang.reflect.Method). Includes ancestor methods."
  (let [methods (into [] (concat (methods-starts-with cl "get" 0) (methods-starts-with cl "is" 0)))
        method2kwd #(let [name (.getName %)] 
                      (cond (.startsWith name "get") (keyword (subs name 3))
                            (.startsWith name "is")  (keyword (subs name 2))
                            :else (throw (Exception. (str "Unrecognized getter method name format." name)))))]
    (zipmap (mapv method2kwd methods) methods)))

(defn setters [cl]
  "Setter methods (map from keyword to java.lang.reflect.Method). Includes ancestor methods.
   This method takes about 1 ms, so it is memonized."
  (let [methods (methods-starts-with cl "set" 1)
        method2kwd #(let [name (.getName %)] 
                      (cond (.startsWith name "set") (keyword (subs (.getName %) 3))
                            :else (throw (Exception. (str "Unrecognized setter method name format." name)))))]
    (zipmap (mapv method2kwd methods) methods)))

(defn elementary? [t]
  "Is the class t an elementary type or not."
  (or (= t Boolean) (= t Byte) (= t Integer) (= t Long) (= t Short) (= t Float) (= t Double) (= t String)))

(defn elementary-pack? [t]
  "Does the class t consists only of elementry data-types?
   Very few things are elementary-packs so this is probably not too useful."
  (let [g (dissoc (getters t) :Class) gk (apply hash-set (keys g))
        s (dissoc (setters t) :Class) sk (apply hash-set (keys s))]
        (println (keys g))
        (println (mapv #(.getReturnType %) (vals g)))
    (if (= (+ (count (set/difference gk sk)) (count (set/difference sk gk))) 0) false
       (= (count (filterv not (mapv #(elementary? (.getReturnType %)) (vals g)))) 0))))

(defn clj-simples [obj]
  "Converts simple java objects into clojure data structures. 
   Returns nil for unrecognized classes (or if the input is nil).
   Some conventions are changed slightly:
   Point is a vector of two numbers rather than a clunkier map with :x and :y.
   We in general keep arrays as native java objects (performance).
   Best practice is to avoid modification unless they are only used locally."
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

(defn java-simples [cl x]
  "Tries to x to a java object of type cl, where x is a 'simple' structure like string.
   Returns nil if it fails."
  (cond
    (nil? cl) nil
    (= cl Boolean) (boolean x)
    (= cl Byte) (byte x) (= cl Integer) (int x) (= cl Long) (long x)
    (= cl Short) (short x) (= cl Float) (float x) (= cl Double) (double x)
    (= cl String) (str x)
    (= cl java.awt.Point)
    (Point. (int (first x)) (int (second x)))
    (= cl java.awt.Dimension)
    (Dimension. (int (first x)) (int (second x)))
    (= cl java.awt.Rectangle)
    (Rectangle. (int (first x)) (int (second x)) (int (nth x 3)) (int (nth x 4)))
    :else nil))

(def last-get-single-level-error (atom nil))

; Does this cost a significant amount in performance?
; TODO: better catching than swallowing the exception.
(defn get-single-level
   "Gets all simple stuff from obj using reflection (with getters).
    A method getFoo() becomes :Foo in the returned map, unless
    getFoo() returns something that isn't in clj-simples, then no :Foo field is created.
    Exceptions are swallowed but the most recent one is stored in @last-get-single-level-error.
    Optional argument: do we get things we can't simplify?
    Nil values DONT return associated keys.
    pred is a function of a KEYWORD that determines whether we include it (or set to true to include all).
    Currently this is NOT used for the java components, since we have very simple defaults.
    It us only used for the events and the graphics states."
  ([obj pred] (get-single-level obj pred false))
  ([obj pred non-simplifiable?] 
    (if (not (nil? obj))
      (let [pred (if (fn? pred) pred (fn [_] pred)) ; value or function
            target-methods_ ((time-limited-memonize getters) (.getClass obj))
            target-methods (select-keys target-methods_ (filterv pred (keys target-methods_)))
            cljify #(let [x (clj-simples %)] (if (and (nil? x) non-simplifiable?) % x))
            results (mapv #(cljify (try (.invoke % obj (into-array Object []))
                                      (catch Exception e (do (reset! last-get-single-level-error e) nil))))
                      (vals target-methods))
            out (into {} (remove (comp nil? second) (zipmap (keys target-methods) results)))]
        out))))

(def _salt {:asdfdghterwedfsdgbtredfgerw 23432.23112})

(def last-set-single-level!-error (atom nil))
(defn set-single-level!
   "Single level for setter methods, only sets changes if kvs-old is supplied.
     keys in kvs that are unrecognized are ignored.
     The values can either be java objects or clojure datastructures (for 'simple' java objects).
     Exceptions are swallowed but the most recent one is stored in @last-set-single-level!-error."
  ([obj kvs] (set-single-level! obj (zipmap (keys kvs) (repeat (count (keys kvs)) _salt)) kvs)) ; _salt => treat kvs-old as different from kvs always.
  ([obj kvs-old kvs] ;Only sets the updates.
    (if (not (nil? obj))
      (let [target-methods ((time-limited-memonize setters) (.getClass obj))
            ; Keep track of changes if need be.
            root-atom (try (.getClientProperty obj "root") (catch Exception e nil))
            tracker (if (instance? clojure.lang.Atom root-atom) (:debug-java-changes @root-atom))]
        (mapv (fn [k] 
                (let [v (get kvs k) v-old (get kvs-old k)]
                  (if (and (get target-methods k) (not= v v-old)) ; method detected and update.
                    (let [cl (first (.getParameterTypes (get target-methods k)))
                          j (let [j0 (java-simples cl v)] (if (nil? j0) v j0))] ; try to convert into a simple java, otherwise assume we are already a java object.
                      (if (or (not (nil? j)) (nil? v)) ; nil? j means it's not simple, unless it was nil to begin with.
                          (do ;(println "set single level:" v-old v (= v v-old))
                            (try 
                              (if tracker (reflection-on-java-change! (get target-methods k) obj (into-array Object [j]))
                                (do 
                                  ;(println "Trying to invoke: " (.getName (get target-methods k)) "on: " (str obj))
                                  (.invoke (get target-methods k) obj (into-array Object [j]))
                                  ;(println "Sucessfully invoked: " (.getName (get target-methods k)) "on: " (str obj))
                                  ))
                                  
                                    (catch Exception e 
                                      (do (reset! last-set-single-level!-error {:Exception e :key k :obj obj :args j})
                                          ;(println "set-single-level! error for: " (type obj) j)
                                          )))))))))
          (keys kvs))))))