;;;;;;;;;;;;;;;;; Copy of https://github.com/bmillare/dj.plurality ;;;;;;;;;;;;;;;;;;;;;;;



;;;;; THIS IS CODE FROM ANOTHER PROJECT ;;;;;

(ns coder.plurality
  (:require [clojure.core.match :as m]))

(defn update-implementation
  "
returns a new plural-fn with updated implementations

updates implementations with new value from call to f on implementations and args

In detail:

A modify-implementations fn must accept the new implementations and
must return the same type of plural-fn but with new implementations

"
  [plural-fn f & args]
  (let [{:keys [implementations modify-implementations]} (:dj.plurality (meta plural-fn))]
    (modify-implementations (apply f implementations args))))

(defn convert-implementations
  "
convenience fn

returns application of f on implementations of plural-fn + args

Used in combination with plural-fn constructors to convert one
plural-fn to another
"
  [plural-fn f & args]
  (apply f
         (-> (meta plural-fn)
             :dj.plurality
             :implementations)
         args))

(defn ->all-fn
  "
plural-fn that returns a seq of the results of all methods
"
  [implementations]
  (with-meta (fn [& args]
               (map #(apply % args)
                    implementations))
    {:dj.plurality {:modify-implementations ->all-fn
                    :implementations implementations}}))

(defn ->broadcast-fn
  [implementations]
  (with-meta (fn [& args]
               (dorun
                (map #(apply % args)
                     implementations)))
    {:dj.plurality {:modify-implementations ->broadcast-fn
                    :implementations implementations}}))

(defn ->random-fn
  ([implementations]
     (with-meta (fn [& args]
                  (if (empty? implementations)
                    nil
                    (apply (rand-nth implementations)
                           args)))
       {:dj.plurality {:modify-implementations ->random-fn
                       :implementations implementations}}))
  ([]
     (->random-fn [])))

(defn ->simple-multi-fn
  "
Multimethods without hierarchies.

TODO: Add hierarchies.

implementations must be a map of dispatch-values -> method fns
dispatch-fn is like a multimethod dispatch-fn

Arity optimized for 5 or less args, but supports greater arity.
"
  ([implementations dispatch-fn]
     (with-meta (fn
                  ([a1]
                     ((implementations (dispatch-fn a1))
                      a1))
                  ([a1 a2]
                     ((implementations (dispatch-fn a1 a2))
                      a1
                      a2))
                  ([a1 a2 a3]
                     ((implementations (dispatch-fn a1 a2 a3))
                      a1
                      a2
                      a3))
                  ([a1 a2 a3 a4]
                     ((implementations (dispatch-fn a1 a2 a3 a4))
                      a1
                      a2
                      a3
                      a4))
                  ([a1 a2 a3 a4 a5]
                     ((implementations (dispatch-fn a1 a2 a3 a4 a5))
                      a1
                      a2
                      a3
                      a4
                      a5))
                  ([a1 a2 a3 a4 a5 & args]
                     (apply (implementations (apply dispatch-fn a1 a2 a3 a4 a5 args))
                            a1
                            a2
                            a3
                            a4
                            a5
                            args)))
       {:dj.plurality {:modify-implementations (fn [imps]
                                                 (->simple-multi-fn imps dispatch-fn))
                       :implementations implementations}}))
  ([implementations default-imp dispatch-fn]
     (with-meta (fn
                  ([a1]
                     (let [f (implementations (dispatch-fn a1))]
                       (if f
                         (f a1)
                         (default-imp a1))))
                  ([a1 a2]
                     (let [f (implementations (dispatch-fn a1 a2))]
                       (if f
                         (f a1
                            a2)
                         (default-imp
                           a1
                           a2))))
                  ([a1 a2 a3]
                     (let [f (implementations (dispatch-fn a1 a2 a3))]
                       (if f
                         (f a1
                            a2
                            a3)
                         (default-imp
                           a1
                           a2
                           a3))))
                  ([a1 a2 a3 a4]
                     (let [f (implementations (dispatch-fn a1 a2 a3 a4))]
                       (if f
                         (f a1
                            a2
                            a3
                            a4)
                         (default-imp
                           a1
                           a2
                           a3
                           a4))))
                  ([a1 a2 a3 a4 a5]
                     (let [f (implementations (dispatch-fn a1 a2 a3 a4 a5))]
                       (if f
                         (f a1
                            a2
                            a3
                            a4
                            a5)
                         (default-imp
                           a1
                           a2
                           a3
                           a4
                           a5))))
                  ([a1 a2 a3 a4 a5 & args]
                     (let [f (implementations (apply dispatch-fn a1 a2 a3 a4 a5 args))]
                       (if f
                         (apply f
                                a1
                                a2
                                a3
                                a4
                                a5
                                args)
                         (apply default-imp
                                a1
                                a2
                                a3
                                a4
                                a5
                                args)))))
       {:dj.plurality {:modify-implementations (fn [imps]
                                                 (->simple-multi-fn imps default-imp dispatch-fn))
                       :implementations implementations}})))

(defn ->recursive-simple-multi-fn
  "
Experimental!

Multimethods with recursion off multi-fn but without using vars (but
using very fast AtomicReferences for delayed binding)

values of :implementations map must accept the multifn and return the implementation

ex:
 {:some-key
  (fn [multi-fn] (fn [x y] (+ (multi-fn x) y)))}

ie. the called implementation will be generated using values in :implementations

Arity optimized for 5 or less args, but supports greater arity.
"
  [implementations default-imp dispatch-fn]
  (let [ ;; this is a really fast reference, slightly faster than atoms, and much faster than vars
        implementations-ref (java.util.concurrent.atomic.AtomicReference.)
        default-imp-ref (java.util.concurrent.atomic.AtomicReference.)
        multi-fn (fn
                   ([a1]
                      (let [f ((.get implementations-ref) (dispatch-fn a1))]
                        (if f
                          (f a1)
                          ((.get default-imp-ref) a1))))
                   ([a1 a2]
                      (let [f ((.get implementations-ref) (dispatch-fn a1 a2))]
                        (if f
                          (f a1
                             a2)
                          ((.get default-imp-ref)
                            a1
                            a2))))
                   ([a1 a2 a3]
                      (let [f ((.get implementations-ref) (dispatch-fn a1 a2 a3))]
                        (if f
                          (f a1
                             a2
                             a3)
                          ((.get default-imp-ref)
                            a1
                            a2
                            a3))))
                   ([a1 a2 a3 a4]
                      (let [f ((.get implementations-ref) (dispatch-fn a1 a2 a3 a4))]
                        (if f
                          (f a1
                             a2
                             a3
                             a4)
                          ((.get default-imp-ref)
                            a1
                            a2
                            a3
                            a4))))
                   ([a1 a2 a3 a4 a5]
                      (let [f ((.get implementations-ref) (dispatch-fn a1 a2 a3 a4 a5))]
                        (if f
                          (f a1
                             a2
                             a3
                             a4
                             a5)
                          ((.get default-imp-ref)
                            a1
                            a2
                            a3
                            a4
                            a5))))
                   ([a1 a2 a3 a4 a5 & args]
                      (let [f ((.get implementations-ref) (apply dispatch-fn a1 a2 a3 a4 a5 args))]
                        (if f
                          (apply f
                                 a1
                                 a2
                                 a3
                                 a4
                                 a5
                                 args)
                          (apply (.get default-imp-ref)
                                 a1
                                 a2
                                 a3
                                 a4
                                 a5
                                 args)))))]
    (.set implementations-ref (reduce-kv (fn [ret k v]
                                           (assoc ret
                                             k
                                             (v multi-fn)))
                                         {}
                                         implementations))
    (.set default-imp-ref (default-imp multi-fn))
    (with-meta multi-fn
      {:dj.plurality {:modify-implementations (fn [imps]
                                                (->recursive-simple-multi-fn imps default-imp dispatch-fn))
                      :implementations implementations}})))

(defn ->simple-predicate-fn
  [implementations]
  (with-meta (fn [& args]
               (let [match (->> implementations
                                (filter (fn [[pred? implementation]]
                                          (apply pred? args)))
                                first)]
                 (apply match args)))
    {:dj.plurality {:modify-implementations ->simple-predicate-fn
                    :implementations implementations}}))

(defmacro ->macro-predicate-fn
  "
implementations should be a literal vector of pairs, matcing form -> fn

Only up to 5 arity is supported
"
  [implementations]
  `(let [imps# ~implementations]
     (with-meta ~(if (empty? implementations)
                   `(fn [])
                   (case (count (first (first implementations)))
                     1 `(fn [a1#]
                          ((m/match [a1#]
                                    ~@(apply concat implementations))
                           a1#))
                     2 `(fn [a1# a2#]
                          ((m/match [a1# a2#]
                                    ~@(apply concat implementations))
                           a1# a2#))
                     3 `(fn [a1# a2# a3#]
                          ((m/match [a1# a2# a3#]
                                    ~@(apply concat implementations))
                           a1# a2# a3#))
                     4 `(fn [a1# a2# a3# a4#]
                          ((m/match [a1# a2# a3# a4#]
                                    ~@(apply concat implementations))
                           a1# a2# a3# a4#))
                     5 `(fn [a1# a2# a3# a4# a5#]
                          ((m/match [a1# a2# a3# a4# a5#]
                                    ~@(apply concat implementations))
                           a1# a2# a3# a4# a5#))))
       {:dj.plurality {:modify-implementations ~(let [imps (gensym "imps")]
                                                  `(fn [~imps]
                                                     ;; Since this is a
                                                     ;; macro, a recursive
                                                     ;; call would create an
                                                     ;; infinite expansion,
                                                     ;; we wrap this in an
                                                     ;; eval to delay
                                                     ;; computation.
                                                     (-> `(dj.plurality/->macro-predicate-fn
                                                           ~~imps)
                                                         macroexpand-1
                                                         eval)))
                       :implementations imps#}})))

;; ----------------------------------------------------------------------

(defmacro defplural
  "
Convenience macro to create a var

I suggest using values when possible or refs when you need more
control with concurrency
"
  [name constructor & args]
  `(def ~name
     (~constructor ~@args)))

(defmacro defimplementation
  "
Convenience macro to modify an implementation

You need to specify how to add implementation (depends on datastructure)

I suggest using values when possible or refs when you need more
control with concurrency
"
  [name constructor f & args]
  `(alter-var-root ~name
                   update-implementation
                   ~f
                   ~@args))