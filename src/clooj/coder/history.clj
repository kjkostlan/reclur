; Keeps track of changes to atoms and java objects, etc.
; Used for functional reactive programming.
(ns clooj.coder.history
  (:require [clooj.collections :as collections] [clojure.pprint :as pprint]
    [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Functions for recording history (to a willing atom) ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro atom-update [call]
  "Applies call to atom and stores the result in :history.
   Either deref, swap!, reset!, or compare-and-set!, recording the
   result into :function-history if it is specified and is a vector.
   If :history isn't specified we run the function normally.
   An error is thrown if the atom's value or the return fn's value is not a map.
   Fields in each element of :history:
    :call = simply a list of all commands.
    :where = [the namespace name (almost the filename), line, column].
    :result = what the call's return value is.
    :t0, :t1 = time b4 and after call. for swap! t0 is b4 the beginning of the first call; t1 after the end of the last call.
    :deref = the value of the atom b4 the update.
    :mutate = the new value of the atom after the update.
    :Thread = the java thread obj calling this.
    This function preserves atomity.
    This macro returns the same value of the unwrapped call.
    WARNING: if the args inside call are (), this macro will evaluate them twice."
  (let [f (first call) at (second call) args (drop 2 call) t0 (gensym "t0")
        meta-form (meta &form) x [(str *ns*) (:line meta-form) (:column meta-form)]] ; Location where we are
    `(if (:history @~at)
       (let [~t0 (. System (nanoTime))]
         ~(cond (or (= f 'swap!) (= f `swap!)) ; The history is only updated at the last swap.
           `(swap! ~at
              (fn [st#] (let [h# (:history st#) resu# (~(first args) st# ~@(rest args))
                              st1# (dissoc st# :history) resu1# (dissoc resu# :history)
                              h1# (conj h# {:call (list ~@call) :where ~x :result resu1# :t0 ~t0 :t1 (. System (nanoTime)) :deref st1# :mutate resu1# :Thread (Thread/currentThread)})]
                          (assoc resu# :history h1#))))
           (or (= f 'reset!) (= f `reset!))
           `(swap! ~at
              (fn [st#] (let [h# (:history st#) resu# ~(first args)
                              st1# (dissoc st# :history) resu1# (dissoc resu# :history)
                              h1# (conj h# {:call (list reset! ~at resu1#) :where ~x :result resu1# :t0 ~t0 :t1 (. System (nanoTime)) :deref st1# :mutate resu1# :Thread (Thread/currentThread)})]
                          (assoc resu# :history h1#))))
           (or (= f 'compare-and-set!) (= f `compare-and-set!)) ; the :history is only updated if we have a change. Note: it uses identical not =.
           `(let [changed?# (atom false)]
              (swap! ~at
                (fn [st#] (let [h# (:history st#) old-val# ~(first args) resu# ~(second args)
                                st1# (dissoc st# :history) resu1# (dissoc resu# :history)
                                h1# (conj h# {:call (list ~@call) :where ~x :result resu1# :t0 ~t0 :t1 (. System (nanoTime)) :deref st1# :mutate resu1# :Thread (Thread/currentThread)})]
                            (if (= st# old-val#) ; the compare part.
                              (do (reset! changed?# true) (assoc resu# :history h1#))
                              (do (reset! changed?# false) st#))))) @changed?#)
          (or (= f 'deref) (= f `deref))
          `(swap! ~at 
             (fn [st#] (let [h# (:history st#) st1# (dissoc st# :history)
                             h1# (conj h# {:call (list ~@call) :where ~x :result st1# :t0 ~t0 :t1 (. System (nanoTime)) :deref st1# :Thread (Thread/currentThread)})]
                         (assoc st# :history h1#))))
          :else (throw (Exception. (str "Unrecognized function name: " f " " (= f 'reset!)))))) ~call)))

(defmacro java-update [call the-atom]
  "Like atom-update but records changes (or inquires) to a java object in the-atom.
   The java update and the changes to the atom aren't toghether atomic (this would be almost impossible with mutable java objects).
   Again, this macro returns the same value of the unwrapped call.
   WARNING: if the args inside call are (), this macro will evaluate them twice."
  (let [meta-form (meta &form) x [(str *ns*) (:line meta-form) (:column meta-form)]]
    `(if (:history @~the-atom)
       (let [t0# (. System (nanoTime)) result# ~call]
         (swap! ~the-atom #(assoc % :history     ; Quote the first element of call b/c java .functions don't resolve at compile time.
                             (conj (:history %) {:call (list '~(first call) ~@(rest call)) :where ~x :result result# :t0 t0# :t1 (. System (nanoTime)) :Thread (Thread/currentThread)})))
         result#) ~call)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;; Functions to visualize and analyze the history ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _strtype [x] ; abbreviated form.
  (-> x type str (collections/sus 6) (string/replace #"clojure\.lang\." "") (string/replace #"java\.lang\." "")))
(defn _str-fn [f] ; abbreviated form.
  (-> f str (string/replace #"_+BANG_+" "!") (string/replace #"_+QMARK?_+\d*" "?")
    (string/replace #"_+EQ_+" "=") (string/replace #"_+STAR_+" "*") (string/replace #"_+SLASH_+" "/")
    (string/replace #"_+MINUS_+" "-") (string/replace #"_+PLUS_+" "+")
    (string/replace #"_+LT_+" "<") (string/replace #"_+GT_+" ">")
    (string/replace #"clojure.core\$" "") (string/replace #"@[0-9a-fA-F]+" "")))
(defn obj-to-ptr [x & abbrev-fns?]
   "Recursivly converts objects in x to pointers. Preserves lazyness (so can take infinite seqs).
    Useful for generting printouts because atoms may reference themselves."
  (cond (coll? x) (collections/cmap :flatten #(obj-to-ptr % abbrev-fns?) x) 
     (or (number? x) (string? x) (char? x) (symbol? x) (keyword? x) 
         (instance? java.util.regex.Pattern x)
         (instance? java.lang.Thread x)) x
     (and (first abbrev-fns?) (fn? x)) (symbol (_str-fn x))
     :else (symbol (str (_strtype x) (System/identityHashCode x)))))

(defn hprint [x]
  "History-compatable printing that does not get into infinite loops when an atom stores itself.
   We need many more tools because the history quickly gets long-winded."
  (pprint/pprint (obj-to-ptr x true)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Debug/testing scratchwork ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(clooj.coder.repl/clc)

;(def x (atom {:val [1 2 3] :history []}))

;((if true clooj.coder.repl/previty pprint/pprint)
;  (macroexpand '(atom-update (swap! x #(assoc % :val (first (:val %)))))))

;(atom-update (reset! x {:val [2 3 4]}))
;(atom-update (swap! x #(assoc % :val (+ (first (:val %)) 10))))
;(atom-update (deref! x))

;((if true clooj.coder.repl/previty pprint/pprint)
;  (macroexpand '(java-update (.setText textBox "txt") the-atom)))

;(hprint @x)
