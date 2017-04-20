; Functional namespaces that preserve immutability (shallow copy on modify).
; Used in conjunction with the repl.
; There is no mutability.
(ns clooj.coder.funs 
  (:require [clojure.set :as set] [clojure.walk :as walk] [clooj.collections :as collections]))

(defonce funs-as (gensym "funsas")) ; Used for the exotic set! command.

(defn make-ns []
  "Sets up the namespace with a few defaults."
  (let [en-s (create-ns (gensym "funs"))]
    (binding [*ns* en-s] 
      (eval (list 'clojure.core/use ''clojure.core))  ; Probably a better way than eval here.
      (eval (list 'require (list 'quote ['clooj.coder.funs :as funs-as])))) en-s))

(defn make-nsbun []
  "Makes a namespace bundle: a namespace, printouts, bindings, and the repl's result."
   {:ns (make-ns)
    :bindings {} ; Symbol -> value. The set! command is rebound to store values here.
    :result nil ; The last result.
    :futures [] ; {:result :bindings :error} of things in in progress.
    :error nil}) ; The last error (an Exception object), set to nil if there is no error.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; Copy on modify code ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn copy-ns [ns0]
  "Shallow copy of a namespace.
   We treat this separately from the (set!) stuff.
   Atoms et al are not copied (of course), they are shared across both namespaces.
   Var roots may not be copied.
   Copying a namespace takes about 3ms, and there is a memory leak
   that becomes significant after about 30000 copies, which isn't a problem if we are
   copying only once per command for a modest session.
   Hopefully importing stuff doesn't decrease this buffer much!"
  (let [i0 (ns-imports ns0)
        r (ns-refers ns0)
        v (ns-interns ns0)
        ns1 (make-ns)
        i1 (ns-imports ns1)
        i (set/difference (apply hash-set (vals i0)) (apply hash-set (vals i1)))] ; The 96 free imports take ~30 ms to do, saving us up to 10 fold.
    (binding [*ns* ns1] 
      (mapv (fn [x] (eval (list 'clojure.core/import* (pr-str x)))) i) ; Imports need an eval.
      (eval (list 'require (list 'quote ['clooj.coder.funs :as funs-as])))) ; Probably a better way than eval here.
    (mapv #(. ^clojure.lang.Namespace ns1 (refer (symbol %1) %2)) (keys r) (vals r)) ; Refers can use the java notation from the source.
    (mapv #(intern ns1 (symbol %1) (deref %2)) (keys v) (vals v)) ; intern is relatively normal. 
    ns1))

(defn copy-nsbun [ns-bun]
   "Copies a namespace bundle."
  (update ns-bun :ns copy-ns)) ; only the :ns needs to be copied explicitly.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Evalutation code ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn up-to-date [ns-bun]
  "Keep us up-to-date with what our futures are doing.
   Applies the futures (which are done) to :bindings, and gets rid of futures that are done.
   Also sets :result and :error to the last element of future done."
  (let [futs (:futures ns-bun)
        done? (mapv future-done? futs) ; pretend that no futures finish after this call.
        futs1 (into [] (keep-indexed (fn [ix x] (if (not (nth done? ix)) x)) futs)); keep the ones not done.
        vals (into [] (keep-indexed (fn [ix x] (if (nth done? ix) @x)) futs))] ; deref the ones done.
    (if (= (count vals) 0) ns-bun ; no vals => no change.
      (assoc ns-bun :futures futs1 :error (:error (last vals)) :result (:result (last vals))
      :bindings (reduce (fn [acc v] (reduce #(assoc %1 %2 (get (:bindings v) %2)) acc (keys (:bindings v)))) 
                  (:bindings ns-bun) vals))))) ; most recent futures replace older futures. 

(defn us-qual [sym] 
  "Qualifies a symbol to us, all working on functions rather than syntax-quotes macros.
   gensym is used to prevent collisions."
  (symbol (str funs-as "/" sym)))

(defn set-check [code]
  "Returns the :symbol and :code of a line of a set! statement.
   Returns nil if there is no set! statement. 
   Throws an error if set! is used elsewhere, not including inside a quote.
   This is for filtering out set! where it would be hard to use."
  (let [s (gensym "setty")
        munge-f (fn [c] (walk/postwalk #(if (= % 'set!) s %) c))
        demunge-f (fn [c] (walk/postwalk #(if (= % s) 'set! %) c))
        msg "In the REPL, set! can only be used as an outer-level statement."
        throw-set-f (fn [c] (walk/postwalk #(if (= % 'set) (throw (Exception. msg)) %) c))
        quote-munge (walk/postwalk #(if (and (list? %) (= (first %) 'quote)) (munge-f %) %) code)]
    (demunge-f (throw-set-f quote-munge)) ; assertion statement.
    (if (= (first code) 'set!) {:symbol (second code) :code (rest (rest code))})))

(defn bindify [code bindings]
  "Generates code with the bindings added around the code."
  (list 'binding (into [] (interleave (keys bindings) (vals bindings))) code))

(defn _get-result-in [ns-bun code] ; returns :bindings :result and :error.
  (let [sc (set-check code)
        code-e (try {:code (bindify code (:bindings ns-bun)) :e nil} (catch Exception e {:code code :e e}))]
    (if (:e code-e) (assoc ns-bun :result nil :error (:e code-e)) ; compilation error.
      (binding [*ns* (:ns ns-bun)] ; Evaluate it in the namespace that will be in the returned ns-bun.
        (let [result-e (try {:result (eval (:code code-e)) :error nil} ; mutates our copy of the namespace.
                         (catch Exception e {:result nil :error e}))] 
          ; deref the atoms and put the result/error in:
          {:bindings (if (and sc (nil? (:error result-e))) (assoc (:bindings ns-bun) (:symbol sc) (:result result-e)) (:bindings ns-bun))
            :result (:result result-e) :error (:error result-e)})))))

(defn eval-in [ns-bun0 code]
  "Evaluates code in a namespace-bundle, returning the new namespace-bundle.
   The bundle's :result becomes the new code."
  (let [ns-bun (up-to-date (copy-nsbun ns-bun0))
        resu (_get-result-in ns-bun code)]
    (assoc ns-bun :bindings (:bindings resu) :result (:result resu) :error (:error resu))))

(defn future-eval-in [ns-bun0 code]
  "Evaluates code as a future that will modify the namespace bindings upon completion.
   Does not immediately bind any vars to the namespace.
   The :bindings, :result, and :error are contained in :futures, use @ on it to get the actual result."
  (let [ns-bun (up-to-date (copy-nsbun ns-bun0))
        fut (future (_get-result-in ns-bun code))]
    (update ns-bun :futures #(conj % fut))))