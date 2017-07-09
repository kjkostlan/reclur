; The repl node may have a variety of children where each child gets one command.
; All children save a namespace. The namespace can be a user or a core.

(ns clooj.app.claytreerepl
  (:require [clooj.app.claytreetext :as cltext]
    [clooj.app.claytreetree :as clatre]
    [clooj.app.claytreephysics :as claphy]
    [clooj.coder.funs :as funs]))

(def ^:dynamic *max-string-length* 100000) ; reduce lag.

(defn apply-ns [namespace-node]
   "Makes a new namespace on a given :repl node."
   (let [ns1 (funs/make-ns)]
     (cltext/on-text-change (assoc-in (assoc namespace-node :namespace ns1 :type :namespace) [:tbox :pieces] [(str ns1)]))))

(defn new-ns-node []
  "Makes a new namespace."
  (apply-ns (clatre/make-node :namespace "")))

(defn new-repl-node [namespace-node]
  "Makes a new command node that shares the namespace of the :namespace node."
  (let [c (claphy/center namespace-node)
        nv #(assoc-in % [:tbox :visible-spacers?] false)
        f #(assoc (nv %) :physics (assoc (:physics %) :lock-x (+ (first c) 100) :lock-y (+ (second c) 50) :locked? false))]
    (f (assoc (clatre/make-node :repl "REPL") :future nil :namespace (:namespace namespace-node)))))

(defn add-repl-node [namespace-node]
  "Adds a command node to the :namespace node's :children."
  (update namespace-node :children #(conj % (new-repl-node namespace-node))))

(defn shift-enter? [ky-evt]
  "Checks if the command was shift-enter."
  (and (:ShiftDown ky-evt) (= (:KeyCode ky-evt) 10)))

(defn limit-str [s]
    "Length limit on the strings."
  (if (<= (count (str s)) *max-string-length*) s 
    (str (subs (str s) 0 *max-string-length*) "... <Too large to show>")))

(defn update-if-done [cmd-node]
  "Updates the cmd node to reflect the result if the futures finish. 
   Call this every frame for all visible nodes."
 (if-let [f (:future cmd-node)]
   (if (future-done? f) 
     (cltext/on-text-change (assoc-in cmd-node [:tbox :pieces 1] (str "\n" @f))) cmd-node) cmd-node))

(defn eval! [cmd-node]
  "Evaluates the first piece of the node sotring the result as a future in the second piece.
   We store a reference to the namespace in the node."
  (let [cmd (first (:pieces (:tbox cmd-node))) 
        fut (future 
              (binding [*ns* (:namespace cmd-node)] 
                (limit-str ; Multible levels of errors:
                  (try (let [code (read-string cmd)]
                         (try (let [x (eval code)]
                                (try (pr-str x)
                                  (catch Exception e (str "String representation error:\n" e))))
                           (catch Exception e (str "Runtime error:\n" (pr-str e)))))
                    (catch Exception e (str "Syntax error:\n" (pr-str e)))))))]
    (cltext/on-text-change 
      (assoc-in (assoc cmd-node :read-only? #{1} :future fut)
                  [:tbox :pieces] [cmd "\n<In Progress>"]))))