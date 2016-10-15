; Wrapper for the GUI. Also the thread gatekeeper (safe to call these fns from any thread).

; The GUI state itself is stored as an immutable clj tree structure:
; {... :Children}.
; Within ... is :Type (options are JPanel, JScrollPane, etc).
;        TODO: :Layout, which has :Type as well.
;        other stuff as well.
; Keep things functional!

; Cliant properties store refereces to the main atom:
   ; "root" stores the atom holding the root (including the objects).
   ; "path" stores a vector of keys that gets us from the root to the clojure data.
      ; Example: [:state :Children 2 :Children 0] => :state to the clojure data, then drill down two levels of children.

(ns clooj.java.gui
  (:require [clooj.java.layout :as layout] [clooj.java.widget :as widget]
            [clooj.java.listener :as listener]
            [clooj.java.updater :as updater]
            [clooj.coder.grammer :as grammer]
            [clooj.java.errcatch :as errcatch]
            [clooj.java.thread :as thread]
            [clooj.collections :as collections]
            [clooj.coder.history :as history]
            [clooj.java.clojurize :as clojurize])
  (:import [javax.swing SwingUtilities] [java.awt Window]))
(def debug (atom []))
; TODO: should thse belong in clooj.java.thread?
(defn edt? [] (SwingUtilities/isEventDispatchThread))

;;;;;;;;;;;;;;;;;;;;;;;; Dangerous debugging of atoms: ;;;;;;;;;;;;;;;;;;;;;
(def debug-reset-traces (atom []))
(defn filter-reset-traces [at] (filterv #(= (:atom %) at) @debug-reset-traces))
(def DANGER-debug-atoms false)
(if DANGER-debug-atoms
  (alter-var-root ; these three words spell danger.
    (var clojure.core/reset!)
    (fn [_] (fn [^clojure.lang.IAtom at newval]
              (do (if DANGER-debug-atoms (swap! debug-reset-traces conj {:atom at :trace (thread/get-stack-trace) :oldval @at :newval newval})) 
                (.reset at newval))))))
(if DANGER-debug-atoms (println "Warning: gui/DANGER-debug-atoms is enabled."))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn control-edt? [f on-edt?]
  "Calls (f) either on or off the event dispatch thread.
   Returns a future (which may be simply wrapping f's output)."
  (cond
    (or (and (not (edt?)) (not on-edt?)) (and (edt?) on-edt?)) 
    (let [out (f)] (future out)) ; run on this thread then wrap into a future.
    (and (edt?) (not on-edt?))
    (future (f)) ; run on another thread like a normal future.
    :else ; run on the edt, setting an atom when done. The future spins to keep checking the atom.
    ; note the difference: (future (+ 1 2)) vs (SwingUtilities/invokeLater #(+ 1 2))
    (let [a (atom nil) done (atom false) f1 #(do (reset! a (f)) (reset! done true))
          _ (SwingUtilities/invokeLater #(f1))
          dt (atom 0.001) ; longer and longer checking intervals, starting at 1us ending at 1s.
          spin #(do (while (not @done) ; spin step.
                    (do (Thread/sleep (long @dt) (int (* @dt 1000)))
                        (reset! dt (Math/min (* @dt 2) 1000)))) @a)] ; @a when we know @ has been set.
      (future (spin)))))

(defn dispose-all-windows!! []
  "Simply disposes all windows. MEMORY LEAK WARNING: Does not run windowClosing events.
   Does not call any updating to java changes."
  (control-edt? (fn [] (let [windows (Window/getWindows)] (mapv #(.dispose %) windows))) true))

; TODO: is this call to .setVisible nessessary?

(defn _set-vis! [state java path]
  ; sets whether stuff is :Visible, recursivly. 
  ; Called at the end for performance and anti flicker (windows bieng shown then moved immediatly).
  ; only use when starting up, not when updating.
  (let [obj (get-in java (concat path [:obj]))
        root-atom (.getClientProperty obj "root")
        substate (get-in state path)
        visible (if (:Visible obj) (:Visible obj) true)] ; Visible not specified = true. 
    (history/java-update (.setVisible obj visible) root-atom)
    (mapv #(_set-vis! state java %)
      (mapv #(conj path :Children %) (collections/ckeys (:Children substate)))))) ; the children are 1:1.

(defn setup [state & opts]
  "Creates the actual GUI from an (immutable) clojure data-structure that reperesents the GUI state.
   Returns an atom with :state and :java as well as some other flags (it may take some time for the java objects to fully setup).
   opts has:
      :history = track the history. Useful for debugging.
   The atom is created atomisticly if we are NOT on the edt (the java may take longer?)
     if we are on the edt we can check for :setting-up? = true, but we may deadlock if we wait for it.
     :state IS atomistic."
  (let [opts (cond (= (count opts) 0) {}  ; no second arg.
                   (= (count opts) 1) (first opts) ; a map as the second arg.
                   :else (if (even? (count opts))
                           (zipmap (take-nth 2 opts) (take-nth 2 (rest opts)))
                         (throw (Exception. "The second argument must be a map"))))  ;an array of key-value pairs.
        stated (try (updater/add-defaults-recursive state)
                 (catch Exception e (do (println "Error adding defaults:" e) (throw (Exception. "See above message")))))
        
        root-atom (atom {:state stated :setting-up? true :updating-java-components? true})
        _ (if (:history opts) (swap! root-atom assoc :history [])) ; track changes to the different java objects.
        IaW #(SwingUtilities/invokeAndWait (errcatch/wrap %)) ; takes in a function.
        setup-gui!! (fn [] ; ran NOT on the edt.
                     ; invoke-and-wait all our calls.
                     ; this allows using invokeAndWait, ensuring all events have been fired, etc.
                     (IaW #(try (updater/setup! root-atom) (catch Exception e (println "Error setting up:" e))))
                     (let [update-fns {:update! updater/update! :process-event updater/process-event}
                           root @root-atom]
                       ; An extra update seems to help reduce flicker:
                       (IaW #(try (updater/update! @root-atom stated false update-fns root-atom "extra update on setup") (catch Exception e (println "Error re-updating:" e))))
                       
                       ; set visible at the end to reduce flicker:
                       (IaW #(try (_set-vis! (:state root) (:java root) []) (catch Exception e (println "Error setting visible:" e))))
                       ; re-update in case we changed stuff upon _set-vis! (due to events triggering).
                       ; If there is a change it will show as a flicker, but it helps ensure the state is proper.
                       (IaW #(try (updater/update! @root-atom stated false update-fns root-atom "final just-in-case update on setup") (catch Exception e (println "Error re-updating:" e))))
                       (swap! root-atom assoc :setting-up? false)))]
    ; If we are not on the edt, the deref has negligable effect but makes sure that we are fully done modifying the atom and with the edt queue.
    ; if we are on the edt, derefing would deadlock with the IaW call.
    (let [f (control-edt? (errcatch/wrap setup-gui!!) false)] (if (not edt?) @f)) root-atom))

(defn delete! [root-atom]
  "Deletes 'everything' by removing the top level GUI component and nilling the atom. The GC should handle the rest.
   IMPORTANT: delete! must be called upon bieng finished or else there will be a memory leak (window closings will call delete)."
  (control-edt? (errcatch/wrap #(updater/delete! root-atom)) true))

(defn get-state [root-atom]
  "Gets state describing the gui, not the java objects themselves. This will be immutable (though it may contain mutables).
   Useful for storing the old GUI structure and reverting if need be."
  (:state @root-atom))

(defn update! [root-atom new-state]
  "Updates a state. Does not return anything."
  (control-edt? (errcatch/wrap #(updater/update! root-atom (updater/add-defaults-recursive new-state) "gui/update call")) true))

;(defn debug-munge-java-changes [debug-java-changes munge-java?]
;  "Represents the java changes in a terse format that does not generate infinite loops.
;   (the java objects have references to root atom which references back to the objects)."
;  (loop [ix 0 out [] munge {} cl-counts {}] ; outer loop through each change.
;    (if (= ix (count debug-java-changes)) out
;      (let [ch (nth debug-java-changes ix) ; single change, multible arguments.
;            x (loop [jx 0 jout [] jmunge munge jcl-counts cl-counts] ; inner loop through each argument.
;                  (if (= jx (count ch)) {:munge jmunge :outi jout :cl-counts jcl-counts}
;                    (let [arg (nth ch jx)
;                          cl (.replace (.replace (str (type arg)) "class " "") "clooj.java.widget.proxy$" "")
;                          ;  Somethings don't need to be changed:
;                          m (if (or (contains? #{"clojure.lang.Atom"} cl) ; we may add more to this hash-set. 
;                                  (and munge-java? (or (.contains cl ".swing.") (.contains cl "JFrameClient")))) ; stuff to munge.
;                                (get munge arg) arg)
;                          nil2zero #(if (nil? %) 0 %)
;                          m-new (if (nil? m) (str cl (nil2zero (get jcl-counts cl))) m)]
;                   (recur (inc jx) (conj jout m-new) 
;                     (assoc jmunge arg m-new) (assoc jcl-counts cl (inc (nil2zero (get jcl-counts cl))))))))]
;      (recur (inc ix) (conj out (:outi x)) (:munge x) (:cl-counts x))))))