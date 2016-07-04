; Repl that uses futures so that multiple calculations can be active.
(ns clooj.coder.repl
  (:require [clooj.java.file :as jfile]))

; List of calculations that are running or finished:
; Each calculation:
;   :cmd is the string input.
;   :ns is what namespace it is evaled in.
;   :future is the future object that will yield the string result as would be seen in the repl.
(def calculations (atom []))

; Update the bindings (the bindings are global, surprisingly).
(def star-vars ['*ns* '*compile-path* '*unchecked-math* '*warn-on-reflection*]) ; TODO: are there more? 

(defmacro binding-wrap [m code] ;m is a map from symbols to namespaces for bindings.
  (let [binding-vec (into [] (apply concat (mapv (fn [v] [v `(get ~m (quote ~v))]) star-vars)))]
    `(binding ~binding-vec ~code)))
(defmacro get-bindings [] ; returns a map with the bindings.
  (zipmap (mapv #(list 'quote %) star-vars) star-vars))
(def bindings (atom (zipmap star-vars (mapv eval star-vars)))) ; start with the default namespaces.


; Create our own repl namespace with (use) access to the clojure.core and this file.
; The future wrap allows waiting for this file to be compiled before (use)ing it.
; We have to put it at the end of the file.
(defonce user-ns (create-ns 'user))
(swap! bindings assoc '*ns* user-ns)

(def default-bindings @bindings)


(defn abridged-str [cmd val our-ns] 
  "An abridged string representation that is formatted repl-style."
  (let [max-length 1000 ; a bit low but there are performance issues with the textarea for now.
        limit #(let [s (try (str %) (catch Exception e (str "STRING REPRESENTATION ERROR: " e)))]
                 (if (> (count s) max-length) (str (subs s 0 max-length) "...<too large to show>") s))]
   (str our-ns "=> " (limit cmd) "\n" (limit val))))

; Save the calculation string so that we can look it up in O(1) time.
(def cached-string (atom nil)) ; all the commands.
(defn update-cached-string!! []
  "Every time the string would change."
	(let [calcs @calculations 
		  cmds (mapv :cmd calcs) futs (mapv :future calcs) ns-s (mapv :ns calcs)
		  ; If some become done partway through this loop no big deal:
		  done?s (mapv future-done? futs)
		  strs (mapv #(if %3 @%2 (abridged-str %1 "<in progress>" %4)) cmds futs done?s ns-s)
		  liny #(apply str (interpose "\n" %))]
	  (reset! cached-string (liny strs))))

(defn eval!! [cmd]
   "Evaluate cmd (a string) in our repl-ns, returning the result.
    Returns a vector with a header iff there are multible commands, otherwise just returns the command.
    Does not catch errors."
  (binding-wrap @bindings
    (let [codes (read-string (str "[" cmd "]"))
          results (mapv eval codes) nr (count results)]
      (reset! bindings (get-bindings)) ; the eval may have changed the results.
      (cond (= nr 0) [] ; can this ever happen?
            (= nr 1) (first results)
            :else (apply list (concat ["Multible commands:    "] (interpose "    " results)))))))

(defmacro undef [var]
  "Undefines a variable. e.g. (undef foo) no quotes on foo as we are a macro."
  `(ns-unmap repl-ns (quote ~var)))

(defn add-to-repl!! 
    "Sends a command to the repl, running it on a future, returning immediately.
     eval false means that we don't actually evaluate anything."
  ([cmd] (add-to-repl!! cmd true))
  ([cmd eval?] 
    (let [; Two resets! of the cached-string. Both are soon-after we make a change to the calculation array.
          ; soon-after is safe but soon-before isn't if the @calculations is between the two changes.
          ns-cmd (get @bindings '*ns*)
          cmd (str cmd) fut (future (if eval? (abridged-str cmd (try (eval!! cmd) (catch Exception e e)) ns-cmd) cmd))]
      (swap! calculations conj {:cmd cmd :future fut :ns ns-cmd})
      ; Two updates to the cached string (AFTER the swap! calculations step):
      ; The first is for "we have a new calculation". The second is for "we are done".
      (future (do (update-cached-string!!) @fut (update-cached-string!!))))))
(defn get-repl-output [] @cached-string)

(defn clear-done-cmds!! []
  "Clears all calculations whose future is done.
   Use this upon user requesting to clear the repl (followed by a string)."
  (swap! calculations (fn [c] (filterv #(not (future-done? (:future %))) c)))
  (future (update-cached-string!!)))

(defn clear-all-cmds!! []
  "Clears all commands from the repl queue, using future-cancel.
   Note: future-cancel is NOT guaranteed to stop the CPU/memory usage."
  (swap! calculations (fn [c] (mapv #(future-cancel (:future %)) c))) ; it's fine to cancel a done future.
  (reset! calculations [])
  (future (update-cached-string!!)))

(defn reload-file!! [file]
  "Orthogonal to the other functions.
   Reloads a file's namespace. ANY namespace that uses said file will now use updated variables.
   HOWEVER, deleted variables (or the old name for renamed ones) are still accessable untill program restarts. TODO: fix this.
   Finally, this can print an error to the console despite working properly at times. TODO: thier bug.
   Does not catch errors."
  (let [dotpath (jfile/file2namespace file) namespace (create-ns '_debugger.namespace)]
    (binding [*ns* namespace] (require (symbol dotpath) :reload))))

(defn reset-ns!! []
  (reset! bindings default-bindings)
  (let [nms (get @bindings '*ns*)]
    ;http://stackoverflow.com/questions/3636364/can-i-clean-the-repl
    (mapv #(ns-unmap nms %) (keys (ns-interns nms)))))

; Do this at the end so we capture all functions.
(future (while (try  (binding [*ns* user-ns] 
                        ; Use the clojure.core libraries and fns from this file:
                       (eval '(clojure.core/use 'clojure.core))
                       (eval '(clojure.core/use 'clooj.coder.repl))
                       (println "used repl sucessfully.") false)
                 (catch Exception e true)) (Thread/sleep 5)))