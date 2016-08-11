; Debugging involves you putting in a breakpoint (a call to repl.debugger/breakpoint)
; The actual debug code that needs to be inserted to make a breakpoint:
; See stackoverflow.com/questions/29033853/dynamically-loading-an-aliased-namespace-to-another-clojure-namespace
; A human-in-the-loop debugger.

; Special commands (type these with a /cmd):
; /cont  (continues the debug calculation).
; ... more to come.

(ns clooj.repl.debugger_old
  (:require
            [clooj.utils :as utils]
            [clooj.java.file :as jfile]
            [clooj.app.state_old :as app_old]
            [clojure.string :as string]
            [clooj.coder.tracer :as tracer]
            [clooj.coder.io :as cio]
            [clooj.java.textarea_old :as jtext_old]
            [clooj.coder.grammer :as grammer]
            [clooj.collections :as collections]
            [clooj.coder.namespacer :as namespacer])
  (:import (javax.swing SwingUtilities)))


(defn output [text] 
  "Print text to the output. This calls invoke later so you can call this fn from any thread."
  (SwingUtilities/invokeLater #(jtext_old/append-text! app_old/repl-out-text-area text true))) ;a common function.

;; Stack trace handling.
;; TODO: refactor this exception code into an Exception class except for the parts that actually do the printing.
(defn _singlestak [gotofn!! atomtext line] ; the line has no \n at the end.
  (let [file (jfile/namespace2file (first (string/split line #"/")))
        lineno (dec (Integer. (last (re-seq #"\d+" line)))) ; lines start at 1 not 0 thus the dec.
        contents (if (jfile/exists? file) (jfile/load-textfile file) nil)]
    (if (nil? contents) (jtext_old/append-text! atomtext (str line " <cant find file> \n")); can't load file, don't provide linl.
      (let [fx (first (grammer/lines2caret contents [lineno]))
            open (last (collections/which #(= % \( ) (into [] line))) open (if (nil? open) 0 open)
            close (last (collections/which #(= % \)) (into [] line))) close (if (nil? close) (count line) close)
            textix [(inc open) close]]
    (jtext_old/append-link1! gotofn!! atomtext (str line "\n") textix file fx)))))
(defn print-stack-trace! [gotofn!! atomtext cleanup]
  (let [e (:lasterr @app_old/repl-state)
        stacktxt (if (not (nil? e)) (tracer/exception2stack e cleanup) [])
        head (if (nil? e) "No Exception to show.\n" "")]
    (if (and cleanup (not (nil? e)))
      ; cleanup is so clean that we can append the text.
      (let [lines (into [] (rest stacktxt))]
        (jtext_old/append-text! atomtext head)
        (doall (map #(_singlestak gotofn!! atomtext %) lines)))
      (jtext_old/append-text! atomtext (str head (apply str (interpose "\n" stacktxt)) "\n")))))

;; The debugging breakpoint code that is inserted and variable storage:

(defn set-local-ns!! [nams]
  "The breakpoint function can call *ns* to get the current namespace."
  (println "TODO"))

(defn store-local-names!! [&names]
  "breakpoint passes names of variables here, &env is the technic that gets the variables."
  (println "TODO"))

(defn store-local-vars!! [&vals]
  "breakpoint passes values of variables here, &env is the technic that gets the variables."
  (println "TODO"))

(defn debug-loop!! []
  ; Turn on debugging and wait for the user to stop debugging (ran from the dispatch thread).
  (swap! app_old/repl-state assoc :debugging true :running false)
  (println "TODO need to load the vars.")
(println "DEBUG mode starts.")
  (if (.contains (str (Thread/currentThread)) "AWT-EventQueue")
    (do (output "\nCan't enter a breakpoint from the eventDispatch thread")
        (swap! app_old/repl-state assoc :debugging false)) ;avoid the program freezing.
    (output "hit breakpoint\n")) ;on a seperate thread.TODO: file/line info.
  (while (@app_old/repl-state :debugging) (Thread/sleep 50))) ; user in debugging mode for a while. ;(println "waiting in loop")


(defn get-local-var [varname]
  "Breakpoint finally gets back the modified local variables."
  (println "TODO"))


(defmacro breakpoint!!! [code]
  (let [local-names (mapv name (keys &env)) ; strings.
        local-syms (mapv #(symbol %) local-names)
        get-code #(list `(debugger/get-local-var ~%) %)
        ]
    `(do 
       ; Store the *ns* symbol, variables and their names to the debugger:
       (debugger/set-local-ns!! *ns*)
       (debugger/store-local-vars!! ~@(apply list local-names))
       (debugger/store-local-names!! ~@(apply list local-syms))
       
       ; Run the loop until the user continues or aborts (Exception thrown) the debugging attempt:
       (debugger/debug-loop!!)
       ; Load the local variables back.
       (let [~@(mapv get-code local-syms)] 
         ; run the code normally:
         ~code))))

;; Repl code/commands. Commands are not run like code and are prefixd with a /


; commands that are not ran per-se but tell us to do things. Useful since no ns qualify is nessessary.
(defn non-dbg-cmd [cmd]
  (if (or (= cmd "/abort") (= cmd "/stop") (= cmd "/halt") (= cmd "/cancel"))
    (if (:running @app_old/repl-state) (do (future-cancel (:future @app_old/repl-state)) (swap! app_old/repl-state assoc :running false) (output "task cancelled, but may still be CPU consuming.\n"))
      (output "no task was running.\n"))))

(defn dbg-cmd [cmd]
  ;TODO: abort function.
  (if (= cmd "/cont") (do (output "continuing\n") (swap! app_old/repl-state assoc :debugging false)); continue.
    (if (or (= cmd "/local") (= cmd "/locals"))
      (output (str "\n\nlocalvars:\n" (apply str (interpose "\n" (keys (:varslocal @app_old/repl-state)))) "\n"))
      )))

(defn non-debug-eval!!! [cmd]
   "We need to be cogniscient of the namespaces here."
   (let [nss 'u]
     (if (nil? (find-ns nss)) 
       ; bind the namespace and use the core:
       (let [n (create-ns nss)]
         (binding [*ns* n] (eval '(clojure.core/use 'clojure.core)))))
     (binding [*ns* (find-ns nss)] 
         (eval (read-string cmd)))))

(defn non-debug-repl!!! [cmd]
      (let [result
            (try (str (non-debug-eval!!! cmd))
            (catch Exception e (do (swap! app_old/repl-state assoc :lasterr e) (tracer/exception2str e))))]
        (output (str cmd \newline result \newline))))
    
; we hit a breakpoint then the user triggered on-text-to-repl!!!.
(defn debug-repl!!! [cmd]
        (let [varsym "TODO" varvals "TODO" dbns "TODO"
              code (into [] (apply concat (mapv #(vector %1 `(nth varvals %2)) varsym (range (count varvals)))))
              cmd-code (read-string cmd)
              result (eval
                      `(do (in-ns ~dbns) ; Set the namespace so the user can get use and set any defs.
                           (let ~code) ; Make all funciton-local available for the dubbing scope:
                           (try (eval ~cmd-code) (catch Exception e (str "Runtime debug err:" (tracer/exception2str e))))))]
          ; TODO: allow setting debugger variables.
          (output (str cmd \newline result \newline))))

; when the user passes the string text to the repl.
; start a new thread (so that we can add listeners to be dequeued on the main thread).
(defn on-text-to-repl!!! [cmd]
  (swap! app_old/repl-history conj cmd)
  (reset! app_old/repl-history-ind (count @app_old/repl-history)); one after end.
  ; TODO: autofix to quote et al command.
  (let [dbg (:debugging @app_old/repl-state) runn (:running @app_old/repl-state)]
    (if (= (str (first (string/trim cmd))) "/") ; are we a cmd that is not to be ran as code? 
      (if dbg (dbg-cmd cmd) (non-dbg-cmd cmd)); commands allow us to do things w/o directly using the ns.
      (if runn (output (str "<Task in progress, wait or cancel with /abort>" \newline))
            (do (swap! app_old/repl-state assoc :running true)
                    ; this next task may take a while or trigger a breakpoint that waits for more events to dispatch!
                    (swap! app_old/repl-state assoc :future (future (do (if dbg (debug-repl!!! cmd) (non-debug-repl!!! cmd))
                        (swap! app_old/repl-state assoc :running false)))))))))