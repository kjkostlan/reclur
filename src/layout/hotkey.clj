; Key bindings (hotkeys).
(ns layout.hotkey
  (:require
    [javac.warnbox :as warnbox]
    [layout.layouts :as layouts]
    [app.orepl :as orepl]
    [app.hintbox :as hintbox]
    [app.graphbox :as graphbox]
    [app.iteration :as iteration]
    [app.multicomp :as multicomp]
    [javac.cpanel :as cpanel]
    [navigate.strfind :as strfind]
    [navigate.funcjump :as funcjump]
    [layout.selectmovesize :as selectmovesize]
    [coder.logger :as logger]))

;;;;; Specific hotkey functions ;;;;;

(defn store-state! [s]
  "stores the state in our own buffer atom, to be retrieved later. 
   More sophisticated undo system TODO."
  (swap! cpanel/one-atom
         assoc
         :reclur.state-snapshot
         s
         #_ (dissoc s :reclur.state-snapshot) ;; this version prevents chaining
         ) s)

(defn retrieve-state! []
  (assoc-in (:reclur.state-snapshot @cpanel/one-atom)
            [:precompute :desync-supersafe-mod?]
            true))

(defn close [s]
  "Closes the active window if there is an active window.
   Recursivly contracts children."
  (selectmovesize/clear-selecion (assoc (reduce #(multicomp/close-component %1 %2) s (:selected-comp-keys s)) :selected-comp-keys #{})))

(defn toggle-typing [s] (update s :typing-mode? not))

;;;;; The hotkeys themselves ;;;;;

(defn hotkeys [] ; fn [s] => s, where s is the state.
  {"C-w" close ; all these are (fn [s]).
   "esc" toggle-typing
   "C-S-r r r" #(if (or (globals/are-we-child?) (warnbox/yes-no? "Relaunch app, losing any unsaved work? Does not affect the child app." false)) 
                  (do (future (eval 'core.launch-main-app!)) (throw (Exception. "This iteration is dead, reloading."))) %)
   "C-l" (fn [s] (layouts/next-layout s)) 
   "C-S-l" (fn [s] (layouts/prev-layout s))
   "C-`" selectmovesize/swap-on-top 
   "C-f" (fn [s] (strfind/add-search-box s)) 
   "C-p p" (fn [s] (orepl/log-and-toggle-viewlogbox s))
   "C-p x" (fn [s] (do (logger/remove-all-loggers!) ; clear loggers
                      #_(println "Cleared all loggers") s))
   "C-p C-x" (fn [s] (do (logger/clear-logs!) ; clear logs
                       #_(println "Cleared all logs") s))
   "C-p ^^" (fn [s] (funcjump/try-to-go-ups s false true))
   ; The saving system: 
   ; ctrl+s = save onto child generation.
   ; ctrl+shift+s = pull child onto ourselves (TODO: do this when we quit as well).
   ; The child is viewed as the most up-to-date at all times, and it is occasionally copied back to us.
   "C-s" (fn [s] (iteration/save-state-to-disk!! s)) ; save to the child, rapid iteration.
   "C-S s s s" (fn [s] ; copy from child to us if we are the parent
                 (if (and (globals/can-child?) (not (globals/are-we-child?)))
                   (let [s1 (iteration/ensure-childapp-folder-init!! s)]
                     (iteration/copy-child-to-us!! s1) s1) s))
   "C-S-h" (fn [s] (hintbox/try-to-toggle-hint-box s))
   "C-S-g" (fn [s] (graphbox/try-to-toggle-graph-box s))
   "C-S c" store-state!
   "C-S z" (fn [_] (retrieve-state!))
   "C-^^" (fn [s] (funcjump/try-to-go-ups s true false))
   "C-S-^^" (fn [s] (funcjump/try-to-go-ups s false false))
   "C-vv" (fn [s] (funcjump/try-to-go-into s))
   "C-S-M-n ^^ ^^ vv vv << >> << >> b a" (fn [s] (println "We hope you enjoy this sandbox-genre game!") s)})
