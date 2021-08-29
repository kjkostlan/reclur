; Key bindings (hotkeys).
(ns layout.hotkey
  (:require
    [javac.warnbox :as warnbox]
    [layout.layouts :as layouts]
    [app.orepl :as orepl]
    [app.hintbox :as hintbox]
    [app.graphbox :as graphbox]
    [app.codebox :as codebox]
    [app.iteration :as iteration]
    [app.multicomp :as multicomp]
    [app.rtext :as rtext]
    [javac.cpanel :as cpanel]
    [navigate.strfind :as strfind]
    [navigate.funcjump :as funcjump]
    [layout.selectmovesize :as selectmovesize]
    [coder.logger :as logger]
    [coder.crosslang.langs :as langs]))

;;;;;Helper functions ;;;;;

(defn do-to-selected-box [s f & type-range]
  "Returns the modified s. type-range is an optional arugment to set what types of boxes (:codebox, :orepl) we allow."
  (let [sel-k (first (:selected-comp-keys s)) box (get (:components s) sel-k)
        rng (first type-range)]
    (if (and box (or (not rng) (contains? (set rng) (:type box))))
      (assoc-in s [:components sel-k] (f box)) s)))

;;;;; Box-based hotkey functions ;;;;;

(defn splice-at-cursor [box]
  "Splices at the cursor. (foo | (bar baz)) => (bar baz)"
  (let [box (codebox/set-precompute box)
        ixs (codebox/contain-ixs box)
        ix0 (first ixs) ix1 (second ixs)
        substr (subs (rtext/rendered-string box) (first ixs) (second ixs))
        inter-levels (langs/interstitial-depth substr (:langkwd box))
        keeps (filterv #(> (nth inter-levels %) 1) (range (count inter-levels)))
        keep-ix0 (if (first keeps) (+ ix0 (first keeps)))
        keep-ix1 (if (last keeps) (+ ix0 (last keeps)))
        
        box1 (if keep-ix0 (rtext/edit box (inc keep-ix1) ix1 "" []) box)
        box2 (if keep-ix0 (rtext/edit box1 ix0 (dec keep-ix0) "" []) box1)]
    (codebox/set-precompute box2)))

(defn wrap-at-cursor [box txt]
  "The opposite of splice. For example, wrapping a (time x) around x."
  (let [box (codebox/set-precompute box)
        ixs (codebox/contain-ixs box)
        ix0 (first ixs) ix1 (second ixs)
        edit? (and ix0 ix1)
        box1 (if edit? (rtext/edit box ix1 ix1 ")" []) box)
        box2 (if edit? (rtext/edit box1 ix0 ix0 (str "(" txt " ") []) box1)]
    (codebox/set-precompute box2)))

;;;;; State hotkey functions ;;;;;

(defn close [s]
  "Closes the active window if there is an active window.
   Recursivly contracts children."
  (selectmovesize/clear-selection (assoc (reduce #(multicomp/close-component %1 %2) s (:selected-comp-keys s)) :selected-comp-keys #{})))

(defn toggle-typing [s] (update s :typing-mode? not))

;;;;; The hotkeys themselves ;;;;;

(defn hotkeys [] ; fn [s] => s, where s is the state.
  {"C-w" close ; all these are (fn [s]).
   "esc" toggle-typing
   "C-r" (fn [s] (do-to-selected-box s codebox/hint-sym-qual #{:codebox :orepl :siconsole})) ;Don't worry about having to type the whole symbol
   "C-S-r r r" #(if (warnbox/yes-no? "Relaunch app, losing any unsaved work?" false) 
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
   "C-s" (fn [s] (iteration/save-state-to-disk!! s))
   "C-S-h" (fn [s] (hintbox/try-to-toggle-hint-box s))
   "C-S-g" (fn [s] (graphbox/try-to-toggle-graph-box s))
   "M-s" (fn [s] (do-to-selected-box s splice-at-cursor))
   "M-p" (fn [s] (do-to-selected-box s #(wrap-at-cursor % "coder.logger/pr-reportm")))
   "C-^^" (fn [s] (funcjump/try-to-go-ups s true false))
   "C-S-^^" (fn [s] (funcjump/try-to-go-ups s false false))
   "C-vv" (fn [s] (funcjump/try-to-go-into s))
   "C-S-M-n ^^ ^^ vv vv << >> << >> b a" (fn [s] (println "We hope you enjoy this sandbox-genre game!") s)})
