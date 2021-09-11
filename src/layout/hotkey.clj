; Key bindings (hotkeys).
(ns layout.hotkey
  (:require
    [javac.warnbox :as warnbox]
    [layout.spatial.layoutcore :as layoutcore]
    [layout.spatial.selectmovesize :as selectmovesize]
    [layout.keyanal :as ka]
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
  (let [ixs (codebox/contain-ixs box)
        ix0 (first ixs) ix1 (second ixs)
        substr (subs (rtext/rendered-string box) (first ixs) (second ixs))
        inter-levels (langs/interstitial-depth substr (:langkwd box))
        keeps (filterv #(> (nth inter-levels %) 1) (range (count inter-levels)))
        keep-ix0 (if (first keeps) (+ ix0 (first keeps)))
        keep-ix1 (if (last keeps) (+ ix0 (last keeps)))

        box1 (if keep-ix0 (rtext/edit box (inc keep-ix1) ix1 "" []) box)
        box2 (if keep-ix0 (rtext/edit box1 ix0 (dec keep-ix0) "" []) box1)] box2))

(defn wrap-at-cursor [box txt]
  "The opposite of splice. For example, wrapping a (time x) around x."
  (let [ixs (codebox/contain-ixs box)
        ix0 (first ixs) ix1 (second ixs)
        edit? (and ix0 ix1)
        box1 (if edit? (rtext/edit box ix1 ix1 ")" []) box)
        box2 (if edit? (rtext/edit box1 ix0 ix0 (str "(" txt " ") []) box1)] box2))

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
   "C-l" (fn [s] (layoutcore/next-layout s))
   "C-S-l" (fn [s] (layoutcore/prev-layout s))
   "C-`" selectmovesize/swap-on-top
   "C-f" (fn [s] (strfind/add-search-box s))
   "C-p p" (fn [s] (orepl/log-and-toggle-viewlogbox! s))
   "C-p ;" (fn [s] (orepl/add-cursor-lrepl s))
   "C-p x" (fn [s] (do (logger/remove-all-loggers!) ; clear loggers
                      #_(println "Cleared all loggers") s))
   "C-p C-x" (fn [s] (do (logger/clear-logs!) ; clear logs
                       #_(println "Cleared all logs") s))
   "C-p ^^" (fn [s] (funcjump/try-to-go-ups s false true))
   "C-s" (fn [s] (iteration/save-state-to-disk!! s))
   "C-S-h" (fn [s] (hintbox/try-to-toggle-hint-box s))
   "C-S-g" (fn [s] (graphbox/try-to-toggle-graph-box s))
   "M-s" (fn [s] (do-to-selected-box s splice-at-cursor))
   "M-p" (fn [s] (do-to-selected-box s #(wrap-at-cursor % "coder.logger/pr-reportm")))
   "C-^^" (fn [s] (funcjump/try-to-go-ups s true false))
   "C-S-^^" (fn [s] (funcjump/try-to-go-ups s false false))
   "C-vv" (fn [s] (funcjump/try-to-go-into s))
   "C-S-M-n ^^ ^^ vv vv << >> << >> b a" (fn [s] (println "We hope you enjoy this sandbox-genre game!") s)})

(defn hotkey-cycle [evt-g evt-c s k]
  "The emacs-like command's counter (per-command) gets incremented for multi-key commands.
   Any commands that are triggered reset all counters."
  (if (and (= k :keyPressed) (or (ka/normal? evt-g) (ka/escape? evt-g) (ka/backspace? evt-g) (ka/enter? evt-g)))
    (let [hotkey-map (hotkeys) hotlist (keys hotkey-map)
          lengths (zipmap hotlist (mapv ka/emacs-count hotlist))
          hot-ix (if-let [x (:hotkey-indexes s)] x {})
          hot-ix (reduce #(if (get %1 %2) %1 (assoc %1 %2 0)) hot-ix hotlist)
          next-ix (fn [ix txt] (if (ka/emacs-hit? txt evt-g ix) (inc ix) 0)) ; reset if we fail.
          hot-ix1 (zipmap hotlist (mapv #(next-ix (get hot-ix %) %) hotlist))
          triggers (filterv #(= (get hot-ix1 %) (get lengths %)) hotlist)
          triggered? (> (count triggers) 0)
          active? (or triggered? (first (filter #(> % 0) (vals hot-ix1))))
          hot-ix2 (if triggered? (reduce #(assoc %1 %2 0) hot-ix1 (keys hot-ix1)) hot-ix1)
          s1 (reduce #((get hotkey-map %2) %1) s triggers)]
      (assoc s1 :hotkey-indexes hot-ix2 :tmp-hotkey-block? (or active? triggered?)))
    (assoc s :tmp-hotkey-block? false)))