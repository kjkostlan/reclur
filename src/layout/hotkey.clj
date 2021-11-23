; Key bindings (hotkeys).
(ns layout.hotkey
  (:require [c]
    [javac.warnbox :as warnbox]
    [layout.spatial.layoutcore :as layoutcore]
    [layout.spatial.selectmovesize :as selectmovesize]
    [layout.spatial.xform :as xform]
    [layout.keyanal :as ka]
    [layout.lispy :as lispy]
    [app.orepl :as orepl]
    [app.hintbox :as hintbox]
    [app.graphbox :as graphbox]
    [app.codebox :as codebox]
    [app.varbox :as varbox]
    [app.iteration :as iteration]
    [app.multicomp :as multicomp]
    [app.rtext :as rtext]
    [app.tabgroup :as tabgroup]
    [javac.cpanel :as cpanel]
    [navigate.strfind :as strfind]
    [navigate.funcjump :as funcjump]
    [coder.logger :as logger]))

;;;;;Helper functions ;;;;;

(defn do-to-selected-box [s f & type-range]
  "Returns the modified s. type-range is an optional arugment to set what types of boxes (:codebox, :orepl) we allow."
  (let [sel-k (first (:selected-comp-keys s)) box (get (:components s) sel-k)
        rng (first type-range)]
    (if (and box (or (not rng) (contains? (set rng) (:type box))))
      (assoc-in s [:components sel-k] (f box)) s)))

;;;;; Box-based hotkey functions ;;;;;

(defn tab-toggle [s delta]
  (let [kys (selectmovesize/onscreen-kys s)
        boxes (:components s)
        reading-score (fn [k] (let [box (get boxes k)
                                    [x0 x1 y0 y1] (xform/box-xxyy box)]
                                (+ (* 8 (+ y1 y0)) x0 x1 (tabgroup/tab-order-of box))))
        kys-sort (into [] (sort-by reading-score kys))
        sel-kys (set (:selected-comp-keys s))
        ix (first (c/where #(contains? sel-kys %) kys-sort)) ix (if ix ix -1)
        ix1 (mod (+ ix delta) (count kys))
        ky1 (nth kys-sort ix1)
        s1 (assoc s :selected-comp-keys #{ky1})]
    (selectmovesize/update-selection-box
      (tabgroup/selected-on-top s1))))

;;;;; State hotkey functions ;;;;;

(defn close [s]
  "Closes the active window if there is an active window.
   Recursivly contracts children."
  (selectmovesize/clear-selection (assoc (reduce #(multicomp/close-component %1 %2) s (:selected-comp-keys s)) :selected-comp-keys #{})))

(defn toggle-typing [s] (update s :typing-mode? not))

;;;;;;;;;;;;; Misc hotkey fns ;;;;;;

(defn move-view [s xf]
  "Hotkeys vs mouse."
  (let [cam0 (:camera s)
        cam1 (xform/xx xf cam0)]
    (assoc s :camera cam1)))

(defn move-some-boxes [s kys xf]
  (let [s1 (update s :components #(selectmovesize/apply-xform % kys xf))
        s2 (assoc s1 :components (tabgroup/sync-on-update (:components s) (:components s1)))]
    (selectmovesize/update-selection-box s2)))

(defn generic-move [s dx0 dy0 zoom0]
  "Move selected boxes or the camera.
   How far we move depends on the movement priors."
  (let [cam? (empty? (:selected-comp-keys s))
        prph [::hotkey-state :move-priors]
        priors (get-in s prph {})

        px (get priors :dx 0) py (get priors :dy 0) pz (get priors :dlogz 0)
        momentum-dot (+ (* px dx0) (* py dy0) (* pz (Math/log zoom0)))

        speed-old (get priors :speed 1.0)
        speed (cond (> momentum-dot 0.0005) (* speed-old 2.0)
                (< momentum-dot -0.0005) (* speed-old 0.5)
                :else speed-old)
        speed (min 8.0 (max speed (/ 1.0 32.0)))

        dx (* (if cam? dx0 (- dx0)) speed)
        dy (* (if cam? dy0 (- dy0)) speed)
        zoom-modifier-power 0.5
        zoom (Math/pow (if cam? zoom0 (/ 1.0 zoom0)) (Math/pow speed zoom-modifier-power))
        xc [dx dy zoom zoom]
        pix (xform/screen-pixels)
        xj [(* (first pix) -0.5) (* (second pix) -0.5) 1 1]
        xj-1 (xform/x-1 xj)
        xform-total (xform/xx (xform/xx xj-1 xc) xj)
        s1 (assoc-in s prph (assoc priors :dx dx0 :dy dy0 :dlogz (Math/log zoom0) :speed speed))]
    (if cam?
      (move-view s1 xform-total) (move-some-boxes s1 (:selected-comp-keys s) xform-total))))

(defn open-var-at-cursor [s]
  "Opens a variable with a varbox, assuming one is at the cursor.
   Edits to the var-box will have temporary effects that disappear once it is closed."
  (let [sel-k (first (:selected-comp-keys s)) box (get (:components s) sel-k)
        sel-sym (codebox/hint-sym-qual box false false)]
    (if sel-sym ((:add-component (:layout s)) s
                  (varbox/load-var sel-sym) (gensym 'varbox)))))

(defn show-error+ [s]
  "Shows the last thing that went into error+"
  (let [code `(mt/get-error+) boxk ::hotkey-repl]
    (orepl/add-repl s boxk code 0 true)))

(defn toggle-fullscreen! [s]
  "The titlebar is useless."
  (let [full? (cpanel/fullscreen?)]
    (println "Fullscreen toggled")
    (if full? (cpanel/windowed!) (cpanel/fullscreen!)) s))

;;;;; The hotkeys themselves ;;;;;

(defn hotkeys-typing-mode []
  "Only active when typing mode is active."
  {"M-s" (fn [s] (do-to-selected-box s lispy/splice-at-cursor))
   "M-9" (fn [s] (do-to-selected-box s #(lispy/wrap-at-cursor % "" "(" ")")))
   "M-[" (fn [s] (do-to-selected-box s #(lispy/wrap-at-cursor % "" "[" "]")))
   "M-S-[" (fn [s] (do-to-selected-box s #(lispy/wrap-at-cursor % "" "{" "}")))
   "M-S-C-[" (fn [s] (do-to-selected-box s #(lispy/wrap-at-cursor % "" "#{" "}")))
   "M-'" (fn [s] (do-to-selected-box s #(lispy/wrap-at-cursor % "" "\"" "\"")))
   "M->>" (fn [s] (do-to-selected-box s #(lispy/slurp-barf % true 1)))
   "M-<<" (fn [s] (do-to-selected-box s #(lispy/slurp-barf % true -1)))
   "M-S->>" (fn [s] (do-to-selected-box s #(lispy/slurp-barf % false -1)))
   "M-S-<<" (fn [s] (do-to-selected-box s #(lispy/slurp-barf % false 1)))
   "C->>" (fn [s] (do-to-selected-box s #(lispy/next-tok % 1 false)))
   "C-<<" (fn [s] (do-to-selected-box s #(lispy/next-tok % -1 false)))
   "C-S->>" (fn [s] (do-to-selected-box s #(lispy/next-tok % 1 true)))
   "C-S-<<" (fn [s] (do-to-selected-box s #(lispy/next-tok % -1 true)))
   "C-vv" (fn [s] (do-to-selected-box s #(lispy/next-thing % 1 false)))
   "C-^^" (fn [s] (do-to-selected-box s #(lispy/next-thing % -1 false)))
   "C-S-vv" (fn [s] (do-to-selected-box s #(lispy/next-thing % 1 true)))
   "C-S-^^" (fn [s] (do-to-selected-box s #(lispy/next-thing % -1 true)))
   "C-g vv" (fn [s] (funcjump/try-to-go-into s))
   "C-g ^^" (fn [s] (funcjump/try-to-go-ups s true false))
   "C-S-g ^^" (fn [s] (funcjump/try-to-go-ups s false false))
   "C-M-p" (fn [s] (do-to-selected-box s #(lispy/wrap-at-cursor % "coder.logger/pr-reportm " "(" ")")))
   "C-p p" (fn [s] (orepl/log-and-toggle-viewlogbox! s))
   "C-p ;" (fn [s] (orepl/add-cursor-lrepl s))
   "C-r" (fn [s] (do-to-selected-box s #(codebox/hint-sym-qual % true false)
                   #{:codebox :orepl :siconsole :varbox})) ;Don't worry about having to type the whole symbol
   "C-e" (fn [s] (open-var-at-cursor s))})

(defn hotkeys-notype-mode []
  "Only active when typing mode is INactive."
  {"w" (fn [s] (generic-move s 0 32 1.0))
   "s" (fn [s] (generic-move s 0 -32 1.0))
   "a" (fn [s] (generic-move s 32 0 1.0))
   "d" (fn [s] (generic-move s -32 0 1.0))
   "q" (fn [s] (generic-move s 0 0 (/ 1.0 (Math/pow 2.0 0.125))))
   "e" (fn [s] (generic-move s 0 0 (Math/pow 2.0 0.125)))
   "`" (fn [s] (selectmovesize/update-selection-box (assoc s :selected-comp-keys #{})))
   "C-a" (fn [s] (selectmovesize/update-selection-box
                   (assoc s :selected-comp-keys (set (selectmovesize/onscreen-kys s)))))
   "tab" (fn [s] (tab-toggle s 1))
   "S-tab" (fn [s] (tab-toggle s -1))
   "S-ret" (fn [s] (println "Running all repls")
             (reduce orepl/run-repl s (filterv #(= (get-in s [:components % :type]) :orepl) (keys (:components s)))))})

(defn hotkeys-both-modes [] ; fn [s] => s, where s is the state.
  "Active when both typing and non-typing mode is at play."
  {"C-w" close ; all these are (fn [s]).
   "esc" (fn [s] (assoc-in (toggle-typing s) [::hotkey-state :move-priors] {}))
   "C-S-r r r" #(if (warnbox/yes-no? "Relaunch app, losing any unsaved work?" false)
                  (do (future (eval 'core.launch-main-app!)) (throw (Exception. "This iteration is dead, reloading."))) %)
   "C-l" (fn [s] (layoutcore/next-layout s))
   "C-S-l" (fn [s] (layoutcore/prev-layout s))
   "C-`" selectmovesize/swap-on-top
   "C-f" (fn [s] (strfind/add-search-box s))
   "C-p x" (fn [s] (do (logger/remove-all-loggers!) ; clear loggers
                      #_(println "Cleared all loggers") s))
   "C-p C-x" (fn [s] (do (logger/clear-logs!) ; clear logs
                       #_(println "Cleared all logs") s))
   "C-p ^^" (fn [s] (funcjump/try-to-go-ups s false true))
   "M-p" (fn [s] (show-error+ s))
   "C-s" (fn [s] (iteration/save-state-to-disk!! s))
   "C-S-h" (fn [s] (hintbox/try-to-toggle-hint-box s))
   "C-S-g" (fn [s] (graphbox/try-to-toggle-graph-box s))
   "C-t" (fn [s] (let [[x y] (:mouse-pos-world s)
                       mevt-c {:X x :Y y}
                       s1 (update s :components #(tabgroup/toggle-tabs-for-boxes-under-mouse % x y))
                       s2 (selectmovesize/seltool-mousepress mevt-c s1)
                       s3 (selectmovesize/seltool-mouserelease mevt-c s2)] s3))
   "C-S-M-f" (fn [s] (toggle-fullscreen! s))
   "C-S-M-n ^^ ^^ vv vv << >> << >> b a" (fn [s] (println "We hope you enjoy this sandbox-genre game!") s)})

(defn hotkey-cycle [evt-g evt-c hotkey-state k typing-mode?]
  "The emacs-like command's counter (per-command) gets incremented for multi-key commands.
   Any commands that are triggered reset all counters."
  (if (and (= k :keyPressed) (or (ka/normal? evt-g) (ka/escape? evt-g) (ka/backspace? evt-g) (ka/enter? evt-g)
                               (ka/arrow-key evt-g)))
    (let [hotkey-map (merge (if typing-mode? (hotkeys-typing-mode) (hotkeys-notype-mode)) (hotkeys-both-modes))
          hotlist (keys hotkey-map)
          lengths (zipmap hotlist (mapv ka/emacs-count hotlist))
          hot-ix (if-let [x (:indexes hotkey-state)] x {})
          hot-ix (reduce #(if (get %1 %2) %1 (assoc %1 %2 0)) hot-ix hotlist)
          next-ix (fn [ix txt] (if (ka/emacs-hit? txt evt-g ix) (inc ix) 0)) ; reset if we fail.
          hot-ix1 (zipmap hotlist (mapv #(next-ix (get hot-ix %) %) hotlist))
          triggers (filterv #(= (get hot-ix1 %) (get lengths %)) hotlist)
          triggered? (> (count triggers) 0)
          active? (or triggered? (first (filter #(> % 0) (vals hot-ix1))))
          hot-ix2 (if triggered? (reduce #(assoc %1 %2 0) hot-ix1 (keys hot-ix1)) hot-ix1)
          fns-to-run (mapv #(get hotkey-map %) triggers)]
      (assoc hotkey-state :indexes hot-ix2 :block? (boolean (or active? triggered?))
        :fns-to-run fns-to-run))
    (assoc hotkey-state :block? false :fns-to-run [])))

(defn global-hotkey-cycle [s evt-g evt-c k]
  (let [tymod? (:typing-mode? s)
        ph (if tymod? [::hotkey-state :yestype] [::hotkey-state :notype])
        hk0 (get-in s ph)
        hk1 (hotkey-cycle evt-g evt-c hk0 k tymod?)
        fns-to-run (:fns-to-run hk1)
        hk2 (dissoc hk1 :fns-to-run)
        s1 (reduce #(%2 %1) s fns-to-run)]
    (assoc-in s1 ph hk2)))

(defn global-hotkey-block? [s]
  "Are hotkeys blocking anything?"
  (let [hk-state (get s ::hotkey-state)]
    (or (:block? (:notype hk-state)) (:block? (:yestype hk-state)))))

(defn global-remove-blocks [s]
  "Remove any blocks to the hotkeys."
  (-> (assoc-in s [::hotkey-state :notype :block?] false)
    (assoc-in [::hotkey-state :yestype :block?] false)))
