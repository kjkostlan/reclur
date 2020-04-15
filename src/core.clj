; Launches main app.
; :components is from keywords to the components.
  ; We add a :position vector.
; :selected-comp-keys = components that are selected.
; :camera is a transform.
; The repls share the same state-space.
; Trying not to have too much component-specific code here, but the coupling is just so tight for the structural editor.

(ns core
  (:require 
    [clojure.string :as string]
    [clojure.set :as set]
    [javac.cpanel :as cpanel]
    [javac.file :as jfile]
    [javac.warnbox :as warnbox]
    [javac.clojurize :as clojurize]
    [layout.onecode :as onecode]
    [layout.undo :as undo]
    [app.multicomp :as multicomp] 
    [app.multisync :as multisync] 
    [app.orepl :as orepl]
    [app.fbrowser :as fbrowser] 
    [app.codebox :as codebox]
    [app.hintbox :as hintbox]
    [app.singlecomp :as singlecomp]
    [app.selectmovesize :as selectmovesize]
    [app.xform :as xform]
    [app.siconsole :as siconsole]
    [app.iteration :as iteration]
    [app.funcjump :as funcjump]
    [search.strfind :as strfind]
    [layout.keybind :as kb]
    [coder.logger :as logger]))

(declare launch-main-app!) ; avoids a circular dependency with launch main app depending on earlier fns.

;;;;;;;;;;;;;;;; Library of commands ;;;;;;;;;;;;;;;;;;;;;

(defn close [s]
  "Closes the active window if there is an active window.
   Recursivly contracts children."
  (selectmovesize/clear-selecion (assoc (reduce #(multicomp/close-component %1 %2) s (:selected-comp-keys s)) :selected-comp-keys #{})))

(defn toggle-typing [s] (update s :typing-mode? not))

(declare get-tools) ; avoids a circular dependency with get-tools bieng used by the other fns.
(defn use-tool [s tool-kwd] 
  (let [tool (first (filter #(= (:name %) tool-kwd) (get-tools)))]
    (if tool (assoc s :active-tool tool)
      (throw (Exception. (str "no tool with :name " tool-kwd))))))

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

;;;;;;;;;;;;;;;; Keyboard interaction with hotkeys ;;;;;;;;;;;;;;;;;;;;;

(defn hotkeys [] ; fn [s] => s, where s is the state.
  {"C-w" close ; all these are (fn [s]).
   "esc" toggle-typing
   "C-S-r" #(if (or (globals/are-we-child?) (warnbox/yes-no? "Relaunch app, losing any unsaved work? Does not affect the child app." false)) 
                            (do (future (launch-main-app!)) (throw (Exception. "This iteration is dead, reloading."))) %)
   "C-`" selectmovesize/swap-on-top 
   "C-f" (fn [s] (strfind/add-search-box s)) 
   "C-p p" (fn [s] ; logger.
                        (let [k (first (:selected-comp-keys s))]
                          (if (= (:type (get-in s [:components k])) :codebox)
                            (let [filename-ix (multicomp/cursor-locate s k)
                                  fname (first filename-ix) cursor-ix (second filename-ix)
                                  cache-txt (multicomp/open-cache s fname)]
                              (logger/log-toggle-at-cursor s fname cursor-ix cache-txt (orepl/new-repl)))
                            s)))
   "C-p x" (fn [s] (do (logger/remove-all-loggers!) ; clear loggers
                      (println "Cleared all loggers") s))
   "C-p C-x" (fn [s] (do (logger/clear-logs!) ; clear logs
                       (println "Cleared all logs") s))
   "C-p u" (fn [s] (do (logger/toggle-ui-loggers!) s)) ; toggle UI log
   ; The saving system: 
   ; ctrl+s = save onto child generation.
   ; ctrl+shift+s = pull child onto ourselves (TODO: do this when we quit as well).
   ; The child is viewed as the most up-to-date at all times, and it is occasionally copied back to us.
   "C-s" (fn [s] (iteration/save-state-to-disk!! s)) ; save to the child, rapid iteration.
   "C-S s" (fn [s] ; copy from child to us if we are the parent
                           (if (and (globals/can-child?) (not (globals/are-we-child?)))
                             (let [s1 (iteration/ensure-childapp-folder-init!! s)]
                               (iteration/copy-child-to-us!! s1) s1) s))
   "C-S-h" (fn [s] ; show the hint box
                              (if-let [fc (get (:components s) (first (:selected-comp-keys s)))]
                                (if (or (= (:type fc) :codebox) (= (:type fc) :orepl))
                                  (if-let [hb (hintbox/codebox-hint s fc)] 
                                    (if (let [hb0 (get-in s [:components ::hintbox])]
                                          (and hb0 (= (mapv int (:position hb0)) (mapv int (:position hb)))))
                                      (update s :components 
                                        #(dissoc % ::hintbox))
                                      (assoc-in s [:components ::hintbox] hb)) s) s) s))
   "C-S c" store-state!
   "C-S z" (fn [_] (retrieve-state!))
   "C-^^" (fn [s]
                             (if-let [fc (get (:components s) (first (:selected-comp-keys s)))]
                                (if (= (:type fc) :codebox)
                                  (if-let [file-ix01s (funcjump/find-users s fc)]
                                    (let [fnames (first file-ix01s)
                                          ix0s (second file-ix01s) ix1s (nth file-ix01s 2)
                                          lys (:gotos (:layout s))] 
                                      (lys s fnames ix0s ix1s)) s) s) s))
   "C-vv" (fn [s]
                             (if-let [fc (get (:components s) (first (:selected-comp-keys s)))]
                                (if (= (:type fc) :codebox)
                                  (if-let [file-ix01 (funcjump/find-def s fc)]
                                    (let [fname (first file-ix01)
                                          ix0 (second file-ix01) ix1 (nth file-ix01 2)
                                          ly (:goto (:layout s))] 
                                      (ly s fname true ix0 ix1)) s) s) s))
   "C-S-M-n ^^ ^^ vv vv << >> << >> b a" (fn [s] (println "We hope you enjoy this sandbox-genre game!") 
                                       s)})

;;;;;;;;;;;;;;;; Adding a component on the top of the z-stack ;;;;;;;;;;;;;;;;;;;;;

(defn add-component [s box kwd] ((:add-component (:layout s)) s box kwd))

(defn root-fbrowser [s] 
  "Creates a new fbrowser from our own folder once per startup, otherwise just copies an existing root fbrowser (we never allow closing all root fbrowsers)."
  (let [comps (:components s)
        kys (filterv #(multicomp/rootfbrowser? (get comps %)) (keys comps))
        new-comp (if (> (count kys) 0) (get comps (first kys)) 
                   (fbrowser/load-from-folder (globals/get-working-folder)))] new-comp))

;;;;;;;;;;;;;;;; Mid level control flow ;;;;;;;;;;;;;;;;;;;;;

(defn diff-checkpoint [s f] 
  "Runs f on s, syncing the components unless f doesn't change the components or flags [:precompute :desync-safe-mod?] to true.
   Also updates the tags in all cases."
  (let [s (assoc-in (assoc-in s [:precompute :desync-supersafe-mod?] false)
                    [:precompute :desync-safe-mod?] false) ; safe until "proven" fast.
        s1 (f s) ; f has a chance to set :desync-safe-mod? for optimization.
        sync-f (cond (or (get-in s1 [:precompute :desync-supersafe-mod?]) 
                       (multisync/comps-eq? (:components s) (:components s1)))
                     (fn [comps0 comps1] comps1)
                     (get-in s1 [:precompute :desync-safe-mod?])
                     multisync/update-keytags 
                     :else multisync/comprehensive-sync)] ; the comprehensive sync comes with update-keytags.
    (assoc s1 :components (sync-f (:components s) (:components s1)))))

(defn single-comp-dispatch [evt-c s comp-k]
  (let [comp (get-in s [:components comp-k])
              evt (xform/xevt (xform/x-1 (singlecomp/pos-xform (:position comp))) evt-c)
              s (let [ifn-heavy (get (:interact-fns comp) :dispatch-heavy)] ; allows modifying s itself.
                  (if ifn-heavy (ifn-heavy evt s comp-k) s))
              comp (get-in s [:components comp-k])
              comp1 (let [ifn (get (:interact-fns comp) :dispatch)]
                        (if ifn (ifn evt comp) comp))]
         (assoc-in s [:components comp-k] comp1)))

(defn single-comp-dispatches [evt-c s]
  (let [sel (set/intersection (apply hash-set (keys (:components s))) (apply hash-set (:selected-comp-keys s)))] ; normalize this.
    (reduce 
      (fn [s k] (single-comp-dispatch evt-c s k)) 
         (assoc s :selected-comp-keys sel) sel)))

; The maybe-x fns return the effects of doing x if the task is triggered, else s.
(defn maybe-open-file [evt-c s compk]
  (let [comp (get (:components s) compk)
        s (assoc-in s [:precompute :desync-safe-mod?] true) 
        evt (xform/xevt (xform/x-1 (singlecomp/pos-xform (:position comp))) evt-c)
        fname (if (and (= (:type evt-c) :mousePressed) (= (:type comp) :fbrowser)) 
                 (fbrowser/fullfile-click evt comp))
        non-folder? (if fname (fbrowser/non-folder-file-click? evt comp))]
    (if (and fname non-folder?) 
      (let [lix (fbrowser/pixel-to-line comp (:X evt) (:Y evt))
            pos (:position comp) sz (:size comp)
            cbox (assoc (if (jfile/exists? fname) (assoc (multicomp/load-from-file (:components s) fname) :path fname) 
                          (assoc (codebox/new-codebox) :path fname)) 
                   :position (mapv + pos (mapv * sz [0.25 0.75])))
            s1 (assoc-in s [:components compk] comp)]
        (add-component s1 cbox (gensym 'codebox))) s)))

(defn update-mouse [evt-g evt-c s k]
  (if (or (= k :mouseMoved) (= k :mouseDragged)) 
             (assoc s :mouse-pos-world [(:X evt-c) (:Y evt-c)]) s))

(defn expand-child [mevt-c s]
  (let [x (:X mevt-c) y (:Y mevt-c) comps (:components s)] 
    (if-let [target-key (selectmovesize/under-cursor x y comps)] 
      (multicomp/expand-child target-key (gensym "child") mevt-c s) s)))

(defn expand-child-tool []
  {:mousePressed expand-child})

(defn open-file-tool []
  {:mousePressed (fn [mevt-c s]
                   (diff-checkpoint s #(maybe-open-file mevt-c % (first (:selected-comp-keys s)))))})

(defn single-select [mevt-c s] 
  (let [mp (:mousePressed (selectmovesize/get-tool))
        s1 (selectmovesize/clear-selecion s)]
    (mp mevt-c s1)))

;;;;;;;;;;;;;;;; High level control flow ;;;;;;;;;;;;;;;;;;;;;

(defn hotkey-cycle [evt-g evt-c s k]
  "The emacs-like command's counter (per-command) gets incremented for multi-key commands. 
   Any commands that are triggered reset all counters."
  (if (and (= k :keyPressed) (or (kb/normal? evt-g) (kb/escape? evt-g) (kb/backspace? evt-g) (kb/enter? evt-g)))
    (let [hotkey-map (hotkeys) hotlist (keys hotkey-map)
          n-vals (zipmap hotlist (mapv kb/emacs-count hotlist))
          hot-ix (if-let [x (:hotkey-index s)] x {})
          hot-ix (reduce #(if (get %1 %2) %1 (assoc %1 %2 0)) hot-ix (keys hotkey-map))
          next-ix (fn [ix txt] (if (kb/emacs-hit? txt evt-g ix) (inc ix) 0)) ; reset if we fail.
          hot-ix1 (zipmap hotlist (mapv #(next-ix (get hot-ix %) %) hotlist))
          matches (filterv #(= (get hot-ix1 %) (get n-vals %)) hotlist)
          matches? (> (count matches) 0)
          hot-ix2 (if matches? (reduce #(assoc %1 %2 0) hot-ix1 (keys hot-ix1)) hot-ix1)
          s1 (reduce #((get hotkey-map %2) %1) s matches)]
      (assoc s1 :hotkey-index hot-ix2 :tmp-hotkey-triggered? matches?))
    (assoc s :tmp-hotkey-triggered? false)))

(defn maybe-use-tool [evt-c s tool]
  (let [k (:type evt-c)] (if (not tool) (throw (Exception. "nil tool.")))
    (if-let [f (get tool k)] (f evt-c s) s)))

(defn maybe-common-tools [evt-c s]
  "Apply common tools using where the user clicks, the button type, etc as a switchyard.
   Here we DO NOT check for syncing components."
  (let [ek (:type evt-c)
        ech (expand-child-tool)
        ofl (open-file-tool)
        sms (selectmovesize/get-tool)
        cam (selectmovesize/get-camera-tool)
        ut #(maybe-use-tool evt-c s %)
        
        click-target (if (= ek :mousePressed) (selectmovesize/under-cursor (:X evt-c) (:Y evt-c) (:components s)))
        double? (and (= ek :mousePressed) (= (:ClickCount evt-c) 2))
        shift? (:ShiftDown evt-c)
        ctrl? (:ControlDown evt-c)
        alt? (:AltDown evt-c)
        meta? (:MetaDown evt-c)]
    (cond
      (= ek :mouseReleased) (ut sms)
      (and click-target ctrl? double?) (ut ech)
      (and click-target double?) (ut ofl)
      (or (and (= ek :keyPressed) (kb/emacs-hit? "C-1" evt-c))
        (= ek :mouseWheelMoved) (and (or (= ek :mousePressed) (= ek :mouseDragged)) (or meta? ctrl?))) (ut cam)
      :else (ut sms))))

(defn on-quit-attempt [evt-q s]
  (let [fs (iteration/file-status s)
        n (apply + (mapv count [(:new-files fs) (:changed-files fs) (:deleted-files fs) (:renamed-map fs)]))]
    (if (= n 0) ; no disk changes. 
      (let [choice? (warnbox/yes-no? "quit?" false)]
        (if choice? (System/exit 0) (do (println "Quit cancelled by user.") s)))
      (let [choice (warnbox/choice (str n " files added/changed/renamed/deleted, save?") [:yes :no :cancel] :cancel)]
        (cond (= choice :cancel) (do (println "Quit cancelled by user.") s)
          (= choice :no) (System/exit 0)
          (= choice :yes) (do (iteration/save-state-to-disk!! s) (System/exit 0)))))))

(defn dispatch-hot-repls [s evt-c selected+-]
  "Mouse move evts and everyFrame evts are ignored, as they cost CPU battery life.
   But what if you are using the repl as a video game? They should let you do anything. We store the hot repl list."
  (let [hots (:hot-repls s)
        sel (set (:selected-comp-keys s))
        target (cond (= selected+- 0) hots (> selected+- 0) (set/intersection hots sel)
                 (< selected+- 0) (set/difference hots sel))]
    (reduce (fn [s k] (single-comp-dispatch evt-c s k)) 
        s target)))

(defn dispatch-warm-repls [s evt-c selected+-]
  "Warm repls have events, but not mose moved or every frame events."
  (let [warms (set (:warm-repls s))
        sel (set (:selected-comp-keys s))
        target (cond (= selected+- 0) warms (> selected+- 0) (set/intersection warms sel)
                 (< selected+- 0) (set/difference warms sel))]
    (reduce (fn [s k] (single-comp-dispatch (assoc evt-c :unselected? (not (contains? sel k))) s k)) 
        s target)))

(defn update-warmhot-repls [s]
  "Which repls have user-customized events?
   :warm-repls = have events besides :mouseMoved and :everyFrame. :hot-repls = have :mouseMoved or :everyFrame events."
  (let [warm-fns #{:mousePressed :mouseReleased :keyPressed :keyReleased :mouseDragged}
        hot-fns #{:mouseMoved :everyFrame}
        comps (:components s)
        repl-kys (filterv #(= (:type (get comps %)) :orepl) (keys comps))
        warm-kys (filterv #(> (count (set/intersection (set (keys (get comps %))) warm-fns)) 0) repl-kys)
        hot-kys (filterv #(> (count (set/intersection (set (keys (get comps %))) hot-fns)) 0) repl-kys)]
   (assoc s :warm-repls warm-kys :hot-repls hot-kys)))

(defn dispatch-listener [evt-g s] 
  "Transforms and dispatches an event (that doesn't change :typing-mode? and :active-tool).
   Only certain changes to f need diff checking."
  (cond 
    (= (:type evt-g) :everyFrame)
    (if (> (count (:hot-repls s)) 0) (dispatch-hot-repls s evt-g 0) s)
    (= (:type evt-g) :mouseMoved) ; don't let mouse moves eat up CPU time, but still update the mouse itself.
    (let [evt-c (xform/xevt (xform/x-1 (:camera s)) evt-g)
          s1 (update-mouse evt-g evt-c s :mouseMoved)]
      (if (> (count (:hot-repls s)) 0) (dispatch-hot-repls s1 evt-c 0)) s1)
    (= (:type evt-g) :quit)
    (on-quit-attempt evt-g s)
    :else
    (let [k (:type evt-g)
          s0 s evt-c (xform/xevt (xform/x-1 (:camera s)) evt-g)
          x (if (= k :parent-in) ; only used to update namespaces for now.
              (let [txt (:contents evt-g)]
                (try (eval (read-string txt))
                  (catch Exception e
                    (throw (Exception. (str "Eval of: " txt "\n Produced this error: " (.getMessage e)))))))) 
          s (if x (siconsole/log s (str "Parent command result:\n" x)) s)
          s (update-mouse evt-g evt-c s k)
          s (dispatch-warm-repls s evt-c -1) ; dispatch unselected so they get events.
          
          s (diff-checkpoint s #(hotkey-cycle evt-g evt-c % k))
          hk? (boolean (:tmp-hotkey-triggered? s)) ; a recognized hotkey.
          s (dissoc s :tmp-hotkey-triggered?)]
      (update-warmhot-repls 
        (if hk? s ; hotkeys block other actions.
        (let [; Typing mode forces single component use.
              s (if (and (:typing-mode? s) (= k :mousePressed)) (single-select evt-c s) s)
              s (cond (and (= k :mouseDragged) (or (:ControlDown evt-c) (:MetaDown evt-c))) 
                  (maybe-use-tool evt-c s (selectmovesize/get-camera-tool))
                  (:typing-mode? s) (diff-checkpoint s #(single-comp-dispatches evt-c %))
                  :else (diff-checkpoint s #(maybe-common-tools evt-c %)))        
              ] s))))))

;;;;;;;;;;;;;;;; Rendering ;;;;;;;;;;;;;;;;;;;;;

(defn globalize-gfx [comps cam local-comp-renders selected-comp-keys tool-sprite sel-move-sz-sprite typing?]
  "Takes the local gfx of components as well as tool information to make the global gfx"
  (let [sel-keys (apply hash-set selected-comp-keys)
        
        comp-sprites (zipmap (keys comps) 
                       (mapv (fn [k] 
                               (let [c (get comps k)

                                     cam1 (xform/xx cam (singlecomp/pos-xform (:position c)))]
                                 {:bitmap-cache? true :camera cam1 :gfx (get local-comp-renders k) :z (:z c)}))
                             (keys comps)))
        bg-scale 10.0
        bg-perspective-effect 2.0 ; make it in the background.
        bg {:camera cam :z -1e100
            :gfx [[:bitmap [(* bg-scale -500) (* bg-scale -500) bg-scale bg-perspective-effect "./assets/forest.jpg"]]]}
        sel-sprite {:camera cam :z 2e10
                    :gfx (into [] (apply concat (mapv #(multicomp/draw-select-box comps % [0 0 1 1]) sel-keys)))}
        haze-sprite {:camera [0 0 1 1] :z -1e99
                     :gfx (if typing? [[:fillRect [0 0 1500 1500] {:Color [0 0 0 0.333]}]] [])}]
    (assoc comp-sprites ::TOOL-SPRITE-CORE tool-sprite ::TOOL-SMS-SPRITE sel-move-sz-sprite ::TOOL-SEL-SPRITE sel-sprite 
    ::BACKGROUND-SPRITE bg ::HAZE-SPRITE haze-sprite)))

(defn update-gfx [s]
  "Returns s with updated graphics information. app-render will then use the updated graphics to return a sprite map."
  (let [precompute (if-let [x (:gfx (:precompute s))] x {})
        comps (:components s)
        cam (:camera s)
        _ (multisync/nil-assert comps "The update-gfx fn is missing comps")
        old-comps (if-let [x (get precompute :comps-at-render)] x {})
        old-compgfx (if-let [x (get precompute :comp-renders)] x {})
        draw-or-reuse (fn [k] (let [c (get comps k) c0 (get old-comps k) g0 (get old-compgfx k)] 
                                ; Cant use comp-eq? b/c comp-eq? is only for the :pieces not the gfx.
                                (if (and (:pure-gfx? (:optimize c)) (= (dissoc c0 :position) (dissoc c :position)))
                                  g0 (singlecomp/draw-component-l c true))))
        gfx-comps-l (zipmap (keys comps) (mapv draw-or-reuse (keys comps)))
        
        precompute1 (assoc precompute :comps-at-render comps :comp-renders gfx-comps-l)
        s1 (assoc-in s [:precompute :gfx] precompute1)
        
        tool (:active-tool s) ; don't precompute the tools, not necessary, at least for now.
        ; TODO: use the cam or use no cam?
        tool-sprite {:camera cam :gfx (if-let [rf (:render tool)] (rf s) [])}
        
        sel-move-sz-sprite (if (:typing-mode? s) {:camera [0 0 1 1] :gfx []} 
                             {:camera cam :gfx ((:render (selectmovesize/get-tool)) s) :z 1e10})
        ;tool-hud-sprite {:camera [0 0 1 1] :z 1e100
        ;                 :gfx (multicomp/which-tool-hud s)}
        global-gfx (globalize-gfx (:components s) (:camera s) gfx-comps-l (:selected-comp-keys s) tool-sprite sel-move-sz-sprite
                     (:typing-mode? s))]
    (assoc-in s1 [:precompute :gfx :global] global-gfx)))

(defn app-render [s]
  "Creates a map of kys to sprites. Each sprite has a :camera that renders it's :gfx,
   a :z value, and an optional :bitmap-cache? value.
   This function itself should be minimal since no logging is possible, thus we only pull precomputed stuff out."
   (get-in s [:precompute :gfx :global]))

;;;;;;;;;;;;;;;; The setup ;;;;;;;;;;;;;;;;;;;;;

(defonce modified-files (atom {})) ; name -> new text. Disk singleton.

(defn get-tools []
  "Non-common tools that are interactive versions of commands."
  [{:name :no-tool}])

(defn logged-function-run [f s & args]
  "(apply f s args), logging printlns within f as well as exceptions to s.
   f returns the new value of s. If f throws an exception s isn't changed apart from logging."
  (let [outs clojurize/*our-out*
        s1-or-ex (binding [*out* outs] (try (apply f s args) (catch Exception e e)))
        txt (clojurize/extract! outs)
        _ (if (> (count txt) 0) (println txt)) ; mirror the output in the real console.
        err? (instance? java.lang.Exception s1-or-ex) 
        s2 (if err? s s1-or-ex)
        s3 (if (= (count txt) 0) s2 (siconsole/log s2 txt))]
     (if err? (do (println (orepl/pr-error s1-or-ex)) (siconsole/log s3 (orepl/pr-error s1-or-ex))) s3)))

(defn undo-wrapped-listener [evt-g s]
  (let [ty (:type evt-g)
        undo? (and (= ty :keyPressed) (kb/emacs-hit? "C-z" evt-g))
        redo? (and (= ty :keyPressed) (kb/emacs-hit? "C-S-z" evt-g))]
    (cond undo? (undo/undo!) redo? (undo/redo!)
      :else (let [s1 (logged-function-run #(dispatch-listener %2 %1) s evt-g)]
              (undo/maybe-report! evt-g s s1) s1))))

(defn launch-main-app! []
  (cpanel/stop-app!)
  (let [layout (onecode/layout)
        s {:layout layout :components {} :camera [0 0 1 1] :typing-mode? true :active-tool (first (get-tools))}
        s1 (if (and (globals/can-child?) (not (globals/are-we-child?))) (iteration/ensure-childapp-folder-init!! s) s)
        s2 ((:initial-position layout) s1 (root-fbrowser s1) (orepl/new-repl) (siconsole/new-console))
        
        s3 (assoc s2 :selected-comp-keys #{})
        s4 (update s3 :components #(multisync/update-keytags {} %))]
    (cpanel/launch-app! s4 undo-wrapped-listener
      (fn [s] (logged-function-run update-gfx s))
      app-render))) ; keep app-render minimal, no logging is allowed here.

; DONT call save-file, we can't run swing stuff from shutdown hooks reliably.
; Instead we do this on the exit-if-close function.
; TODO: get this working so cmd+q doesn't quit.
(defn on-shutdown [] ())

;;  startup
(defn -main [& args] (launch-main-app!))
