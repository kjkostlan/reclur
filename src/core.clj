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
    [javac.gfxcustom :as gfxcustom]
    [layout.undo :as undo]
    [app.orepl :as orepl]
    [app.fbrowser :as fbrowser]
    [app.codebox :as codebox]
    [app.siconsole :as siconsole]
    [app.multicomp :as multicomp]
    [app.multisync :as multisync]
    [app.iteration :as iteration]
    [app.tabgroup :as tabgroup]
    [layout.keyanal :as ka]
    [layout.hotkey :as hk]
    [layout.background :as background]
    [layout.spatial.collide :as collide]
    [layout.spatial.xform :as xform]
    [layout.spatial.selectmovesize :as selectmovesize]
    [layout.spatial.layoutcore :as layoutcore]
    [coder.cnav :as cnav]
    [coder.cbase :as cbase]
    [coder.unerror :as unerror]
    [coder.plurality :as plurality]))

(declare launch-main-app!) ; avoids a circular dependency with launch main app depending on earlier fns.

(def ^:dynamic *comp-k*) ; The key of the component the current event is triggered on.

;;;;;;;;;;;;;;;; Library of commands ;;;;;;;;;;;;;;;;;;;;;

(declare get-tools) ; avoids a circular dependency with get-tools bieng used by the other fns.
(defn use-tool [s tool-kwd]
  (let [tool (first (filter #(= (:name %) tool-kwd) (get-tools)))]
    (if tool (assoc s :active-tool tool)
      (throw (Exception. (str "no tool with :name " tool-kwd))))))

;;;;;;;;;;;;;;;; Mid level control flow ;;;;;;;;;;;;;;;;;;;;;

(defn to-f [f-or-code]
  "If code, evals into functions. Code can be syntax-quoted for a portable, serializable function."
  (if (fn? f-or-code) f-or-code
    (let [f (eval f-or-code)]
      (if (fn? f) f
        (throw (Exception. (str "This code doesn't eval into an fn:" f-or-code)))))))

(defn call [f-or-code & args]
  "Calls f on args. But f can be code, which makes serialization easier."
  (apply (to-f f-or-code) args))

(defn simple-dispatch-call [f-or-map evt box]
  "Applies dispatch multi-methods."
  (let [f-or-map (if (map? f-or-map) (zipmap (keys f-or-map) (mapv to-f (vals f-or-map)))
                   (to-f f-or-map))
        f (if (map? f-or-map) (plurality/->simple-multi-fn f-or-map
                                (fn [e-clj box] box)
                                (fn [e-clj box] (:type e-clj))) f-or-map)]
    (f evt box)))

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
        evt (xform/xevt (xform/x-1 (xform/pos-xform (:position comp))) evt-c)
        comp (get-in s [:components comp-k])
        comp1 (let [ifn (get comp :dispatch)]
                (if ifn (simple-dispatch-call ifn evt comp) comp))
        s1 (assoc-in s [:components comp-k] comp1)
        s2 (let [ifn-heavy (get comp :dispatch-heavy)] ; allows modifying s itself.
            (if ifn-heavy (call ifn-heavy evt s s1 comp-k) s1))]
    s2))

(defn single-comp-dispatches [evt-c s]
  (let [sel (set/intersection (apply hash-set (keys (:components s)))
              (apply hash-set (:selected-comp-keys s)))] ; normalize this.
    (reduce
      (fn [s k] (binding [*comp-k* k] (single-comp-dispatch evt-c s k)))
         (assoc s :selected-comp-keys sel) sel)))

(defn dispatch-hot-events [s evt-c]
  "Mouse move evts and everyFrame evts are ignored, as they cost CPU battery life.
   But what if you are making a video game? They should let you do anything. We store the hot box list."
  (reduce (fn [s k] (single-comp-dispatch evt-c s k)) s (:hot-boxes s)))

(defn update-hot-boxes [s]
  "Sets :hot-boxes to anything with :mouseMoved or :everyFrame events.
   These events trigger even if the boxes aren't selected."
  (let [hot-fns #{:everyFrame :mouseMoved}
        boxes (:components s)
        has-dispatch-fn? (fn [box-k fn-k]
                            (let [dispatch (get-in boxes [box-k :dispatch])]
                              (and (map? dispatch) (contains? dispatch fn-k))))
        hot? (fn [box-k] (boolean (first (filter #(has-dispatch-fn? box-k %) hot-fns))))]
   (assoc s :hot-boxes (set (filterv hot? (keys boxes))))))

; The maybe-x fns return the effects of doing x if the task is triggered, else s.
(defn maybe-open-file [evt-c s compk]
  (let [box (get (:components s) compk)
        evt (xform/xevt (xform/x-1 (xform/pos-xform (:position box))) evt-c)]
    (fbrowser/open-file-attempt evt s compk (codebox/new-codebox) codebox/from-text)))

(defn update-mouse [evt-g evt-c s k]
  (if (or (= k :mouseMoved) (= k :mouseDragged))
    (assoc s :mouse-pos-world [(:X evt-c) (:Y evt-c)]) s))

(defn expand-child [mevt-c s]
  (let [x (:X mevt-c) y (:Y mevt-c) comps (:components s)]
    (if-let [target-key (collide/under-cursor x y comps)]
      (multicomp/expand-child target-key (gensym "child") mevt-c s) s)))

(defn expand-child-tool []
  {:mousePressed expand-child})

(defn open-file-tool []
  {:mousePressed (fn [mevt-c s]
                   (diff-checkpoint s #(maybe-open-file mevt-c % (first (:selected-comp-keys s)))))})

(defn single-select [mevt-c s]
  (let [mp (:mousePressed (selectmovesize/get-selection-tool))
        s1 (selectmovesize/clear-selection s)]
    (mp mevt-c s1)))

;;;;;;;;;;;;;;;; High level control flow ;;;;;;;;;;;;;;;;;;;;;

(defn maybe-use-tool [evt-c s tool]
  (let [k (:type evt-c)] (if (not tool) (throw (Exception. "nil tool.")))
    (if-let [f (get tool k)] (f evt-c s) s)))

(defn maybe-common-tools [evt-c s]
  "Apply common tools using where the user clicks, the button type, etc as a switchyard.
   Here we DO NOT check for syncing components."
  (let [ek (:type evt-c)
        ech (expand-child-tool)
        ofl (open-file-tool)
        sms (selectmovesize/get-selection-tool)
        cam (selectmovesize/get-camera-tool)
        ut #(maybe-use-tool evt-c s %)

        click-target (if (= ek :mousePressed) (collide/under-cursor (:X evt-c) (:Y evt-c) (:components s)))
        double? (and (= ek :mousePressed) (= (:ClickCount evt-c) 2))
        shift? (:ShiftDown evt-c)
        ctrl? (:ControlDown evt-c)
        alt? (:AltDown evt-c)
        meta? (:MetaDown evt-c)]
    (cond
      (= ek :mouseReleased) (ut sms)
      (and click-target ctrl? double?) (ut ech)
      (and click-target double?) (ut ofl)
      (or (and (= ek :keyPressed) (ka/emacs-hit? "C-1" evt-c))
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

(defn send-off-resize-listeners [s0 s]
  "If the :size is changed."
  (let [boxes0 (:components s0) boxes (:components s)
        kys (set/intersection (set (keys (:components s0))) (set (keys (:components s))))
        boxes1 (reduce
                 (fn [acc k]
                   (let [box0 (get boxes0 k)
                         box (get boxes k)
                         box1 (if (or (not (:size box0)) (not (:size box)) (= (:size box0) (:size box))) box
                                (if-let [f (get-in box [:dispatch :resized])]
                                  (let [evt {:Time (System/currentTimeMillis)
                                             :type :resized
                                             :sz-old (:size box0) :sz-new (:size box)}]
                                    (f evt box)) box))]
                     (assoc acc k box1))) boxes kys)]
    (assoc s :components boxes1)))

(defn dispatch-listener [evt-g s]
  "Transforms and dispatches an event (that doesn't change :typing-mode? and :active-tool).
   Only certain changes to f need diff checking."
  (cond
    (= (:type evt-g) :everyFrame)
    (if (> (count (:hot-boxes s)) 0) (let [evt-c evt-g] (send-off-resize-listeners s (dispatch-hot-events s evt-c))) s)
    (= (:type evt-g) :mouseMoved)
    (let [evt-c (xform/xevt (xform/x-1 (:camera s)) evt-g)
          s1 (update-mouse evt-g evt-c s :mouseMoved)]
      (send-off-resize-listeners s
        (if (> (count (:hot-boxes s)) 0) ; Only dispatch any mouse-moves if there are hot repls.
          (dispatch-hot-events s evt-c) s1)))
    (= (:type evt-g) :quit)
    (on-quit-attempt evt-g s)
    :else
    (let [k (:type evt-g)
          evt-c (xform/xevt (xform/x-1 (:camera s)) evt-g)
          x (if (= k :parent-in) ; only used to update namespaces for now.
              (let [txt (:contents evt-g)]
                (try (eval (read-string txt))
                  (catch Exception e
                    (throw (Exception. (str "Eval of: " txt "\n Produced this error: " (.getMessage e))))))))
          s1 (if x (siconsole/log s (str "Parent command result:\n" x)) s)
          s2 (update-mouse evt-g evt-c s1 k)
          s3 (diff-checkpoint s2 #(hk/global-hotkey-cycle % evt-g evt-c k))
          hk? (hk/global-hotkey-block? s3) ; A hotkey fn has been triggered.
          s3_5 (hk/global-remove-blocks s3)
          s4 (if (= k :mousePressed) (update s3_5 :components #(tabgroup/tab-group-global-click % evt-c)) s3_5)]
      (#(update-hot-boxes (orepl/orepl-based-updates! s %))
        (if hk? (send-off-resize-listeners s s4) ; hotkeys block other actions.
            (let [; Typing mode forces single component use.
                  s5 (if (and (:typing-mode? s4) (= k :mousePressed)) (single-select evt-c s4) s4)
                  s6 (cond (and (= k :mouseDragged) (or (:ControlDown evt-c) (:MetaDown evt-c)))
                      (maybe-use-tool evt-c s5 (selectmovesize/get-camera-tool))
                      (:typing-mode? s5) (diff-checkpoint s5 #(single-comp-dispatches evt-c %))
                      :else (diff-checkpoint s5 #(maybe-common-tools evt-c %)))]
              (send-off-resize-listeners s s6)))))))

;;;;;;;;;;;;;;;; Rendering ;;;;;;;;;;;;;;;;;;;;;

(defn make-global-sprites [comps local-comp-renders selected-comp-keys
                           local-tab-renders tab-zvals
                           already_global_sprites cam typing?]
  "Sprites store :camera and :z which allows the graphics to place it in the world."
  (let [sel-keys (apply hash-set selected-comp-keys)
        comp-sprites (zipmap (keys comps)
                       (mapv (fn [k]
                               (let [c (get comps k)
                                     cam1 (xform/xx cam (xform/pos-xform (:position c)))]
                                 {:bitmap-cache? true :camera cam1 :gfx (get local-comp-renders k) :z (:z c)}))
                             (keys comps)))
        bg {:camera cam :z -1e100
            :gfx [[:bitmap [(* background/bg-scale (* -0.5 (first background/bg-size)))
                            (* background/bg-scale (* -0.5 (second background/bg-size)))
                            background/bg-scale background/bg-perspective-effect background/bg-filename]]]}
        sel-sprite {:camera cam :z 2e10
                    :gfx (into [] (apply concat (mapv #(multicomp/draw-select-box comps % [0 0 1 1]) sel-keys)))}
        haze-sprite {:camera [0 0 1 1] :z -1e99
                     :gfx (if typing? [[:fillRect [0 0 3200 1600] {:Color [0 0 0.5 0.333]}]] [])}
        tab2sprite (fn [tab-render z-val]
                     {:camera cam :z z-val :gfx tab-render})
        tab-sprites (zipmap (keys local-tab-renders)
                      (mapv tab2sprite (vals local-tab-renders) (vals tab-zvals)))]
    (merge comp-sprites tab-sprites already_global_sprites {::BACKGROUND-SPRITE bg ::HAZE-SPRITE haze-sprite})))

(defn draw-component-l [comp focused?]
  (if (nil? comp) (throw (Exception. "nil component")))
  (if (not (map? comp)) (throw (Exception. "Non-map component")))
  (let [gfx (try ((:render comp) (dissoc comp :position) focused?)
              (catch Exception e
                (gfxcustom/err-gfx e "The :render fn crashed for this box.")))]
        (if (= (count gfx) 0) (println "WARNING: no graphics drawn."))
    gfx))

(defn update-gfx [s]
  "Returns s with updated graphics information. app-render will then use the updated graphics to return a sprite map."
  (let [precompute (if-let [x (:gfx (:precompute s))] x {})
        boxes (:components s)
        cam (:camera s)
        _ (multisync/nil-assert boxes "The update-gfx fn is missing boxes")
        old-boxes (if-let [x (get precompute :comps-at-render)] x {})
        old-boxesgfx (if-let [x (get precompute :comp-renders)] x {})
        draw-or-reuse (fn [k] (let [c (get boxes k) c0 (get old-boxes k) g0 (get old-boxesgfx k)]
                                ; Cant use comp-eq? b/c comp-eq? is only for the :pieces not the gfx.
                                (if (and (:pure-gfx? (:optimize c)) (= (dissoc c0 :position) (dissoc c :position)))
                                  g0 (draw-component-l c true))))
        gfx-boxes-l (zipmap (keys boxes) (mapv draw-or-reuse (keys boxes)))
        precompute1 (assoc precompute :comps-at-render boxes :comp-renders gfx-boxes-l)
        s1 (assoc-in s [:precompute :gfx] precompute1)

        [gfx-tabs-l tab-zvals] (tabgroup/render-tab-groups boxes)

        tool (:active-tool s)
        tool-sprite {:camera cam :gfx (if-let [rf (:render tool)] (rf s) [])}

        sel-move-sz-sprite (if (:typing-mode? s) {:camera [0 0 1 1] :gfx []}
                             {:camera cam :gfx ((:render (selectmovesize/get-selection-tool)) s) :z 1e10})

        global-gfx (make-global-sprites (:components s) gfx-boxes-l (:selected-comp-keys s)
                     gfx-tabs-l tab-zvals
                     {::tool-sprite tool-sprite ::sel-move-size-sprite sel-move-sz-sprite}
                     cam (:typing-mode? s))]
    (assoc-in s1 [:precompute :gfx :global] global-gfx)))

(defn app-render [s]
  "Creates a map of kys to sprites. Each sprite has a :camera that renders it's :gfx,
   a :z value, and an optional :bitmap-cache? value.
   This function itself should be minimal since no logging is possible, thus we only pull precomputed stuff out."
   (get-in s [:precompute :gfx :global]))

;;;;;;;;;;;;;;;; The setup ;;;;;;;;;;;;;;;;;;;;;

(defn get-tools []
  "Non-common tools that are interactive versions of commands."
  [{:name :no-tool}])

(defn attempt-repair [s]
  "Tries to remove common problems with s that can throw an exception.
   Does not remove all problems and will not help if the event function is broken, etc."
  ;(print "ATTEMPT REPAIR" (keys s) "\n")
  (let [ensure-place #(let [c1 (if (:position %) % (assoc % :position [0.0 0.0]))
                            c2 (if (:size c1) c1 (assoc c1 :size [800.0 600.0]))
                            c3 (if (:z c2) c2 (assoc c2 :z 0.0))] c3) ; Easily forgotten.
        s (update-in s [:components] #(zipmap (keys %) (mapv ensure-place (vals %))))]
    s))

(defn logged-function-run [f s & args]
  "(apply f s args), logging to siconsole printlns within f as well as exceptions to s.
   f returns the new value of s. If f throws an exception s isn't changed apart from logging and an attempted repair."
  (let [outs clojurize/*our-out*
        s1-or-ex (binding [*out* outs] (try (apply f s args) (catch Exception e e)))
        txt (clojurize/extract! outs)
        _ (if (> (count txt) 0) (println txt)) ; mirror the output in the real console.
        err? (instance? java.lang.Exception s1-or-ex)
        s2 (if err? s s1-or-ex)
        s3 (if (= (count txt) 0) s2 (siconsole/log s2 txt))
        s4 (background/limit-cam s3)
        s5 (if err? (attempt-repair s4) s4)]
     (if err? (do (println (unerror/pr-error s1-or-ex)) (siconsole/log s5 (unerror/pr-error s1-or-ex))) s5)))

(defn undo-wrapped-listener [evt-g s]
  (let [ty (:type evt-g)
        undo? (and (= ty :keyPressed) (ka/emacs-hit? "C-z" evt-g))
        redo? (and (= ty :keyPressed) (ka/emacs-hit? "C-S-z" evt-g))]
    (cond undo? (undo/undo!) redo? (undo/redo!)
      :else (let [s1 (logged-function-run #(dispatch-listener %2 %1) s evt-g)]
              (undo/maybe-report! evt-g s s1) s1))))

(defn _init-state-fn []
  "Opening the window first then setting up the app state should be better."
  (let [layout (layoutcore/default-lmode)
        s {:layout layout :components {} :camera [0 0 1 1] :typing-mode? true :active-tool (first (get-tools))}
        s1 ((:initial-position layout) s (fbrowser/add-root-fbrowser s) (orepl/new-repl) (siconsole/new-console))
        s2 (assoc s1 :selected-comp-keys #{})]
   (update s2 :components #(multisync/update-keytags {} %))))

(defn launch-main-app! []
    (cpanel/launch-app! _init-state-fn undo-wrapped-listener
      (fn [s] (logged-function-run update-gfx s))
      app-render)) ; keep app-render minimal, no siconsole logging is allowed here.

; DONT call save-file, we can't run swing stuff from shutdown hooks reliably.
; Instead we do this on the exit-if-close function.
; TODO: get this working so cmd+q doesn't quit.
(defn on-shutdown [] ())

(defn -main [& args] (launch-main-app!)
  (try (do (require 'startup) ((eval 'startup/startup-core)))
    (catch Exception e (do (println "User startup script failed") (println (unerror/pr-error e))))))
