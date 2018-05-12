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
    [app.multicomp :as multicomp] 
    [app.multisync :as multisync] 
    [app.orepl :as orepl]
    [app.fbrowser :as fbrowser] 
    [app.codebox :as codebox]
    [app.singlecomp :as singlecomp]
    [app.selectmovesize :as selectmovesize]
    [app.xform :as xform]
    [app.siconsole :as siconsole]
    [app.iteration :as iteration]))

(def this-ns *ns*)

(declare launch-main-app!!) ; avoids a circular dependency with launch main app.

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
         ))

(defn retrieve-state! []
  (assoc-in (:reclur.state-snapshot @cpanel/one-atom)
            [:precompute :desync-safe-mod?]
            true))

(defn swap-on-top [s] ; all selected comps.
  (let [s (assoc-in s [:precompute :desync-safe-mod?] true)
        x (first (:mouse-pos s)) y (second (:mouse-pos s))
        comps (:components s)
        kys (selectmovesize/unders-cursor x y comps)
      
        zs (mapv #(double (if-let [z (:z (get comps %1))] z %2)) kys (range))
        min-z (apply min 1e100 zs)
        ix-min (first (filter #(= (nth zs %) min-z) (range (count zs)))) ; non-unique min is uniquieified.

        zs (mapv #(if (and (= %1 min-z) (not= %2 ix-min)) (+ %1 1e-9) %1) zs (range))
        max-z (apply max -1e100 zs)
        second-min-z (apply min 1e50 (filter #(not= % min-z) zs))
        drop (- second-min-z min-z)
        zs1 (mapv #(if (= % min-z) max-z (- % drop)) zs)
        comps1 (reduce #(assoc-in %1 [(nth kys %2) :z] (nth zs1 %2)) comps (range (count kys)))]
    (assoc s :components comps1)))

;;;;;;;;;;;;;;;; Keyboard interaction with hotkeys ;;;;;;;;;;;;;;;;;;;;;

(defn shift-enter? [key-evt] (and (:ShiftDown key-evt) (= (:KeyCode key-evt) 10)))

(defn ctrl+? [kevt letter] 
  (and (or (:ControlDown kevt) (:MetaDown kevt)) (not (:ShiftDown kevt))
    (= (str letter) (str (:KeyChar kevt)))))

(defn ctrl-shift+? [kevt letter] 
  (and (or (:ControlDown kevt) (:MetaDown kevt)) (:ShiftDown kevt)
    (= (str letter) (str (:KeyChar kevt)))))

(defn esc? [kevt] (= (:KeyCode kevt) 27))

(defn hotkeys [] ; fn [s] => s, where s is the state.
  {#(ctrl+? % "w") close ; all these are (fn [s]).
   #(esc? %) toggle-typing
   #(ctrl-shift+? % "r") #(if (or (globals/are-we-child?) (warnbox/yes-no? "Relaunch app, losing any unsaved work? Does not affect the child app.")) 
                            (do (future (launch-main-app!!)) (throw (Exception. "This iteration is dead, reloading."))) %)
   #(ctrl+? % "`") swap-on-top 
   ; The saving system: 
   ; ctrl+s = save onto child generation.
   ; ctrl+shift+s = pull child onto ourselves (TODO: do this when we quit as well).
   ; The child is viewed as the most up-to-date at all times, and it is occasionally copied back to us.
   #(ctrl+? % "s") (fn [s] (iteration/save-state-to-disk!!! s)) ; save to the child, rapid iteration.
   #(ctrl-shift+? % "s") (fn [s] ; copy from child to us if we are the parent
                           (if (and (globals/can-child?) (not (globals/are-we-child?)))
                             (let [s1 (iteration/ensure-childapp-folder-init!!! s)]
                               (iteration/copy-child-to-us!!! s1) s1) s))
   #(ctrl-shift+? % "c") store-state!
   #(ctrl-shift+? % "z") (fn [_] (retrieve-state!))})

;;;;;;;;;;;;;;;; Run by symbol with evals ;;;;;;;;;;;;;;;;;;;;;

(defn recognized-cmd? [sym] (boolean (ns-resolve this-ns sym)))

(defn run-cmd [s cmd-sym args]
  "Returns the modified state."
  ; TODO: add a sort of spell-check.
  (if (recognized-cmd? cmd-sym) 
    (let [tool-fn (binding [*ns* this-ns] (eval cmd-sym))] 
      (apply tool-fn s args))
    (throw (Exception. (str "Unrecognized command: " cmd-sym)))))

;;;;;;;;;;;;;;;; Adding a component on the top of the z-stack ;;;;;;;;;;;;;;;;;;;;;

(defn _max-z [s] (apply max 0 (mapv #(if (:z %) (:z %) 0) (vals (:components s)))))

(defn add-component [s comp kwd]
  (let [z (+ (_max-z s) 1.0)]
    (update (assoc-in s [:precompute :desync-safe-mod?] true) 
      :components #(assoc % kwd (assoc comp :z z)))))

(defn open-fbrowser [s] 
  "Creates a new fbrowser from our own folder once per startup, otherwise just copies an existing root fbrowser (we never allow closing all root fbrowsers)."
  (let [comps (:components s)
        kys (filterv #(multicomp/rootfbrowser? (get comps %)) (keys comps))
        new-comp (if (> (count kys) 0) (get comps (first kys)) 
                   (fbrowser/load-from-folder (globals/get-working-folder)))]
    (add-component s new-comp (keyword (gensym 'files)))))

(defn open-repl [s] (add-component s (orepl/new-repl) (keyword (gensym 'repl))))

(defn open-console [s] (add-component s (siconsole/new-console) (keyword (gensym 'repl))))

;;;;;;;;;;;;;;;; Mid level control flow ;;;;;;;;;;;;;;;;;;;;;

(defn diff-checkpoint [s f] 
  "Runs f on s, syncing the components unless f doesn't change the components or flags [:precompute :desync-safe-mod?] to true."
  (let [s (assoc-in s [:precompute :desync-safe-mod?] false) ; safe until "proven" fast.
        s1 (f s)] ; f has a chance to set :desync-safe-mod? for optimization.
    (if (or (get-in s1 [:precompute :desync-safe-mod?]) 
          (multisync/comps-eq? (:components s) (:components s1))) s1 
      ; update-open-file-paths can't create a desync nor can it (I think) can the lack of updated paths make the resync fn fail.
      ; update-line-nos is, of course, safe to do after. 
      (assoc s1 :components (multisync/comprehensive-sync (:components s) (:components s1))))))

(defn single-comp-dispatches [evt-c s]
  (let [sel (set/intersection (apply hash-set (keys (:components s))) (apply hash-set (:selected-comp-keys s)))] ; normalize this.
    (reduce 
      (fn [s k]
        (let [comp (get-in s [:components k])
              evt (xform/xevt (xform/x-1 (singlecomp/pos-xform (:position comp))) evt-c)
              ifn (get (:interact-fns comp) (:type evt-c))
              comp1 (if ifn (assoc (ifn evt (dissoc comp :position)) :position (:position comp)) comp)]
         (assoc-in s [:components k] comp1))) 
         (assoc s :selected-comp-keys sel) sel)))
         
; The maybe-x fns return the effects of doing x if the task is triggered, else s.
(defn maybe-open-file [evt-c s compk]
  (let [comp (get (:components s) compk)
        s (assoc-in s [:precompute :desync-safe-mod?] true) 
        evt (xform/xevt (xform/x-1 (singlecomp/pos-xform (:position comp))) evt-c)
        fname (if (and (= (:type evt-c) :mousePressed) (= (:type comp) :fbrowser)) 
                 (fbrowser/fullfile-click evt comp))
        ;_ (println "Opening file: " fname fname)
        non-dir? (if fname (fbrowser/non-folder-file-click? evt comp))]
    ; Add the file to the key :fname-gui, these will be used to effect changes when the fbrowser is changed:
    (if (and fname non-dir?) 
      (let [lix (fbrowser/pixel-to-line comp (:X evt) (:Y evt))
            comp1 (assoc-in comp [:pieces lix :fname-gui] fname)
            pos (:position comp) sz (:size comp)
            cbox (assoc (if (jfile/exists? fname) (assoc (codebox/load-from-file fname) :path [fname]) (assoc (codebox/new-codebox) :path [fname])) 
                   :position (mapv + pos (mapv * sz [0.25 0.75])))
            s1 (assoc-in s [:components compk] comp1)
            s2 (update s1 :components #(multisync/spread-fname-gui % (hash-set compk)))]
        (add-component s2 cbox (gensym 'codebox))) s)))

(defn maybe-run-repl [evt-g evt-c s k]
  (if (and (= k :keyPressed) (shift-enter? evt-c))
    (reduce #(if (= (:type (get (:components %1) %2)) :orepl)
               (orepl/run-repl %1 %2 recognized-cmd? run-cmd) %1) s (:selected-comp-keys s)) s))

(defn update-mouse [evt-g evt-c s k]
  (if (or (= k :mouseMoved) (= k :mouseDragged)) 
             (assoc s :mouse-pos [(:X evt-c) (:Y evt-c)]) s))

(defn expand-child [mevt-c s]
  (let [x (:X mevt-c) y (:Y mevt-c) comps (:components s)] 
    (if-let [target-key (selectmovesize/under-cursor x y comps)] 
      (multicomp/expand-child target-key (gensym "child") mevt-c s) s)))

(defn expand-child-tool []
  {:mousePressed expand-child})

(defn open-file-tool []
  {:mousePressed (fn [mevt-c s] #_(println "open file tool ") (maybe-open-file mevt-c s (first (:selected-comp-keys s))))})

(defn single-select [mevt-c s] 
  (let [mp (:mousePressed (selectmovesize/get-tool))
        s1 (selectmovesize/clear-selecion s)]
    (mp mevt-c s1)))

;;;;;;;;;;;;;;;; High level control flow ;;;;;;;;;;;;;;;;;;;;;

(defn maybe-hotkey [evt-g evt-c s k]
  (let [hotkey-map (hotkeys)
        hk (if (= k :keyPressed) (first (filter #(% evt-g) (keys hotkey-map))))]
    (if hk ((get hotkey-map hk) s) s)))

(defn maybe-use-tool [evt-c s tool]
  (let [k (:type evt-c)] (if (not tool) (throw (Exception. "nil tool.")))
    (if-let [f (get tool k)] (f evt-c s) s)))

(defn maybe-common-tools [evt-c s]
  "Apply common tools using where the user clicks, the button type, etc as a switchyard.
   No need for syncing components."
  (let [ek (:type evt-c)
        ech (expand-child-tool)
        ofl (open-file-tool)
        sms (selectmovesize/get-tool)
        cam (selectmovesize/get-camera-tool)
        ut #(maybe-use-tool evt-c s %)
        
        click-target (if (= ek :mousePressed) (selectmovesize/under-cursor (:X evt-c) (:Y evt-c) (:components s)))
        m? (or (= ek :mousePressed) (= ek :mouseDragged))
        double? (and (= ek :mousePressed) (= (:ClickCount evt-c) 2))
        shift? (:ShiftDown evt-c)
        ctrl? (:ControlDown evt-c)
        alt? (:AltDown evt-c)
        meta? (:MetaDown evt-c)]
    (cond
      (= ek :mouseReleased) (ut sms)
      (and click-target ctrl? double?) (ut ech)
      (and click-target double?) (ut ofl)
      (or (and (= ek :keyPressed) (not ctrl?) (not meta?)) 
        (and (or (= ek :mousePressed) (= ek :mouseDragged)) (or meta? ctrl?))) (ut cam)
      :else (ut sms))))

(defn dispatch-listener [evt-g s k] 
  "Transforms and dispatches an event (that doesn't change :typing-mode? and :active-tool).
   Only certain changes to f need diff checking."
  (let [s0 s evt-c (xform/xevt (xform/x-1 (:camera s)) evt-g) 

        x (if (= k :parent-in) ; only used to update namespaces for now.
            (let [txt (:contents evt-g)]
              (try (eval (read-string txt))
                (catch Exception e
                  (throw (Exception. (str "Eval of: " txt "\n Produced this error: " (.getMessage e)))))))) 
        s (if x (siconsole/log s (str "Parent command result:\n" x)) s)
        s (update-mouse evt-g evt-c s k)
        
        s (diff-checkpoint s #(maybe-hotkey evt-g evt-c % k))
        s (diff-checkpoint s #(maybe-run-repl evt-g evt-c % k))
        
        ; Typing mode forces single component use.
        s (if (and (:typing-mode? s) (= k :mousePressed)) (single-select evt-c s) s)
        s (cond (and (= k :mouseDragged) (or (:ControlDown evt-c) (:MetaDown evt-c))) 
            (maybe-use-tool evt-c s (selectmovesize/get-camera-tool))
            (:typing-mode? s) (diff-checkpoint s #(single-comp-dispatches evt-c %))
            :else (maybe-common-tools evt-c s))        
        ] s))
        
;;;;;;;;;;;;;;;; Rendering ;;;;;;;;;;;;;;;;;;;;;

(defn update-gfx [s]
  "Creates the gfx commands in the :precompute that are returned in app-render, see javac/gfx for format details."
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
        
        precompute1 (assoc precompute :comps-at-render comps :comp-renders gfx-comps-l)]
    (assoc-in s [:precompute :gfx] precompute1)))

(defn app-render [s]
  (let [pre-gfx (:comp-renders (:gfx (:precompute s)))
        ;_ (println "RENDERGFX: " pre-gfx)
        cam (:camera s) comps (:components s)

        sel-kys (apply hash-set (:selected-comp-keys s))
        
        comp-sprites (zipmap (keys comps) 
                       (mapv (fn [k] 
                               (let [c (get comps k)
                                     cam1 (xform/xx cam (singlecomp/pos-xform (:position c)))]
                                 {:camera cam1 :gfx (get pre-gfx k) :z (:z c)})) (keys comps)))
        tool (:active-tool s) ; don't precompute the tools, not necessary, at least for now.
        ; TODO: use the cam or use no cam?
        tool-sprite {:camera cam :gfx (if-let [rf (:render tool)] (rf s) []) :no-sprite? true}
        
        sms-sprite (if (:typing-mode? s) {:camera [0 0 1 1] :gfx []} 
                     {:camera cam :gfx ((:render (selectmovesize/get-tool)) s) :z 1e10 :no-sprite? true})
        
        ;_ (println "tool gfx: " tool-sprite)
        
        sel-keys (apply hash-set (:selected-comp-keys s))
    
        sel-sprite {:no-sprite? true :camera cam :z 2e10
                    :gfx (into [] (apply concat (mapv #(multicomp/draw-select-box comps % [0 0 1 1]) sel-keys)))}
        which-tool {:no-sprite? true :camera [0 0 1 1] :z 1e100
                    :gfx (multicomp/which-tool-hud s)}]
    (assoc comp-sprites :TOOL-SPRITE-CORE tool-sprite :TOOL-SMS-SPRITE sms-sprite :TOOL-SEL-SPRITE sel-sprite :TOOL-WHICH-MODE which-tool)))

;;;;;;;;;;;;;;;; The setup ;;;;;;;;;;;;;;;;;;;;;

(defonce modified-files (atom {})) ; name -> new text. Disk singleton.

(defn get-tools []
  "Non-common tools that are interactive versions of commands."
  [{:name :no-tool}])

(defn try-dispatch-listener [evt-g s kwd]
  "Can't use a generic f as input b/c functions won't always get updated when that happens.
   Logs prints within the listener to si-console."
  (try (let [tuple (clojurize/capture-out-tuple dispatch-listener evt-g s kwd) ; try to get the new state.
             s1-or-ex (first tuple) txt (second tuple)
             _ (if (> (count txt) 0) (println txt)) ; mirror the output in the real console.
             err? (instance? java.lang.Exception s1-or-ex)
             s2 (if err? s s1-or-ex)
             s3 (if (= (count txt) 0) s2 (siconsole/log s2 (str txt "\\n")))]
         (if err? (siconsole/log s3 (orepl/pr-error s1-or-ex)) s3))
    (catch Exception e
      (let [err-report (orepl/pr-error e)]
        (println "DISPATCH EVENT ERROR")
        (println (orepl/pr-error e))
        (siconsole/log s err-report)))))

(defn low-cpu-mouse-move [evt-g s]
  (let [evt-c (xform/xevt (xform/x-1 (:camera s)) evt-g)]
    (update-mouse evt-g evt-c s :mouseMoved)))

(defn listener-fns []
    {:mousePressed (fn [evt-g s] (try-dispatch-listener evt-g s :mousePressed))
     :mouseMoved   (fn [evt-g s] (if cpanel/*low-cpu?* (low-cpu-mouse-move evt-g s) (try-dispatch-listener evt-g s :mouseMoved)))
     :mouseDragged (fn [evt-g s] (try-dispatch-listener evt-g s :mouseDragged))
     :mouseReleased (fn [evt-g s] (try-dispatch-listener evt-g s :mouseReleased))
     :keyPressed (fn [evt-g s]    (try-dispatch-listener evt-g s :keyPressed))
     :keyReleased (fn [evt-g s] (try-dispatch-listener evt-g s :keyReleased))
     :everyFrame (fn [evt-g s] (try-dispatch-listener evt-g s :everyFrame))
     :mouseWheelMoved (fn [evt-g s] (try-dispatch-listener evt-g s :mouseWheelMoved))
     :parent-in (fn [evt-g s] (try-dispatch-listener evt-g s :parent-in))})

(defn launch-main-app!! []
  (cpanel/stop-app!!)
  (let [s {:components {} :camera [0 0 1 1] :typing-mode? true :active-tool (first (get-tools))}
        s1 (if (and (globals/can-child?) (not (globals/are-we-child?))) (iteration/ensure-childapp-folder-init!!! s) s)
        s2 (-> s1 (open-console) (open-fbrowser) (open-repl))
        s3 (update s2 :components multicomp/grid-layout)
        s4 (assoc s3 :selected-comp-keys #{})]
    (cpanel/launch-app!! (update s4 :components multicomp/unique-z) (listener-fns)
      (fn [& args] (try (apply update-gfx args)
                     (catch Exception e (do (println "error: " e)))))
      (fn [& args] (try (apply app-render args)
                     (catch Exception e (do (println "error: " e))))))))

; DONT call save-file, we can't run swing stuff from shutdown hooks reliably.
; Instead we do this on the exit-if-close function.
; TODO: get this working so cmd+q doesn't quit.
(defn on-shutdown [] ())

;;  startup
(defn -main [& args] (launch-main-app!!))
