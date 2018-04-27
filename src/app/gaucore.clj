; Launches main app.
; :components is from keywords to the components.
  ; We add a :position vector.
; :selected-comp-keys = components that are selected.
; :camera is a transform.
; The repls share the same state-space.

(ns app.gaucore
  (:require [javac.cpanel :as cpanel]
    [app.multicomp :as multicomp] 
    [app.multisync :as multisync] 
    [app.orepl :as orepl]
    [app.fbrowser :as fbrowser] 
    [app.codebox :as codebox]
    [app.singlecomp :as singlecomp]
    [app.selectmovesize :as selectmovesize]
    [app.xform :as xform]
    [javac.file :as jfile]
    [javac.warnbox :as warnbox]
    [clojure.string :as string]
    [app.siconsole :as siconsole]
    [clojure.set :as set]))

(def this-ns *ns*)

(defonce _state-buf (atom {}))

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

(defn store-state [s]
  "stores the state in our own buffer atom, to be retrieved later. 
   More sophisticated undo system TODO."
  (reset! _state-buf s) s)

(defn retrieve-state [] (assoc-in @_state-buf [:precompute :desync-safe-mod?] true))

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

(defn save1!!! [fname txt clj?]
  (if clj? 
    (let [report (orepl/save-and-update!!! fname txt)]
      (if (:error report)
        (println "Saved:" fname "Compile error:" (:error report))
        (println "Saved:" fname "Namespace updated.")))
    (do (jfile/save!!! fname txt) (println "Saved:" fname "Not a clojure file"))))

(defn _save-core!!! [s usfiles open2text new-files changed-files deleted-files0 missing-files renamed-map copied-map]
  "Applys the save, with warning dialogues. Returns the modified s."
  (println "crtl+s results: new: " new-files "changed:" changed-files
    "deleted if click yes: " deleted-files0 "missing: " missing-files "renamed: " renamed-map "copied: " copied-map)
  (let [deleted-files (if (and (> (count deleted-files0) 0) (warnbox/yes-no? (str "Delete: " deleted-files0))) deleted-files0 #{})]
    ;(throw (Exception. "Save disabled for safety reasons.")) ; DEBUG safety.
    ; TODO: rename file reload namespace somehow.
    (mapv #(jfile/rename!!! %1 %2) (keys renamed-map) (vals renamed-map))
    (mapv (fn [fname] 
            (let [text (jfile/open fname)]
              (mapv #(jfile/save!!! % text) (get copied-map fname)))) (keys copied-map))
    (mapv #(save1!!! % (if-let [x (get open2text %)] x "") (jfile/clj? %)) (set/union new-files changed-files))
    (mapv jfile/delete!!! deleted-files)
    ; The fbrowser was already updated. Thus only missing files or files the user decided not to delete:
    (let [new-fileset (-> usfiles
                        (set/difference missing-files)
                        (set/union (set/difference deleted-files0 deleted-files)))]
      (multicomp/set-filetree s (multicomp/wrap-tree new-fileset) true))))

(defn save-all!!! [s]
   "Everything is saved, deletions will be prompted."
  (let [disk (apply hash-set (multicomp/get-filelist {:components {:tmp {:pieces [(first (:pieces (fbrowser/load-from-disk)))] :type :fbrowser}}} false nil))
        ; disk has ./folder/file.clj format, and is more than just clj files.
        comps (:components s) codeboxks (filterv #(= (:type (get comps %)) :codebox) (keys comps))
        open (apply hash-set (mapv #(first (:path (get comps %))) codeboxks))
        new2?old (multicomp/new2?old-files s)
        open2text (zipmap (into [] open)
                    (mapv #(multicomp/get-filetext s %) (into [] open)))
        get-old (fn [new] (if-let [x (get new2?old new)] x new))
        old2new (dissoc (zipmap (vals new2?old) (keys new2?old)) nil false)

        ; Types of files that weren't left as-is (the new filename is used for files that are both renamed and changed or missing):
        renamed-map (let [diffs (filterv #(let [o (get new2?old %)] (and o (not= o %) (not= (get new2?old o) o))) (keys new2?old))] ; the (get new2?old o) is to exclude directly copying the file.
                      (zipmap (mapv #(get new2?old %) diffs) diffs))
        new-files (apply hash-set (filterv #(and (not (get new2?old %)) (not (jfile/exists? %))) (keys new2?old)))
        missing-files (apply hash-set (filterv #(let [o (get new2?old %)] (and o (not (jfile/exists? o)))) (keys new2?old)))
        changed-files (let [change? (fn [new] (not= (jfile/open (get-old new)) (get open2text new)))]
                        (apply hash-set (filterv change? (set/difference open new-files))))
        copied-map (reduce (fn [acc fname] 
                             (let [old (get new2?old fname)]
                               (if (or (not old) (= (get renamed-map old) fname) (get new-files fname) (= old fname)) acc
                                 (update acc old #(if % (conj % fname) [fname]))))) {} (keys new2?old))
        deleted-files (apply hash-set (filterv #(and (not (get old2new %)) (not (contains? new2?old %))) disk))]
    (_save-core!!! s (apply hash-set (keys new2?old)) open2text new-files changed-files deleted-files missing-files renamed-map copied-map)))

;;;;;;;;;;;;;;;; Keyboard interaction with hotkeys ;;;;;;;;;;;;;;;;;;;;;

(defn shift-enter? [key-evt] (and (:ShiftDown key-evt) (= (:KeyCode key-evt) 10)))

(defn ctrl+? [kevt letter] 
  (and (or (:ControlDown kevt) (:MetaDown kevt))
    (= (str letter) (str (:KeyChar kevt)))))

(defn ctrl-shift+? [kevt letter] 
  (and (or (:ControlDown kevt) (:MetaDown kevt)) (:ShiftDown kevt)
    (= (str letter) (str (:KeyChar kevt)))))

(defn esc? [kevt] (= (:KeyCode kevt) 27))

(defn hotkeys [] ; hot keys in typing mode take in key events and return functions.
  {#(ctrl+? % "w") close ; all these are (fn [s]).
   #(esc? %) toggle-typing
   #(ctrl+? % "s") #(save-all!!! %)
   #(ctrl+? % "`") swap-on-top 
   #(ctrl-shift+? % "c") store-state
   #(ctrl-shift+? % "z") (fn [_] (retrieve-state))})

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

(defn open-file-browser [s] (add-component s (fbrowser/load-from-disk) (keyword (gensym 'files))))

(defn open-repl [s] (add-component s (orepl/new-repl) (keyword (gensym 'repl))))

(defn open-console [s] (add-component s (siconsole/new-console) :console)) ; only one :console for errors, maby allow better consoles.

;;;;;;;;;;;;;;;; Mid level control flow ;;;;;;;;;;;;;;;;;;;;;

(defn diff-checkpoint [s f] 
  "Runs f on s, syncing the components unless f doesn't change the components or flags [:precompute :desync-safe-mod?] to true."
  (let [s (assoc-in s [:precompute :desync-safe-mod?] false) ; safe until "proven" fast.
        s1 (f s)] ; f has a chance to set :desync-safe-mod? for optimization.
    (if (or (get-in s1 [:precompute :desync-safe-mod?]) 
          (multisync/comps-eq? (:components s) (:components s1))) s1 
      ; update-open-file-paths can't create a desync nor can it (I think) can the lack of updated paths make the resync fn fail.
      ; update-line-nos is, of course, safe to do after. 
      (multisync/iterative-sync s s1))))

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
        file (if (and (= (:type evt-c) :mousePressed) (= (:type comp) :fbrowser)) 
                (fbrowser/fullfile-click evt comp))
        non-dir? (if file (fbrowser/non-folder-file-click? evt comp))]
    ; Add the file to the key :fname-gui, these will be used to effect changes when the fbrowser is changed:
    (if (and file non-dir?) 
      (let [lix (fbrowser/pixel-to-line comp (:X evt) (:Y evt))
            comp1 (assoc-in comp [:pieces lix :fname-gui] file)
            pos (:position comp) sz (:size comp)
            cbox (assoc (if (jfile/exists? file) (codebox/load-from-file file) (assoc (codebox/new-codebox) :path [file])) 
                   :position (mapv + pos sz))
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
    (assoc comp-sprites :TOOL-SPRITE-GAUCORE tool-sprite :TOOL-SMS-SPRITE sms-sprite :TOOL-SEL-SPRITE sel-sprite :TOOL-WHICH-MODE which-tool)))

;;;;;;;;;;;;;;;; The setup ;;;;;;;;;;;;;;;;;;;;;

(defonce modified-files (atom {})) ; name -> new text. Disk singleton.

(defn get-tools []
  "Non-common tools that are interactive versions of commands."
  [{:name :no-tool}])

(defn try-disptach-listener [evt-g s kwd]
  "Can't use a generic f as input b/c functions won't always get updated when that happens."
  (try (dispatch-listener evt-g s kwd) ; try to get the new state.
    (catch Exception e
      (let [err-report (orepl/pr-error e)]
        (println "DISPATCH EVENT ERROR")
        (update-in s [:components :console]
          #(siconsole/set-text % err-report))))))

(defn low-cpu-mouse-move [evt-g s]
  (let [evt-c (xform/xevt (xform/x-1 (:camera s)) evt-g)]
    (update-mouse evt-g evt-c s :mouseMoved)))

(defn listener-fns []
    {:mousePressed (fn [evt-g s] (try-disptach-listener evt-g s :mousePressed))
     :mouseMoved   (fn [evt-g s] (if cpanel/*low-cpu?* (low-cpu-mouse-move evt-g s) (try-disptach-listener evt-g s :mouseMoved)))
     :mouseDragged (fn [evt-g s] (try-disptach-listener evt-g s :mouseDragged))
     :mouseReleased (fn [evt-g s] (try-disptach-listener evt-g s :mouseReleased))
     :keyPressed (fn [evt-g s]    (try-disptach-listener evt-g s :keyPressed))
     :keyReleased (fn [evt-g s] (try-disptach-listener evt-g s :keyReleased))
     :everyFrame (fn [evt-g s] (try-disptach-listener evt-g s :everyFrame))
     :mouseWheelMoved (fn [evt-g s] (try-disptach-listener evt-g s :mouseWheelMoved))})

(defn launch-main-app!! []
  (cpanel/stop-app!!)
  (let [s {:components {} :camera [0 0 1 1] :typing-mode? true :active-tool (first (get-tools))}
        s1 (-> s (open-console) (open-file-browser) (open-repl))
        s2 (update s1 :components multicomp/grid-layout)
        s3 (assoc s2 :selected-comp-keys #{})]
    (cpanel/launch-app!! (update s3 :components multicomp/unique-z) (listener-fns)
      (fn [& args] (try (apply update-gfx args)
                     (catch Exception e (do (println "error: " e)))))
      (fn [& args] (try (apply app-render args)
                     (catch Exception e (do (println "error: " e))))))))