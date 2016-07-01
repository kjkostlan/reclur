(ns clooj.java.updater
  (:import [javax.swing SwingUtilities] [javax.swing JFrame] 
           [java.awt.event WindowEvent]
           ;[java.util EventObject] ; nessessary?
           ;[jcode EventRelay]
           )
  (:require [clooj.java.widget :as widget]
            [clooj.java.thread :as thread]
            [clojure.set :as set]
            [clojure.repl]
            [clooj.java.layout :as layout]
            [clooj.java.listener :as listener]
            [clooj.coder.grammer :as grammer]
            [clooj.java.gfx :as gfx]
            [clooj.java.clojurize :as clojurize]
            [clooj.collections :as collections]))

(def debug-total-num-events (atom 0)) ; debugging.

; Target fps:
; Note: internally every-frame is NOT represented as an event listener. 
; Instead there is a seperate list of :anim-tracker which is a vector of which paths which have an :Every-frame event.
; TODO: allow the user to control the frame rate.
(def frame-rate 30.0) 

(defn edt? [] (SwingUtilities/isEventDispatchThread))

(defn assert-edt []
  (if edt? true (throw (Exception. "Updater's functions must be called from the event dispatch thread."))))

(defn ty-changed? [delta old-state new-state]
  (if (not= (keyword (:Type (get-in old-state delta)))
            (keyword (:Type (get-in new-state delta))))
    true false))

(defn update-java-tree-structure [java deltas old-state new-state root-atom]
  "Returns the new java structure, remove and creating the java objects.
   Does not do anything to the already existing java objects."
  (let [; Remove the :obj key for removed items:
        tychnages (filterv #(ty-changed? % old-state new-state) (:change deltas)) ; type changes are remove + add.
        removed0 (reduce #(collections/updayte-in %1 %2 dissoc :obj) java (concat tychnages (:remove deltas)))
        removed (reduce #(collections/updayte-in %1 %2 dissoc :Children) removed0 (concat (:remove deltas))); a more complete removal.
        jadd (fn [j p] (assoc-in j (concat p [:obj]) (widget/make-component new-state root-atom p)))
        added (reduce jadd removed (concat tychnages (:add deltas)))
        tychanged (reduce #(if (ty-changed? %2 old-state new-state) (jadd %1 %2) %1) added (:change deltas))] tychanged))

(defn _add-remove-childs [java deltas add? old-state new-state]
  (let [g #(get-in java (concat % [:obj])) ; extra :obj so that the java object does not collide with :Children. 
        tychnages (filterv #(ty-changed? % old-state new-state) (:change deltas))
        ch (concat tychnages (if add? (:add deltas) (:remove deltas)))] ;changes are a removed and an add.
    (mapv #(vector (g (butlast (butlast %))) (g %) (butlast (butlast %)) %) 
      (filterv #(> (count %) 1) ch)))) ; filterv blocks out the root path, we never add or remove the root to anything

(defn add-childs [new-java deltas old-state new-state]
  "Gets a list of which objects need to be added to which other objects. Does not yet add the children.
  delta is from the tree-delta betwene the old-state and new-state.
  Objects must first be created.
  Each element is [parent child parent-path child-path]."
  (_add-remove-childs new-java deltas true old-state new-state))

(defn remove-childs [old-java deltas old-state new-state]
  "Opposite of add-childs. This will include unnessessary removals but that shouldn't be a big deal."
  (_add-remove-childs old-java deltas false old-state new-state))

(defn stop-anim!! [root-atom]
  (thread/stop!! (str "updater every frame:" root-atom)))

(defn delete! [root-atom]
  "Deletes 'everything' by removing the top level GUI component and nilling the atom. The GC should handle the rest."
  (assert-edt)
  (let [root-ob (:obj (:java @root-atom))] 
      (try (clojurize/on-java-change (.removeAll (.getContentPane root-ob)) root-atom) (catch Exception e))
      (if (instance? JFrame root-ob) ; close the root if it is a JFrame.
          (clojurize/on-java-change (.dispatchEvent root-ob (WindowEvent. root-ob WindowEvent/WINDOW_CLOSING)) root-atom))
      (stop-anim!! root-atom)
      (reset! root-atom nil)))

(defn _daycare! [java path old-state new-state]
  "Some functions like .pack JFrame need all the child components first set, etc.
   Call this at the end of java updating."
  (let [old-substate (get-in old-state path) new-substate (get-in new-state path)]
    ; Leaf step:
    ; Several different rules:
    (if 
    ; More major changes (fails with guitest/square):
    ;(or (not= (keyword (:Type old-substate)) (keyword (:Type new-substate))) (not= (:Children old-substate) (:Children new-substate)))
    ; Minor changes as well (may cause performance or other problems):
    (and (not= old-substate new-substate) (:Type new-substate)) ; sometimes new-substate may be removed, so we make sure it has a type.
    ; :Type or :Children modifications.
      (do 
      (if (nil? (get-in java (concat path [:obj]))) (/ 0))
      (widget/daycare! (get-in java (concat path [:obj])) old-substate new-substate)))))

(defn update-defaults-recursive [old-substate new-substate]
  "Only when there are changes."
  (if (or (= old-substate new-substate) (not (:Type new-substate))) new-substate ; no work needed.
    (widget/add-defaults 
      (if (:Children new-substate) ; recursive part after alignment.
        (let [ch-aligned (widget/align-children old-substate new-substate) ; map of [old new] pairs.
              ch1 (grammer/cmap :vals #(update-defaults-recursive (first %) (second %)) ch-aligned)
              ;This is a chokepoint that means only maps and vectors work. fix this TODO? 
              ; only update keys on the new substate:
              ch2 (reduce #(assoc %1 %2 (get ch1 %2)) (:Children new-substate) 
                    (grammer/ckeys (:Children new-substate)))]
          (assoc new-substate :Children ch2)) new-substate))))

(defn add-defaults-recursive [substate]
  "Recursivly adds defaults if they aren't already speficied."
  (update-defaults-recursive nil substate))

(defn _assert-valid-state [old-state new-state path]
  ; Some common vaidity of states. Does not fix bad stuff sent to the java.
  ; old-state is there to only check the changes (performance).
  (let [old-sub (get-in old-state path) new-sub (get-in new-state path)]
    (cond (or (not new-sub) (= (count new-sub) 0)) {:path path :error "nil/empty state"}
          (not (map? new-sub)) {:path path :error "Not a map."}
          (nil? (:Type new-sub)) {:path path :error "Nil type."}
          (nil? (get widget/make-ob-fns (keyword (:Type new-sub)))) {:path path :error (str "Unrecognized type: "  (:Type new-sub))}
          :else (if (and (:Children new-sub) (not (vector? (:Children new-sub))) (not (map? (:Children new-sub))))
                  {:path path :error "Children are not a vector or map."}
                  (if (not= (:Children old-sub) (:Children new-sub))
                    (let [nkeys (grammer/ckeys (:Children new-sub))]
                      (first (filterv identity (mapv 
                            #(_assert-valid-state old-state new-state (conj path :Children %)) nkeys)))))))))
                  
(defn assert-valid-state [old-state new-state tips] 
  "Catches some common errors with the state changing."
   (let [x (_assert-valid-state old-state new-state [])]
     (if x (throw (Exception. (str "State error when: " (first tips) ": " (:error x) " on branch: " (:path x)))))))
   
;_ (if (not (map? new-substate)) (throw (Exception. "Listener-fns must always return the modified state, and do so as a map.")))

(defn update! [root-state-old new-state set-visible? update-fns root-atom & tips]
  "Updates all of the fields in the event tracker. The only modification is to the java objects.
   The atom is not modified nor accessed in any way, it's just set as a ClientProperty into new java objs.
   tips = helps localize error messages, optional.
   Used in cojunction of updating the atom: (reset! root-atom old-root (update old-root new-state ...))"
  (assert-edt)
  (let [root-state root-state-old old-state (:state root-state)
        _ (assert-valid-state old-state new-state tips)
        old-event-tracker (:event-tracker root-state)
        deltas (widget/tree-delta old-state new-state)
        new-event-tracker (listener/update-event-tracker old-event-tracker old-state new-state deltas)
        new-anim-tracker (listener/update-anim-tracker (:anim-tracker root-state) old-state new-state deltas)
        new-java (update-java-tree-structure (:java root-state) deltas old-state new-state root-atom)
        ch-removes (remove-childs (:java root-state) deltas old-state new-state)
        ch-adds (add-childs new-java deltas old-state new-state)

        ;_ (if (> (count ch-removes) 0)
        ;   (println "Deltas:" deltas "remove:" (mapv str (mapv second ch-removes))))
        ;_ (if (> (count ch-adds) 0)
        ;   (println "Deltas:" deltas "add:" (mapv str (mapv second ch-adds))))
        
        unwrapped-deltas (concat (:add deltas) (:change deltas) (:remove deltas))
        
        ; Everything that needs to be taken care of.
        needs-daycare (apply hash-set (concat unwrapped-deltas (apply concat (mapv listener/ancestor-paths unwrapped-deltas))))

        ; java objects need to be updated:
        ; We block updates right before reflection.
        needs-update (filterv #(let [olds (get-in old-state %) news (get-in new-state %)]
                                (if (and (not= (dissoc olds :Children) (dissoc news :Children))
                                         (= (keyword (:Type olds)) (keyword (:Type news)))) true false)) 
                       (:change deltas))
        ; all updates: set :Repaint? to false if it is true.
        ; If Repaint? deep in a hirearchy is set true by a function, it will force updates and the associated repainting.
        new-state1 (reduce #(let [subst (get-in %1 %2)] 
                              (if (:Repaint? subst) (collections/asoc-in %1 (assoc subst :Repaint? false))
                                  %1)) new-state needs-update)
        ; The children changed so we must revalidate this:
        nil2mtyvec #(if % (into [] %) [])
        needs-children-changed-refresh
          (if old-state ; skip when the old-state is nil to speed startup time (we are already doing it).
            (let [struct-changes (concat (:add deltas) (:remove deltas) 
                      (filterv #(not= (keyword (:Type (get-in old-state %))) (keyword (:Type (get-in new-state %))))
                        (:change deltas)))
                 need0 (apply hash-set (mapv #(nil2mtyvec (butlast (butlast %))) struct-changes))]
              (filterv #(get-in new-java (concat % [:obj])) need0))); new-java may not have certain stuff in old java.
        ]
    (mapv #(widget/remove-child! (first %) (second %)) ch-removes) ; remove before add.
    (mapv #(widget/add-child! (first %) (second %) new-state (nth % 2)) ch-adds)
    ; update component for changes with no :Type change.
    ; TODO: added components don't appear immediatly.
    (mapv #(widget/update-component! (get-in new-java (concat % [:obj])) 
             (get-in old-state %) (get-in new-state %) set-visible?) needs-update)

    (mapv #(widget/refresh! (get-in new-java (concat % [:obj]))) needs-children-changed-refresh)
    
    ; listener updates:
    (listener/update-java-listeners! new-java old-event-tracker new-event-tracker old-state new-state deltas update-fns)

    ; TODO: order of daycare?
    (mapv #(_daycare! new-java % old-state new-state) needs-daycare)
    
    (assoc root-state :state new-state1 :java new-java :event-tracker new-event-tracker
      :anim-tracker new-anim-tracker))) ; return the modified state.

; TODO: let the user define filters to events.
; old-root and new-root are kind of a before and after @root-atom.

(defn process-event [root-state path ec listener-key updating-for-another-event? do-builtin? do-user?]
  "Returns the new state, NOT the root-state (the root-state is only to get java objects).
   To get the new root-state, it must be update!'d with this new-state.
   updating-for-another-event? = another event has fired and we are doing an update for that.
   it bieng true prevents all builtin listeners, and user listeners
   (unless the flag :Recursive-event? is true).
   do-builtin? = do we do the builtin event.
   do-user? = do we do the user events."
  (let [ec (assoc ec :path path)
        obj (get-in root-state (concat [:java] path [:obj])) ; extra :obj key.
        dispatch (fn [state listenerf] ; returns the modified state.
                     (if listenerf 
                       (let [old-substate (get-in state path)
                             old-sub-listen (listenerf old-substate ec obj)
                             new-substate (update-defaults-recursive old-substate old-sub-listen)
                             _ (if (not (map? new-substate)) (throw (Exception. (str "Listener function didn't return a valid state: " listenerf " for type: " (:Type old-substate)))))
                             ]
                         (collections/asoc-in state path new-substate)) state))
        old-sub (get-in root-state (concat [:state] path))
        builtin-listenerfs (widget/upkeep-listener-fcns (keyword (:Type old-sub))); (get widget/all-widget-wrappers (keyword (:Type substate))))

        ; The builtin shouldn't modify the java, except in a few corner cases that we don't need to worry about.
        mid-state (if (and updating-for-another-event? do-builtin?) (:state root-state) 
                    (dispatch (:state root-state) (listener-key builtin-listenerfs)))
        new-state (if (and do-user? 
                        (or (not updating-for-another-event?) (:Recursive-event? (get-in (:state root-state) path))))
                    (dispatch mid-state (listener-key old-sub)) mid-state)]
   new-state))
       
(defn anim-loop!! [root-atom] ; Gets put into a thread/pulse!! and is animated each step.
  (fn [] (SwingUtilities/invokeAndWait 
    (fn []
      ; TODO: gaurentee the order of the animation loop.
      (let [root-state @root-atom]
        (if root-state
          (let [t0 (thread/get-time-ns)
                ; Processing is always false since this is a seperate thing.
                ; To avoid timing issues, all :every frame events hapen on thier states:
                new-state (reduce #(process-event (assoc root-state :state %1) %2 
                                                 {:nanos t0} :Every-frame false true true)
                             (:state root-state) (:anim-tracker root-state))
                update-fns {:update! update! :process-event process-event}]
        (reset! root-atom (update! root-state new-state true update-fns root-atom "every frame")))))))))

(defn setup! [root-atom] 
  "Setups the state which is bound as :state to @root-atom.
   atomisticly modifies the atom but the java can take a bit longer."
  (assert-edt)
  (let [root-state @root-atom
        update-fns {:update! update! :process-event process-event}]
    ;(println "resetting atom setup!:" (str root-atom)  (:state @root-atom) "->" init-state)
    (reset! root-atom (update! {} (:state root-state) false update-fns root-atom "setup"))
    ; Does not directly read or write from the atom:
    (thread/pulse!! (anim-loop!! root-atom) frame-rate 
      (str "updater every frame:" root-atom))))