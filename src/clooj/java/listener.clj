; Handles two main tasks:
; Figureing out which listeners are valid and converting between java and clojure's formats.
; Keeping track of what is listening to what (the :event-tracker).

(ns clooj.java.listener
  (:require [clooj.java.widget :as widget]
            [clooj.java.detail :as detail]
            [clooj.java.clojurize :as clojurize]
            [clojure.set :as set]
            [clooj.coder.grammer :as grammer]
            [clojure.string :as string]
            [clooj.java.rawdoc :as rawdoc]
            [clooj.java.errcatch :as errcatch]
            [clooj.coder.history :as history]
            [clooj.collections :as collections]))
(def debug (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Part 1: working with java and listeners ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: cleaner code if we represent listeners as keywords in our map. Currently it's mostly symbols.
; Import the listeners:
(def bad-listener-classes #{}) ; just in case.
(do (import '(java.beans ExceptionListener VetoableChangeListener ; misc stuff.
               PropertyChangeListener)
      '(java.awt.event MouseAdapter)
      '(java.awt.event ComponentEvent)))

; TODO: the rawdoc class list is redundent with the full listener list.
(defmacro listener-class-imports [_]
  (let [s2v (fn [s] (mapv symbol (string/split s #"\n")))
        cls (into [] (concat (mapv #(vector 'javax.swing.event %) (s2v rawdoc/javax-swing-event-listener-classes))
                             (mapv #(vector 'java.awt.event %)  (s2v rawdoc/java-awt-event-listener-classes))))]
    ;(println `(import ~@(mapv #(list 'quote (apply list %)) cls)))
    `(import ~@(mapv #(list 'quote (apply list %)) cls))))

(do (listener-class-imports _))

(def listener-list
  "{:class :adapter :methods :event-classes}, all symbols.
   Methods and event-classes are an array of symbols."
  (let [jumbled-lines (string/split rawdoc/listener-list-raw #"\n")
        lines (loop [acc [] ix 0]
                (if (= ix (count jumbled-lines)) acc
                  (let [l (nth jumbled-lines ix)
                        lasty (dec (count acc))
                        partial (re-matches #"[a-zA-Z0-9]+[\(\<].*" l)]
                    (if partial (recur (assoc acc lasty (str (nth acc lasty) " " l)) (inc ix)) ; partial line
                                (recur (conj acc l) (inc ix)))))) 
        list1 
         (fn [line]
           (let [pieces (string/split (string/replace line #" +" " ") #" ")
                 method-infos (mapv (fn [s] (string/split s #"[\(\<\)\>]")) (subvec pieces 2))]
             {:class (symbol (first pieces))
              :adapter (let [sc (second pieces)] (if (= sc "none") nil (symbol sc)))
              :methods (mapv #(symbol (first %)) method-infos)
              :event-classes (mapv #(symbol (second %)) method-infos)}))]
    (filterv #(not (contains? bad-listener-classes (:class %)))
      (mapv list1 lines))))

(def listener-map
  "A map from listener classes as keywords to listener functions, built from listener-list."
  (zipmap (mapv #(keyword (:class %)) listener-list)
    (mapv #(dissoc % :class) listener-list)))

(def listener-fns
  "All listner functions, as a hash-set of keywords. Used to determine which listeners we need to listen to."
  (apply hash-set (reduce #(concat %1 (mapv keyword (:methods (%2 listener-map)))) [] (keys listener-map))))

(def flavored-listener-fns
  "Like listener-fns but all combinations of listener and qualifier (<none>, :above, :below, :and and -any-event)."
  (let [lf listener-fns prepend #(keyword (str %1 (subs (str %2) 1)))]
    (apply hash-set (concat lf (mapv #(prepend "above-" %) lf) (mapv #(prepend "below-" %) lf)
               (mapv #(prepend "self-" %) lf) [:anyEvent :above-anyEvent :below-anyEvent :self-anyEvent]))))

; TODO: check and resolve for namespace collision (there may be custom components).
(def inverse-listener-map
  "From listener functions as keywords to classes as symbols. Each function's name is unique so there is no namespace collision."
  (reduce 
    (fn [acc pack] (reduce #(assoc %1 (keyword %2) (:class pack)) acc (:methods pack))) ; pack is {:class :methods}
  {} (mapv #(hash-map :class (:class %) :methods (:methods %)) listener-list)))

; Makes java listener objects using proxies.
; The state's fields will depend on the component (and anything else you add) but
; all components share the same listener functions (nils are ignored).
; We convert fns from the state into fns that use e to access the mutable states.
; The mutable atoms, accessed via (.getClientProperty (.getSource e)), are setup later (when the GUI is built).
(defmacro listener [l-type update-fns]
  "Creates a listener of a given (leaf) type."
  (let [names (mapv str (:methods ((keyword l-type) listener-map)))
        _ (if (nil? names) (throw (Exception. (str "Unrecognized type of listener to add: " l-type))))
        syms (mapv gensym names) ;make sysm distinct from names.
        kys (mapv keyword names) ; keywords to look-up the state's functions in.
        e (gensym 'e) c (gensym 'c)
        out `(let [~c #(listen-fn % ~update-fns)
                  ; Apply the listen-convert function to each point.
                  ~@(into [] (apply concat (mapv (fn [sy ky] (vector sy `(~c ~ky))) syms kys)))]
               (proxy [~l-type] []
                 ~@(mapv #(list %1 [e] `(~%2 ~e)) names syms)))] 
         out))

(defn leaf-sym [x]
  "Gets the leaf of x (symbol form), x can be a class, symbol, or string. 
   TODO: allow us to subclass java components but return the root-of-the-leaf type.
   TODO: check for repetative code wiht some of my code analysis functions."
  (let [full (if (= (type x) Class) (subs (str x) 6) (str x))]
    (symbol (last (.split full "\\.")))))

(defn abs-to-rel [path-abs path-ref]
    "Converts absolute paths to relative paths.
     To get from path-ref to path-abs first go :up and then :down (vectors), empty [] for no travel.
     :up means the absolute path is above the reference, :down means below."
  (let [c (min (count path-abs) (count path-ref))
        ; How much of the path is in common:
        ncomm (loop [ix 0] (if (and (< ix c) (= (nth path-abs ix) (nth path-ref ix))) (recur (inc ix)) ix))]
    {:up (subvec path-ref ncomm) :down (subvec path-abs ncomm)}))

(defn path-vector [root-state]
  "Makes a vector of {:state :path :obj} out of some root atom state.
   :obj = the java object."
  (let [objr (:java root-state)
        stater (:state root-state)
        listv (fn list-vec [path st ob]
                (into [] (apply concat [{:path path :state st :obj (:obj ob)}] 
                           (if (nil? (:Children st)) []  ; done.
                             (mapv #(list-vec (conj path :Children %1) %2 %3)
                               (collections/ckeys (:Children st)) (collections/cvals (:Children st)) 
                               (collections/cvals (:Children ob)))))))]
    (listv [:state] stater objr)))

; Note: if the type of the object also matters we can get it's type.
(defn event-clojureize [e obj]
  ; Makes a clojure-friendly map for our events and adds some information (i.e. new positions, etc).
  ; The object is not always the .getSource of the event, for i.e. JScrollPane it's the parent.
  (if e
    (let [tyl (type e)]
      (let [encdr (let [eec detail/event-encode x (get eec (type e))] ; Our event is encoded.
                    (if x x (get eec (first (filter #(instance? % e) (keys eec))))))] ; or it is a subclass.
        ;(if encdr (println "Encoded event found for type: " (type obj) (type e) "Value: " (encdr e obj)))
        (if encdr (encdr e obj) (clojurize/get-single-level e true false)))))) ; Use the encoder or use a default one.

(defn updating-for-event? [root-state path]
  "Are we in the midst of updating an event?
   updating is set true when we are about to call an update! and set false again when we are done with the event callback.
   It is set per object. It is used to prevent infinite loops where an event will cause a change which triggers another event, etc."
  (let [o (get-in root-state (concat [:java] path [:obj]))]
    (if o (.getClientProperty o "listener/listen-fn->updating-for-event")
      false)))

(defn _listen-fn [listener-key update-fns]
  "Creates the java-style listener function that calls any event(s) (with update-fns)
   and then atomistically updates the root-atom. The :java is atomistic but the java changes aren't atomistic."
    (fn [e] ; f is ran on the EDT, of course.
        (let [obj-source (if e (detail/get-source e)) ; the JPanel, JButton, etc.
              ; The main object:
              obj (detail/get-main-obj e obj-source)
              ; block client proprty change events. They risk making an infinite loop:
              client-block? (if (instance? java.beans.PropertyChangeEvent e)
                              (not (nil? (.getClientProperty obj-source (.getPropertyName e))))
                              false)]
          (if (not client-block?)
            (let [root-atom (if obj (.getClientProperty obj "root")); an atom holding the root.
                  root-state0 (if root-atom (history/atom-update @root-atom))
                  ty-kwd (if root-atom (history/java-update (.getClientProperty obj "type") root-atom) (.getClientProperty obj "type"))
                  process-event (:process-event update-fns)
                  update! (:update! update-fns)]
                  
                  ; process-event [root-state path ec listener-key root-atom]
                  ; Fire the event on ego, :below events on ancestors, :above events on :descendents, and all objects.
                  ;These, of course, only do anything if there is an appropiate function.
                  (if (and root-state0 (not (:setting-up? root-state0)) ; Check for setting up.
                       (or (not (:blocked? (ty-kwd detail/widgets))) (not ((:blocked? (ty-kwd detail/widgets)) obj e)))) ; check for blocked events.
                     (let [path (if root-atom (history/java-update (.getClientProperty obj "path") root-atom) (.getClientProperty obj "path"))
                           ec (assoc (event-clojureize e obj) :event-type listener-key); :event-type is not redundant if the user maps multible listeners to the same fn.
                           ec1 (assoc ec :destination-path path)
                           
                           sub-state (get-in root-state0 (concat [:state] path)); updated state after this change.
    
                           processing? (updating-for-event? root-state0 path)

                           ; ONLY the builtin events:
                           invoke-builtin (fn [root-state new-state] ; only self events are builtin, no other keys.
                                             (let [root-state1 (assoc root-state :state new-state)]
                                               (process-event root-state1 path ec1 listener-key processing? true false)))
                           ; The mid-root represents the true root-state (it SHOULD, at least, we can never be sure)
                           ;   before it is altered by any user events:
                           mid-root (assoc root-state0 :state 
                                      (invoke-builtin root-state0 (:state root-state0)))
                                      
                           ; Invokes ALL events with this (this also does the builtins all over again, but they are very simple callbacks):
                           invoke (fn [root-state new-state flavor destination-path] ; flavor is a keyword.
                                     ; Give relative and absolute path information:
                                     (let [root-state1 (assoc root-state :state new-state)
                                           p-rel (abs-to-rel path destination-path)
                                           state1 (if (= flavor :self) ; for self path = destination-path.
                                                         (let [st1 (process-event root-state1 path ec1 listener-key processing? true true)
                                                               lk1 (keyword (str "self-" (subs (str listener-key) 1)))
                                                               st2 (process-event (assoc root-state :state st1) path ec1 lk1 processing? true true)]
                                                           st2) (:state root-state1))
                                           ec11 (assoc ec :path destination-path :ancestor (:up p-rel) :descendent (:down p-rel) 
                                                 :origin-path path :origin-state sub-state)
                                           ]
                                       (process-event (assoc root-state :state state1) destination-path ec11
                                         (keyword (str (subs (str flavor) 1) "-" (subs (str listener-key ) 1))) processing? true true)))          
                           ; All callbacks run without changing the callbacks array:
                           callbacks (get-in root-state0 (concat [:event-tracker] path [:val]))
                           new-state (reduce (fn [st fl] (reduce #(invoke root-state0 %1 fl %2) st  ; non-existant keys will do nothing
                                                   (get-in callbacks [listener-key fl])))
                                             (:state root-state0) [:self :below :above])] ; atomistic update.
                           
                     ; update! if the user has made changes (we do NOT update for builtin changes):   
                     (if (not= (:state mid-root) new-state) 
                       ; the update is from the mid-root, NOT the root-state0:
                       (do (history/java-update (.putClientProperty obj "listener/listen-fn->updating-for-event" true) root-atom)
                           (let [new-root (update! mid-root new-state true update-fns root-atom 
                                  (str "builtin + usr " listener-key " path=" path))]
                             (history/atom-update (reset! root-atom new-root))
                             (history/java-update (.putClientProperty obj "listener/listen-fn->updating-for-event" false) root-atom)))
                       ; No user events, simply reset the atom:
                       (history/atom-update (reset! root-atom mid-root))))))))))

(defn listen-fn [listener-key update-fns]
  (errcatch/wrap (_listen-fn listener-key update-fns)))

; Makes multible listeners from a list. The listener is as a KEYWORD.
(defmacro _listener-makers [_]
  (let [llist-sym (mapv grammer/kwd2sym (keys listener-map))
        llist (mapv keyword (keys listener-map)) ; keywords are more useful for the user to use rather than symbols.
        update-fns (gensym 'update-fns)
        fcns (mapv (fn [l] `(fn [~update-fns] (listener ~l ~update-fns))) llist-sym)
        out (zipmap llist fcns)] out))

(def listener-makers (_listener-makers _))

(defn get-listeners [substate include-builtin? type-of-widget]
  "Event listeners from a given state as a hash-set of keywords. Does not get the children's listeners.
   include-builtin? = true to add built-in listeners.
   The :Type of the substate will only be used for adding the builtin listeners.
   It does not affect which user-defined listeners we get."
  ;(println "getting listeners: " substate include-builtin?)
  (let [ns (count substate)
        nl (count flavored-listener-fns)
        custom (apply hash-set ; For performance take the smaller loop:
                 (if (> (* nl 4) ns)
                   (filterv #(get flavored-listener-fns (keyword %)) (keys substate)) ; small hash-maps.
                   (filterv #(or (get substate %) (get substate (str %))
                                 (get substate (str (grammer/kwd2sym %)))
                                 (get substate (grammer/kwd2sym %))) flavored-listener-fns))) ; large hash-maps.
        builtin (if (and include-builtin? (keyword type-of-widget))
                      (keys (widget/upkeep-listener-fcns type-of-widget)) #{})]
    (set/union custom (apply hash-set builtin))))

(defn java-callback-update! [lfns-old lfns-new obj update-fns]
  "Updates which listeners are needed in the java object.
   lfns-old and lfns-new, which are collections of keywords,
   can be a superset of the actual listener functions.
   Nil java objects are ignored because they can mean that there was a java obejct there 
   and it's gone now."
  (if obj 
    (let [old-lfs (apply hash-set (mapv #(keyword (get inverse-listener-map %)) lfns-old))
          new-lfs (apply hash-set (mapv #(keyword (get inverse-listener-map %)) lfns-new))
          
          valids (apply hash-set (widget/supported-listeners (.getClientProperty obj "type")))
          added-ls (set/intersection valids (set/difference new-lfs old-lfs))
          removed-ls (set/intersection valids (set/difference old-lfs new-lfs))]
      (mapv #(widget/remove-listener! obj (get listener-makers %) %) removed-ls)
      (mapv #(widget/add-listener! obj % (get listener-makers %) update-fns) added-ls))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; Part 2:keeping track of the different listeners ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; How we keep track of events:
; Flavors:
;   :self (the default, no need to prefix with :self-) = events on our own component.
;   :below, :above = events coming from a direct descendent/a direct ancestor in the tree.
;  (order of execution = [:self, :below, :above], but all callbacks run without changing the calback array).
;  Within each group the order is undefined and if there is 
; Keeping track:
; :event-tracker example [:Children 0 :Children 1 :val :mousePressed :below] => #{[] [:Children 0] ...}
;         ; the :val holds all listeners. Is there really a need to group them with :below, etc? 
;           At least it makes it easier to understand.
(defn group-by-flavor [listener-kwds]
  "Groups listeners by thier flavor, and removes the flavor.
  [:above-mousePressed :above-keyPressed :keyPressed] => {:above #{:mousePressed keyPressed}, :self #{:keyPressed}} "
  (let [r (re-pattern "-")]
    (reduce #(let [pieces (string/split (str %2) r) n (count pieces)
                   fl? (> (count pieces) 1) kwd (if fl? (keyword (second pieces)) %2)
                   fl (if fl? (keyword (subs (first pieces) 1)) :self)
                   growing (let [x (get %1 fl)] (if x x (hash-set)))]
               (assoc %1 fl (conj growing kwd))) {} listener-kwds)))

(defn ancestor-paths [path]
  ; Does not include ourselves nor paths ending in :Children.
  (mapv #(subvec path 0 %) (range 0 (count path) 2)))

(defn _descendent-paths [state path] ; does include ourselves.
  (let [p1 #(conj path :Children %) kys (collections/ckeys (get-in state (conj path :Children)))]
    (into [] (conj (apply concat (mapv #(_descendent-paths state (p1 %)) kys)) path))))
(defn descendent-paths [state path]
  ; Does not include ourselves.
  (into [] (rest (_descendent-paths state path))))

(defn _apply-delta [event-tracker old-state new-state delta-path add?]
  ; Applies a single delta: Us listening to various objects.
  ; Nots not include the other object listening to us.
  ; Removals must be done before adding because they can share a path.
  (let [state (if add? new-state old-state); remove = out with the old, add = in with the new.
        delta-path (into [] delta-path)
        ; remove-DEV because the get-listeners doesn't use detail.
        our-ty (detail/remove-DEV (keyword (:Type (get-in state delta-path))))
        lfs (group-by-flavor (get-listeners (get-in state delta-path) true our-ty)) ; our listener functions.
        nil2set (fn [x] (if x x #{}))
        point (if add? ; point update for a single path and a single 
                (fn [et from-path to-path lfn flavor] ; events on call-path are listened by to us.
                  (let [p (conj from-path :val lfn flavor)]
                    (assoc-in et p (conj (nil2set (get-in et p)) to-path))))
                (fn [et from-path to-path lfn flavor] ; may empty the callback but not eliminate it.
                  (let [p (conj from-path lfn :val lfn flavor)]
                    (assoc-in et p (disj (get-in et p) to-path)))))
        ; Descendent and ancestor paths:
        ; TODO: can be inneficient since we always check for any path that could listen to us.
        descendents (descendent-paths state delta-path) 
        ancestors (ancestor-paths delta-path)
        
        ; Us listening to ourselves:
        changed-self (reduce #(point %1 delta-path delta-path %2 :self) event-tracker (:self lfs)) ; self-listeners.
        belows (:below lfs) aboves (:above lfs)
        
        ; us listening to stuff below:
        changed-below (reduce (fn [et cpath] (reduce #(point %1 cpath delta-path %2 :below) et belows)) changed-self descendents)
        ; us listening to stuff above:
        changed-above (reduce (fn [et cpath] (reduce #(point %1 cpath delta-path %2 :above) et aboves)) changed-below descendents)

        ; Stuff listening to us:
        down-to-us-listeners (mapv #(:below (group-by-flavor (get-listeners (get-in state %) true our-ty))) ancestors)
        up-to-us-listeners (mapv #(:above (group-by-flavor (get-listeners (get-in state %) true our-ty))) descendents)
        
        changed-to-us (reduce (fn [et ix] (reduce #(point %1 delta-path (nth ancestors ix) %2 :below) et (nth down-to-us-listeners ix))) changed-above (range (count ancestors)))
        changed-from-us (reduce (fn [et ix] (reduce #(point %1 delta-path (nth descendents ix) %2 :below) et (nth up-to-us-listeners ix))) changed-to-us (range (count descendents)))
        ]
    changed-from-us))

(defn did-events-change? [path old-state new-state]
  ; TODO: listener-fns may be a little long, for performance pick the lesser one?
  (let [old-sub (get-in old-state path)
        new-sub (get-in new-state path)]
    (not= (select-keys old-sub listener-fns) (select-keys new-sub listener-fns))))

(defn change-status [path old-state new-state]
  ; Returns whether we changed: [:any? :above? :below? :path].
  (let [old-sub (get-in old-state path) 
        new-sub (get-in new-state path)
        ; Only changes in the functions: 
        ks1 (set/intersection (apply hash-set (keys old-sub)) flavored-listener-fns)
        ks2 (set/intersection (apply hash-set (keys new-sub)) flavored-listener-fns)
        type? (not= (keyword (:Type old-sub)) (keyword (:Type new-sub)))
        any? (or type? (not= ks1 ks2)) ; any change, including type changes.
        above? (not= (filterv #(.contains (str %) "above-") ks1) (filterv #(.contains (str %) "above-") ks2))
        below? (not= (filterv #(.contains (str %) "below-") ks1) (filterv #(.contains (str %) "below-") ks2))]
    {:any? any? :above? above? :below :below? :type? type? :path path}))

(defn update-event-tracker [event-tracker old-state new-state deltas]
  "Gets the new event-tracker given the old event tracker and our delta changes.
   delta should be (updater/tree-delta old-state and new-state)."
  ;(println "deltas: " deltas)
  (let [; Performance: Only the changes that changed events, not the other stuff, count. There are uasually >> changes than listener changes.
        chstat (mapv #(change-status %1 old-state new-state) (:change deltas))    

        ; Remove the objects that got :remove'd (all removals must be done before adding because different paths cna map to the same skill).
        removed0 (reduce #(_apply-delta %1 old-state new-state %2 false) event-tracker (:remove deltas)) 
        removed (reduce #(collections/asoc-in %1 %2 nil) removed0 (:remove deltas)) ; more complete removal.       

        ; All :changes are equivalent to a remove followed by an add (but no need to to the more complete removal):
        cremoved (reduce #(_apply-delta %1 old-state new-state %2 false) removed (mapv :path (filterv :any? chstat)))

        ; Now the add part of the change:
        cadded (reduce #(_apply-delta %1 old-state new-state %2 true) cremoved (mapv :path (filterv :any? chstat)))

        ; Regular added:
        added (reduce #(_apply-delta %1 old-state new-state %2 true) cadded (:add deltas))
        ] added))

(defn update-anim-tracker [anim-tracker old-state new-state deltas]
  "Updates the animation tracker. The animation tracker is simply a hash-set of paths that have :Every-frame listeners."
  (let [anim-tracker (if (nil? anim-tracker) #{} anim-tracker)
        ch-removes (filterv #(and (not (:Every-frame (get-in old-state %))) (:Every-frame (get-in new-state %))) (:change deltas))
        ch-addings (filterv #(and (:Every-frame (get-in old-state %)) (not (:Every-frame (get-in new-state %)))) (:change deltas))
        addings (filterv #(:Every-frame (get-in new-state %)) (:add deltas))
        removed (reduce #(disj %1 %2) anim-tracker (concat (:remove deltas) ch-removes))
        added (reduce #(conj %1 %2) removed (concat addings ch-addings))] 
    added))

(defn _getfns [sub-track] (apply hash-set (keys (:val sub-track)))) ; hash-set of event functions.
(defn _update-java-listeners-callbacks! [java oldet newet path update-fns]
  (let [old (get-in oldet path)
        new (get-in newet path)
        jp (concat path [:obj])] 
    (if (not= (:val old) (:val new))
      (java-callback-update! (_getfns old) (_getfns new) (get-in java jp) update-fns))
    (if (not= (:Children old) (:Children new))
      (let [cha (widget/align-children old new)]
        (mapv #(_update-java-listeners-callbacks! java (first (get cha %)) (second (get cha %)) % update-fns) cha)))))
(defn _update-java-listers-tychange! [java et path update-fns] ; when the ty changes.
  (let [lfns (_getfns (get-in et path))
        jp (concat path [:obj])]
    (java-callback-update! #{} lfns (get-in java jp) update-fns)))
(defn update-java-listeners! [new-java old-event-tracker new-event-tracker old-state new-state deltas update-fns]
  "Two updates: how the callbacks changed and if the java objects changed type in place.
   No need to update the old-java objects, and no need to do anything if the new java objects are nil."
  (let [; The changes can include paths not in the deltas of state because of descendents/ancestors:
        et-changes (widget/tree-delta old-event-tracker new-event-tracker)]
    (mapv #(_update-java-listeners-callbacks! new-java old-event-tracker new-event-tracker 
            % update-fns) (concat (:add et-changes) (:change et-changes) (:remove et-changes))))
  ; The java type changes, which means all listeners are lost and have to be replaced:
  (mapv #(if (not= (keyword (:Type (get-in old-state %))) (keyword (:Type (get-in new-state %))))
           (_update-java-listers-tychange! new-java new-event-tracker % update-fns))
    (:change deltas)))