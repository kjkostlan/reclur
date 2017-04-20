; Applies high level java mutations to an atom, using uirelate as an engine.
; This is the gate-keeper to make sure the functions are ran ont he event dispatch thread.

; The atom:
  ; :clj -> the state that uirelate uses.
  ; :java -> the java objects themselves, 1:1 with :val in the :clj.

(ns clooj.guinew.jui
  (:import [javax.swing SwingUtilities])
  (:require [clooj.guinew.trinket :as trinket]
            [clooj.guinew.uirelate :as uirelate]
            [clooj.guinew.cutnpaste :as cutnpaste]
            [clojure.string :as string]
            [clojure.set :as set]
            [clooj.guinew.gfx :as gfx]
            [clooj.guinew.getset :as getset]
            [clooj.java.thread :as thread]))

(defn edt? [] (SwingUtilities/isEventDispatchThread))

(defn on-edt [f!!]
  "Runs a function (that takes no arguments but should have mutation) on the event dispatch thread.
   If we are already on the edt we just run it, otherwise we invoke-later."
  (if (edt?) (f!!) (SwingUtilities/invokeLater f!!)))

(defn on-edt-wait [f!!]
  "Waits for the event if we aren't on the edt."
  (if (edt?) (f!!) (SwingUtilities/invokeAndWait f!!)))

(defn assert-edt []
  "Asserts that we are on the event dispatch thread, throws an error otherwise."
  (if (not (edt?)) (throw (Exception. "A function that needed to be ran on the event dispatch thread wasn't."))))

(defn empty-gui-atom []
  (atom {:java {} :clj {}}))

(defn get-key [java-ob] 
  "Gets the key that is used to identify the given object."
  (assert-edt)
  (.getClientProperty java-ob "key"))

(defn get-gui-atom [java-ob]
  "Gets the atom that holds everything from the java-ob."
  (assert-edt)
  (.getClientProperty java-ob "guiAtom"))

(defn get-evt-type [java-e]
  "Java event -> :mousePressed. Parses the paramString (I can't seem to find a better way).
   This shouldn't need overriding."
  (assert-edt)
  (let [^String s (.paramString java-e)
        tokens (re-seq #"[A-Z_]+" s)
        tok (first (filter #(.contains % "_") tokens)) ; they tend to be at the beginning.
        tok-pieces (string/split tok #"_")]
    (keyword (apply str (.toLowerCase (first tok-pieces)) (mapv #(str (.toUpperCase (subs % 0 1)) (.toLowerCase (subs % 1))) (rest tok-pieces))))))

(defn new-ob-builtin-listen-fns [type]
  "Returns a map from the listener type (i.e. :mousePressed) to
   an (fn [e-clj this-ob-clj this-ob-java]) => new object's value.
   By default we just use a change listener and return the object."
  (let [brute-force (fn [e-clj this-ob-clj this-ob-java] 
                      (assoc (getset/java-to-clj this-ob-java trinket/java-to-clj-override trinket/simple-java-to-clj)
                        :Type (:Type this-ob-clj)))]
    (if-let [x (trinket/new-ob-builtin-listen-fns-override type)] x
      {:stateChanged brute-force})))

(defn update-ob! [java-ob old-clj-value new-clj-value]
  "Updates an object. Of course, it can't change the type of the object."
  ; Look for changed keys (we only need to update the changes):
 (on-edt (fn []
  (if (not= (:Type old-clj-value) :Empty)
    (let [; :Type can occasionally have a java meaning, use :Type-field for these cases.
          tf #(if (:Type-field %) (assoc % :Type (:Type-field %)) (dissoc % :Type))
          old-clj-value (tf old-clj-value) new-clj-value (tf new-clj-value)
          ks (set/union (apply hash-set (keys old-clj-value)) (apply hash-set (keys new-clj-value)))
          changes (filterv #(not= (get old-clj-value %) (get new-clj-value %)) ks) ; remove irrelevent ones.
          sets (getset/setters (.getClass java-ob))]
      ; Very similar code to clj-to-java, as we are going two levels down. We shouldn't
      ; need to go deeper and face loops, etc.
      (if-let [x (trinket/update-ob-override! java-ob old-clj-value new-clj-value changes sets)] x
        (mapv #(if (contains? sets %) ; some changes may not affect us (i.e. be user data).
                 (let [v (get new-clj-value %)
                       ^java.lang.reflect.Method m (get sets %)
                       ^java.lang.Class cl (nth (.getParameterTypes m) 0)
                       ^java.lang.Class cl ; box the class.
                         (cond (= cl Integer/TYPE) Integer (= cl Float/TYPE) Float (= cl Double/TYPE) Double
                           (= cl Long/TYPE) Long (= cl Character/TYPE) Character
                           (= cl Boolean/TYPE) Boolean (= cl Short/TYPE) Short :else cl)]
                   (.invoke m java-ob (object-array [(getset/clj-to-java cl v trinket/clj-to-java-override trinket/simple-clj-to-java)])))) changes))
    (if (not (trinket/update-graphics-override! java-ob old-clj-value new-clj-value)) 
      (gfx/update-graphics! java-ob old-clj-value new-clj-value))) java-ob))))

(defn default-to-string [java-ob]
  "Much cleaner to not dump so much stuff."
  (let [t (str (type java-ob))
        cl (last (butlast (string/split t #"\$")))
        cl-leaf (if cl (last (string/split cl #"\.")))
        ident (System/identityHashCode java-ob)]
    (str cl-leaf "@" "0x" (Integer/toHexString ident))))

(defmacro default-proxy [ty] ; ty is a SYMBOL, we return a FUNCTION.
  (let [this 'this] `(fn [] (proxy [~ty] [] ; the symbol 'this must be literal.
        (toString [] (default-to-string ~this))
        (paintComponent [g#] ; Can't really access the root-atom to store history here...
          (if (not (.getClientProperty ~this "blockPaint")) (do (proxy-super paintComponent g#) (gfx/defaultPaintComponent! g# ~this))))))))

(defn ob-from-kwd [kwd]
  (assert-edt)
  (let [ty-sym (symbol (str "javax.swing." (subs (str kwd) 1)))
        code (list `default-proxy ty-sym)]
    (try ((eval code))
      (catch Exception e (throw (Exception. (str "Unrecognized keyword type: " kwd)))))))

(defn new-ob [ob-key clj gui-atom]
  "Creates a new object with the user data stored in the object.
   The object's user data (all strings):
   type = a keyword for the clojure type, can save some code.
   guiAtom = the handle from which listeners mutate the state.
   key = what we use to identify constraints, etc, such as :my-scrollpane.
   listeners = map from listener classes added, such as MouseListener, to the actual java listener objects.
   listenerFns = map from stuff like :mousePressed to functions that take in the event and mutate the atom.
     when the last class is removed we remove the last listener."
  (assert-edt)
  (if (not (map? clj)) (throw (Exception. "New objects must be in map form.")))
  (if (not (keyword? (:Type clj))) (throw (Exception. ":Type must be a keyword for new objects.")))
  (if (not= (:Type clj) :Empty) ; Special type that does not change anything.
    (let [ob (if-let [o (trinket/constructor-override clj)] o (ob-from-kwd (:Type clj)))]
      ;(try (.setFocusable ob true) (catch Exception e)) ; focus doesn't seem to work well.
      (.putClientProperty ob "type" (:Type clj)) (.putClientProperty ob "key" ob-key) 
      (.putClientProperty ob "guiAtom" gui-atom) ; we don't open or modify the atom.
      (.putClientProperty ob "listeners" {})
      (.putClientProperty ob "listenerFns" {})
      (update-ob! ob {} clj) ob)))
    
(defn add-child! [parent-ob child-ob]
  "Simple adding children that can be overridden."
  (assert-edt)
  (if-let [x (trinket/add-child-override! parent-ob child-ob)] x (.add parent-ob child-ob)) parent-ob)

(defn remove-child! [parent-ob child-ob]
  "Simple removing children that can be overridden."
  (if-let [x (trinket/remove-child-override! parent-ob child-ob)] x (.remove parent-ob child-ob)) parent-ob)

(def all-listeners ; map from class to vectors of functions-as-keywords.
  (let [get-tokens (fn [s] (string/split s #"[() \n\t]+")) ; Work with strings for most of this fn.
        tokens (into [] (get-tokens cutnpaste/listener-list-raw))
        listener?s (mapv #(.contains ^String % "Listener") tokens)
        event?s (mapv #(.contains ^String % "Event") tokens)
        adapter?s (mapv #(.contains ^String % "Adapter") tokens)
        l-map (loop [acc {} ix 0 mode ""] ; Map from string to array of strings.
                (if (= ix (count tokens)) acc
                  (let [tok (nth tokens ix)
                        acc1 (cond (nth listener?s ix) (assoc acc tok #{}) 
                               (and (not (nth event?s ix)) (not (nth adapter?s ix))) 
                               (update acc mode #(conj % tok)) :else acc)
                        mode1 (if (nth listener?s ix) tok mode)]
                    (recur acc1 (inc ix) mode1))))
        awt-listeners (apply hash-set (get-tokens cutnpaste/java-awt-event-listener-classes))
        swing-listeners (apply hash-set (get-tokens cutnpaste/javax-swing-event-listener-classes))
        ; Convert to class and keyword form:
        listeners (dissoc 
                    (zipmap (mapv #(cond (get awt-listeners %) (eval (symbol (str "java.awt.event." %)))
                         (get swing-listeners %) (eval (symbol (str "javax.swing.event." %)))
                         :else :unrecognized) (keys l-map)) 
                      (mapv #(mapv keyword %) (vals l-map))) :unrecognized)]
    (trinket/clean-up-listener-map listeners)))

(def all-listeners-inverse ; inverse map of all-listeners: map from keywords to classes (all listener function names are unique).
  (let [classes (into [] (keys all-listeners))]
    (reduce (fn [acc cl] (reduce #(assoc %1 %2 cl) acc (get all-listeners cl))) {} classes)))

(defn gen-listener [cl fns]
 "Creates a listener using evals and proxy.
  Fns is a map from keywords to listener functions (these listener functions work with the java event)."
  (assert-edt)
  (let [all-kwds (get all-listeners cl) ; all functions as symbols.
        k-sym (mapv #(symbol (subs (str %) 1)) (keys fns)) ; symbol form converts :mousePressed -> mousePressed.
        k2s #(symbol (subs (str %) 1))
        body (apply list (map #(list (k2s %) ['event] (if (get fns %) (list (get fns %) 'event))) all-kwds))
        cl-sym (symbol (.getName cl))
        code (apply list 'proxy [cl-sym] [] body)]
  (eval code)))

(defn add-remove-listener! [java-ob listener-function-key on-evt!!-fcn]
  "Adds a listener iff on-evt!!-fcn is non-nil, otherwise removes it."
  (assert-edt)
  (let [listeners (.getClientProperty java-ob "listeners")
        ^Class cl (get all-listeners-inverse listener-function-key)
        _ (if (nil? cl) (throw (Exception. (str "unrecognized listener-function keyword: " listener-function-key))))
        cl-leaf (last (string/split (.getName cl) #"\."))
        ad (str "add" cl-leaf) rm (str "remove" cl-leaf)
        listenerFns (assoc (.getClientProperty java-ob "listenerFns") listener-function-key on-evt!!-fcn)
        new-l (if on-evt!!-fcn (gen-listener cl listenerFns))
        ^"[Ljava.lang.Class" clbox (into-array [cl])
        ^java.lang.reflect.Method metd-add (.getMethod (.getClass java-ob) ad clbox)
        ^java.lang.reflect.Method metd-rem (.getMethod (.getClass java-ob) rm clbox)]
    (if (or (not metd-add) (not metd-rem))
      (throw (Exception. "Can't find the reflect methods.")))
    (if-let [x (get listeners cl)] (.invoke metd-rem ^Object java-ob (object-array [x]))) ; remove the old listener.
    (if on-evt!!-fcn (.invoke metd-add ^Object java-ob (object-array [new-l])))
    (.putClientProperty java-ob "listenerFns" listenerFns)
    (.putClientProperty java-ob "listeners" 
      (if on-evt!!-fcn (assoc listeners cl new-l) (dissoc listeners cl)))))

(defn add-listener! [java-ob listener-function-key on-evt!!-fcn]
  (if (not (fn? on-evt!!-fcn)) (throw (Exception. "A fn must be specified to add.")))
  (add-remove-listener! java-ob listener-function-key on-evt!!-fcn))

(defn remove-listener! [java-ob listener-function-key]
  "listener-function-key is in keyword form such as :mousePressed."
  (add-remove-listener! java-ob listener-function-key nil))

(declare on-event!!) ; on-event!! apply-java-change! state-update! loop dependence.
(defn apply-java-change! [java change gui-atom]
  "Applies a java change, returns the new java map from keywords to objects.
   It also modifies the java objects themselves, thus the mutation.
   Does not look at or modify the atom, only stores a reference of it in the data."
  (assert-edt) ; we are both java-ing and returning so we can't run on the other thread.
  (if (not (map? java)) (throw (Exception. "The java storage variable is not a map.")))
  (let [ty (first change) c1 (second change) c2 (get change 2) c3 (get change 3)
        l-object? (not= c1 :Clock)] ; some listeners don't have an object.
    (cond (= ty :new) (assoc java c1 (new-ob c1 c2 gui-atom)) ; the only one that needs the atom.
      (= ty :mod) (do (update-ob! (get java c1) c2 c3) java)
      (= ty :del) (do (trinket/delete-ob-finalizations! (get java c1)) (dissoc java c1))
      ; Only the first listener added and last listener removed. on-event!! handles all the listeners.
      ; Note: the listeners come from the user as well as new-ob-builtin-listen-fns
      (= ty :new-first-lis) (do (if l-object? (add-listener! (get java c1) c2 on-event!!)) java)
      (= ty :del-last-lis) (do (if l-object? (remove-listener! (get java c1) c2)) java)
      (= ty :child) (do (add-child! (get java c1) (get java c2)) java)
      (= ty :unchild) (do (remove-child! (get java c1) (get java c2)) java))))

(defn get-cmds [gui-state listener clj-e]
  "Gets the commands by running the listener's function with the appropriate args. 
   Most of this fn would also be at home in uirelate."
  (let [lfn (first listener) larg-keys (rest listener)
        ; Symbols become java objects, keywords clojure objects:
        largs (mapv #(if (symbol? %) (get (:java gui-state) (keyword %)) (get (:val (:clj gui-state)) %)) larg-keys)]
    (into [] (apply lfn clj-e largs)))); the running of the listener function itself.]))

; Animation and frames:
(def ^{:dynamic true} *frame-time-ms* 30)
(defonce atoms-need-anim (atom #{})) ; atoms that need to be animated.
(declare state-update!) ; state-update! every-frame!! atoms-need-anim dependency loop.
(defn get-frame-listeners [gui-state] ; returns the listeners to each frame.
  (let [clock-l (get-in gui-state [:clj :listeners :Clock])
        kys (filterv #(= (first %) :everyFrame) (keys clock-l))]
    (mapv #(get clock-l %) kys)))
(defn needs-anim? [atm-state] 
  (let [x (get-frame-listeners atm-state)]
    (and x (> (count x) 0))))
(defn update-anim-atoms-list!! [atm check-gui-state] ; every time the state changes check to see if we need atoms.
  (swap! atoms-need-anim 
    #(if (= % :active-off) %
       (if (needs-anim? check-gui-state) (conj % atm) (disj % atm)))))

(defonce ef-err (atom false))
(defn every-frame!! []
  "This fcn runs every frame and applies the listener functions to every atom in need."
  (on-edt-wait (fn [] ; wait so we don't think we return fast while the edt gets clogged. 
   (try ; try-catch so that errors don't stop the anim
    (let [still-needs-anim
           (apply hash-set (filterv identity (mapv  ; mutates the atoms and returns list of atoms that still need the frame tracker.
             (fn [needy-atom]
               (let [gui-state @needy-atom
                     new-state (reduce ; call the event
                                 #(state-update! (:clj gui-state) (:java gui-state) 
                                    (get-cmds %1 %2 {:NanoTime (System/nanoTime)}) needy-atom) 
                                  gui-state (get-frame-listeners gui-state))]
                 (reset! needy-atom new-state)
                 (if (needs-anim? new-state) needy-atom))) 
             (let [x @atoms-need-anim] (if (= x :active-off) #{} x)))))]
       ; Clear everything if it isn't active.
       (swap! atoms-need-anim #(if (= % :active-off) (do (println "Crushed.") (hash-set)) still-needs-anim)))
      (catch Exception e 
        (do (if (not @ef-err) (println "Every frame error:" e)) (reset! ef-err e)))))))

(defn clear-all-anims!! []
  "Gets rid of those CPU eating functions."
  (on-edt (fn [] (println "Reset!!") (reset! atoms-need-anim :active-off)))) ; on the edt to maintain execution order, with :active-off to stop the frame listener.

; The framerate will drop for slower frame listeners but the pausing gap stays constant so it should degrade gracefully.
(thread/pulse!! every-frame!! (fn [_] *frame-time-ms*) (keyword (str "jui-main" "thread-pulse")) (atom true) false)

(defn state-update! [clj java cmds gui-atom]
  "Returns the updated :clj and :java. Also mutates the java objects passed to it.
   Does not use use or modify the atom, only stores a reference to it in java objects if need be."
  (assert-edt) ; we are both java-ing and returning stuff so we can't run on the other thread.
  (uirelate/assert-valid-commands cmds)
  (loop [clji clj javai java ix 0]
    (if (= ix (count cmds)) (do (update-anim-atoms-list!! gui-atom {:clj clji :java javai}) ; a rare !! mutation on a relatively disconnected var.
                              {:clj clji :java javai})
      (let [cmdi (nth cmds ix) changes (uirelate/get-changes clji cmdi new-ob-builtin-listen-fns)]
        (recur (update (uirelate/apply-changes clji changes) :command-hist (if (:store-history clj) #(conj % cmdi) identity))
          (reduce #(apply-java-change! %1 %2 gui-atom) javai changes) (inc ix))))))

(defn translate-event [java-e java-ob]
  "Translates the event into a clojure form.
   The :Type of the event returned is something like :MousePressed"
  (assert-edt)
  (let [evt-type (get-evt-type java-e)]
    (if-let [x (trinket/translate-event-override java-e java-ob evt-type)] x
      (let [clean-up #(dissoc % :Source :Component :Class)
            add-sc #(assoc % :ParamString (.paramString java-e))
            f0 #(assoc (clean-up (add-sc %)) :Type evt-type)] ; a default function.
        (f0 (getset/java-to-clj java-e trinket/java-to-clj-override trinket/simple-java-to-clj)))))) ; direct java -> clj translation.

(defn get-event-target [java-e]
  "What component did this event happen to?"
  (assert-edt)
  (if-let [x (trinket/get-event-target-override java-e)] x
    (.getSource java-e)))

(defn on-event!! [java-e]
  "Dispatches an event, finding the atom from the java's client properties
   and changing both the atom and the java. This function itself is passed into
   java's add-listener.
   We should already be on the event dispatch thread (since events are given this function).
   Doesn't return anything."
  (on-edt 
    (fn []
      (let [java-ob (get-event-target java-e)
            clj-e (translate-event java-e java-ob)
            gui-atom (get-gui-atom java-ob)
            gui-state @gui-atom
            ob-key (get-key java-ob)
            l-on-object (get-in gui-state [:clj :listeners ob-key])
            ; Only listeners of the right type (i.e. :keyPressed).
            e-type (:Type clj-e)
            get-listeners-that-match (fn [x] (mapv #(get x %) (filterv #(= (first %) e-type) (keys x))))
            listeners (get-listeners-that-match l-on-object)]
        ; Sequentially update the state.
        (loop [state (:clj gui-state) java (:java gui-state) ix 0]
          (if (= ix (count listeners))
            (reset! gui-atom {:clj state :java java}) ; store back into the atom.
            (let [cmds (get-cmds gui-state (nth listeners ix) clj-e)
                  x (state-update! state java cmds gui-atom)]
              (recur (:clj x) (:java x) (inc ix)))))))) nil)

(defn setup [cmds]
  "Setups a list of commands. Returns the atom. This is the main function that you use."
  (if (not (sequential? cmds)) (throw (Exception. "Cmds must be a vector.")))
  (reset! ef-err false)
  (let [atom-0 (atom {})]
    (on-edt
      (fn [] (reset! atom-0 (state-update! uirelate/empty-state {} cmds atom-0))))
    atom-0))