; User interaction defined with keywords and relations with history.
; The app is defined by a state and a list of commands:
; TODO: implement tree-based timelines (hash-map from a path to a command, and some values).
  ; An example path is [[0 123] [1 345] etc], where 1234 is the number of steps and 0 3 2 are the directions at each intersection.
  ; Undoing will create a new branch.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; The state ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A map of components, of which there are three types:
; :val = clojure representations of java objects, a map from the object's :name to the object itself.
; :command-hist stores the history of the object, if :store-history is true.
  ; :command-hist can be used to calculate the state history.
; The listener commands are stored in this map, but they have thier own map:
    ; each element is a maps from the object to maps of [<type> <listener id>] to clj listeners.
; :children = map from a key to it's children (a hash-set of keys).
; :parent = map from each object to it's parent.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Types of commands ;;;;;;;;;;;;;;;;;;;;;;;;;
; Object commands (an :Empty type can be used to define part of the state that is not a java object).
; Nil for the third arg means delete the object. 
; For changing the object, only the keys supplied will be modified (delete + new to reset the state).
; If the second arg (the name) already refers to an object it gets changed, otherwise a new object is created.
; [:object :mysplitpane {:Type :JSplitPane ...}]

; Relation commands:
; [:relation :splitpane :editbox-scroll], means :editbox-scroll is a child of :splitpane.
; Do not depend on order for conflicting relations.
; The order of relations determine the order of children.

; Unrelation commands:
; [:unrelation :x :y] means :x is no longer a parent of y.
; If no y is specified it means :x loses all children.

; Listener commands:
; [:listener :tree :treeSelected :my-id [(fn [e tree editbox editbox-scroll] ...) :tree :editbox :editbox-scroll]]
; The first two arguments determine what to listen to and what listener to use.
; The third is an extra id that allows multible listners.
; The internal [] contains the function optionally followed by any widgets or other queries that we need.
; Changing a listener completly deletes the old listener.
; Use keywords to access the clj representations of java objects, use symbols to access the java objects themselves.
; e is a clojurized event, and the other keywords tell us what to take (they can be objects or commands).
; The function returns a vector of commands, which can even include listener commands, etc
; A nil or missing 5th element means we don't have the last listener.
; We also can listen to the clock:
; [:listener :Clock :everyFrame :xyz [(fn [clock-e foo bar] foo bar)]]

; Note: calling replace and delete will remove all listeners with this object-listener combination.
; Note: these functions are uasually standard clojure functions, thus maintaining the functionalness of things.

; The history stores each state (node) along as the functions/args that transform between.
; All intermediate states are stored in the history, making undo easier.
  ; Our copy-on-modify state saves a lot of memory here, will still fill up with a huge number of commands.
  ; Non-linear undo can be implemented by running some of the commands forward, though it won't always make sense.
(ns clooj.guinew.uirelate
)

(def empty-state {:val {} :command-hist [] :listeners {} :children {} :parent {} :store-history false})

(defn convert-listener-fns [lfns ob-keyword]
  "Converts a function that takes the clj event, clj object, and java object
   and returns the modified clj object to the one that returns an :object command."
  (zipmap (keys lfns) (mapv (fn [f] #(vector [:object ob-keyword (f %1 %2 %3)])) (vals lfns))))

(defn _get-changes [changes state cmd new-ob-builtin-listen-fns-fn]
  (if (not (sequential? cmd)) (throw (Exception. "Cmd must be a vector/seq/etc")))
  (if (< (count cmd) 2) (throw (Exception. "Cmd must have at least 2 elements.")))
  (let [c0 (first cmd) c1 (second cmd) c2 (get cmd 2) c3 (get cmd 3)
        cr (rest (butlast cmd)) val (:val state)]
    ; Store the :java-old and :java-new
    (cond (= c0 :object)
          (let [ob-old? (boolean (get val c1)) ob-new? (boolean c2) 
                ty-change? (and (:Type c2) (not= (get-in val [c1 :Type]) (:Type c2)))
                parent (get (:parent state) c1) children (if-let [x (get (:children state) c1)] x #{})]
            (cond (and ob-old? ob-new? (not ty-change?)) ; gentle changes = easy.
                  (conj changes [:mod c1 (get val c1) (reduce #(assoc %1 %2 (get c2 %2)) (get val c1) (keys c2))])
                  (and ob-new? (not ob-old?)) ; New objects add listeners.
                  (reduce conj (conj changes [:new c1 c2]) 
                    (let [lfns (if (:Type c2) (convert-listener-fns (new-ob-builtin-listen-fns-fn (:Type c2)) c1) {})
                          added-changes (mapv #(vector :new-first-lis c1 %1 :*builtin* [%2 c1 (symbol (subs (str c1) 1))]) (keys lfns) (vals lfns))] 
                      added-changes))
                  (and (not ob-old?) (not ob-new?)) changes ; No real change.
                  :else ; delete the old object (and any relations) and create a new object if applicable.
                  (let [all-rel (mapv #(vector c1 %2) children)
                        all-rel (if parent (conj all-rel [parent c1]) all-rel)
                        changes1 (conj (reduce #(conj %1 [:unchild (first %2) (second %2)]) changes all-rel) [:del c1])
                        changes2 (if (not ob-new?) changes1 ; replace if applicable.
                                   (conj (reduce #(conj %1 [:child (first %2) (second %2)]) changes1 all-rel) [:new c1 (get cmd 2)]))]
                    changes2)))
          (= c0 :relation) ; adding children.
          (if (or (nil? c1) (nil? c2)) (throw (Exception. "Nill parent and/or child relation."))
            (let [p-old (get (:parent state) c2)]
              (cond (not p-old) (conj changes [:child c1 c2])
                (not= p-old c1) (conj changes [:unchild p-old c2] [:child c1 c2])
                    :else changes)))
          (= c0 :unrelation) ; removing children.
          (if (nil? c1) (throw (Exception. "nil parent to remove child(ren) from."))
            (if (not (nil? c2)) ; non-nil means to not be recursive.
              (let [p-old (get (:parent state) c2)]
                (if (or (not p-old) (not= p-old c1)) changes (conj changes [:unchild c1 c2])))
              (reduce #(_get-changes %1 state [:unrelation c1 %2] new-ob-builtin-listen-fns-fn) changes (if-let [x (get (:children state) c1)] x #{})))) ; a small recursive part.
          (= c0 :listener)
          (cond (nil? c1) (throw (Exception. "Null listener type."))
            (nil? c2) (throw (Exception. "Null listener target."))
            :else
            (let [c1-listeners (when-let [x (get-in state [:listeners [c1 c2]])] x {})
                  old-l (get c1-listeners c3) new-l (get cmd 4) n (count c1-listeners)]
              (cond (and (not old-l) new-l (= n 0)) (conj changes [:new-first-lis c1 c2 c3 new-l])
                    (and (not old-l) new-l) (conj changes [:new-lis c1 c2 c3 new-l])
                    (and old-l new-l) (conj changes [:mod-lis c1 c2 c3 new-l])
                    (and old-l (not new-l) (> n 1)) (conj changes [:del-lis c1 c2 c3])
                    (and old-l (not new-l)) (conj changes [:del-last-lis c1 c2 c3]))))
          :else (throw (Exception. (str "Unrecognized type: " c0))))))

(defn get-changes [state cmd new-ob-builtin-listen-fns-fn]
  "Compiles the cmd into a more usesable form, making a more explicit code.
   For example, it will differentiate between new vs deleted and add :unchild changes for deleted objects.
   Each cmd is a vector with the first element bieng the type of change.
   Types of changes:
     [:new <ob key> <ob val>] [:mod <ob key> <old ob val> <new ob val>] [:del <ob key>]
     [:new-first-lis <lis ob key> <lis type> <lis id> <lis val>]
     [:new-lis <target ob key> <lis type> <lis id> <lis val>] 
     [:mod-lis <target ob key> <lis type> <lis id> <lis val>]
     [:del-lis <target ob key> <lis type> <lis id>] 
     [:del-last-lis <lis ob key> <lis type> <lis id>]
     [:child <parent key> <child key>] [:unchild <parent key> <child key>]."
  (filterv identity (_get-changes [] state cmd new-ob-builtin-listen-fns-fn))) ; drop nil ones.

(defn apply-change [state change]
  "Applies a single change, updating the maps within the state 
   These changes have been compiled by get-changes, :del will always be accompanied by :del-lis if there are listeners, etc."
  (let [ty (first change) c1 (second change) c2 (get change 2) c3 (get change 3) c4 (get change 4)]
    (cond (= ty :new) (update state :val #(assoc % c1 c2))
      (= ty :mod) (update state :val #(assoc % c1 c3))
      (= ty :del) (update state :val #(dissoc % c1))
      (or (= ty :new-first-lis) (= ty :new-lis) (= ty :mod-lis) (= ty :del-lis) (= ty :del-last-lis)) 
      (let [listeners (if-let [x (get-in state [:listeners c1])] x {})]
        (assoc-in state [:listeners c1]
          (cond (or (= ty :new-first-lis) (= ty :new-lis)) (assoc listeners [c2 c3] c4)
            (or (= ty :del-lis) (= ty :del-last-lis)) (dissoc listeners [c2 c3])
              :else (throw (Exception. "Coding oops.")))))
      (= ty :child)
      (assoc-in (update-in state [:children c1] #(conj (if-let [x %] x (hash-set)) c2))
        [:parent c2] c1)
      (= ty :unchild)
      (update (update-in state [:children c1] #(disj % c2)) :parent
        #(dissoc % c2))
      :else (throw (Exception. "Buggy oops error.")))))

(defn apply-changes [state changes]
  "Applies the changes to the clj state. Should be used in parrallel with java changes.
   Does not store anything to the command history."
 (reduce apply-change state changes))

(defn _assert-valid-command [cmd]
  (if (nil? cmd) (throw (Exception. "Command is nil.")))
  (if (not (sequential? cmd)) (throw (Exception. "Command isn't a vector."))) ; slightly tigher error msg.
  (let [cmd (into [] cmd) ty (first cmd) c1 (get cmd 1) c2 (get cmd 2)
        th #(throw (Exception. (str %)))]
    (if (< (count cmd) 2) (th "Command is too short a vector."))
    (if (not (keyword? ty)) (th "First element of command isn't a keyword."))
    (cond (= ty :object) 
      (cond (not (keyword? c1)) (th "Second element of command must be a keyword (the object's key).")
        (> (count cmd) 3) (th "Too many elements in :object command"))
      (= ty :relation)
      (cond (not= (count cmd) 3) (th ":relation commands must have three elements.")
        (not (keyword? c1)) (th "Second element of :relation command must be a keyword (the parents key).")
        (not (keyword? c2)) (th "Third element of :relation command must be a keyword (the child' key)."))
      (= ty :unrelation)
      (cond (> (count cmd) 3) (th "Too many elements in :unrelation command")
        (not (keyword? c1)) (th "Second element of command must be a keyword (the parents key).")
        (and c2 (not (keyword? c2))) (th "Third element of :unrelation command must non-existant (for removing all children), nil (for removing all children) of be a keyword (the child' key for removing one child)."))
      (= ty :listener)
      (cond (not= (count cmd) 5) (th ":listener commands must have 5 elements.")
        (not (sequential? (get cmd 4))) (th "The 5th element of a :listener cmd must be a vector.")
        (not (fn? (first (get cmd 4)))) (th "The 5th element of a :listener cmd must begin with the listener function."))
      :else (th (str "Unrecognized command type: " ty)))))
(defn assert-valid-commands [cmds]
  "Some error checking to look for bad commands."
 (mapv _assert-valid-command cmds))

(defn add-commands [state cmds new-ob-builtin-listen-fns-fn]
  "Adds commands to a given state."
  (let [changed-state (reduce (fn [s cmd] (apply-changes s (get-changes s cmd new-ob-builtin-listen-fns-fn))) state cmds)]
    (if (:store-history state) 
      (assoc changed-state :command-hist (reduce conj (:command-hist state) cmds)) changed-state)))
