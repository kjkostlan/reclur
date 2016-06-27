(ns clooj.java.widget
  (:require [clooj.java.gfx :as gfx]
            [clojure.string :as string] [clooj.java.rawdoc :as rawdoc]
            [clooj.java.clojurize :as clojurize]
            [clooj.java.detail :as detail]
            [clojure.set :as set] [clooj.collections :as collections]
            [clooj.coder.grammer :as grammer])
  (:import [java.awt Dimension] [java.awt Point]
           [jcode JFrameClient] [java.lang.reflect Modifier]
           [javax.swing SwingUtilities]))

; non-listener code for handling creating, modifying and monitoring the widgets.

;;;;;;;;;;;;;;;;;;;;;; Some simple collection functions that are somewhat specific ;;;;;;;;;;;;;;;;;;;;;; 

(defn align-children [old-substate new-substate]
  "Creates a map from x to [old child, new child], where x is the union of the grammer/ckeys of the two states.
   Non-existant children are set to nil."
  (let [get-k #(apply hash-set (grammer/ckeys (:Children %)))
        get-c #(get-in %1 [:Children %2])
        ch-keys (set/union (get-k old-substate) (get-k new-substate))]
    (zipmap ch-keys (mapv #(vector (get-c old-substate %) (get-c new-substate %)) ch-keys))))

(defn _tree-delta [acc old new path]
  (let [path (into [] path)
        old-i (get-in old path)
        new-i (get-in new path)
        ; recursive on a 1:1 relationship with nils filling in the gaps:
        acc1 (if (not= (:Children old-i) (:Children new-i))
               (let [chpairs (align-children old-i new-i)]
                 (reduce #(_tree-delta %1 old new (conj path :Children %2))
                   acc (sort (keys chpairs)))) acc)]
    ;(println "new value: " new-i)
    ;(println "types: " (type acc1) (type old-i) (type new-i) (type acc1) (type (:change acc1)))
    (cond (and (nil? old-i) (not (nil? new-i))) (update acc1 :add conj path)
          (and (nil? new-i) (not (nil? old-i))) (update acc1 :remove conj path)
          (and (not (nil? old-i)) (not (nil? new-i))
            (not= (dissoc old-i :Children) (dissoc new-i :Children))) (update acc1 :change conj path)
          :else acc1)))
(defn tree-delta [oldstate newstate]
  "Gets the changes between the old and new.
   :add => paths for which (get-in new-substate) but not (get-in old-substate).
   :remove => the opposite of add.
   :change => changed paths. Does not include changes to the children.
   This function leverages clojures under-the-hood copy on modify so that = on small changes to
     even massive datastructures are fast as long as there aren't a very large number of nesting levels.
   At any given level the updates are sorted in the order of the keys, so children will be added in the right order, etc.
   Should not be given substate (because the :path) would have to be added."
  (_tree-delta {:add [] :remove [] :change []} oldstate newstate []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def debug (atom nil))
(def widget-classes
  "Vector of java.lang.Class"
  (let [widget-syms (into [] (apply hash-set 
             (mapv (fn [s] (symbol (re-find #"[a-zA-Z0-9]+" s)))
               (string/split rawdoc/javax-swing-widgets #"\n"))))
        classes (mapv #(eval (symbol (str "javax.swing." %))) widget-syms)]
   ; Remove abstract and final classes:
   (filterv (fn [cl] (let [m (.getModifiers cl)]
              (and (not (Modifier/isAbstract m)) (not (Modifier/isFinal m))))) classes)))
(def widget-symbols
  (mapv #(grammer/class2sym % true) widget-classes))

(def mapsym2widget-cl
  (merge 
    (zipmap widget-symbols widget-classes)
    (zipmap (mapv #(symbol (str "javax.swing." %)) widget-classes) widget-classes)))
(def mapkwd2widget-cl
  (merge 
    (zipmap (mapv keyword widget-symbols) widget-classes)
    (zipmap (mapv #(keyword (str "javax.swing." %)) widget-classes) widget-classes)))
(def mapwidget-cl2kwd (zipmap widget-classes (mapv keyword widget-symbols)))
(def mapstr2widget-cl
  (merge
    (zipmap (mapv str widget-symbols) widget-classes)
    (zipmap (mapv str widget-classes) widget-classes)
    (zipmap (mapv #(str "javax.swing." %) widget-symbols) widget-classes)))

(defn x2widget-cl [x]
  "Converts x, keyword class, etc, to the widget class. Always a leaf class."
  (let [out (cond (class? x) x
              (symbol? x) (get mapsym2widget-cl x)
              (string? x) (get mapstr2widget-cl x)
              (keyword? x) (x mapkwd2widget-cl)
              :else nil)]
    (if (nil? out) (throw (Exception. (str "Unrecognized widget identity: " x " type: " (type x)))))
    out))

; Import all the widgets with this macro followed by the (do):
(defmacro import-widgets [_]
  `(import ~@(mapv #(list `quote (vector `javax.swing %)) widget-symbols)))
(do (import-widgets _))

(defn extract-from [sub-statef component]
  "Applys componentf to the state of component and returns the result.
   This does not modify the component but accesses the mutable atom.
   Returns the result, not a function (this lets you use it in reify/proxies/etc)."
  (if (nil? sub-statef) nil
     (let [state (get-in @(.getClientProperty component "root") 
                         (.getClientProperty component "path"))]
            (sub-statef state))))

(defn recognized-fields [ty]
  "Which fields are recognized as settings on the given object.
   Returns a vector of keywords."
  (let [ty (x2widget-cl ty) gets (clojurize/getters ty) sets (clojurize/setters ty)
        bothn (set/intersection (apply hash-set (keys gets)) (apply hash-set (keys sets)))]
    (into [] bothn)))

(defn defaultPaintComponent! [g this-obj]
  "Allows us to use our gfx/paint! function on paint-component.
   does NOT include the proxy-super, so you need to:
   (proxy-super paintComponent g) (defaultPaintComponent g this)"
  (let [root-atom (.getClientProperty this-obj "root")
        path (.getClientProperty this-obj "path")
        cmds (get-in @root-atom (concat [:state] path [:Graphics]))]
    (if cmds (gfx/paint! g cmds))))
        
; TODO: exactly what objects need a getPreferredSize and what don't?
(defn toDim [v] (if (vector? v) (Dimension. (first v) (second v)) v)) ; TODO: fit toDim into clojurize (or remove entirely).
(defn defaultGetPreferredSize [this-obj]
  (let [siz (toDim 
              (extract-from 
                    (fn [s] (let [sz nil] ; TODO: actually override the preferred size?
                              (cond (and (vector? sz) (> (count sz) 1)) ; nil for no size.
                                    sz   (fn? sz) (sz s)))) ; vector or fn alloed.
              this-obj))] siz))

(defn to-string [obj]
  "A simple toString method with minimal fluff: just the leaf type and the address."
  (let [t (str (type obj))
        cl (last (butlast (string/split t #"\$")))
        cl-leaf (if cl (last (string/split cl #"\.")))
        ident (System/identityHashCode obj)]
    (str cl-leaf "@" "0x" (Integer/toHexString ident))))

; Use macros to build proxy functions (functions you call to get an object with the correct proxies).
; The paintComponent, etc in proxies are expanded into clooj.java.widget/paintComponent, etc but still work.
; TODO: we don't track these functions in debug mode.
(defmacro default-proxy [ty]
  (let [this 'this] `(fn [] (proxy [~ty] [] ; the symbol 'this must be literal.
        (toString [] (to-string ~this))
        (paintComponent [g#] 
          (if (not (.getClientProperty ~this "blockPaint")) (do (proxy-super paintComponent g#) (defaultPaintComponent! g# ~this))))))))

(defmacro sized-proxy [ty]
  (let [this 'this] `(fn [] (proxy [~ty] []
        (toString [] (to-string ~this))
        (paintComponent [g#] 
          (if (not (.getClientProperty ~this "blockPaint")) (do (proxy-super paintComponent g#) (defaultPaintComponent! g# ~this))))
        (getPreferredSize [] (let [siz# (defaultGetPreferredSize ~this)]
                               (if siz# siz# (proxy-super getPreferredSize))))))))

; map from leaf keyword to the creator function. 
; Each creator function takes no arguments and returns a proxy of the desieried type.
(defmacro _make-ob-fns []
  (let [ws mapkwd2widget-cl
        mk-fn (fn [ty-sym ty-kwd] ; MUST be a keyword.
                (if (not (keyword? ty-kwd)) (throw (Exception. "Compile-time error not passed a keyword."))
                  (let [details (ty-kwd detail/widgets)
                        proxy-class (:use-user-class details)
                        sized? (:use-sized-proxy? details)]
                    (if proxy-class `(fn [] (~proxy-class))
                      (if sized? `(sized-proxy ~ty-sym)
                                 `(default-proxy ~ty-sym))))))]
    (zipmap (keys ws) (mapv #(mk-fn (grammer/class2sym %2) %1 ) (keys ws) (vals ws)))))
(def make-ob-fns (_make-ob-fns))

(defn refresh! [obj]
  "A simple refresh option."
  (clojurize/on-java-change (.revalidate obj) obj)
  (clojurize/on-java-change (.repaint obj) obj))

; TODO: _reflect-call! is a little repetative with clojurize functions.
(defn _reflect-call! [method-name changes-obj? obj & args] ; calls reflection.
  (let [methods (.getMethods (.getClass obj)) ; About 300 methods, should that matter for performance?
        target-methods (filterv #(and (= (count (.getParameterTypes %)) 1)  (= (.getName %) method-name)) methods)]
    (if (= (count target-methods) 1)
      (if changes-obj?
        (clojurize/reflection-on-java-change! (first target-methods) obj (into-array Object args))
        (.invoke (first target-methods) obj (object-array args))) ; no need to check to see if we need to track the changes.
      (throw (Exception. (str "There should be exactly one method reflection found, but there are: " (count target-methods) " for " method-name))))))


;;;;;;;;;;;;;;;;;;;;;;;;More public code used by other modules;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Map from object class as kwd to a hash-set of listeners (as keywords). 
; TODO: we are controlling what goes where at the type level but it would be better to control at the individual object level.
(def ego-listeners
  "What listeners can be added directly to this type (keyword form) of object."
  (let [llist (fn [_methods] (filterv (fn [m] (re-find #"add[a-zA-Z]+Listener" m))
                              (mapv #(.getName %) _methods)))
        llistc (fn [sym] (apply hash-set (llist (.getMethods (eval sym)))))
        lout (zipmap (mapv keyword widget-symbols) (mapv llistc widget-symbols))
        out (grammer/cmap :vals (fn [v] (grammer/cmap :vals #(keyword (subs % 3)) v)) lout)]
    ; Didn't quite make the raw doc:
    (assoc out :Document #{:DocumentListener})))

(defn supported-listeners [ty-kwd]
  (let [egos (ty-kwd ego-listeners)
        detai (ty-kwd detail/widgets)]
    (set/union egos (apply hash-set (keys (:childs-with-listeners detai))))))

(defn _add-remove-listener! [obj lclass-as-kwd lmaker update-fns add?]
  ; Looks for the corresponding object:
  (if (nil? lclass-as-kwd) (throw (Exception. (str "nil listener type"))))
  (if (nil? lmaker) (throw (Exception. (str "Unrecognized listener type:" lclass-as-kwd))) 
  (let [ty-kwd (keyword (.getClientProperty obj "type"))
        ch-lookup (get-in detail/widgets [ty-kwd :childs-with-listeners lclass-as-kwd])
        ; Add the listener to these objects:
        objs (if ch-lookup (mapv #(% obj) ch-lookup) [obj])
        java-fn-name (if add? (str "add" (subs (str lclass-as-kwd) 1)) 
                              (str "remove" (subs (str lclass-as-kwd) 1)))
        get-fn-name (if (not add?) (str "get" (subs (str lclass-as-kwd) 1)))
        ladd (if add? (lmaker update-fns)) ; the listener we must add.
        ; Note: only set the tracker and change flags when we are on the main object.
        lset! (if add? 
                (fn [obs] 
                  (mapv #(_reflect-call! java-fn-name (not ch-lookup) % ladd) obs))
                (fn [obs] 
                  (let [rmo (fn [o] ; single object removal.
                              (let [lstnrs (_reflect-call! get-fn-name false o)] ; should only be one listener.
                                (mapv #(_reflect-call! java-fn-name (not ch-lookup) o %) lstnrs)))]
                    (mapv rmo obs))))]
    (lset! objs))))

(defn add-listener! [obj lclass-as-kwd lmaker update-fns] 
    (_add-remove-listener! obj lclass-as-kwd lmaker update-fns true))

(defn remove-listener! [obj lclass-as-kwd lmaker]
  (_add-remove-listener! obj lclass-as-kwd lmaker nil false)) ; no update-fns needed for removal.


(defn type-to-vital-defaults [ty-kwd]
  "What defaults are nessessary for creating reasonable looking components."
  (let [vitals (get-in detail/widgets [ty-kwd :vital-fields])
  
        ; Resonable for any component that needs these parameters:
        all-defaults {:Text (str "A " ty-kwd) :View [0 0] :Selected false
                      :Location [100 100] :PreferredSize [500 500] :Visible true}
        defaults (select-keys all-defaults vitals)] defaults))
(def type-to-vital-defaults-memo (memoize type-to-vital-defaults)) ; probably not too helpful.

(defn add-defaults [sub-state]
  "Fills in any missing values with reasonable defaults."
  ;memoize because there is a performance bottleneck. The set of allowed types is quite small.
  (let [vital-defaults (type-to-vital-defaults-memo (keyword (:Type sub-state)))]
    
    (collections/fillin-defaults sub-state vital-defaults)))

(defn upkeep-listener-fcns [ty]
  "A map of listener keywords => fns that keep track of position, etc (BEFORE any custom listeners)."
  (let [ty-kwd (keyword ty) detai (ty-kwd detail/widgets)]
    (if (:upkeep detai) (:upkeep detai) {})))

(defn update-component! [obj old-substate new-substate update-visible-flag?]
  "Updates a java object to fit the new-substate. Non-relevent keys are ignored, of course.
   Does NOT update the children, and can't change the tyope of the object."
  (if (not= (keyword (:Type old-substate)) (keyword (:Type new-substate)))
      (throw (Exception. "Updating the component can't change the type of the component."))
      (let [dvs #(if update-visible-flag? % (dissoc % :Visible)) ; disable :Visible if we don't need it yet.
            old1 (dvs old-substate) new1 (dvs new-substate)]
        (clojurize/set-single-level! obj old1 new1) ; default settings.
        (let [exup! (:extra-update! ((keyword (:Type new-substate)) detail/widgets))] ; extra updating step.
          (if exup! (exup! obj old1 new1)))
        (if (or (:Repaint? new1) (not= (:Graphics old1) (:Graphics new1)))  ; (not= old1 new1)
          (refresh! obj)))))

(defn make-component [state root-atom path]
  "Creates the java object. The atom is not read or written to,
   it is just .putClientProperty'ed into the widget."
  (let [sub-state (get-in state path)
        ty-kwd (keyword (:Type sub-state))
        fno (get make-ob-fns ty-kwd)
        construct (get-in detail/widgets [ty-kwd :construct])
        xtra-make! (get-in detail/widgets [ty-kwd :extra-make!])
        
        _ (if (and (not construct) (not fno)) (throw (Exception. (str "Unrecognized type: " (:Type sub-state) " (path= " path ")"))))
        
        obj (if construct (construct state root-atom path to-string) (fno))
        ]
    (clojurize/on-java-change (.putClientProperty obj "root" root-atom) root-atom)
    (clojurize/on-java-change (.putClientProperty obj "path" path) root-atom)
    (clojurize/on-java-change (.putClientProperty obj "type" ty-kwd) root-atom)
    (clojurize/on-java-change (.setFocusable obj true) root-atom)
    (if xtra-make! (xtra-make! obj sub-state root-atom path))
    (update-component! obj {:Type (:Type sub-state)} sub-state false) obj))


(defn add-child! [parent-obj child-obj state path-to-parent]
  "Adds the child-obj to the parent-obj, both are java objects."
  (let [ppath path-to-parent
        ty (.getClientProperty parent-obj "type")
        num-children (count (:Children (get-in state ppath)))
        detai (ty detail/widgets)]
    ; some objects are limited in allowed # of children. JPanels can act as packages.
    (if (and (:max-children detai) (> num-children (:max-children detai)))
      (throw (Exception. (str ty " can only have " (:max-children detai) " children (not including it's own builtin children)."))))
    (if (:add-child! detai)
        ((:add-child! detai) parent-obj child-obj state path-to-parent)
        (clojurize/on-java-change (.add parent-obj child-obj) parent-obj)))) ; default add child.

(defn remove-child! [parent-obj child-obj]
  "Adds the child-obj to the parent-obj, both are java objects."
  (let [ty (.getClientProperty parent-obj "type") detai (ty detail/widgets)]
    (if (:remove-child! detai)
      ((:remove-child! detai) parent-obj child-obj)
      (clojurize/on-java-change (.remove parent-obj child-obj) parent-obj))))

(defn daycare! [obj old-substate new-substate]
  (let [ty (.getClientProperty obj "type") detai (ty detail/widgets)]
    (if (:daycare! detai) ((:daycare! detai) obj old-substate new-substate)) nil))
