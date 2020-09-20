; Shows what code uses what functions.
(ns app.graphbox
  (:require [app.rtext :as rtext]
    [coder.plurality :as plurality] [coder.textparse :as textparse]
    [coder.cbase :as cbase] [coder.cnav :as cnav] [coder.crosslang.langs :as langs]
    [app.orepl :as orepl] [app.hintbox :as hintbox] [app.codebox :as codebox]
    [layout.selectmovesize :as selectmovesize]
    [app.fbrowser :as fbrowser] [app.rtext :as rtext]
    [collections]))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Small helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare interact-fns) ; Possible dependency cycle with the new function being used by some interact fns.

(defn colorize [box pieces piece-ixs char-ix0 char-ix1]
  (let [cols [[0.5 0.6 1.0 1] [1 1 1 1] [0.7 1.0 0.7 1]]]
    (mapv (fn [ix] (nth cols ix)) piece-ixs)))

(defn new-graphbox []
  (assoc (merge rtext/empty-text (interact-fns)) :pieces [{:text ""} {:text ""} {:text ""}]
    :outline-color [0 0.5 0.75 1] :path "" :type :graphbox :colorize-fn colorize
    :subtype false))

(defn qual-name [code cbox]
  "Qualified name for a defn in code."
  (let [ns-sym (langs/file2ns (fbrowser/devec-file (:path cbox)))
        unqual-sym (second code)]
    (symbol (str ns-sym "/" unqual-sym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-sym [box sym-qual]
  "Sets the symbol we focus on."
  (let [set-box (fn [t m b e?] (rtext/fit-to-text (assoc box :pieces [{:text t} {:text m} {:text b}] :error? e?
                                                    :cursor-ix (count t) :selection-start 0 :selection-end 0) 
                                 true true))]
    (cond (and (symbol? sym-qual) (textparse/qual? sym-qual))
      (let [outwards (cbase/uses-of sym-qual)
            inwards (cbase/used-by sym-qual)
            sp #(hintbox/lineanate %)]
        (set-box (str (sp outwards) "\n") (str sym-qual "\n") (sp inwards) false))
      (symbol? sym-qual)
      (set-box "\n" "(local, unrecognized or java symbol)" "\n" true)
      (coll? sym-qual)
      (set-box "\n" "(Collection)" "\n" true)
      :else
      (set-box "\n" (str "(Literal " (subs (str (type sym-qual)) 6) ")") "\n" true))))

(defn cursorix2sym [box]
  "Where are we pointed?"
  (let [txt (.replace ^String (rtext/rendered-string box) "\n" " ")
        cur-ix (:cursor-ix box)
        space-ixs (filterv #(= (nth txt %) \ ) (range (count txt)))
        ix0 (apply max 0 (filterv #(<= % cur-ix) space-ixs))
        ix1 (apply min (count txt) (filterv #(> % cur-ix) space-ixs))]
    (symbol (.trim ^String (subs txt ix0 ix1)))))

(defn add-graph-box [s cbox]
  "The down and dirty with positioning, etc."
  (let [cix (:cursor-ix cbox)
        xy (mapv + (rtext/cursor-ix-to-pixel cbox) (:position cbox))
        box (assoc (set-sym (new-graphbox) (codebox/x-qual-at-cursor cbox)) 
              :z (+ (:z cbox) 0.01))
        box (rtext/fit-to-text box true true) sz (:size box)
        m 7 ; margin in pixels
        box (assoc box :position 
              (mapv - xy [(* (first sz) 0.5) (+ (second sz) m)]))]
    (selectmovesize/fit-to-screen s box)))

(defn try-to-toggle-graph-box [s]
  (if-let [fc (get (:components s) (first (:selected-comp-keys s)))]
    (if (or (= (:type fc) :codebox) (= (:type fc) :orepl))
      (if-let [gb (add-graph-box s fc)] 
        (if (get-in s [:components ::graphbox])
          (update s :components 
            #(dissoc % ::graphbox))
          (assoc-in s [:components ::graphbox] gb)) 
        (do (println "A nonlocal symbol needs to be aimed at.") s)) 
      (do (println "A codebox or orepl needs to be selected.") s)) 
    (do (println "No components selected.") s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interaction functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interactions beyond the usual rtext interactions.

(defn key-press [key-evt box]
  "Read-only"
  (let [ed (rtext/key-to-edit box key-evt)]
    (if (or (= (:type ed) :arrow) (= (:type ed) :copy)) (rtext/key-press key-evt box) box)))

(defn mouse-pressed [m-evt box]
  "Make the mouse press move in the graph."
  (let [box1 (rtext/mouse-press m-evt box)]
    (if (:error? box1) box1 (set-sym box1 (cursorix2sym box1)))))

;;;;;;;;;;;;;;;;;;;;; Child UI functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; No child UI is planned in the near future.
(defn expandable? [mouse-evt box] false)
(defn expand-child [mouse-evt box] (throw (Exception. "No plans to implement hintbox child-UI.")))
(defn contract-child [box child] (throw (Exception. "No plans to implement hintbox child-UI.")))

;;;;;;;;;;;;;;;;;;;;;;;; Compiling interaction events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def dispatch 
  (plurality/->simple-multi-fn
    {:mousePressed mouse-pressed
     :mouseDragged rtext/mouse-drag
     :mouseWheelMoved rtext/mouse-wheel
     :keyPressed key-press
     :keyReleased rtext/key-release}
     (fn [e-clj comp] comp)
     (fn [e-clj comp] (:type e-clj))))

(defmacro updaty-fns [code] 
  (let [a1 (gensym 'args)] 
    (zipmap (keys code) (mapv #(list `fn ['& a1] (list `apply % a1)) (vals code)))))
(defn interact-fns [] (updaty-fns
  {:dispatch dispatch
   :render rtext/render
   :expandable? expandable?
   :expand-child expand-child :contract-child contract-child
   :is-child? (fn [box] false)}))