; The simple hintbox.

(ns app.hintbox
  (:require [app.rtext :as rtext]
    [coder.plurality :as plurality]
    [coder.cbase :as cbase] [coder.crosslang.langs :as langs]
    [coder.textparse :as textparse]
    [app.orepl :as orepl]
    [clojure.string :as string]
    [app.codebox :as codebox]
    [app.fbrowser :as fbrowser]
    [app.selectmovesize :as selectmovesize]))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Setting stuff up ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare interact-fns) ; Possible dependency cycle with the new function being used by some interact fns.

(defn colorize [box pieces piece-ixs char-ix0 char-ix1]
  (let [cols [[1 1 1 1] [1 1 0 1] [0.5 0.75 1 1]]]
    (mapv (fn [ix] (nth cols ix)) piece-ixs)))

(defn new-hintbox []
  (assoc rtext/empty-text :interact-fns (interact-fns) :pieces [{:text "\n"}]
    :outline-color [0 0.75 0 1] :path "" :type :hintbox :colorize-fn colorize
    :subtype false))
 
(defn get-text [box]
  (:text (first (:pieces box))))

(defn set-text [box txt]
  (assoc box :pieces [{:text txt}]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Core functionality ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-hint-box [s cbox txts subtype]
  "The down and dirty with positioning, etc."
  (let [cix (:cursor-ix cbox)
        xy (mapv + (rtext/cursor-ix-to-pixel cbox) (:position cbox))
        box (assoc (new-hintbox) :pieces (mapv #(hash-map :text %) txts) 
              :z (+ (:z cbox) 0.01))
        box (rtext/fit-to-text box true true) sz (:size box)
        m 7 ; margin in pixels
        box (assoc box :position 
              (mapv - xy [(* (first sz) 0.5) (+ (second sz) m)]))
        box (selectmovesize/fit-to-screen s box)] 
    (assoc box :subtype subtype)))

(defn lineanate [syms]
  (let [target-x_y 15
        nc (count (apply str syms))
        w (Math/sqrt (* target-x_y nc))
        lines (reduce #(let [l (last %1) s (str %2)]
                         (if (< (+ (count l) (* (count s) 0.5) 1) w)
                           (assoc %1 (dec (count %1)) (str l s " "))
                           (conj %1 (str s " ")))) [""] syms)]
    (apply str (interpose "\n" lines))))

(defn _whats-at-cursor [cbox]
  "Returns the symbol or literal at the cursor, resolved as a fully qualified symbol if possible.
   Metadata has :special? and :resolved? keywords."
  (let [real (codebox/real-string cbox)
        real-ixs (codebox/real-string-ixs-for-thing-at-cursor cbox)
        piece (apply subs real real-ixs)        
        sym (first (filter identity ; foo/ isn't a valid symbol, but it should be an autocompletable.
                     (map #(try (read-string %) (catch Exception e false))
                       [piece (subs piece 0 (dec (count piece)))])))
        no? (= piece "false")
        specials #{'def 'let* 'var 'quote 'if 'loop* 'recur '. 'new 'throw 'catch 'monitor-enter 'monitor-exit 'set!}]
    (cond (= piece "nil") nil
      no? false
      (and (symbol? sym) (get specials sym))
      (with-meta sym {:special? true})
      (and (symbol? sym) (= (last piece) \/))
      (symbol (str piece))
      (symbol? sym)
      (let [ns-sym (if (= (:type cbox) :orepl) 'app.orepl
                     (langs/file2ns (fbrowser/devec-file (:path cbox))))
            sym-resolved (if ns-sym (try (langs/resolved ns-sym sym)
                                      (catch Exception e (resolve sym)))
                            (resolve sym))
            sym-resolved (cond (symbol? sym-resolved) sym-resolved
                           (not sym-resolved) nil
                           :else (symbol (subs (str sym-resolved) 2)))]
        (if sym-resolved (with-meta sym-resolved {:resolved? true}) sym))
      :else sym)))

(defn whats-at-cursor [cbox]
  (if-let [x (_whats-at-cursor cbox)] x
    (_whats-at-cursor (update cbox :cursor-ix dec))))

(defn codebox-hint [s cbox]
  "Generates a component based on the cursor's position in the codebox.
   nil = no hint could be found, so no box generated."
  (let [x (codebox/x-qual-at-cursor cbox)
        special? (fn [sym] 
                   (contains? #{'def 'let* 'var 'quote 'if 'loop* 'recur '. 'new 'throw 'catch 'monitor-enter 'monitor-exit 'set!}
                     sym))]
    (cond (nil? x) (add-hint-box s cbox ["Nothing here"] :doc)
      (and (symbol? x) (special? x))
      (add-hint-box s cbox [(str x) " (special form)" ] :doc)
      (and (symbol? x) (string/includes? (str x) "/") 
        (langs/findable-ns? (textparse/sym2ns x))
        (:arglists (langs/var-info x false)))
      (let [info (langs/var-info x true)
            doc (if (coll? (:source info)) (second (rest (rest (:source info)))) (:source info))
            doc (if (:doc info) (str (:doc info)) (str doc))
            txts [(str x " ") (apply str (interpose " " (:arglists info))) (str "\n" doc)]]
        (add-hint-box s cbox txts :doc))
      (symbol? x)
      (let [ty (:type cbox)
            ns-sym (cond (= ty :orepl) (symbol (str orepl/r-ns))
                     (= ty :codebox) (langs/file2ns (fbrowser/devec-file (:path cbox)))
                     :else (symbol (str *ns*)))
            hints (try (cbase/auto-complete ns-sym x)
                    (catch Exception e
                      (cbase/auto-complete 'clojure.core x)))]
        (if (> (count hints) 0)
          (add-hint-box s cbox [(lineanate hints)] :autocomplete)
          (add-hint-box s cbox [(str x " (java method, local variable or can't be resolved).")] :doc)))
      :else (add-hint-box s cbox ["Literal " (.replace ^String (str (type x)) "class " "")] :doc))))

(defn try-to-toggle-hint-box [s]
  "If it can't add a hintbox and none exists it returns the unmodified s."
  (if-let [fc (get (:components s) (first (:selected-comp-keys s)))]
    (if (or (= (:type fc) :codebox) (= (:type fc) :orepl))
      (if-let [hb (codebox-hint s fc)] 
        (if (let [hb0 (get-in s [:components ::hintbox])]
              (and hb0 (= (mapv int (:position hb0)) (mapv int (:position hb)))))
          (update s :components 
            #(dissoc % ::hintbox))
          (assoc-in s [:components ::hintbox] hb)) 
          (do (println "No hint found at location") s)) 
        (do (println "A codebox or orepl needs to be selected.") s)) 
    (do (println "No components selected.") s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interaction functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interactions beyond the usual rtext interactions.

(defn key-press [key-evt box]
  "Read-only"
  (let [ed (rtext/key-to-edit box key-evt)]
    (if (or (= (:type ed) :arrow) (= (:type ed) :copy)) (rtext/key-press key-evt box) box)))


;;;;;;;;;;;;;;;;;;;;; Child UI functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; No child UI is planned in the near future.
(defn expandable? [mouse-evt box] false)
(defn expand-child [mouse-evt box] (throw (Exception. "No plans to implement hintbox child-UI.")))
(defn contract-child [box child] (throw (Exception. "No plans to implement hintbox child-UI.")))

;;;;;;;;;;;;;;;;;;;;;;;; Compiling interaction events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def dispatch 
  (plurality/->simple-multi-fn
    {:mousePressed rtext/mouse-press
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