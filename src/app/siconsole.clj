; Just a place to grab println et al, coder/logger is the more "proper" debug logging tool.
(ns app.siconsole
 (:require [app.rtext :as rtext]
   [layout.colorful :as colorful]
   [layout.keybind :as kb]
   [coder.plurality :as plurality]))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Other ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *max-text-length* 10000) 

(declare interact-fns) ; Possible dependency cycle with the new function being used by some interact fns.

(defn colorize [box pieces piece-ixs char-ix0 char-ix1]
  "Use colorfulness."
  (let [max-ix (if-let [x (last piece-ixs)] x 0)
        cols (mapv #(conj (colorful/printix2rgb %) 1) (range (inc max-ix)))]
    (mapv #(nth cols %) piece-ixs)))

(defn new-console []
  (assoc rtext/empty-text :interact-fns (interact-fns) :pieces [{:text "\n"}]
  :outline-color [0 0.75 0 1] :path "console" :type :siconsole :show-line-nums? false :colorize-fn colorize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interaction functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interactions beyond the usual rtext interactions.

(defn log1 [console msg]
  (let [pieces0 (:pieces console)
        msg1 (if (= (str (last msg)) "\n") msg (str msg "\n"))
        pieces1 (conj pieces0 {:text msg1}) ; add the msg as a separate piece.
        ; Remove old stuff:
        starting-ixs (into [] (reductions + (mapv #(count (:text %)) pieces1)))
        cut-ix (- (last starting-ixs) *max-text-length*) ; stuff before this gets cut.
        
        pieces2 (mapv (fn [st p] (update p :text #(subs % (min (count %) (max 0 (- cut-ix st)))))) 
                   starting-ixs pieces1)
        first-nz-ix (if-let [x (first (filter #(> (count (:text (nth pieces2 %))) 0) (range (count pieces2))))] x (count pieces2))
        gran (colorful/num-cmd-cycle)
        pieces3 (into [] (subvec pieces2 (* (int (/ (int first-nz-ix) (int gran))) (int gran))))
        
        console1 (assoc console :pieces pieces3 :scroll-top 1000000000000)]
    (rtext/scroll-bound console1)))

(defn log [s msg]
  "Logs msg to all consoles in s. Includes the newline."
  (let [comps (:components s)
        comps1 (zipmap (keys comps) (mapv #(if (= (:type %) :siconsole) (log1 % msg) %) (vals comps)))]
    (assoc s :components comps1)))

;;;;;;;;;;;;;;;;;;;;; Child UI functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; No child UI is planned in the near future.
(defn expandable? [mouse-evt box] false)
(defn expand-child [mouse-evt box] (throw (Exception. "No plans to implement siconsole child-UI.")))
(defn contract-child [box child] (throw (Exception. "No plans to implement siconsole child-UI.")))

;;;;;;;;;;;;;;;;;;;;;;;; Compiling interaction events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def dispatch 
  (plurality/->simple-multi-fn
    {:mousePressed rtext/mouse-press
     :mouseDragged rtext/mouse-drag
     :mouseWheelMoved rtext/mouse-wheel
     :keyPressed rtext/key-press
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