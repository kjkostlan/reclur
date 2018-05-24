; Just a place to print results, eventual TODO of browsing features and macro log file location.
(ns app.siconsole
 (:require [app.rtext :as rtext]))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Other ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare interact-fns) ; Possible dependency cycle with the new function being used by some interact fns.

(defn new-console []
  (assoc rtext/empty-text :interact-fns (interact-fns) 
  :outline-color [0 0.75 0 1] :path "console" :type :siconsole :show-line-nums? false))

(defn get-text [box]
  (:text (first (:pieces box))))

(defn set-text [box txt]
  (assoc box :pieces [{:text txt}]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interaction functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interactions beyond the usual rtext interactions.

(defn key-press [key-evt box]
  "Read-only"
  (let [ed (rtext/key-to-edit box key-evt)]
    (if (or (= (:type ed) :arrow) (= (:type ed) :copy)) (rtext/key-press key-evt box) box)))

(defn log1 [console msg]
  (let [max-text-ln 10000 ; performance tradeoff. Needs to increase if we get more performance in the rtexts.
        s (str (get-text console) "\n" msg)
        console1 (assoc console :scroll-top 1000000000000)
        s1 (if (> (count s) max-text-ln) (subs s (- (count s) max-text-ln)) s)]
    (rtext/scroll-bound (set-text console1 s1))))

(defn log [s msg]
  "Logs msg to all consoles in s. Includes the newline."
  (let [comps (:components s)
        comps1 (zipmap (keys comps) (mapv #(if (= (:type %) :siconsole) (log1 % msg) %) (vals comps)))]
    (assoc s :components comps1)))

;;;;;;;;;;;;;;;;;;;;; Child UI functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; No child UI is planned in the near future.
(defn expandable? [mouse-evt box] false)
(defn expand-child [mouse-evt marker box] (throw (Exception. "No plans to implement siconsole child-UI.")))
(defn contract-child [box child] (throw (Exception. "No plans to implement siconsole child-UI.")))
(defn unwrapped-tree [box] [])
(defn implement-diffs [box diffs] box)

;;;;;;;;;;;;;;;;;;;;;;;; Compiling interaction events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro updaty-fns [code] 
  (let [a1 (gensym 'args)] 
    (zipmap (keys code) (mapv #(list `fn ['& a1] (list `apply % a1)) (vals code)))))
(defn interact-fns [] (updaty-fns
  {:mousePressed rtext/mouse-press
   :mouseDragged rtext/mouse-drag
   :keyPressed key-press
   :keyReleased rtext/key-release
   :mouseWheelMoved rtext/mouse-wheel
   :everyFrame (fn [_ box] box)
   :render rtext/render
   :mouseMoved (fn [_ box] box)
   :expandable? expandable?
   :expand-child expand-child :contract-child contract-child
   :is-child? (fn [box] false) :unwrapped-tree unwrapped-tree :implement-diffs implement-diffs}))