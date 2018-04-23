; Just a place to print results, maybe features will be added one day.
(ns app.siconsole
 (:require [app.rtext :as rtext]))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Other ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare interact-fns) ; Possible dependency cycle with the new function being used by some interact fns.

(defn new-console []
  (assoc rtext/empty-text :interact-fns (interact-fns) 
  :outline-color [0.5 0.3 0.2 1] :path "console" :type :siconsole :show-line-nums? false))

(defn set-text [box s]
  (assoc box :pieces [{:text s}]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interaction functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interactions beyond the usual rtext interactions.

(defn key-press [key-evt box]
  "Read-only"
  (let [ed (rtext/key-to-edit box key-evt)]
    (if (or (= (:type ed) :arrow) (= (:type ed) :copy)) (rtext/key-press key-evt box) box)))

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