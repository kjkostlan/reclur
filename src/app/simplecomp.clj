; Simple testing hello world component.
(ns app.simplecomp
  (:require
    [app.rtext :as rtext]))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Other ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare interact-fns) ; Possible dependency cycle with the new function being used by some interact fns.

(defn new-test []
  (assoc (assoc rtext/empty-text :pieces [{:text "helloWorld"}]) :interact-fns (interact-fns) 
   :outline-color [0.5 0.3 0.2 1] :position [0 0] :size [100 100] :path "test" :type :simplecomp :show-line-nums? false))

;;;;;;;;;;;;;;;;;;;;; Child UI functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; No child UI is planned in the near future.
(defn expandable? [mouse-evt box] false)
(defn expand-child [mouse-evt box] (throw (Exception. "No plans to implement siconsole child-UI.")))
(defn contract-child [box child] (throw (Exception. "No plans to implement siconsole child-UI.")))

;;;;;;;;;;;;;;;;;;;;;;;; Compiling interaction events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#_(defmacro updaty-fns [code] 
  (let [a1 (gensym 'args)] 
    (zipmap (keys code) (mapv #(list `fn ['& a1] (list `apply % a1)) (vals code)))))
(defn interact-fns []
  {:mousePressed rtext/mouse-press
   :mouseDragged rtext/mouse-drag
   :keyPressed rtext/key-press
   :keyReleased rtext/key-release
   :mouseWheelMoved rtext/mouse-wheel
   :everyFrame (fn [_ box] box)
   :render rtext/render
   :mouseMoved (fn [_ box] box)
   :expandable? expandable?
   :expand-child expand-child :contract-child contract-child
   :is-child? (fn [box] false)})