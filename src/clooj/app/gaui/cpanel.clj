; A panel that listenes to inputs and can draw (black background).
; All buttons, etc have to be implemented manually, a level of control 
; needed when gamifing it.

(ns clooj.app.gaui.cpanel
  (:require [clooj.guinew.jui :as jui]
    [clooj.app.gaui.rtext :as rtext])
  (:import [java.awt.event MouseWheelListener]))

;;;;;;;;;;;;; Machinery for making tests ;;;;;;;;;;;;;;

; The panel's :Graphics will be rendered each frame.
(defonce listener-fns (atom {})) ; these can be changed without changing the panel.
                                 ; They have the format (f evt panel).
                                 ; Putting this in an atom is useful for debugging, not useful in production code.
(defonce last-mouse-down-evt (atom {}))

(defn on-mouse-move [mouse-evt panel]
  "Move events are far more common in fun applications."
  (if-let [f (:mouseMoved @listener-fns)] (f mouse-evt panel) panel))
  
(defn on-mouse-press [mouse-evt panel]
  "Store the mouse press for the drag event later."
  (reset! last-mouse-down-evt mouse-evt)
  (if-let [f (:mousePressed @listener-fns)] (f mouse-evt panel) panel))

(defn on-mouse-drag [mouse-evt panel]
  "Click and drag is awesome."
  (let [evt0 @last-mouse-down-evt
        ; X0 and X1 instead of X.
        m-evt (assoc evt0 :X0 (:X evt0) :Y0 (:Y evt0))
        m-evt (assoc (dissoc m-evt :X :Y) :X1 (:X mouse-evt) :Y1 (:Y mouse-evt))]
    (if-let [f (:mouseDragged @listener-fns)] (f m-evt panel))))

(defn on-mouse-release [mouse-evt panel]
  "Release events are rarer."
  (if-let [f (:mouseReleased @listener-fns)] (f mouse-evt panel) panel))

(defn on-key-press [key-evt panel]
  "Key press is the other BIG THING we must listen to."
  (if-let [f (:keyPressed @listener-fns)] (f key-evt panel) panel))

(defn on-key-release [key-evt panel]
  "Key releases are so boring."
  (if-let [f (:keyReleased @listener-fns)] (f key-evt panel) panel))

; mouse-wheels don't work in the standard way.
;(defn on-mouse-wheel [wheel-evt panel]
;  "MouseWheels spin and never end."
;  (if-let [f (:mouseWheelMoved @listener-fns)] (f wheel-evt panel) panel))

(def ^{:dynamic true} *block-frames* false) ; debug emergency stop.
(defn _on-every-frame [clock-evt panel panel-j]
  "Run a few sim steps every frame and update the graphics. No other function affects the graphics directly."
  ; For some reason we have to add our own mouse wheel event.
  (if (not (.getClientProperty panel-j "addedWheel"))
    (do
      (.addMouseWheelListener panel-j 
        (reify MouseWheelListener 
          (mouseWheelMoved [this evt] 
            (let [gui-atom (jui/get-gui-atom panel-j)
                  gui-state @gui-atom mwheel (:mouseWheelMoved @listener-fns)]
              (if mwheel 
                (let [evt-clj (jui/translate-event evt panel-j)]
                  (swap! gui-atom (fn [x] (update-in x [:clj :val :panel] #(mwheel evt-clj %))))))))))
      (.putClientProperty panel-j "addedWheel" true)))
  (if *block-frames* panel
    (if-let [f (:everyFrame @listener-fns)] (f clock-evt panel) panel)))

(defn on-every-frame [clock-evt panel]
  "Allows us to change the every-frame function in case we end up making massive amounts of crap.
   TODO: doesn't seem to be used."
  (_on-every-frame clock-evt panel))

(defn listenerify [f]
  "Turns one of the above on functions into a listener function."
  #(vector [:object :panel (f %1 %2)]))

(defn make-window []
  "A single JPanel that listens to events. Populate the listener-fns atom to listen to us."
  [[:object :window {:Type :JFrame :Size [700 700] :DefaultCloseOperation javax.swing.WindowConstants/DISPOSE_ON_CLOSE}] ; evantually it will exit when closed after finializaitons.
  [:object :panel {:Type :JPanel :Focusable true}]
  [:relation :window :panel]
  [:listener :Clock :everyFrame :xyz [(fn [e panel jpanel] [[:object :panel (_on-every-frame e panel jpanel)]]) :panel 'panel]]
  [:listener :panel :mouseMoved :xyz [(listenerify on-mouse-move) :panel]]
  [:listener :panel :mouseDragged :xyz [(listenerify on-mouse-drag) :panel]]
  [:listener :panel :mousePressed :xyz [(listenerify on-mouse-press) :panel]]
  [:listener :panel :mouseReleased :xyz [(listenerify on-mouse-release) :panel]]
  ; The mousewheel doesn't work with jui, so we must manually do it.
  ;[:listener :panel :mouseWheelMoved :xyz [(listenerify on-mouse-wheel) :panel]]
  [:listener :panel :keyPressed :xyz [(listenerify on-key-press) :panel]] ; the window seems to eat the key presses.
  [:listener :panel :keyReleased :xyz [(listenerify on-key-release) :panel]]
  ])

(defn grequel [gr]
  "Black background and monospaced font, added to the beginning of graphics."
  (into [] (concat [[:java (fn [g] (.setFont g (java.awt.Font. "Monospaced" 0 10)))]
             [:fillRect [0 0 3000 3000] {:Color [0.01 0 0 1]}]] gr)))

(defn launch-app 
  "Listener-functions come alive!
   Allowed keys: :everyFrame :mousePressed :mouseReleased :mouseMoved :mouseDragged :keyPressed :keyReleased :mouseWheelMoved.
   Each function is (f evt panel).
   puts v into a single k within panel, such that (get panel k) = v."
[k v fns]
  (jui/clear-all-anims!!) 
  (reset! listener-fns fns)
  (jui/setup (assoc-in (make-window) [1 2 k] v))) ; singleton needed for the functions to run.


;;;;;;;;;;;;; Logging from an anim, only log once! ;;;;;;;;;;;;;;;;

(defonce already-logged (atom #{}))

(defn print1 [id x]
  (if (not (get @already-logged id))
    (do (swap! already-logged #(conj % id))
      (println x))))
; Cut-n-paste for logging 1 time.
;; (do (require '[clooj.app.gauitest.ctest :as gtest]) (gtest/print1 :id01 x))

; Stop all animations:
(defn stop [] (jui/clear-all-anims!!))
;; (do (require '[clooj.guinew.jui :as jui]) (jui/clear-all-anims!!))