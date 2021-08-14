; A panel that listenes to inputs and can draw (black background).
; All buttons, etc have to be implemented manually, a level of control useful to gameify the app.

(ns javac.cpanel
  (:require [clojure.string :as string] [globals]
    [javac.gfx :as gfx]
    [javac.clojurize :as clojurize]
    [coder.unerror :as unerror]
    [app.iteration :as iteration]
    [crossplatform.cp :as crossp]
    [collections])
  (:import [java.awt.event KeyAdapter MouseAdapter WindowEvent ComponentAdapter]
    [javax.swing SwingUtilities]
    [java.awt FlowLayout] 
    [javax.swing JFrame JPanel]))


;;;;;;;;;;;;;;;;;;;;; Settings ;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *frame-time-ms* 30) ; a somewhat complex mechanism to ensure graceful degradation when the events are heavy.
(def ^:dynamic *nframe-slowdown-when-idle* 15) ; Same CPU when nothing is requesting an every frame event.
(def ^:dynamic *drop-frames-queue-length-threshold* 1024) ; Not needed for now but we may change how we deal with slow dispatches.
(def ^:dynamic *drop-frames-report-every* 50)
(def ^:dynamic *mac-keyboard-kludge?* false) ; A huuuuuuge bug with typing involving accents stealing focus. 
(def ^:dynamic *add-keyl-to-frame?* true) ; tab only works on the frame. False goes to panel.

;;;;;;;;;;;;;;;;;;;;; Other Support functions ;;;;;;;;;;;;;;;;;;;;;

(defonce _ (println "If you are on a mac this may help:   defaults write -g ApplePressAndHoldEnabled -bool false"))

(defn empty-state []
  {:evt-queue (collections/queue) :external-state {:X0 0 :Y0 0 :X1 0 :Y1 0}})

;;;;;;;;;;;;;;;;;;;;; Queing ;;;;;;;;;;;;;;;;;;;;;

(defn update-external-state [e-clj kwd old-extern]
  (cond (= kwd :mousePressed)
    (assoc
      (cond (= (:Button e-clj) 0) (assoc old-extern :Button0 true :Button 0)
        (= (:Button e-clj) 1) (assoc old-extern :Button1 true :Button 1)
        (= (:Button e-clj) 2) (assoc old-extern :Button2 true :Button 2)
        :else (assoc old-extern :Button (:Button e-clj)))
      :X0 (:X e-clj) :Y0 (:Y e-clj))
    (or (= kwd :mouseDragged) (= kwd :mouseMoved))
    (assoc old-extern :X1 (:X e-clj) :Y1 (:Y e-clj))
    (= kwd :mouseReleased)
    (cond (= (:Button e-clj) 0) (assoc old-extern :Button0 false :Button false)
      (= (:Button e-clj) 1) (assoc old-extern :Button1 false :Button false)
      (= (:Button e-clj) 2) (assoc old-extern :Button2 false :Button false))
    (or (= kwd :keyPressed) (= kwd :keyReleased))
    (do (assoc old-extern :ShiftDown (:ShiftDown e-clj) :ShiftDown (:ShiftDown e-clj)
      :AltDown (:AltDown e-clj) :ControlDown (:ControlDown e-clj)
      :AltGraphDown (:AltGraphDown e-clj)))
    :else old-extern))

(defonce _dropped-frame-counter (atom 0)) ; Reports if many frames were dropped.
(defn queue1 [x e-clj kwd] 
  "Queues e, with reasonable limits to prevent everyFrame events piling-up to infinity.
   Handles both java and clojure datastructures for e, converting e into clojure if it is a java event."
  (let [_ (if (not (map? e-clj)) (throw (Exception. "all events need to be converted to clojure maps for queue1.")))
        e-clj (assoc (dissoc e-clj :ParamString :ID :When) :type kwd)
        evtq (:evt-queue x)]
    (assoc x :evt-queue (conj evtq e-clj))))

(defn dispatch [x evt] 
  "Processes one event on x. Returns x if evt is nil.
   Does not affect the event queue."
  (if evt 
    (let [kwd (:type evt)
          extern (:external-state x)
          evt (if (or (= kwd :mouseDragged) (= kwd :mouseMoved))
                (assoc evt :X0 (:X0 extern) :Y0 (:Y0 extern) :X1 (:X1 extern) :Y1 (:Y1 extern)) evt)
          ;_ (println "Removing: " kwd "old queue len" (count (:evt-queue x)) "new queue len: " (count (into [] (rest (:evt-queue x)))))
          f (:dispatch x)
          new-external (update-external-state evt (:type evt) extern)
          new-state (try (if f (f evt (:app-state x)) (:app-state x))
                      (catch Exception e (println e) (:app-state x)))
          ud? true ; TODO: better gfx update rules.
          ]
     (assoc x :app-state new-state :external-state new-external :needs-gfx-update? ud?)) x))

(defn dispatch-all [x]
  "Keeps dispatching events from x until it's queue is empty."
  (if (empty? (:evt-queue x)) x
    (let [evts-unsorted (into [] (:evt-queue x))
          evts (sort-by #(get % :Time 0) evts-unsorted); Sort by time because the future does not preserve order.
          x1 (reduce (fn [x-acc evt]
                       (try (dispatch x-acc evt)
                         (catch Exception e (do (println "dequeue error") x-acc)))) x evts)]
      (assoc x1 :evt-queue (collections/queue)))))

;;;;;;;;;;;;;;;;;;;;; Mutation ;;;;;;;;;;;;;;;;;;;;;

(def one-atom globals/one-atom)
(defonce _ (reset! one-atom (empty-state)))

(defn can-add? [e-clj report-if-fail?]
  "Not too many every frame events."
  (let [evtq (:evt-queue @one-atom) ; safe to be async with respect to the locking code.
        kwd (:type e-clj)
        add? (or (not (= kwd :everyFrame)) 
               (< (count evtq) *drop-frames-queue-length-threshold*))]
    (cond add? true
      (not report-if-fail?) false
      :else 
      (do (swap! _dropped-frame-counter 
            #(if (< % *drop-frames-report-every*) (inc %)
               (do (println "Dropped many frames due to slow evts and/or gfx: " %) 0))) false))))

(defn lock-swap! [a f]
  "Like swap! but we lock the atom so that it doesn't run more than once per call.
   The event calls can cause all sorts of mutations and high-cost computations.
   All modifications of the atom must be lock-swap'ed (I think), because swap! doesn't fully respect locks on the atom.
   We use futures on the lightweight fns to avoid freezing the gui, etc."
   (locking a
     (reset! a (f @a))))

(defn _update-graphics! [x]
  "Causes mutation due to calls to the graphics functions."
  (let [udgfx (:update-gfx-fn x)]
    (if udgfx
      (let [gfx-updated-app-state (try (udgfx (:app-state x)) (catch Exception e (do (println "app gfx update error:" e) (:app-state x))))
            new-gfx (try ((:render-fn x) gfx-updated-app-state) (catch Exception e (do (println "gfx render error." e) [])))]
        (if-let [panel (:JPanel x)] (try (gfx/update-graphics! panel (:last-drawn-gfx x) new-gfx)
                                      (catch Exception e (println "gfx/update-graphics! error:" e))))
        (assoc x :last-drawn-gfx new-gfx :app-state gfx-updated-app-state :needs-gfx-update? false)) x)))
(defn update-graphics! []
  "Causes mutation due to calls to the graphics functions."
  (lock-swap! one-atom _update-graphics!))

(defn event-queue! [e kwd]
  (let [e-clj (if (map? e) e
                (try (clojurize/translate-event e (.getSource e))
                  (catch Exception e
                    (do (println "Evt failed to be clojurized and will be dropped: " e)
                      (throw e)))))
        e-clj (assoc e-clj :Time (System/currentTimeMillis) :type kwd)
        add? (can-add? e-clj true)]
    (if add? (future (lock-swap! one-atom #(queue1 % e-clj kwd))))))

(defn store-window-size! [^JFrame frame]
  (future (lock-swap! one-atom 
            #(assoc-in % [:external-state :window-size] 
              (let [^java.awt.Dimension sz (.getSize (.getContentPane frame))]
                [(.getWidth sz) (.getHeight sz)])))))

; Continuously polling is a (tiny) CPU drain, but it is way easier than a lock system that must ensure we don't get overlapping calls to dispatch-loop!
; The idle CPU is 0.7% of one core for the total program (tested Aug 13th 2019).
(defonce _polling? (atom false)) ; Used to make sure we don't have multiple frame loops, an extra safeguard.
(defonce _frame-counter (atom -1)) ; Counts the time elapsed in frames, used for idle mode.
(if (not @_polling?)
  (future 
    (loop [last-millis -1e100] ; This loop runs until the application is quit.
        (let [elapsed-millis (max 0 (- (System/currentTimeMillis) last-millis))
              sleep-time (- *frame-time-ms* elapsed-millis)]
          (if (> sleep-time 0) (Thread/sleep *frame-time-ms*)))
        (let [x @one-atom
              s (:app-state x)
              t (swap! _frame-counter inc)]
          (if (or (> (count (:hot-boxes s)) 0) ; A single empty hot box adds about 1.25% CPU, tripling our CPU usage.
                (= (mod t *nframe-slowdown-when-idle*) 0)) 
            (event-queue! {:Time (System/currentTimeMillis)} :everyFrame)))
        (lock-swap! one-atom dispatch-all)
        (if (:needs-gfx-update? @one-atom) (update-graphics!))
        (recur (System/currentTimeMillis)))))
(reset! _polling? true)

;;;;;;;;;;;;;;;;;;;;; Java listeners and windowing ;;;;;;;;;;;;;;;;;;;;;

(defn add-mouse-listeners! [^JPanel panel]
  (.addMouseListener panel
    (proxy [MouseAdapter] []
      (mouseClicked [e] (event-queue! e :mouseClicked))
      (mouseEntered [e] (event-queue! e :mouseEntered))
      (mouseExited [e] (event-queue! e :mouseExited))
      (mousePressed [e]
        #_(.requestFocus (:JPanel @one-atom))
        #_(let [frame (.getParent (.getParent (.getParent (.getParent (.getSource e)))))]
          (.requestFocus frame)) 
        (event-queue! e :mousePressed))
      (mouseReleased [e] (event-queue! e :mouseReleased))))
  (.addMouseWheelListener panel
    (proxy [MouseAdapter] []
      (mouseWheelMoved [e] (event-queue! e :mouseWheelMoved))))
  (.addMouseMotionListener panel
    (proxy [MouseAdapter] []
      (mouseMoved [e] (event-queue! e :mouseMoved))
      (mouseDragged [e] (event-queue! e :mouseDragged)))))

(defn add-key-listeners! [x]
  (.addKeyListener x
    (proxy [KeyAdapter] []
      (keyPressed [e] (do (store-window-size! ^JFrame (:JFrame @one-atom)) (event-queue! e :keyPressed)))
      (keyReleased [e] (event-queue! e :keyReleased)))))

(defn add-resize-listener! [^JFrame frame]
   (.addComponentListener frame
     (proxy [ComponentAdapter] []
       (componentShown [e] (store-window-size! frame))
       (componentResized [e] (store-window-size! frame)))))

(defn add-parentin-listener! [] 
  "Adds a listener for stdin."
  (future
    (while [true]
      (let [s (try (iteration/get-input) (catch Exception e (do (Thread/sleep 1000) (str "ERROR in iteration/get-input:\\n" (unerror/pr-error e)))))] ; waits here until the stream has stuff in it.
        (SwingUtilities/invokeLater #(event-queue! {:contents s :type :parent-in} :parent-in)))))) ; all evts are queued on the edt thread, not sure if this is ideal.
(if (globals/are-we-child?) (add-parentin-listener!))

(defn proxy-panel []
  (proxy [javax.swing.JPanel] [] 
    (paintComponent [g]
      (do (proxy-super paintComponent g) 
        (gfx/defaultPaintComponent! g this)))))

(defn new-window []
  "Sets up all listeners and atoms. EDT thread only."
  (let [^JFrame frame (JFrame.);(JFrame. "The app")
        ^JPanel panel (proxy-panel)]
    ; Example from: https://www.javatpoint.com/java-jframe
    (.setLayout panel (FlowLayout.))
    (add-mouse-listeners! panel)
    ; Gets tab working: http://www.java2s.com/Code/Java/Event/KeyEventDemo.htm
    (.setFocusTraversalKeysEnabled frame false) 
    (.add frame panel)
    (add-resize-listener! frame)
    (.setSize frame 1440 877)
    (.setLocationRelativeTo frame nil)
    (.setVisible frame true)
    (if *add-keyl-to-frame?* (add-key-listeners! frame)
      (do (add-key-listeners! panel) (.setFocusable panel true) (.requestFocus panel)))
    (crossp/add-quit-request-listener! #(event-queue! % :quit))
    [frame panel]))

(defn swing-or-kludge [f]
  (if *mac-keyboard-kludge?* (f)
    (SwingUtilities/invokeAndWait f)))

(defn launch-app! [init-state dispatch-fn update-gfx-fn render-fn]
  "app is singleton, launching 
   dispatch-fn including is (f evt-clj state), we make our own every-frame event.
   update-gfx-fn is (f state-clj), returns the new state, and should cache any expensive gfx cmds.
   render-fn is (f state-clj) but some render commands are functions on the java object."
  (swing-or-kludge
    (fn [& args]
      ;; https://stackoverflow.com/questions/1234912/how-to-programmatically-close-a-jframe
      (if-let [old-frame (:JFrame @one-atom)]
        (do (.dispatchEvent old-frame (WindowEvent. old-frame WindowEvent/WINDOW_CLOSING)))
        (swap! one-atom #(dissoc % :JFrame)))
      (let [[frame panel] (new-window)]
        (reset! one-atom
               (assoc (empty-state)
                      :app-state init-state
                      :dispatch dispatch-fn :last-drawn-gfx nil :update-gfx-fn update-gfx-fn :render-fn render-fn 
                      :external-state {}
                      :JFrame frame :JPanel panel))))))

(defn stop-app! []
  (if (not= (dissoc @one-atom :evt-queue) (dissoc (empty-state) :evt-queue)) (println "stopping app (IF any app was open)"))
  (launch-app! {} {} (fn [state] []) (fn [state] [])))
