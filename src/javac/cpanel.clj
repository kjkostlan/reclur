; A panel that listenes to inputs and can draw (black background).
; All buttons, etc have to be implemented manually, a level of control useful to gameify the app.

(ns javac.cpanel
  (:require [clojure.string :as string] [globals]
    [javac.gfx :as gfx] [javac.thread :as jthread]
    [javac.clojurize :as clojurize]
    [coder.unerror :as unerror]
    [crossplatform.cp :as crossp]
    [c])
  (:import [java.awt.event KeyAdapter MouseAdapter WindowEvent ComponentAdapter WindowAdapter]
    [java.awt FlowLayout]
    [javax.swing JFrame JPanel]))

;;;;;;;;;;;;;;;;;;;;; Settings ;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *frame-time-ms* 30) ; a somewhat complex mechanism to ensure graceful degradation when the events are heavy.
(def ^:dynamic *nframe-slowdown-when-idle* 15) ; Same CPU when nothing is requesting an every frame event.
(def ^:dynamic *drop-frames-queue-length-threshold* 1024) ; Not needed for now but we may change how we deal with slow dispatches.
(def ^:dynamic *drop-frames-report-every* 50)
(def ^:dynamic *add-keyl-to-frame?* true) ; tab only works on the frame. False goes to panel.

;;;;;;;;;;;;;;;;;;;;; Other Support functions ;;;;;;;;;;;;;;;;;;;;;

(defonce _ (println "If you are on a mac this may help:   defaults write -g ApplePressAndHoldEnabled -bool false"))

(defn empty-state []
  {:evt-queue (c/queue)})

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
      (= (:Button e-clj) 2) (assoc old-extern :Button2 false :Button false)
      :else (assoc old-extern :Button false))
    (or (= kwd :keyPressed) (= kwd :keyReleased))
    (assoc old-extern :ShiftDown (:ShiftDown e-clj) :ShiftDown (:ShiftDown e-clj)
      :AltDown (:AltDown e-clj) :ControlDown (:ControlDown e-clj)
      :AltGraphDown (:AltGraphDown e-clj))
    :else old-extern))

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
          _ (if (= kwd :mouseWheelMoved)
              (do (crossp/update-inertial-scroll-guess! evt)
                (swap! globals/external-state-atom
                  #(let [sc-h (get % :mouse-wheel-history [])
                         sc-h (if (> (count sc-h) 255) (into [] (rest sc-h)) sc-h)]
                     (assoc % :mouse-wheel-history (conj sc-h evt))))))
          extern @globals/external-state-atom
          evt (if (or (= kwd :mouseDragged) (= kwd :mouseMoved))
                (assoc evt :X0 (:X0 extern) :Y0 (:Y0 extern) :X1 (:X1 extern) :Y1 (:Y1 extern)) evt)
          ;_ (println "Removing: " kwd "old queue len" (count (:evt-queue x)) "new queue len: " (count (into [] (rest (:evt-queue x)))))
          f (:dispatch x)
          new-state (try (if f (f evt (:app-state x)) (:app-state x))
                      (catch Exception e (println e) (:app-state x)))
          ud? true] ; TODO: better gfx update rules.
     (swap! globals/external-state-atom #(update-external-state evt (:type evt) %))
     (assoc x :app-state new-state :needs-gfx-update? ud?)) x))

(defn dispatch-all [x]
  "Keeps dispatching events from x until it's queue is empty."
  (if (empty? (:evt-queue x)) x
    (let [evts-unsorted (into [] (:evt-queue x))
          evts (sort-by #(get % :Time 0) evts-unsorted); Sort by time because the future does not preserve order.
          x1 (reduce (fn [x-acc evt]
                       (try (dispatch x-acc evt)
                         (catch Exception e (do (println "dequeue error:" e) x-acc)))) x evts)]
      (assoc x1 :evt-queue (c/queue)))))

;;;;;;;;;;;;;;;;;;;;; Mutation ;;;;;;;;;;;;;;;;;;;;;

(defn lock-swap! [a f]
  "Like swap! but we lock the atom so that it doesn't run more than once per call.
   The event calls can cause all sorts of mutations and high-cost computations.
   All modifications of the atom must be lock-swap'ed (I think), because swap! doesn't fully respect locks on the atom.
   We use futures on the lightweight fns to avoid freezing the gui, etc."
   (locking a
     (reset! a (f @a))))

(defn lock-reset! [a x] (lock-swap! a (fn [_] x)))

(defn lock-deref [a]
  "Like @a but locks on a."
  (locking a @a))

(defn set-window-size! [w h]
  "Should be called every time the window is resized."
  (swap! globals/external-state-atom #(assoc % :window-size [w h])))

(defn can-add? [e-clj report-if-fail?]
  "Not too many every frame events."
  (let [evtq (:evt-queue (lock-deref globals/sync-app-atom))
        kwd (:type e-clj)
        add? (or (not (= kwd :everyFrame))
               (< (count evtq) *drop-frames-queue-length-threshold*))]
    (cond add? true
      (not report-if-fail?) false
      :else
      (do (swap! globals/external-state-atom
            #(let [n-drop (get % :dropped-frames-since-last-report 0)]
               (if (< n-drop *drop-frames-report-every*) (assoc % :dropped-frames-since-last-report (inc n-drop))
                 (do (println "Dropped many frames due to slow evts and/or gfx: " n-drop)
                   (assoc % :dropped-frames-since-last-report 0))))) false))))

(defn _update-graphics! [x]
  (let [udgfx (:update-gfx-fn x)]
    (if udgfx
      (let [gfx-updated-app-state (try (udgfx (:app-state x)) (catch Exception e (do (println "app gfx update error:" e) (:app-state x))))
            new-gfx (try ((:render-fn x) gfx-updated-app-state) (catch Exception e (do (println "gfx render error." e) [])))
            success? (atom false)]
        (if-let [panel (:JPanel @globals/external-state-atom)]
          (try (do (gfx/update-graphics! panel (:last-drawn-gfx x) new-gfx) (reset! success? true))
            (catch Exception e (println "gfx/update-graphics! error:" e))))
        (if success? (assoc x :last-drawn-gfx new-gfx :app-state gfx-updated-app-state :needs-gfx-update? false) x)) x)))
(defn update-graphics! []
  (lock-swap! globals/sync-app-atom _update-graphics!))

(defn event-queue! [e kwd]
  (let [e-clj (if (map? e) e
                (try (clojurize/translate-event e (.getSource e))
                  (catch Exception e
                    (do (println "Evt failed to be clojurized and will be dropped: " e)
                      (throw e)))))
        e-clj (assoc e-clj :Time (System/currentTimeMillis) :type kwd)
        add? (can-add? e-clj true)]
    (if add? (future (lock-swap! globals/sync-app-atom #(queue1 % e-clj kwd))))))

(defn store-window-size! [^JFrame frame]
  (let [^java.awt.Dimension sz (.getSize (.getContentPane frame))]
    (set-window-size! (.getWidth sz) (.getHeight sz))))

; Continuously polling is a (tiny) CPU drain, but it is way easier than a lock system that must ensure we don't get overlapping calls to dispatch-loop!
; The idle CPU is 0.7% of one core for the total program (tested Aug 13th 2019).
(if (not (:main-polling-loop? @globals/external-state-atom))
  (future
    (loop [last-millis -1e100] ; This loop runs until the application is quit.
        (let [elapsed-millis (max 0 (- (System/currentTimeMillis) last-millis))
              sleep-time (- *frame-time-ms* elapsed-millis)]
          (if (> sleep-time 0) (Thread/sleep *frame-time-ms*)))
        (let [x (lock-deref globals/sync-app-atom)
              s (:app-state x)
              t (:frames-since-app-start
                  (swap! globals/external-state-atom #(assoc % :frames-since-app-start (inc (get % :frames-since-app-start 0)))))]
          (if (or (> (count (:hot-boxes s)) 0) ; A single empty hot box adds about 1.25% CPU, tripling our CPU usage.
                (= (mod t *nframe-slowdown-when-idle*) 0))
            (event-queue! {:Time (System/currentTimeMillis)} :everyFrame)))
        (lock-swap! globals/sync-app-atom dispatch-all)
        (if (:needs-gfx-update? (lock-deref globals/sync-app-atom))
          (update-graphics!))
        (recur (System/currentTimeMillis)))))
(swap! globals/external-state-atom #(assoc % :main-polling-loop? true)) ; Extra sure we do not put more than one thread on this loop.

;;;;;;;;;;;;;;;;;;;;; Java listeners and windowing ;;;;;;;;;;;;;;;;;;;;;

(defn add-mouse-listeners! [^JPanel panel]
  (.addMouseListener panel
    (proxy [MouseAdapter] []
      (mouseClicked [e] (event-queue! e :mouseClicked))
      (mouseEntered [e] (event-queue! e :mouseEntered))
      (mouseExited [e] (event-queue! e :mouseExited))
      (mousePressed [e]
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
      (keyPressed [e] (do (store-window-size! ^JFrame (:JFrame @globals/external-state-atom)) (event-queue! e :keyPressed)))
      (keyReleased [e] (event-queue! e :keyReleased)))))

(defn add-resize-listener! [^JFrame frame]
   (.addComponentListener frame
     (proxy [ComponentAdapter] []
       (componentShown [e] (store-window-size! frame))
       (componentResized [e] (store-window-size! frame)))))

(defn add-close-listener! [^JFrame frame]
  (.addWindowListener frame
    (proxy [WindowAdapter] []
      (windowClosing [e] (event-queue! e :quit)))))

(defn proxy-panel []
  (proxy [javax.swing.JPanel] []
    (paintComponent [g]
      (do (proxy-super paintComponent g)
        (gfx/defaultPaintComponent! g this)))))

(defn new-window [w h]
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
    (.setSize frame w h) ; Does not trigger the resize-listener yet.
    (.setLocationRelativeTo frame nil)
    (.setVisible frame true)
    (if *add-keyl-to-frame?* (add-key-listeners! frame)
      (do (add-key-listeners! panel) (.setFocusable panel true) (.requestFocus panel)))
    (crossp/add-quit-request-listener! (event-queue! {} :quit))
    (.setDefaultCloseOperation frame (JFrame/DO_NOTHING_ON_CLOSE))
    (add-close-listener! frame) ; Not necessarily the least lines of code but gets the job done.
    [frame panel]))

(defn launch-app! [init-state-fn dispatch-fn update-gfx-fn render-fn]
  "app is singleton, launching
   dispatch-fn including is (f evt-clj state), we make our own every-frame event.
   update-gfx-fn is (f state-clj), returns the new state, and should cache any expensive gfx cmds.
   render-fn is (f state-clj) but some render commands are functions on the java object."
    ;; https://stackoverflow.com/questions/1234912/how-to-programmatically-close-a-jframe
    (if-let [old-frame (:JFrame @globals/external-state-atom)]
      (jthread/swing-wait
        (do (.setDefaultCloseOperation old-frame (JFrame/DISPOSE_ON_CLOSE))
               (.dispatchEvent old-frame (WindowEvent. old-frame WindowEvent/WINDOW_CLOSING))
               (swap! globals/external-state-atom #(dissoc % :JFrame)))))
    (let [w 1440 h 877
          [frame panel] (jthread/swing-wait (new-window w h))]
      (swap! globals/external-state-atom #(assoc % :X0 0 :Y0 0 :X1 0 :Y1 0))
      (set-window-size! w h)
      (swap! globals/external-state-atom #(assoc % :JFrame frame :JPanel panel))
      (lock-reset! globals/sync-app-atom
             (assoc (empty-state) :app-state (init-state-fn)
              :dispatch dispatch-fn :last-drawn-gfx nil :update-gfx-fn update-gfx-fn :render-fn render-fn))))

(defn stop-app! []
  "Different from quit and very rarely used."
  (if (not= (dissoc @globals/sync-app-atom :evt-queue) (dissoc (empty-state) :evt-queue)) (println "stopping app (IF any app was open)"))
  (launch-app! {} {} (fn [state] []) (fn [state] [])))
