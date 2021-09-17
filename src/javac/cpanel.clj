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

(def ^:dynamic *add-keyl-to-frame?* true) ; tab only works on the frame. False goes to panel.
(def ^:dynamic *frame-time-ms* 30) ; a somewhat complex mechanism to ensure graceful degradation when the events are heavy.
(def ^:dynamic *nframe-slowdown-when-idle* 15) ; Save CPU when nothing is requesting an every frame event.
(def ^:dynamic *wait-per-event-ms-fn* (fn [queue-length] (int (* queue-length queue-length 5.0)))) ; Slow down the edt and the loop if there are two many events.

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

(defn queue1 [app e-clj kwd]
  "Queues e, with reasonable limits to prevent everyFrame events piling-up to infinity.
   Handles both java and clojure datastructures for e, converting e into clojure if it is a java event."
  (let [_ (if (not (map? e-clj)) (throw (Exception. "all events need to be converted to clojure maps for queue1.")))
        e-clj (assoc (dissoc e-clj :ParamString :ID :When) :type kwd)
        evtq (:evt-queue app)]
    (assoc app :evt-queue (conj evtq e-clj))))

(defn dispatch [app e-clj]
  "Processes one event on x. Returns x if evt is nil.
   Does not affect the event queue."
  (if e-clj
    (let [kwd (:type e-clj)
          _ (if (= kwd :mouseWheelMoved)
              (do (crossp/update-inertial-scroll-guess! e-clj)
                (swap! globals/external-state-atom
                  #(let [sc-h (get % :mouse-wheel-history [])
                         sc-h (if (> (count sc-h) 255) (into [] (rest sc-h)) sc-h)]
                     (assoc % :mouse-wheel-history (conj sc-h e-clj))))))
          extern @globals/external-state-atom
          e-clj (if (or (= kwd :mouseDragged) (= kwd :mouseMoved))
                  (assoc e-clj :X0 (:X0 extern) :Y0 (:Y0 extern) :X1 (:X1 extern) :Y1 (:Y1 extern)) e-clj)
          _ (swap! globals/external-state-atom #(update-external-state e-clj (:type e-clj) %))
          f (:dispatch app)
          new-state (try (if f (f e-clj (:app-state app)) (:app-state app))
                      (catch Exception err (println err) (:app-state app)))
          ud? true] ; TODO: better gfx update rules.
     (assoc app :app-state new-state :needs-gfx-update? ud?)) app))

(defn dispatch-all [app]
  "Keeps dispatching events from x until it's queue is empty."
  (if (empty? (:evt-queue app)) app
    (let [evts-unsorted (into [] (:evt-queue app))
          evts (sort-by #(get % :Time 0) evts-unsorted); Don't know if this is necessary
          app1 (reduce (fn [app-acc e-clj]
                       (try (dispatch app-acc e-clj)
                         (catch Exception err (do (println "dequeue error:" err) app-acc)))) app evts)]
      (assoc app1 :evt-queue (c/queue)))))

(defn give-the-queue-a-breather []
  "We don't want to let the queue get too long. However, we can't wait for it to finish because of the risk of edt deadlocks.
   So we wait quadratically longer and longer as the queue grows leading us to an equilibrium eventually."
  (let [n (count (:evt-queue @globals/app-agent))
        ms (*wait-per-event-ms-fn* n)]
    (Thread/sleep ms)))

;;;;;;;;;;;;;;;;;;;;; Mutation ;;;;;;;;;;;;;;;;;;;;;

(defn set-window-size! [w h]
  "Should be called every time the window is resized."
  (swap! globals/external-state-atom #(assoc % :window-size [w h])))

(defn update-graphics! [app]
  (let [udgfx (:update-gfx-fn app)]
    (if udgfx
      (let [gfx-updated-app-state (try (udgfx (:app-state app)) (catch Exception err (do (println "app gfx update error:" err) (:app-state app))))
            new-gfx (try ((:render-fn app) gfx-updated-app-state) (catch Exception err (do (println "gfx render error." err) [])))
            success? (atom false)]
        (if-let [panel (:JPanel @globals/external-state-atom)]
          (try (do (gfx/update-graphics! panel (:last-drawn-gfx app) new-gfx) (reset! success? true))
            (catch Exception err (println "gfx/update-graphics! error:" err))))
        (if @success? (assoc app :last-drawn-gfx new-gfx :app-state gfx-updated-app-state :needs-gfx-update? false) app)) app)))

(defn evt2clj [e]
 "Must be called on the EDT thread if e is a java event."
  (if (map? e) e
    (try (clojurize/translate-event e (.getSource e))
    (catch Exception err
      (do (println "Evt failed to be clojurized and will be dropped: " err)
        (throw err))))))

(defn event-queue-core [app e kwd]
  (let [e-clj (evt2clj e)
        e-clj (assoc e-clj :Time (System/currentTimeMillis) :type kwd)]
    (queue1 app e-clj kwd)))
(defn event-queue! [e kwd]
  (give-the-queue-a-breather)
  (let [e-clj (evt2clj e)]
    (send globals/app-agent #(event-queue-core % e-clj kwd))))

(defn store-window-size! [^JFrame frame]
  (let [^java.awt.Dimension sz (.getSize (.getContentPane frame))]
    (set-window-size! (.getWidth sz) (.getHeight sz))))

; Continuously polling is a (tiny) CPU drain, but it is way easier than a lock system that must ensure we don't get overlapping calls to dispatch-loop!
(defn every-frame-loop! [app]
  (let [s (:app-state app)
        t (:frames-since-app-start
            (swap! globals/external-state-atom #(assoc % :frames-since-app-start (inc (get % :frames-since-app-start 0)))))
        add-frame-evt? (or (> (count (:hot-boxes s)) 0) ; A single empty hot box adds about 1.25% CPU, tripling our CPU usage.
                         (= (mod t *nframe-slowdown-when-idle*) 0))
        app1 (if add-frame-evt?
                (jthread/swing-wait (event-queue-core app {} :everyFrame)) app)
        app2 (dispatch-all app1)]
    (give-the-queue-a-breather)
    (if (:needs-gfx-update? app2) (update-graphics! app2) app2)))

(if (not (:main-polling-loop? @globals/external-state-atom))
  (future
    (loop [last-millis -1e100] ; This loop runs until the application is quit.
        (let [elapsed-millis (max 0 (- (System/currentTimeMillis) last-millis))
              sleep-time (- *frame-time-ms* elapsed-millis)]
          (if (> sleep-time 0) (Thread/sleep *frame-time-ms*)))
        (await globals/app-agent)
        (send globals/app-agent every-frame-loop!)
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
    (crossp/add-quit-request-listener! (fn [e] (event-queue! {} :quit)))
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
      (let [app0 (assoc (empty-state) :app-state (init-state-fn)
                   :dispatch dispatch-fn :last-drawn-gfx nil :update-gfx-fn update-gfx-fn :render-fn render-fn)]
        (send globals/app-agent (fn [_] app0)))))

