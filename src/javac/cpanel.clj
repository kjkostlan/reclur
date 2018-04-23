; A panel that listenes to inputs and can draw (black background).
; All buttons, etc have to be implemented manually, a level of control useful to gameify the app.

(ns javac.cpanel
  (:require [clojure.string :as string]
    [javac.gfx :as gfx]
    [javac.clojurize :as clojurize])
  (:import [java.awt.event KeyAdapter MouseAdapter WindowEvent]
    [javac JFrameClient] [javax.swing SwingUtilities]
    [java.awt FlowLayout] 
    [javax.swing JFrame JPanel])
  )


;;;;;;;;;;;;;;;;;;;;; Settings ;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *frame-time-ms* 30)
(def ^:dynamic *dispatch-patience-ms* 1000)
(def ^:dynamic *drop-frames-queue-length-threshold* 3)
(def ^:dynamic *low-cpu?* true) ; mousemotion and everyframe events ignored.
(def ^:dynamic *mac-keyboard-kludge?* false) ; A huuuuuuge bug with typing involving accents stealing focus. 
(def ^:dynamic *add-keyl-to-frame?* true) ; tab only works on the frame

;;;;;;;;;;;;;;;;;;;;; Other Support functions ;;;;;;;;;;;;;;;;;;;;;

(defonce _ (println "If you are on a mac don't forget:   defaults write -g ApplePressAndHoldEnabled -bool false"))

(defn empty-state []
  {:evt-queue [] :external-state {:X0 0 :Y0 0 :X1 0 :Y1 0} :upkeep-lock 0 :dispatch-lock 0})

(defn grequel [gr]
  "Black background and monospaced font, added to the beginning of graphics."
  (into [] (concat [[:java (fn [g] (.setFont g (java.awt.Font. "Monospaced" 0 10)))]
             [:fillRect [0 0 3000 3000] {:Color [0.01 0 0 1]}]] gr)))

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

(defn queue1 [x e kwd] 
  "Queues e, but frame events can't stack directly on other frame events or else pileup to infinity."
  (let [e-clj (if (map? e) e (clojurize/translate-event e (.getSource e)))
        e-clj (assoc (dissoc e-clj :ParamString :ID :When) :type kwd)
        evtq (:evt-queue x)
        add? (or (not (= kwd :everyFrame)) 
               (and (not= (:type (last evtq)) :everyFrame)
                 (< (count evtq) *drop-frames-queue-length-threshold*)))]
    (if add? (assoc x :evt-queue (conj evtq e-clj)))))

(defn dequeue1 [x]
  (if-let [evt (first (:evt-queue x))]
    (let [kwd (:type evt)
          extern (:external-state x)
          evt (if (or (= kwd :mouseDragged) (= kwd :mouseMoved))
                (assoc evt :X0 (:X0 extern) :Y0 (:Y0 extern) :X1 (:X1 extern) :Y1 (:Y1 extern)) evt)
          f (get-in x [:listeners kwd])
          new-external (update-external-state evt (:type evt) extern)
          new-state (try (if f (f evt (:app-state x)) (:app-state x))
                      (catch Exception e (println e) (:app-state x)))
          new-queue (into [] (rest (:evt-queue x)))]
     (assoc x :app-state new-state :external-state new-external :evt-queue new-queue)) x))

;;;;;;;;;;;;;;;;;;;;; Mutation ;;;;;;;;;;;;;;;;;;;;;

(defonce one-atom (atom (empty-state)))

(defn dispatch-attempt!! []
  "Attempts to dispatch an event, removing it from the queue.
   run-event won't be called multiple times per queue item (as swap! may do), so it is safe to have side-effects."
  (if (= (:dispatch-lock @one-atom) 0) ; <- This check is probably not needed.
    (let [x (swap! one-atom #(update % :dispatch-lock inc))]
      ; At this point (very lucky) multiple threads will have x at different :dispatch-lock's. But only one will get :dispatch-lock = 1
      (if (and (= (:dispatch-lock x) 1) (> (count (:evt-queue x)) 0)) 
        (reset! one-atom (assoc (dequeue1 x) :dispatch-lock 0))))))

(defn event-queue!! [e kwd]
  (swap! one-atom #(queue1 % e kwd)))

(defn update-graphics!! []
  (let [x @one-atom pnew-gfx ((:render-fn x) (:gfx-precompute x) (:app-state x))
        p (first pnew-gfx) new-gfx (second pnew-gfx)]
    (if-let [panel (:JPanel x)] (gfx/update-graphics! panel (:last-drawn-gfx x) new-gfx))
    (swap! one-atom #(assoc % :last-drawn-gfx new-gfx :gfx-precompute p))))

(defn upkeep-loop!! [update-graphics?]
  "The loop stops unless *low-cpu?* is false or events trigger it."
 (let [t0 (System/nanoTime) ns (* *dispatch-patience-ms* 1e6)]
   (while (and (< (- (System/nanoTime) t0) ns) (> (count (:evt-queue @one-atom)) 0))
     (try (dispatch-attempt!!) (catch Exception e (println "error: " e))))
   (if update-graphics? (update-graphics!!))
   (if (not *low-cpu?*)
     (let [x (swap! one-atom #(update % :upkeep-lock inc))]
       (if (= (:upkeep-lock x) 1)
         (future (Thread/sleep *frame-time-ms*) (swap! one-atom #(assoc % :upkeep-lock 0)) (upkeep-loop!! (not *low-cpu?*))))))))

;;;;;;;;;;;;;;;;;;;;; Java components ;;;;;;;;;;;;;;;;;;;;;

(defn add-mouse-listener! [panel]
  (.addMouseListener panel
    (proxy [MouseAdapter] []
      (mouseClicked [e] (event-queue!! e :mouseClicked) (if *low-cpu?* (upkeep-loop!! true)))
      (mouseEntered [e] (event-queue!! e :mouseEntered) (if *low-cpu?* (upkeep-loop!! true)))
      (mouseExited [e] (event-queue!! e :mouseExited) (if *low-cpu?* (upkeep-loop!! true)))
      (mousePressed [e]
        #_(.requestFocus (:JPanel @one-atom))
        #_(let [frame (.getParent (.getParent (.getParent (.getParent (.getSource e)))))]
          (.requestFocus frame)) 
        (event-queue!! e :mousePressed) 
        (if *low-cpu?* (upkeep-loop!! true)))
      (mouseReleased [e] (event-queue!! e :mouseReleased) (if *low-cpu?* (upkeep-loop!! true)))))
  (.addMouseWheelListener panel
    (proxy [MouseAdapter] []
      (mouseWheelMoved [e] (event-queue!! e :mouseWheelMoved) (if *low-cpu?* (upkeep-loop!! true)))))
  (.addMouseMotionListener panel
    (proxy [MouseAdapter] []
      (mouseMoved [e] (event-queue!! e :mouseMoved) (if *low-cpu?* (upkeep-loop!! false)))
      (mouseDragged [e] (event-queue!! e :mouseDragged) (if *low-cpu?* (upkeep-loop!! true))))))
(defn add-key-listener! [x]
  (.addKeyListener x
    (proxy [KeyAdapter] []
      (keyPressed [e] (event-queue!! e :keyPressed) (if *low-cpu?* (upkeep-loop!! true)))
      (keyReleased [e] (event-queue!! e :keyReleased) (if *low-cpu?* (upkeep-loop!! true))))))

(defn proxy-panel []
  (proxy [javax.swing.JPanel] [] 
    (paintComponent [g]
      (do (proxy-super paintComponent g) 
        (gfx/defaultPaintComponent! g this)))))

(defn new-window []
  "Sets up all listeners and atoms. EDT thread only."
  (let [frame (JFrameClient.);(JFrame. "The app")
        panel (proxy-panel)]
    ; Example from: https://www.javatpoint.com/java-jframe
    (.setLayout panel (FlowLayout.))
    (add-mouse-listener! panel)
    ; Gets tab working: http://www.java2s.com/Code/Java/Event/KeyEventDemo.htm
    (.setFocusTraversalKeysEnabled frame false) 
    (.add frame panel)
    (.setSize frame 1600 1200)
    (.setLocationRelativeTo frame nil)
    (.setVisible frame true)
    (if *add-keyl-to-frame?* (add-key-listener! frame)
      (do (add-key-listener! panel) (.setFocusable panel true) (.requestFocus panel)))
    [frame panel]))

(defn swing-or-kludge [f]
  (if *mac-keyboard-kludge?* (f)
    (SwingUtilities/invokeAndWait f)))

(defn launch-app!! [init-state evt-fns render-fn]
  "app is singleton, launching 
   evt-fns including :everyFrame are (f evt-clj state), we make our own every-frame event.
   render-fn is (f precompute state-clj) but some render commands are functions on the java object."
  (if *low-cpu?* (println "Low CPU mode, less mouse moves and no animations."))
   ; https://nelsonmorris.net/2015/05/18/reloaded-protocol-and-no-implementation-of-method.html
   ; Reload the implementation of the protocol. Other languages must go here.
   ; This is very messy and protocols may be abandoned all together.
   (require '[coder.lang.clojure :reload true])  
  (swing-or-kludge (fn [& args]
      ;https://stackoverflow.com/questions/1234912/how-to-programmatically-close-a-jframe
      (if-let [old-frame (:JFrame @one-atom)]
        (do (.dispatchEvent old-frame (WindowEvent. old-frame WindowEvent/WINDOW_CLOSING)))
          (swap! one-atom #(dissoc % :JFrame)))
      (let [frame-panel (new-window)
            frame (first frame-panel) panel (second frame-panel)]
        (reset! one-atom
          (assoc (empty-state)
             :app-state init-state :listeners evt-fns :last-drawn-gfx nil :render-fn render-fn
             :external-state {}
             :evt-queue []
             :JFrame frame :JPanel panel))
  ))
  ))

(defn stop-app!! []
  (if (not= @one-atom (empty-state)) (println "stopping app"))
  (launch-app!! {} {} (fn [precompute state] [])))
