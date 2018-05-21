; A panel that listenes to inputs and can draw (black background).
; All buttons, etc have to be implemented manually, a level of control useful to gameify the app.

(ns javac.cpanel
  (:require [clojure.string :as string] [globals]
    [javac.gfx :as gfx] [javac.exception :as exception]
    [javac.clojurize :as clojurize]
    globals
    [app.orepl :as orepl]
    [app.iteration :as iteration])
  (:import [java.awt.event KeyAdapter MouseAdapter WindowEvent]
    [javax.swing SwingUtilities]
    [java.awt FlowLayout] 
    [javax.swing JFrame JPanel])
  )


;;;;;;;;;;;;;;;;;;;;; Settings ;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *frame-time-ms* 30) ; a somewhat complex mechanism to ensure graceful degradation when the events are heavy.
(def ^:dynamic *drop-frames-queue-length-threshold* 3)
(def ^:dynamic *low-cpu?* true) ; mousemotion and everyframe events ignored.
(def ^:dynamic *mac-keyboard-kludge?* false) ; A huuuuuuge bug with typing involving accents stealing focus. 
(def ^:dynamic *add-keyl-to-frame?* true) ; tab only works on the frame. False goes to panel.

;;;;;;;;;;;;;;;;;;;;; Other Support functions ;;;;;;;;;;;;;;;;;;;;;

(defonce _ (println "If you are on a mac don't forget:   defaults write -g ApplePressAndHoldEnabled -bool false"))

(defn empty-state []
  {:evt-queue [] :external-state {:X0 0 :Y0 0 :X1 0 :Y1 0}})

(defn grequel [gr]
  "Black background and monospaced font, added to the beginning of graphics."
  (into [] (concat [[:java (fn [g] (.setFont g (java.awt.Font. "Monospaced" 0 10)))]
             [:fillRect [0 0 3000 3000] {:Color [0.01 0 0 1]}]] gr)))

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

(defn queue1 [x e kwd] 
  "Queues e, but frame events can't stack directly on other frame events or else pileup to infinity.
   Handles both java and clojure datastructures for e, converting e into clojure if it is a java event."
  (let [e-clj (if (map? e) e (clojurize/translate-event e (.getSource e)))
        e-clj (assoc (dissoc e-clj :ParamString :ID :When) :type kwd)
        evtq (:evt-queue x)
        add? (or (not (= kwd :everyFrame)) 
               (and (not= (:type (last evtq)) :everyFrame)
                 (< (count evtq) *drop-frames-queue-length-threshold*)))]
    ;(if add? (println "Adding: " kwd "old queue len" (count evtq) "new queue len: " (count (conj (into [] evtq) e-clj))))
    (if add? (assoc x :evt-queue (conj (into [] evtq) e-clj)) x)))

(defn dequeue1 [x] ; returns the modified keys only. Doesn't modify the evt queue, must do that in a swap.
  (if-let [evt (first (:evt-queue x))]
    (let [kwd (:type evt)
          extern (:external-state x)
          evt (if (or (= kwd :mouseDragged) (= kwd :mouseMoved))
                (assoc evt :X0 (:X0 extern) :Y0 (:Y0 extern) :X1 (:X1 extern) :Y1 (:Y1 extern)) evt)
          ;_ (println "Removing: " kwd "old queue len" (count (:evt-queue x)) "new queue len: " (count (into [] (rest (:evt-queue x)))))
          f (get-in x [:listeners kwd])
          new-external (update-external-state evt (:type evt) extern)
          new-state (try (if f (f evt (:app-state x)) (:app-state x))
                      (catch Exception e (println e) (:app-state x)))
          ud? (or (:needs-gfx-update? x) (not *low-cpu?*) (not= kwd :mouseMoved)) ud? true]
     {:app-state new-state :external-state new-external :needs-gfx-update? ud?}) {}))

;;;;;;;;;;;;;;;;;;;;; Mutation ;;;;;;;;;;;;;;;;;;;;;

(def one-atom globals/one-atom)
(defonce _ (reset! one-atom (empty-state)))

(defn update-graphics!! []
  (let [x @one-atom 
        gfx-updated-app-state (try ((:update-gfx-fn x) (:app-state x)) (catch Exception e (do (println "gfx update error") (:app-state x))))
        new-gfx (try ((:render-fn x) gfx-updated-app-state) (catch Exception e (do (println "gfx render error.") {})))]
    (if-let [panel (:JPanel x)] (gfx/update-graphics! panel (:last-drawn-gfx x) new-gfx))
    (swap! one-atom #(assoc % :last-drawn-gfx new-gfx :app-state gfx-updated-app-state :needs-gfx-update? false))))

(defn dispatch-loop!! []
  "Keeps dispatching events from the atom. This function is NOT thread safe, the price we pay for ensuring dequeue1 only is called once per event.
   Note that dequeue1 will sometimes have side-effects and/or be expensive."
  (while (> (count (:evt-queue @one-atom)) 0)
    (let [x @one-atom xd (try (dequeue1 x) (catch Exception e (do (println "dequeue error") x)))]
      (swap! one-atom #(let [x1 (merge % xd) evq (:evt-queue %)]
                        (assoc x1 :evt-queue (into [] (rest evq)))))))) ; O(n) but should stay small.

; Continuously polling is a tiny CPU drain, but it is way easier than a lock system that must ensure we don't get overlapping calls to dispatch-loop!!
; The idle CPU is 0.6% of one core for the total program (tested May 19th 2018).
(defonce _polling? (atom false)) ; only used to make sure we don't have multiple calls to this file, an extra safeguard.
(if (not @_polling?)
  (future (while true (do (Thread/sleep *frame-time-ms*) (dispatch-loop!!) (if (:needs-gfx-update? @one-atom) (update-graphics!!))))))
(reset! _polling? true)

(defn event-queue!! [e kwd]
  (swap! one-atom #(queue1 % e kwd)))

;;;;;;;;;;;;;;;;;;;;; Java listeners and windowing ;;;;;;;;;;;;;;;;;;;;;

(defn add-mouse-listeners! [panel]
  (.addMouseListener panel
    (proxy [MouseAdapter] []
      (mouseClicked [e] (event-queue!! e :mouseClicked))
      (mouseEntered [e] (event-queue!! e :mouseEntered))
      (mouseExited [e] (event-queue!! e :mouseExited))
      (mousePressed [e]
        #_(.requestFocus (:JPanel @one-atom))
        #_(let [frame (.getParent (.getParent (.getParent (.getParent (.getSource e)))))]
          (.requestFocus frame)) 
        (event-queue!! e :mousePressed))
      (mouseReleased [e] (event-queue!! e :mouseReleased))))
  (.addMouseWheelListener panel
    (proxy [MouseAdapter] []
      (mouseWheelMoved [e] (event-queue!! e :mouseWheelMoved))))
  (.addMouseMotionListener panel
    (proxy [MouseAdapter] []
      (mouseMoved [e] (event-queue!! e :mouseMoved))
      (mouseDragged [e] (event-queue!! e :mouseDragged)))))

(defn add-key-listeners! [x]
  (.addKeyListener x
    (proxy [KeyAdapter] []
      (keyPressed [e] (event-queue!! e :keyPressed))
      (keyReleased [e] (event-queue!! e :keyReleased)))))

(defn add-parentin-listener! [] 
  "Adds a listener for stdin."
  (future
    (while [true]
      (let [s (try (iteration/get-input) (catch Exception e (do (Thread/sleep 1000) (str "ERROR in iteration/get-input:\\n" (orepl/pr-error e)))))] ; waits here until the stream has stuff in it.
        (SwingUtilities/invokeLater #(event-queue!! {:contents s} :parent-in)))))) ; all evts are launched on the edt thread.
(if (globals/are-we-child?) (add-parentin-listener!))

(defn proxy-panel []
  (proxy [javax.swing.JPanel] [] 
    (paintComponent [g]
      (do (proxy-super paintComponent g) 
        (gfx/defaultPaintComponent! g this)))))

(defn new-window []
  "Sets up all listeners and atoms. EDT thread only."
  (let [frame (JFrame.);(JFrame. "The app")
        panel (proxy-panel)]
    ; Example from: https://www.javatpoint.com/java-jframe
    (.setLayout panel (FlowLayout.))
    (add-mouse-listeners! panel)
    ; Gets tab working: http://www.java2s.com/Code/Java/Event/KeyEventDemo.htm
    (.setFocusTraversalKeysEnabled frame false) 
    (.add frame panel)
    (.setSize frame 1600 1200)
    (.setLocationRelativeTo frame nil)
    (.setVisible frame true)
    (if *add-keyl-to-frame?* (add-key-listeners! frame)
      (do (add-key-listeners! panel) (.setFocusable panel true) (.requestFocus panel)))
    [frame panel]))

(defn swing-or-kludge [f]
  (if *mac-keyboard-kludge?* (f)
    (SwingUtilities/invokeAndWait f)))

(defn launch-app!! [init-state evt-fns update-gfx-fn render-fn]
  "app is singleton, launching 
   evt-fns including :everyFrame are (f evt-clj state), we make our own every-frame event.
   update-gfx-fn is (f state-clj), returns the new state, and should cache any expensive gfx cmds.
   render-fn is (f state-clj) but some render commands are functions on the java object."
  (if *low-cpu?* (println "Low CPU mode, less mouse moves and no animations.")) 
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
                      :listeners evt-fns :last-drawn-gfx nil :update-gfx-fn update-gfx-fn :render-fn render-fn 
                      :external-state {}
                      :evt-queue []
                      :JFrame frame :JPanel panel))))))

(defn stop-app!! []
  (if (not= @one-atom (empty-state)) (println "stopping app"))
  (launch-app!! {} {} (fn [state] []) (fn [state] [])))
