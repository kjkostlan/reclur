; GUI demos, so it is more clear how to get the gui changed.

(ns test.demogui
  (:require [globals]
    [layout.blit :as blit]
    [app.siconsole :as siconsole]
    [app.rtext :as rtext]
    ;[coder.plurality :as plurality]
    [layout.blit :as blit]
    [clojure.set :as set]))

(defn add-box! [x]
  "MAIN DEMO FN: Adds some component x to the global singleton app state.
   If x has logic for events and rendering it can be interacted with.
   These functions can even modify the state."
  (swap! globals/one-atom
    #(assoc-in % [:app-state :components (gensym "Demobox")] x)) "Box was added to global app state.")
 
(defn set-simple-rtext [box txt]
  "Rtext has multible :pieces for the purposes of selective display of data.
   We only use one piece."
  (assoc box :pieces [{:text txt}]))   
 
(defn light-dispatch-reporter []
  "This box reports and prints dispatches it gets, these are light dispatches that dont make as much use of the API."
  (let [box0 (set-simple-rtext (assoc rtext/empty-text :position [50 20] :size [800 600] :z 10)
               "No event happened to this box yet.")
        
        report1 (fn [evt box] 
                  (let [evt-str (apply str (interpose "\n" (mapv #(str %1 " " %2) (keys evt) (vals evt))))]
                    (set-simple-rtext box (str "Type of event " (:type evt) "\nEvent: \n" evt-str))))
        box1 (assoc box0 :dispatch report1 :render rtext/render)]
    box1))

(defn hot-dispatch-reporter []
  "Mouse moves and animations need special keys in the dispatch. This saves CPU when they are not in use."
  (let [box0 (light-dispatch-reporter)
        report1 (fn [ty evt box]
                  (let [evt-str (apply str (interpose "\n" (mapv #(str %1 " " %2) (keys evt) (vals evt))))]
                    (set-simple-rtext box (str "Type of event " ty "\nEvent: \n" evt-str))))
        dispatch {:mouseMoved (fn [evt box] (report1 :mouseMoved evt box))
                  :everyFrame (fn [evt box] (report1 :everyFrame evt box))}]
    (assoc box0 :dispatch dispatch :render rtext/render)))

(defn heavy-dispatch-reporter []
  "Heavy dispatches allow the component to modify the app state when it recieves events.
   Args: evt state0 state name-of-us.
     State0 is before running light dispatches, state is after. 
   Output: The modified state."
  (let [box0 (set-simple-rtext (assoc rtext/empty-text :position [50 20] :size [800 600] :z 10)
               "Click here to move all other boxes to the right.")
        move-fn (fn [evt state0 state our-name]
                  (if (= (:type evt) :mousePressed) 
                    (let [boxes (:components state)
                          kys (set/difference (set (keys boxes)) #{our-name})
                          +x (fn [box] (update-in box [:position 0] #(+ % 16)))
                          boxes1 (reduce #(update %1 %2 +x) boxes kys)]
                      (assoc state :components boxes1)) state))]
    (assoc box0 :dispatch-heavy move-fn :render rtext/render)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pong game: Real time interacting ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pong-newgame []
  (let [sizex 600 starty 100 mass-paddle 3 mass-ball 1]
    {:score [0 0] :paddles [[50 starty 0 0 mass-paddle] [sizex starty 0 0 mass-paddle]] 
     :paddle-setpoints [[50 starty 0 0 1e100] [sizex starty 0 0 1e100]] :ball [(* sizex 0.5) starty 100 0 mass-ball]}))
(defn pong-physics [game dt]
  "The ball and paddles are electrically charged."
  (let [k-elec 40000
        k-paddle 10 damp-paddle 1.5
        update-v (fn [state path1 path2 k-attract damp pow]
                   (let [r1 (get-in state path1) r2 (get-in state path2) ;x,y,vx,vy,m
                         dx (- (nth r2 0) (nth r1 0)) dy (- (nth r2 1) (nth r1 1))
                         dvx (- (nth r2 2) (nth r1 2)) dvy (- (nth r2 3) (nth r1 3))
                         m1 (nth r1 4) m2 (nth r2 4)
                         
                         dist (+ (Math/sqrt (+ (* dx dx) (* dy dy))) 1e-100)
                         force-mag (* k-attract (Math/pow dist pow))
                         force-x (* force-mag (/ dx dist)) force-y (* force-mag (/ dy dist))
                         diverge-v (+ (* dvx (/ dx dist)) (* dvy (/ dy dist)))
                         damp-x (* diverge-v damp (/ dx dist)) damp-y (* diverge-v damp (/ dy dist))
                         new-r1 (update (update r1 2 #(+ % (* (+ force-x damp-x) (/ m1) dt))) 
                                  3 #(+ % (* (+ force-y damp-y) (/ m1) dt)))
                         new-r2 (update (update r2 2 #(- % (* (+ force-x damp-x) (/ m2) dt))) 
                                  3 #(- % (* (+ force-y damp-y) (/ m2) dt)))]
                     (-> state (assoc-in path1 new-r1) (assoc-in path2 new-r2))))
        update-pos (fn [state path]
                     (let [r (get-in state path)
                           vx (nth r 2) vy (nth r 3)
                           r1 (-> r (update 0 #(+ % (* dt vx))) (update 1 #(+ % (* dt vy))))]
                       (assoc-in state path r1)))]
    (-> game ; update velocity first is better.
      (update-v [:paddles 0] [:ball] (- k-elec) 0.0 -2.0)
      (update-v [:paddles 1] [:ball] (- k-elec) 0.0 -2.0)
      (update-v [:paddles 0] [:paddle-setpoints 0] k-paddle damp-paddle 1.0)
      (update-v [:paddles 1] [:paddle-setpoints 1] k-paddle damp-paddle 1.0)
      (update-pos [:paddles 0]) (update-pos [:paddles 1])
      (update-pos [:ball]))))
(defn pong-ai [game dt]
  (let [lookahead 0.75
        max-speed 250.0
        max-dy (* max-speed dt)
        
        ballx (get-in game [:ball 0])
        bally (get-in game [:ball 1])
        ballvx (get-in game [:ball 2])
        ballvy (get-in game [:ball 3])
        ourx (get-in game [:paddle-setpoints 1 1])
        targety (if (and (> ballvx 0.01) (< ballx ourx)) 
                  (let [t-to-hit (/ (- ourx ballx) ballvx)]
                    (+ bally (* lookahead ballvy t-to-hit)) 0)
                  bally)]
        (update-in game [:paddle-setpoints 1 1]
          #(if (< % targety)
             (min targety (+ % max-dy))
             (max targety (- % max-dy))))))

(defn pong-render-ball [game]
  (let [r 10
        ballx (get-in game [:ball 0])
        bally (get-in game [:ball 1])]
    [[:drawOval [(- ballx r) (- bally r) (* r 2) (* r 2)] {:Color [1 1 0.1 1.0]}]
     [:drawLine [(- ballx r) (- bally r) (+ ballx r) (+ bally r)] {:Color [1 0.6 0.1 1.0]}]
     [:drawLine [(+ ballx r) (- bally r) (- ballx r) (+ bally r)] {:Color [1 0.6 0.1 1.0]}]]))
(defn pong-render-paddle [game ix]
  (let [wr 3 hr 12
        paddlex (get-in game [:paddles ix 0])
        paddley (get-in game [:paddles ix 1])]
    [[:fillRect [(- paddlex wr) (- paddley hr) (* wr 2) (* hr 2)] {:Color [0.0 (+ 0.2 (* ix 0.25)) 1.0 1.0]}]]))
(defn pong-render-score [game]
  [])
(defn pong-render-fn [box focus?]
  "Pong rendering."
  (let [draw-outline (fn [box] 
                       (mapv (fn [x] 
                               [:drawRect [x x (- (first (:size box)) x x) (- (second (:size box)) x x)]
                                {:Color [0.6 0.3 0.4 1.0]}]) 
                         [0 3 6 9]))]
    (collections/vcat (draw-outline box)
      (pong-render-ball (:game box))
      (pong-render-paddle (:game box) 0)
      (pong-render-paddle (:game box) 1)
      (pong-render-score (:game box)))))

(defn pong-human [game mouse-evt]
  (assoc-in game [:paddle-setpoints 0 1] (:Y mouse-evt)))
(defn pong-everyframe [game]
  (let [frame-dt 0.03 ;Should be 1/1000 of javac.cpanel/*frame-time-ms*
        substeps 12 speedup-tweak 2.0
        dt (* (/ frame-dt substeps) speedup-tweak)
        take-substep #(-> % (pong-physics dt) (pong-ai dt))]
    (reduce (fn [acc _] (take-substep acc)) game (range substeps))))
(defn pong-game []
  "Simple graphics and an animation loop."
  (let [box {:position [50 20] :size [800 600] :z 10}
        new-game {:score [0 0]}
        dispatch {:mouseMoved (fn [evt box] (update box :game #(pong-human % evt)))
                  :everyFrame (fn [evt box] (update box :game pong-everyframe))}]
    (assoc box :game (pong-newgame) :dispatch dispatch :render (fn [box focus?] (pong-render-fn box focus?)))))
