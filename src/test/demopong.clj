; What does a game written in a functional programming paradigm look like?

(ns test.demopong)

;^:global (fn [s] (mapv load ["../test/demogui" "../test/demopong"]) ((resolve 'test.demogui/add-box) s ((resolve 'test.demopong/pong-game))))))

(defn pong-newgame [sz]
  (let [sizex (first sz) starty 100 mass-paddle 3 mass-ball 1 paddle-margin 50
        sizex0 (- sizex paddle-margin)]
    {:score [0.0 0.0] :paddles [[50 starty 0 0 mass-paddle] [sizex0 starty 0 0 mass-paddle]]
     :paddle-margin paddle-margin
     :ai-hardness 1.0 :game-over? false :hi-score 1.0
     :max-score-diff 1024
     :paddle-setpoints [[paddle-margin starty 0 0 1e100] [sizex0 starty 0 0 1e100]]
     :ball [(* sizex 0.5) starty 100 25 mass-ball]
     :farlands [[-12.5e6 0 0 0 1e100] [0 12.5e6 0 0 1e100]]}))
(defn pong-physics [game sz dt]
  "The ball and paddles are electrically charged."
  (let [k-elec 160000
        k-paddle 30.0 damp-paddle 5.0
        score-speed 1.0
        km-goal 10000.0
        top-bottom-elastic 0.9
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
                       (assoc-in state path r1)))
        dv-goal1 (* dt (/ km-goal (let [dx (max 1 (first (:ball game)))] (* dx dx))))
        dv-goal2 (* dt (/ km-goal (let [dx (max 1 (- (first sz) (first (:ball game))))] (* dx dx))))]
    (-> game ; update velocity first is better.
      (assoc-in [:paddle-setpoints 1 0] (- (first sz) (:paddle-margin game)))
      (update-v [:paddles 0] [:ball] (- k-elec) 0.0 -2.0)
      (update-v [:paddles 1] [:ball] (- (* k-elec (:ai-hardness game))) 0.0 -2.0)
      (update-in [:ball 2] #(+ % (- dv-goal1 dv-goal2)))
      (update-in [:score 0] #(+ % (* score-speed dv-goal2)))
      (update-in [:score 1] #(+ % (* score-speed dv-goal1)))
      (update :ball #(if (and (<= (second %) 0)
                           (< (get % 3) 0)) (assoc % 3 (* (get % 3) (- top-bottom-elastic))) %))
      (update :ball #(if (and (>= (second %) (second sz))
                           (> (get % 3) 0)) (assoc % 3 (* (get % 3) (- top-bottom-elastic))) %))
      (update-v [:paddles 0] [:paddle-setpoints 0] k-paddle 0.0 1.0)
      (update-v [:paddles 1] [:paddle-setpoints 1] k-paddle 0.0 1.0)
      (update-v [:paddles 0] [:farlands 0] 0.0 damp-paddle 1.0)
      (update-v [:paddles 1] [:farlands 0] 0.0 damp-paddle 1.0)
      (update-v [:paddles 0] [:farlands 1] 0.0 damp-paddle 1.0)
      (update-v [:paddles 1] [:farlands 1] 0.0 damp-paddle 1.0)
      (update-pos [:paddles 0]) (update-pos [:paddles 1])
      (update-pos [:ball]))))
(defn pong-ai [game dt]
  (let [lookahead 1.0
        max-speed 35.0
        lazy-speed-when-far 25.0

        ballx (get-in game [:ball 0])
        bally (get-in game [:ball 1])
        ballvx (get-in game [:ball 2])
        ballvy (get-in game [:ball 3])
        paddlex (get-in game [:paddle-setpoints 1 1])

        t-to-hit (/ (- paddlex ballx) (if (= ballvx 0) 1e-100 ballvx))
        max-dy (* (if (and (> ballvx 0.0) (< t-to-hit 3.0)) max-speed lazy-speed-when-far) dt)

        targety (if (and (> ballvx 0.01) (< ballx paddlex))
                  (+ bally (* lookahead ballvy t-to-hit)) bally)]
        (update-in game [:paddle-setpoints 1 1]
          #(if (< % targety)
             (min targety (+ % max-dy))
             (max targety (- % max-dy))))))
(defn pong-human [game mouse-evt]
  (assoc-in game [:paddle-setpoints 0 1] (:Y mouse-evt)))
(defn pong-gamelogic [game dt]
  (let [rate 0.01 margin (+ (:max-score-diff game) (first (:score game)) (- (second (:score game))))
        game (if (< margin 0.0) (assoc game :game-over? true :hi-score (max (:hi-score game) (:ai-hardness game))) game)]
    (update game :ai-hardness #(+ % (* % rate dt)))))
(defn pong-everyframe [game sz]
  ;(Thread/sleep 500) ; What happens if game logic is slow?
  (let [frame-dt 0.03 ;Should be 1/1000 of javac.cpanel/*frame-time-ms*
        substeps 12 speedup-tweak 4.0
        dt (* (/ frame-dt substeps) speedup-tweak)
        take-substep #(-> % (pong-physics sz dt) (pong-ai dt) (pong-gamelogic dt))]
    (if (:game-over? game) game (reduce (fn [acc _] (take-substep acc)) game (range substeps)))))
(defn pong-render-ball [game]
  (let [r 10
        ballx (get-in game [:ball 0])
        bally (get-in game [:ball 1])]
    [[:drawOval [(- ballx r) (- bally r) (* r 2) (* r 2)] {:Color [1 1 0.1 1.0]}]
     [:drawLine [(- ballx r) (- bally r) (+ ballx r) (+ bally r)] {:Color [1 0.6 0.1 1.0]}]
     [:drawLine [(+ ballx r) (- bally r) (- ballx r) (+ bally r)] {:Color [1 0.6 0.1 1.0]}]]))
(defn pong-render-paddle [game ix]
  (let [wr 3 hr (* 12 (if (= ix 0) 1.0 (Math/sqrt (:ai-hardness game))))
        paddlex (get-in game [:paddles ix 0])
        paddley (get-in game [:paddles ix 1])]
    [[:fillRect [(- paddlex wr) (- paddley hr) (* wr 2) (* hr 2)] {:Color [0.0 (+ 0.2 (* ix 0.25)) 1.0 1.0]}]]))
(defn pong-render-score [game sz]
  (let [num2str #(let [s (str (Math/round %))]
                   (if (> (count s) 3) (str (subs s 0 (- (count s) 3)) "," (subs s (- (count s) 3))) s))
        font 25 texty 30 sc0 (first (:score game)) sc1 (second (:score game))
        margin (+ (:max-score-diff game) sc0 (- sc1))]
    (if (:game-over? game)
      [[:drawString [(str "Game over, AI cheat level: " (num2str (dec (:ai-hardness game)))
                       " Hi-score: " (num2str (dec (:hi-score game))) " enter to restart") 20 texty]
        {:Color [0.9 0.7 0.8 1.0] :FontSize (* font 0.75)}]]
    [[:drawString [(num2str sc0) 20 texty] {:Color [0.9 0.0 0.8 1.0] :FontSize font}]
     [:drawString [(str "Lives: " (num2str margin)) (* (first sz) 0.375) texty] {:Color [0.8 0.7 0.9 1.0] :FontSize font}]
     [:drawString [(num2str sc1) (- (first sz) 100) texty] {:Color [0.0 1.0 0.0 1.0] :FontSize font}]])))
(defn pong-render-outline [sz]
  (c/vcat
    [[:fillRect [0 0 (first sz) (second sz)] {:Color [0 0 0.2 0.6]}]]
    (mapv (fn [x] [:drawRect [x x (- (first sz) x x) (- (second sz) x x)] {:Color [0.6 0.3 0.4 1.0]}])
            [0 3 6 9])))
(defn pong-render [box focus?]
  "Pong rendering."
  ;(Thread/sleep 500) ; What happens if rendering is slow?
  (let []
    (c/vcat (pong-render-outline (:size box))
      (pong-render-ball (:game box))
      (pong-render-paddle (:game box) 0)
      (pong-render-paddle (:game box) 1)
      (pong-render-score (:game box) (:size box)))))
(defn pong-game []
  "Simple graphics and an animation loop."
  (let [box {:position [50 20] :size [800 600] :z 10} nu-game (pong-newgame (:size box))
        dispatch {:mouseMoved (fn [evt box] (update box :game #(pong-human % evt)))
                  :everyFrame (fn [evt box] (update box :game #(pong-everyframe % (get box :size [800 600]))))
                  :keyPressed (fn [evt box] (if (= (:KeyCode evt) 10)
                                              (assoc box :game (assoc nu-game :hi-score (:hi-score (:game box)))) box))}]
    (assoc box :game nu-game :dispatch dispatch :render (fn [box focus?] (pong-render box focus?)))))