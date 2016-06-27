; (do (require '[clooj.gui.textzone :as textzone]) (textzone/debug))
(ns clooj.gui.textzone
  (:require [clooj.java.gui :as gui] [clooj.java.clipboard :as clipboard])
  (:import (java.awt Canvas Dimension Font FontMetrics Color BasicStroke)
           (javax.swing JFrame JScrollPane)))

; All we have is a single JPanel, which simulates scrollpanes.
; Our extra stuff we use is:
; :text = the stuff saved or loaded in files, VECTOR format, use (apply str (:text state)) to get the string.
; :scale-fac = vector of sizes for each char. Code-folding reduces it, 1 is unfolded.
; :cursor = location of the cursor, curser = n means typing leaves the first n chars alone.
; :select = sequence of chars that are selected.
; TODO: linking.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support functions for working with vectors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn modify-vec 
  "Modifies both our 1:1 vectors the same way, kword? true means we pass the keyword as a second arg to f."
  ([state f] (modify-vec state f false))
  ([state f kword?]
    (let [f1 (fn [s k] (if kword? (f s k) (f s)))]
      (assoc state :text (f1 (:text state) :text) :scale-fac (f1 (:scale-fac state) :scale-fac) :repaint? true))))

(defn vcat [& args]
  "Vector concatination that gives us a fresh vector."
  (into [] (into-array (apply concat args)))) ; into-array due to sub-vec issues gaurentee no memory leak (is it nessessary?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support functions for color, string calculations, locations, etc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn isedge [strng]
  "Returns a vector that marks whehther we are on the edge that would get removed when we trim"
  ; It would be useful to have a string library that keeps track of indexes.
  (let [iswhite (map #(or (= % \space) (= % \tab)) (into [] strng))
        n (count strng) o (into [] (repeat n false))
        o1 (loop [acc o ix 0]
             (if (or (>= ix n) (not (nth iswhite ix))) acc
                (recur (assoc acc ix true) (inc ix))))
        o2 (loop [acc o1 ix (dec n)]
             (if (or (< ix 0) (not (nth iswhite ix))) acc
                (recur (assoc acc ix true) (dec ix))))] (into [] o2)))

(defn get-selected-indicator [ta]
  (let [f #(repeat % false) t #(repeat % true)
        sub0 (.getSelectionStart ta) sub1 (.getSelectionEnd ta)
        n (count (.getText ta))]
    (into [] (concat (f sub0) (t (- sub1 sub0)) (f (- n sub1))))))

(defn hsv2rgb [hsv]
 ; 0-1 scale.
  (let [h (* (first hsv) 6) k (int (Math/floor h)) s (second hsv) v (nth hsv 2)
        p0 (- h k) t (- 1 s) n (- 1 (* s p0)) p (- 1 (* s (- 1 p0)))]

    (if (or (= k 0) (= k 6)) [1 p t]
      (if (= k 1)            [n 1 t]
        (if (= k 2)          [t 1 p]
          (if (= k 3)        [t n 1]
            (if (= k 4)      [p t 1]
              (if (= k 5)    [1 t n] [0.5 0.5 0.5]))))))))

(defn hsl2rgb [hsl]
 ; l is on a 0-1 scale, 0 is always black and 1 is always white.
  (let [g 2.2 rgb (hsv2rgb [(first hsl) (second hsl) 1])
       w [0.241 0.691 0.068] ; weight must sum to 1.
       ; actual brightness
       lux (fn [c] (reduce + 0.0 (map #(* (Math/pow %1 %2) %3) c (repeat g) w)))
       ; percieved brightness is inverse gamma to real brightness.
       brt #(Math/pow (lux %) (/ 1.0 g))
       cmax (+ (apply max rgb) 1e-8) ; largest color component.
       b (+ (brt rgb) 1e-8); yes you DO need these numbers to be larger than machine eps.
       bmax (/ b cmax) ; maximum brightness that can be achieved with scaling.
       rgbmax (into [] (map #(* % (/ 1 cmax)) rgb)) ; fully scaled colors.
       bout (nth hsl 2)] 
  (if (< b bout) 
   ; we must make it brighter.
    (if (> b bmax) 
      ; scaling all colors up will not work. Blend with white (lux = brt = 1 for white).
      ; APPRIXIMATE brightness as linear in blending.
      ; Blend the max brightness scaled color with white, higher x menas more white.
      ; bmax*(1-x) + x = bout => x - bmax*x = bout - bmax => x = (bout-bmax)/(1-bmax)
      (let [x (/ (- bout b) (- 1.0 b)) xc (- 1 x)]
        (into [] (map #(+ (* % xc) x) rgbmax)))
      ; scaling will work.
      (into [] (map #(* % (/ bout b)) rgb)))
    ; This formula is the same as well:
    (into [] (map #(* % (/ bout b)) rgb)))))

(defn _level2col [level]
  ; indentation level => color.
  ; This color scheme is carefully designed but may still not be that good.
  ;(let [sat (min 1 (* 0.125 (Math/ceil (/ (- level 0.00001) 6.0)))) ]
  ;  (hsv2rgb [(mod (/ (+ level 5.0) 6.0) 1.0) sat 1]))
  ; Perceptual distances of hues:
  ; http://jov.arvojournals.org/data/Journals/JOV/932812/i1534-7362-13-7-1-f03.jpeg
  (let [hvals (into [] (map #(/ % 360) [0 37 55 80 115 153 180 220 320]))
        b (if (= level 0) 1 (+ 0.87 (* 0.08 (mod level 2))))
        h (nth hvals (mod (dec level) (count hvals)))
        s (if (<= level (count hvals)) 1 0.5)] (hsl2rgb [h s b])))

(defn get-coords [text scale fontw fonth]
  "Gets an a vector, each element is the :left, :bottom, :width, and :height of a character.
   We have a fixed-width font given by fontw and fonth (no tabs)."
  (let [text (into [] text) n (count text)]
    (loop [acc [] ix 0 x 0 y 0]
      (if (= ix n) acc
          (let [si (nth scale ix) w (* si fontw) h (* si fonth) new? (= (nth text ix) \newline)]
            (recur (conj acc {:left (if new? 0 x) :bottom (+ y h (if new? h 0)) :width (if new? 0 w) :height h})
                   (inc ix) (if new? 0 (+ x w)) (if new? (+ y h) y)))))))

(defn edge-coords [text scale fontw fonth]
  "Gets a vector of [:x :bottom :top] edge coords, 1:1 with all cursor locations, one more than the # of cars.
   There is one more than the # of chars. TODO: Chars with different heights are avereged."
  (if (= (count text) 0) [{:x 0 :bottom 16 :top 0}] ; stupid default placeholder when empty, will cause superficial glitches if the fontsize etc si set differently.
    (let [coords (get-coords text scale fontw fonth)
          c0 (first coords) ci (last coords)
          ;m #(* 0.5 (+ %1 %2))
          t #(- (:bottom %) (:height %))]
      (into [] (concat [{:x (:left c0) :bottom (:bottom c0) :top (t c0)}]
                 (mapv (fn [a b] (hash-map :x (+ (:left a) (:width a))
                          :bottom (:bottom a) :top (t a)))
                   (butlast coords) (rest coords)) 
                 [{:x (+ (:width ci) (:left ci)) :bottom (:bottom ci) :top (t ci)}])))))

(defn cursor-vert [c text scale fontw fonth vert]
  "Returns a the new cursor's location moved vertical from where it is currently, - = up + down."
  (let [edges (edge-coords text scale fontw fonth)
        last-on-line? (into [] (concat (mapv #(= % \newline) text) [true])) ; "grabs" cursors further left.
        v+ (> vert 0) ; going up.
        x (:x (nth edges c))
        y (if v+ (:top (nth edges c)) (:bottom (nth edges c))) ; we start our line here.
        result #(let [dx (- (:x (nth edges %)) x) dy (- (if v+ (:bottom (nth edges %)) (:top (nth edges %))) y)] ; them - us
                    {:sign? ((if v+ <= >=) dy 0)
                     :dist (Math/abs dy) ; how far.
                     :bad (if (and (nth last-on-line? %) (>= x (:x (nth edges %)))) 0 (Math/abs dx)) ; how much we miss.
                     })
       rn (range (count edges))
       scorer #(let [r (result %)]
                    (if (:sign? r) (* (+ (:dist r) 1e-100) (+ (:bad r) 1e-100)) 1e200))
       min-score (apply min (mapv scorer rn))
       out (first (filter #(= (scorer %) min-score) rn))]
    (if (or (nil? out) (= min-score 1e200)) c out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Support Java-dependent functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-font []
  "Fixed at size 12 but it is scaled by the size scaling."
  (Font. "Monospaced" Font/PLAIN 12))

(defn get-metrix [font]
  "Gets the font size info as a simple clojure map."
  (let [c (Canvas.) metrix (.getFontMetrics c font)];(.getFontMetrics gfx font)]
      {:width  (.charWidth metrix \A )
       :height (.getHeight metrix)
       :asc (.getAscent metrix)}))

(defn fill-select-box! [gfx pos]
  "Fills a character's background."
  (doto gfx (.setColor (Color. 178 179 255)) 
    (.fillRect (int (:left pos)) (int (- (:bottom pos) (:height pos))) (int (:width pos)) (int (:height pos)))))

(defn draw-char! [gfx char pos font scale]
  "Draws a single char to graphics, given the chars value and it's position."
  (let [font (.deriveFont font Font/PLAIN (float (* scale 12.0)))]
    (doto gfx (.setColor (Color. 0 0 0)) (.setFont font)
      (.drawString (str char) (float (:left pos)) (float (:bottom pos))))))

(defn draw-cursor! [gfx x y1 y2]
  (let [lin #(.drawLine % (inc (float x)) (inc (float y1)) (inc (float x)) (inc (float y2)))]
    (doto gfx (.setColor (Color. 0 0 0)) (.setStroke (BasicStroke. 2)) lin
              (.setColor (Color. 255 255 255)) (.setStroke (BasicStroke. 1)) lin)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GUI callback/interaction functions, including the render! function.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remove-selected [state]
  "Removes the selected region from state and resets selection to []."
  (let [s (sort (:selection state))]
    (if (= (count s) 2)
      (modify-vec (assoc state :selection [] :cursor (first s) :repaint? true)
        #(vcat (subvec % 0 (first s)) (subvec % (second s)))) state)))

(defn xy [state]
  "Returns [x y] dimension in pixels with reasonable padding."
  (let [metrix (get-metrix (get-font)) 
        coords (get-coords (:text state) (:scale-fac state) (:width metrix) (:height metrix))
        padx 200
        pady 200]
  [(+ padx (reduce #(max %1 (+ (:left %2) (:width %2))) 0 coords))
   (+ pady (if (= (count coords) 0) 0 (:bottom (last coords))))]))

(defn painty! [state gfx]
  (let [font (get-font)
        metrix (get-metrix font)
        coords (get-coords (:text state) (:scale-fac state) (:width metrix) (:height metrix))
        edges (edge-coords (:text state) (:scale-fac state) (:width metrix) (:height metrix))
        s (sort (:selection state))]
   ;(println s (:cursor state) (count edges))
   (if (= (count s) 2)
       (mapv #(fill-select-box! gfx %) (subvec coords (first s) (second s))))
   (mapv #(draw-char! gfx %1 %2 font %3) (:text state) coords (:scale-fac state))
     (let [ed (nth edges (:cursor state))]
       (draw-cursor! gfx (:x ed) (:bottom ed) (:top ed))))
  state)

(defn get-closest-to-click [state x y]
  "Gets the closest point to the cursor's clicking."
  ; TODO: there is another distance function used when we move the cursor. Can we merge away this redundancy?
  (let [metrix (get-metrix (get-font))  
        ec (edge-coords (:text state) (:scale-fac state) (:width metrix) (:height metrix)) ; :x, :bottom, :top.
        dist #(let [xx (:x %) y0 (:bottom %) y1 (:top %) ; distance from the cursor line.
                   yy (cond (> y y0) y0 (< y y1) y1 :else y)] 
                (Math/sqrt (+ (* (- x xx) (- x xx) ) (* (- y yy) (- y yy)))))] 
    (apply min-key #(dist (nth ec %)) (range (count ec)))))

(defn mouse-press [state e]
  ; Set the first element of selection:
  (let [closest (get-closest-to-click state (:x e) (:y e))
        shift? (= (:cur-key-mod state) 1)
        sel (:selection state)
        c (:cursor state)
        new-sel (cond (not shift?) [closest]
                      (and shift? (first sel)) [(first sel) closest]
                      :else [c closest])]
    (assoc state :cursor closest :selection new-sel :repaint? true)))

(defn mouse-release [state e]
  state)

(defn mouse-move [state e]
  "x,y = pixels. Will be called lots of times."
  ;(println "mouse-move")
  state)

(defn mouse-drag [state e]
  ;(println "mouse-drag")
   (let [closest (get-closest-to-click state (:x e) (:y e))]
      ; set the second term of the selection:
      (assoc state :cursor closest :selection [(first (:selection state)) closest] :repaint? true)))

(defn key-press [state e]
  "getKeyChar and getModifier but in vector form."
  (let [ky (:char e)
        code (:code e)
        mods (:mods e)
        state (assoc state :cur-key-mod mods)
        c (:cursor state)
        t (:text state)
        sc (:scale-fac state)
        sl (:selection state) sls (sort sl)
        state-rmsl (remove-selected state)
        sel? (= (count sl) 2)
        bnd-c (fn [c0 n] (min (max c0 0) n))]
    ;(println e )
    ;(println e (int ky))
    (cond 
       (and (= code 8) (> c 0)) ; backspace (remove one element or remove selection).
       (if (= t (:text state-rmsl))
           (assoc (modify-vec state #(into [] (concat (subvec % 0 (dec c)) (subvec % c)))) 
             :cursor (dec c) :repaint? true) state-rmsl)
       (= code 8) state-rmsl ; nothing for backspace when not deleting anything (backspace DOES have a char)
       (or (= code 38) (= code 40) (= code 37) (= code 39)); arrow keys.
       (let [fm (get-metrix (get-font))
             shift? (= mods 1)
             cv #(cursor-vert c t sc (:width fm) (:height fm) %) ; move cursor vertically dir.
             c1 (bnd-c (cond (= code 38) (cv 1) (= code 40) (cv -1)   ; up, down (ignores sel?)
                             (= code 37) (if (and (not shift?) sel?) (first sls) (dec c))
                             (= code 39) (if (and (not shift?) sel?) (second sls) (inc c)))
                  (count t))
             sl1 (cond (not shift?) [] (and shift? sel?) [(first sl) c1] :else [c c1])]
         (assoc state :selection sl1 :cursor c1 :repaint? true))
       (and (not (= ky \￿)) (or (= mods 0) (= mods 1))); normal typing, including "enter".
       (let [k (if (= ky \tab) "  " (str ky)) c1 (inc (if sel? (first sls) c))]
         (assoc (modify-vec state-rmsl #(vcat (subvec %1 0 (dec c1)) (%2 {:text [ky] :scale-fac [1.0]}) (subvec %1 (dec c1))) true)
           :cursor c1))
       (and (= mods 4) (= ky \c) (= (count sl) 2)) ; copy to clipboard.
       (do (clipboard/copy!!! (apply subs (apply str t) sls)) state)
       (and (= mods 4) (= ky \v)) ; paste from the clipboard.
       (let [p (clipboard/paste) n (count p) rm {:text (into [] p) :scale-fac (into [] (repeat n 1.0))}
             cr (:cursor state-rmsl)]
         (assoc (modify-vec state-rmsl #(vcat (subvec %1 0 cr) (%2 rm) (subvec %1 cr)) true)
           :cursor (+ cr (count p))))
       (and (= mods 4) (= ky \x)) ; cut from the clipboard.
       (do (clipboard/copy!!! (apply subs (apply str t) sls)) state-rmsl)
       (and (= mods 4) (= ky \a)) ; select all.
       (assoc state :selection [0 (count t)] :repaint? true)
       :else
       state)))

(defn key-release [state e] (assoc state :cur-key-mod 0))

(defn initial-state [] 
  "The state of a fresh textbox thingy."
  {:Type 'JPanel ; simulated, not actual scrollpanes.
   :xy xy :repaint! painty!
   :mousePressed mouse-press :mouseDragged mouse-drag 
   :keyPressed key-press :keyReleased key-release
   :size [600 600]
   :selection [] ; for now a vector with two numbers in it.
   :cur-key-mod 0 ; so the mouse knows what modification we are on.
   :text [] :select [] :scale-fac [] :cursor 0})

; (require '[clooj.gui.textzone :as tbox])
; (tbox/debug)
(defn debug []
  "creates a simple example in a self-contained frame."
  (gui/setup {:Type 'JFrame :size [400 400] :Children
               [{:Type 'JScrollPane :size [400 400] :Children [(initial-state)]}]}))