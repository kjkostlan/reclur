; A layout that focuses on having a single coding area, the files to the left, and the console below. 
(ns layout.onecode
  (:require [globals]
    [app.xform :as xform]))

;;;;;;;;;;;;;;;;;;; More universal layout tools to use otherwise ;;;;;;;;;;;;;;;;;;

(defn screen-pixels []
  "[x y] pixels. The Retnia displays pretend to be 1/2 the resolution."
  (if-let [sz (:window-size (:external-state @globals/one-atom))]
    sz [800 600]))

(defn view-xxyy [cam]
  "x1 x2 y1 y2 of the screen in pixels based on the camera."
  (let [xy (screen-pixels)
        corner0 (xform/xv cam 0 0) corner1 (xform/xv cam (first xy) (second xy))]
    [(first corner0) (first corner1) (second corner0) (second corner1)]))

(defn set-xxyy [component xxyy]
  (assoc component :position [(first xxyy) (nth xxyy 2)] :size [(- (second xxyy) (first xxyy)) (- (nth xxyy 3) (nth xxyy 2))]))

(defn abs-xxyy [screen-xxyy rel-xxyy]
  (let [abs-x0 (+ (nth screen-xxyy 0) (* (- (nth screen-xxyy 1) (nth screen-xxyy 0)) (nth rel-xxyy 0)))
        abs-x1 (+ (nth screen-xxyy 0) (* (- (nth screen-xxyy 1) (nth screen-xxyy 0)) (nth rel-xxyy 1)))
        abs-y0 (+ (nth screen-xxyy 2) (* (- (nth screen-xxyy 3) (nth screen-xxyy 2)) (nth rel-xxyy 2)))
        abs-y1 (+ (nth screen-xxyy 2) (* (- (nth screen-xxyy 3) (nth screen-xxyy 2)) (nth rel-xxyy 3)))]
    [abs-x0 abs-x1 abs-y0 abs-y1]))

(defn set-rel-xxyy [component screen-xxyy rel-xxyy]
  ;(println "setting: " (:type component) screen-xxyy rel-xxyy (abs-xxyy screen-xxyy rel-xxyy))
  (set-xxyy component (abs-xxyy screen-xxyy rel-xxyy)))

(defn max-z [s] (apply max 0 (mapv #(if (:z %) (:z %) 0) (vals (:components s)))))

(defn unique-z [components]
  "Assigns each component a unique z-value."
  (loop [acc (zipmap (keys components) (mapv #(if (:z %) % (assoc % :z 1)) (vals components)))]
    (if (= (count (apply hash-set (mapv :z (vals acc)))) (count acc)) acc
      (recur (zipmap (keys acc) (mapv (fn [c] (update c :z #(+ (* % (+ 1 (* (Math/random) 1e-10))) 1e-100))) (vals acc)))))))

(defn y- [xxyy] 
  "When it's more intuitive to work in normal coords."
 [(first xxyy) (second xxyy) (- 1.0 (nth xxyy 3)) (- 1.0 (nth xxyy 2))])

;;;;;;;;;;;;;;;;;; More layout-specific code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def lparams {:files-x 0.2 :repl-x 0.5 :console-x 0.67 :replconsole-y 0.25 :margin 0.025})

(defn initial-position [s files repl console]
  "Initial positions before the codebox is opened. One code leaves a lot of room for codeboxes.
   Gensyms keys as well."
  (let [screen-xxyy (view-xxyy (:camera s))
        margin (:margin lparams)
        files (set-rel-xxyy files screen-xxyy (y- [0 (:files-x lparams) (:replconsole-y lparams) (- 1.0 margin)]))
        repl (set-rel-xxyy repl screen-xxyy (y- [0 (:repl-x lparams) 0 (:replconsole-y lparams)]))
        console (set-rel-xxyy console screen-xxyy (y- [(:repl-x lparams) 1 0 (:replconsole-y lparams)]))
        gk #(keyword (gensym %))]
    (update s :components #(assoc % (gk 'fbrowser) files (gk 'orepl) repl (gk 'siconsole) console))))

(defn pos-newcommer [s comp]
  ; TODO: actually have a context-sensitive position.
  (set-rel-xxyy comp (view-xxyy (:camera s)) 
    (y- [(:files-x lparams) 1 (:replconsole-y lparams) (- 1 (:margin lparams))])))

(defn add-component [s comp kwd]
  (let [z (+ (max-z s) 1.0)
        comp (pos-newcommer s comp)]
    (update (assoc-in s [:precompute :desync-safe-mod?] true) 
      :components #(assoc % kwd (assoc comp :z z)))))

;;;;;;;;;;;;;;;;;;;;;;;;; Compile it all together ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn layout []
  {:initial-position initial-position
  :add-component add-component})