; Background images look nice.
(ns layout.background
  (:import [java.awt.image BufferedImage])
  (:require [javac.gfxcustom :as gfxcustom]
   [layout.spatial.xform :as xform]))

(def bg-filename "./assets/forest.jpg")
(def bg-scale 16.0)
(def bg-perspective-effect 4.0) ; make it slightly in the background.

(def bg-size (let [^BufferedImage img (gfxcustom/filename2BufferedImage bg-filename)]
               [(.getWidth img) (.getHeight img)]))

(defn limit-cam [s]
  "Cameras are transformations, [x y scalex scaley], scaling first."
  (let [cam (:camera s)
        bg-scale0 (- bg-scale bg-perspective-effect)
        bound-xxyy [(- (* 0.5 bg-scale0 (first bg-size))) (* 0.5 bg-scale0 (first bg-size))
                    (- (* 0.5 bg-scale0 (second bg-size))) (* 0.5 bg-scale0 (second bg-size))]
        cam-limit (xform/visible-xxyy2cam bound-xxyy)

        min-zoom (nth cam-limit 2)
        max-zoom 64.0
        zoom (* 0.5 (+ (nth cam 2) (nth cam 3)))
        zoom1 (max min-zoom (min max-zoom zoom))

        cam-xxyy0 (xform/visible-xxyy cam)
        mX (* 0.5 (+ (nth cam-xxyy0 0) (nth cam-xxyy0 1)))
        mY (* 0.5 (+ (nth cam-xxyy0 2) (nth cam-xxyy0 3)))
        rzoom (/ max-zoom zoom)
        cam1 (if (> zoom max-zoom)
              (xform/xx cam [(* mX (- 1 rzoom)) (* mY (- 1 rzoom)) zoom1 zoom1]) cam)

        cam-xxyy (xform/visible-xxyy cam1)

        ; It works "backwards":
        move+x (* (- (nth cam-xxyy 1) (nth bound-xxyy 1)) zoom1)
        move-x (* (- (nth bound-xxyy 0) (nth cam-xxyy 0)) zoom1)
        move+y (* (- (nth cam-xxyy 3) (nth bound-xxyy 3)) zoom1)
        move-y (* (- (nth bound-xxyy 2) (nth cam-xxyy 2)) zoom1)

        conflictx (max 0.0 (* 0.5 (+ move-x move+x))) conflicty (max 0.0 (* 0.5 (+ move-y move+y)))
        move+x (- move+x conflictx) move-x (- move-x conflictx)
        move+y (- move+y conflicty) move-y (- move-y conflicty)

        ;_ (println "Stuff:" cam-xxyy bound-xxyy move+x move-x move+y move-y)
        ;move+x 0 move-x 0 move+y 0 move-y 0
        cam2x (cond (> move+x 0) (+ (first cam1) move+x) (> move-x 0) (- (first cam1) move-x) :else (first cam1))
        cam2y (cond (> move+y 0) (+ (second cam1) move+y) (> move-y 0) (- (second cam1) move-y) :else (second cam1))
        cam2 [cam2x cam2y zoom1 zoom1]]
    (assoc s :camera cam2)))