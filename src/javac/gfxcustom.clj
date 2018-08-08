; Custom graphics commands for increased rendering performance, which allows the other code to stay in clojure-land.
; TODO: it would be nice if we used some fns from xform and gfx, but they both depend on us.
(ns javac.gfxcustom
  (:import [java.awt.image BufferedImage]
    [java.awt Graphics2D Font Transparency Color GraphicsEnvironment 
      GraphicsDevice GraphicsConfiguration RenderingHints BasicStroke] 
    [java.awt.image BufferedImage]
    [java.awt.geom AffineTransform]
    [java.io File]
    [javax.swing SwingUtilities]
    [javax.imageio ImageIO]
    [globals])
  (:require [javac.file :as jfile]))

(defn set-font-size! [^java.awt.Graphics2D g sz]
  (.setFont g (.deriveFont (.getFont g) (float sz))))

(defn grid-string! [^java.awt.Graphics2D g args]
  "Draws string s with per-char locations xs and ys and per-char colors r g b and a."
  (let [fontsize (first args) 
        ^String s (second args) 
        xs (nth args 2) ys (nth args 3) 
        rs (nth args 4) gs (nth args 5) bs (nth args 6) as (nth args 7)
        n (count s)]
    (set-font-size! g fontsize)
    (loop [ix 0]
      (if (= ix n) "Done"
        (do
          (let [ri (nth rs ix) gi (nth gs ix) bi (nth bs ix) ai (nth as ix)
                sc? (or (= ix 0) (not= ri (nth rs (dec ix))) (not= gi (nth gs (dec ix))) (not= bi (nth bs (dec ix))) (not= ai (nth as (dec ix))))]
            (if sc? (.setColor g ^java.awt.Color (Color. (float ri) (float gi) (float bi) (float ai)))))
          (.drawString g (str (nth s ix)) (int (nth xs ix)) (int (nth ys ix))) ; about 1/2 of the cost of this function.
          (recur (inc ix)))))))

(defn file-bitmap-draw! [^java.awt.Graphics2D g args]
  (let [x (first args) y (second args) zoom (nth args 2)
        filename (nth args 4)
        ^BufferedImage cache (get-in @globals/one-atom [:assets :images filename])
        ^BufferedImage cache (if cache cache
                               (let [_ (if (not (jfile/exists? filename)) (println filename "does not exist"))
                                     x (ImageIO/read (File. filename))]
                                 (swap! globals/one-atom #(assoc-in % [:assets :images filename] x)) x))
        ^AffineTransform affineXform (AffineTransform.)
        xf [x y zoom zoom]
        ; AffineXforms work backwards from our system, thus the translate bf scale.
        _ (.translate affineXform (double (nth xf 0)) (double (nth xf 1)))
        _ (.scale affineXform (double (nth xf 2)) (double (nth xf 3)))
        ^ImageObserver null-observer nil]
    (.drawImage g cache affineXform null-observer)))

(defn xform-grid-string [xform g-cmd keep-width?]
  (let [;xform is [x y scalex scaley], scale first. 
        dx (first xform) dy (second xform) scx (nth xform 2) scy (nth xform 3)
        args (second g-cmd)
        xs1 (mapv #(+ (* % scx) dx) (nth args 2))
        ys1 (mapv #(+ (* % scy) dy) (nth args 3))
        ftsz1 (* (first args) (+ (* scx 0.5) (* scy 0.5)))]
    (assoc g-cmd 1
      (-> args (assoc 0 ftsz1)
        (assoc 2 xs1) (assoc 3 ys1)))))

(defn xform-bitmap [xform g-cmd keep-width?]
  (let [scx (nth xform 2) scy (nth xform 3)
        cam-zoom (+ (* scx 0.5) (* scy 0.5))
        cam-x (/ (- (first xform)) cam-zoom) cam-y (/ (- (second xform)) cam-zoom)
        args (second g-cmd) 
        perspective-effect (args 3)
        im-x (* (args 0) (+ 1 perspective-effect)) 
        im-y (* (args 1) (+ 1 perspective-effect))
        im-zoom (* (args 2) (+ 1 perspective-effect)) 
        
        cam-im-dist (+ perspective-effect (/ cam-zoom))
        new-scale (/ im-zoom cam-im-dist)
        rz (/ (/ cam-zoom) cam-im-dist)
        dx1 (* (* (- im-x cam-x) rz) cam-zoom)
        dy1 (* (* (- im-y cam-y) rz) cam-zoom)
        
        args1 (-> args (assoc 0 dx1) (assoc 1 dy1) (assoc 2 new-scale))]
    (assoc g-cmd 1 args1)))

(def custom-cmds ; Custom commands for the java argument.
  {:FontSize set-font-size!
   :getFontSize (fn [^java.awt.Graphics2D g] (.getSize2D (.getFont g)))
   :grid-string grid-string!
   :bitmap file-bitmap-draw!})

(def custom-xforms 
  {:grid-string xform-grid-string
   :bitmap xform-bitmap})