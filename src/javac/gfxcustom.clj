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
    [javax.imageio ImageIO])
  (:require [clojure.string :as string] 
    [javac.file :as jfile]
    [layout.xform :as xform]
    [coder.unerror :as unerror]
    [globals]))

;;;;;;;;;;;;;;;;;;; Error reporting ;;;;;;;;

(defn err-gfx [java-e our-err-msg]
   "Generates the clj gfx commands from a java exception. Used to debug bad graphics."
   (let [pieces (string/split (unerror/pr-error java-e) #"\n")]
     (collections/vcat [[:drawString [(str our-err-msg) 10 10] {:Color [1 0.5 1 1]}]]
       (mapv #(vector :drawString [(str %1) 10 (+ 20 (* %2 10))] {:Color [1 1 1 1]}) pieces (range)))))

;;;;;;;;;;;;;;;;;;; Defining graphics commands, which can avoid unnecessary gfx reset calls ;;;;;;;;

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

(defn filename2BufferedImage [filename]
  "Caches images to prevent having to load stuff over and over."
  (let [^BufferedImage cache (get-in @globals/external-state-atom [:assets :images filename])
        ^BufferedImage cache (if cache cache
                               (let [_ (if (not (jfile/exists? filename)) (println filename "does not exist"))
                                     x (ImageIO/read (File. filename))]
                                 (swap! globals/external-state-atom #(assoc-in % [:assets :images filename] x)) x))]
    cache))

(defn file-bitmap-draw! [^java.awt.Graphics2D g args]
  (let [x (first args) y (second args) zoom (nth args 2)
        filename (nth args 4)
        ^BufferedImage bufim (filename2BufferedImage filename)
        ^AffineTransform affineXform (AffineTransform.)
        xf [x y zoom zoom]
        ; AffineXforms work backwards from our system, thus the translate bf scale.
        _ (.translate affineXform (double (nth xf 0)) (double (nth xf 1)))
        _ (.scale affineXform (double (nth xf 2)) (double (nth xf 3)))
        ^ImageObserver null-observer nil]
    (.drawImage g bufim affineXform null-observer)))

;;;;;;;;;;;;;;;;;;; Defining how the camera xforms the graphics commands ;;;;;;;;;;;;;;;;;

(defn xform-grid-string [xform-camera g-cmd keep-width?]
  (let [;xform-camera is [x y scalex scaley], scale first. 
        dx (first xform-camera) dy (second xform-camera) scx (nth xform-camera 2) scy (nth xform-camera 3)
        args (second g-cmd)
        xs1 (mapv #(+ (* % scx) dx) (nth args 2))
        ys1 (mapv #(+ (* % scy) dy) (nth args 3))
        ftsz1 (* (first args) (+ (* scx 0.5) (* scy 0.5)))]
    (assoc g-cmd 1
      (-> args (assoc 0 ftsz1)
        (assoc 2 xs1) (assoc 3 ys1)))))

(defn xform-bitmap [xform-camera g-cmd keep-width?]
  (let [scx (nth xform-camera 2) scy (nth xform-camera 3)
        cam-zoom (+ (* scx 0.5) (* scy 0.5))
        args (second g-cmd) ;[x y zoom perspective file]
        
        im-dist-3d (+ (/ cam-zoom) (nth args 3))
        perspective-scale-mult (/ 1.0 cam-zoom im-dist-3d)
        ^BufferedImage im-buf (filename2BufferedImage (nth args 4))
        im-shiftx (* 0.5 (.getWidth im-buf) (- 1.0 perspective-scale-mult))
        im-shifty (* 0.5 (.getHeight im-buf) (- 1.0 perspective-scale-mult))
        xform-image [(first args) (second args) (nth args 2) (nth args 2)]
        xform-p [im-shiftx im-shifty perspective-scale-mult perspective-scale-mult]
        total-xform (xform/xx xform-p (xform/xx xform-camera xform-image))
        
        args1 (-> args (assoc 0 (first total-xform)) (assoc 1 (second total-xform)) (assoc 2 (nth total-xform 2)))]
    (assoc g-cmd 1 args1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Putting it all together ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def custom-cmds ; Custom commands for the java argument.
  {:FontSize set-font-size!
   :getFontSize (fn [^java.awt.Graphics2D g] (.getSize2D (.getFont g)))
   :grid-string grid-string!
   :bitmap file-bitmap-draw!})

(def custom-xforms 
  {:grid-string xform-grid-string
   :bitmap xform-bitmap}) 