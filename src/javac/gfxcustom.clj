; Custom graphics commands for increased rendering performance, which allows the other code to stay in clojure-land.
(ns javac.gfxcustom
  (:import [java.awt.image BufferedImage]
    [java.awt Graphics2D Font Transparency Color GraphicsEnvironment GraphicsDevice GraphicsConfiguration RenderingHints BasicStroke] 
    [javax.swing SwingUtilities]
    [java.awt.image BufferedImage]
    [java.awt.geom AffineTransform]))

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

(def custom-cmds ; Custom commands for the java argument.
  {:FontSize set-font-size!
   :getFontSize (fn [^java.awt.Graphics2D g] (.getSize2D (.getFont g)))
   :grid-string grid-string!})

(def custom-xforms 
  {:grid-string xform-grid-string})