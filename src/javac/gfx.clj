; The default graphics updating that is made to be more functional.
; Performance is favored over a super flexible API, as performace for graphics was found to be the bottleneck.
; As a rule of thumb, light classes like Color are represented by vectors of numbers 1:1 with the constructor.
; Heavy classes such as arrays are represented by the java object themselves (the user must pass in a Java object).
; maybe TODO: handle affine transforms (partially implemented, need to handle the rotate scale etc cases).
; Long term TODO: have a compiler for maximum performance that creates code for a :java command.

; We receive a map from kys to values, where each value is in turn a vector of vectors.
; :camera = camera (a single xform vector)
; :gfx = the gfx ignoring the camera, a vector:
; Each object element of the graphics vector is a vector of commands:
; [:drawOval, [10 10 20 20]]. Most commands must take clojure structures.
; [:fillRect, [10 10 20 20] ; when no third argument is supplied, uses defaults for everything, including color, etc.
; [:drawOval, [10 10 20 20], {:Color [0 1 0], :Stroke [...]}] ; overrides the defaults, ONLY for this one call.
; [:java (fn [^java.awt.Graphics2D g] ...)] ; Puncture the abstraction and call java commands directly on the graphics object.
     ; If this changes graphis's settings it will affect downstream calls until any changes get overriden.
; The first element is the java argument in keyword form.
; The second is the vector of stuff passed to the java arguments.
; The third is a map of settings like color. Settings only apply to the command at hand, they are
  ; not sticky as they would be in the mutatable land of java.
; TODO: have some defaults if the int vs non-int types are messed up.
; TODO: eventually write compiler macros for the gfx commands for even faster results.
(ns javac.gfx
  (:import [java.awt.image BufferedImage]
    [java.awt Graphics2D Font Transparency Color GraphicsEnvironment GraphicsDevice GraphicsConfiguration RenderingHints BasicStroke]
    [java.awt.image BufferedImage]
    [java.awt.geom AffineTransform])
  (:require [clojure.set :as set] [clojure.string :as string]
    [javac.crossplatform :as cp]
    [layout.spatial.xform :as xform]
    [javac.gfxcustom :as gfxcustom] [javac.thread :as jthread]))

(def ^:dynamic *sprite-cache-ms* 500) ; render components as sprites when we aren't actively using them, unless the no-sprite flag is used.
(def ^:dynamic *image-scaleup* 2.0) ; Higher resolution looks better.
(def ^:dynamic *max-image-pixels* 3e7) ; Protect against extreme image size crash.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Specific code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def tform-names "Hash-set of transform names, they are like setters but have a non-standard syntax."
  #{"translate" "rotate" "scale" "shear" "transform"})

(def simple-constructors
  "Map from keywords to constructor functions."
  {:Color '(java.awt.Color. (float (first x)) (float (second x)) (float (nth x 2)) (float (nth x 3))) ;colors are floats from 0-1.
   :Point '(java.awt.Point. (int (first x)) (int (second x)))
   :Rectangle '(java.awt.Rectangle. (int (first x)) (int (second x)) (int (nth x 2)) (int (nth x 3)))
   :AffineTransform '(java.awt.geom.AffineTransform. (double (first x)) (double (second x)) (double (nth x 2)) (double (nth x 3)) (double (nth x 4)) (double (nth x 5)))
   :Stroke '(java.awt.BasicStroke. (float (first x)) (int (second x)) (int (nth x 2)) (float (nth x 3)))})

(def simple-getters
  {:getColor '(fn [^java.awt.Graphics2D g]
                (let [c (.getColor g)]
                  [(/ (.getRed c) 255.0) (/ (.getGreen c) 255.0) (/ (.getBlue c) 255.0) (/ (.getAlpha c) 255.0)]))
   :getAffineTransform '(fn [^java.awt.Graphics2D g]
                          (let [t (.getTransform g)]
                            [(.getScaleX t) (.getShearY t) (.getShearX t) (.getScaleY t) (.getTranslateX t) (.getTranslateY t)]))
   ; Only basic strokes are supported (unless you use the generic :java option).
   :getStroke '(fn [^java.awt.Graphics2D g]
                 (try (let [s (java.awt.BasicStroke (.getStroke g))]
                        [(.getLineWidth s) (.getEndCap s) (.getLineJoin s) (.getMiterlimit s)])
                   (catch Exception e [1 1 1 1])))})

(defn keep-method? [^java.lang.reflect.Method m]
  "Some methods are bad, etc."
  (let [n (.getName m) r (.getReturnType m) cls (.getParameterTypes m)]
    (cond (= n "setPaintMode") false ; non-standard set that is empty.
      (= n "getFont") false (= n "setFont") false ; Font is also wierd.
      (and (= (first cls) java.text.AttributedCharacterIterator) (= (count cls) 3) (= n "drawString")) false
      :else true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Code generation command bank ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn cl2str [^Class cl]
  "Class to string. unify? true unifies all numbers to a generic number class.
   When we construct it, we cast the numbers back."
  (let [s (str cl) su (-> s (string/replace " " "") (string/replace "class" "") (string/replace "interface" "")
                        (string/replace "enum" ""))] su))

(defn _pack-methods [methods]
  (let [methods (filterv keep-method? methods)
        names (mapv #(.getName %) methods)
        classes (mapv #(mapv (fn [x] (cl2str x)) (.getParameterTypes %)) methods)]
    (zipmap (mapv #(vector (keyword %1) (count %2)) names classes) classes)))

(def java-draw-commands
  "Map from [keyword arity] to vectors. The keyword is the method name and
  the vector is a vector of class arguments in string form.
  Only includes commands that directly draw stuff, not setters of graphics, etc.
  Arguments of the same arity will shadow, this is uasually not a big deal."
  (let [methods (.getMethods java.awt.Graphics2D)
        draw-methods (filterv #(let [n (.getName %) s (subs (str n "   ") 0 3)]
                                 (or (and (not (get tform-names n)) (not= s "get") (not= s "set")
                                   (not= n "wait") (not= n "notify") (not= n "notifyAll")
                                   (= (str (.getReturnType %)) "void")) (.startsWith n "draw")))
                       methods)]
    (_pack-methods draw-methods)))

(def java-set-commands
  "Set commands, the same format as java-draw-commands, except that the prefix set is removed from the keywords.
   (the user commands don't use the prefix set)."
  (let [methods (.getMethods java.awt.Graphics2D)
        draw-methods (filterv #(let [n (.getName %) s (subs (str n "   ") 0 3)]
                                 (or (= s "set") (get tform-names n)))
                       methods)
        pack-m (_pack-methods draw-methods)]
    (zipmap (mapv (fn [v] (update v 0 #(if (.startsWith (str %) ":set") (keyword (subs (str %) 4)) %)))
              (keys pack-m)) (vals pack-m))))

(def java-get-commands
  "Set commands, the same format as java-draw-commands.
   Unlike the set commands the prefix is NOT removed (getters are only used internally)."
  (let [methods (.getMethods java.awt.Graphics2D)
        draw-methods (filterv #(let [n (.getName %) s (subs (str n "   ") 0 3)]
                                 (= s "get")) methods)]
    (_pack-methods draw-methods)))

(defn strip-line-col [c] ; This is really for readability. I don't think it impacts anything.
  (let [m (meta c)
        c (cond (list? c) (apply list (map strip-line-col c)) (vector? c) (mapv strip-line-col c)
            (map? c) (zipmap (map strip-line-col (keys c)) (map strip-line-col (vals c)))
            (set? c) (apply hash-set (map strip-line-col c)) :else c)]
    (if m (with-meta c (dissoc m :line :column)) c)))

(defn _xn [c ix] ; replace 'x in c. with '(nth x ~n)
  (cond (= c 'x) (list 'nth 'x ix) (vector? c) (mapv #(_xn % ix) c)
    (sequential? c) (apply list (mapv #(_xn % ix) c)) :else c))
(def _cmd-bank ; auto-generated hash-map of reflection-free functions that call Graphics2D methods.
  (let [ks0 (mapv first (keys java-get-commands))
        m #(with-meta %1 {:tag (symbol (if (instance? java.lang.Class %2) (cl2str %2) (str %2)))})
        mg #(m % java.awt.Graphics2D)
        cl-leaf (fn [cl] (keyword (last (string/split (str cl) #"\."))))
        getters (zipmap (concat ks0 (keys simple-getters))
                  (concat (mapv #(list 'fn [(mg 'g)] (list (symbol (str ".get" (subs (str %) 4))) 'g )) ks0) (vals simple-getters)))
        con (fn [cl] (get simple-constructors (cl-leaf cl)))
        ks0 (mapv first (keys java-set-commands)) cls (mapv #(first (get java-set-commands [% 1])) ks0)
        setters (zipmap ks0
                  (mapv #(let [c (con %1)] ; constructor.
                           (list 'fn [(mg 'g) (if (or c (not %1)) 'x (m 'x %1))]
                             (list (symbol (str ".set" (subs (str %2) 1))) 'g (if c c 'x)))) cls ks0))
        ks (keys java-draw-commands)
        mkd (fn [kwd cls] ; make a command code from a keyword and a class.
              (apply list (symbol (str "." (subs (str kwd) 1))) 'g
                (mapv #(let [c (get simple-constructors (cl-leaf %1))
                             xi (list 'nth 'x %2)]
                         (cond c (_xn c %2) ; constructor class.
                           (or (= %1 "boolean") (= %1 "int") (= %1 "float") (= %1 "double")) (list (symbol %1) (list 'nth 'x %2)) ; primative classes.
                           :else (with-meta (list 'nth 'x %2) {:tag (eval (symbol %1))}))) cls (range))))
        drawers-l (zipmap ks ; commands without the cond for length.
                    (mapv #(mkd (first %) (get java-draw-commands %)) ks))
        ; Combine into a cond statement that checks arity:
        arities (reduce (fn [acc k] (update acc (first k) #(conj (if % % []) (second k)))) {} (keys drawers-l))
        drawers (zipmap (keys arities)
                  (mapv (fn [k] (let [ars (get arities k) codecs (mapv #(get drawers-l [k %]) ars)]
                                  (list 'fn '[^java.awt.Graphics2D g x]
                                    (list 'let ['ar '(count x)]
                                      (apply list
                                        (concat ['cond] (interleave (map #(list '= 'ar %) ars) (map #(get drawers-l [k %]) ars))
                                          (list ':else (list 'throw (list 'Exception. (list 'str "Unrecognized arity for " k "(" 'ar ")"))))))))))
                    (keys arities)))]
    (strip-line-col
      (zipmap (concat (keys getters) (keys setters) (keys drawers))
        (concat (vals getters) (vals setters) (vals drawers))))))

(def cmd-bank ; command bank with type-hinted functions, calculated from _cmd-bank with eval.
              ; same anem as java fns except keywords and setters i.e. setColor is replaced with :Color.
              ; and getters are :defaultColor, etc since they only apply on defaults.
  (zipmap (concat (keys _cmd-bank) (keys gfxcustom/custom-cmds))
          (concat (mapv eval (vals _cmd-bank)) (vals gfxcustom/custom-cmds))))

;(require '[clojure.pprint]) (binding [*print-meta* true] (println (clojure.pprint/pprint _cmd-bank)))
; Cmd bank is of the format (with the fns evaled and type cast/hints)
; {:drawString (fn [g x] (let [ar (count x)] (cond (if (= ar 3) (.drawString ...) :else (throw ...))))) ...}

(def set-get-map ; map from :Color to :getColor, etc.
  (let [vs (filterv #(.startsWith (str %) ":get") (keys cmd-bank))]
    (zipmap (mapv #(keyword (subs (str %) 4)) vs) vs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Gfx parameters to set ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn interpolate-img! [^Graphics2D g]
  "Is bilinear better? Not if we have to do it in software, but hardware systems should uncomment."
  (let [^java.util.HashMap rhs (java.util.HashMap.)
        ;_ (.put rhs (RenderingHints/KEY_INTERPOLATION) (RenderingHints/VALUE_INTERPOLATION_BILINEAR))
        _ (.put rhs (RenderingHints/KEY_RENDERING) (RenderingHints/VALUE_RENDER_SPEED))
        ^RenderingHints rh (RenderingHints. rhs)]
    (.setRenderingHints g rh)))

(defn healthy-stroke! [^Graphics2D g width]
  "More than 1 is enough so that downscaled images don't look so horrible, at least with bilinear interp.
   Does not affect fonts, but as long as fonts render thick enough it should be OK."
  (.setStroke g ^BasicStroke (BasicStroke. (float width))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; How cameras xform clojure gfx structures ;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-location [xform name-kwd old-loc]
  "Default behavior to get the new location (which is the second argument of the graphics, in vector form).
   what we call 'location' is really the list of arguments passed to the java graphics call.
   but it acts like location to a large extent."
  ; only numbers are converted.
  ; the default pattern is [x y width height width height width height].
  ; dievations are handled in the default.
  ; changes to linewidth are NOT handled here.
  ; The code could be made shorter by having functions that perform each elementary transform.
  (let [x (first xform) y (second xform) sx (nth xform 2) sy (nth xform 3)]
    (cond (and (= x 0) (= y 0) (= sx 1) (= sy 1)) old-loc ; no change if not bieng transformed.
      (or (= name-kwd :drawBytes) (= name-kwd :drawChars)) ; don't know how to size these.
      [(first old-loc) (second old-loc) (nth old-loc 2)
       (+ (* (nth old-loc 3) sx) x) (+ (* (nth old-loc 4) sy) y)]
      (= name-kwd :drawImage)
      (cond (< (count old-loc) 6) (throw (Exception. ":drawImage must use a form that specifies the width and height."))
        (< (count old-loc) 8) ; unscaled form, must scale it.
        (into []
          (concat
            [(first old-loc) (+ (* (second old-loc) sx) x) (+ (* (nth old-loc 2) sy) y)
             (+ (* (+ (second old-loc) (nth old-loc 3)) sx) x)
             (+ (* (+ (nth old-loc 2) (nth old-loc 4)) sy) y)] (subvec old-loc 5)))
        :else
        (into []
          (concat [(first old-loc) (+ (* (second old-loc) sx) x) (+ (* (nth old-loc 2) sy) y)
                   (+ (* (nth old-loc 3) sx) x) (+ (* (nth old-loc 4) sy) y)]
           (subvec old-loc 5))))
      (= name-kwd :drawLine)
      [(+ (* (first old-loc) sx) x) (+ (* (second old-loc) sy) y)
       (+ (* (nth old-loc 2) sx) x) (+ (* (nth old-loc 3) sy) y)]
      (or (= name-kwd :drawPolygon) (= name-kwd :drawPolyline))
      (throw (Exception. "TODO: support polygons and polylines"))
      (or (= name-kwd :drawRect) (= name-kwd :fillRect) (= name-kwd :drawOval) (= name-kwd :fillOval))
      [(+ (* (first old-loc) sx) x) (+ (* (second old-loc) sy) y)
       (* (nth old-loc 2) sx) (* (nth old-loc 3) sy)] ; x y w h
      (= name-kwd :drawString)
      [(first old-loc) (+ (* (second old-loc) sx) x) (+ (* (nth old-loc 2) sy) y)]
      :else ; guessing, this list isn't complete.
      (mapv #(cond (not (number? %1)) %1
              (even? %2) (+ (* %1 sx) x)
              :else (+ (* %1 sy) y)) old-loc (range)))))

(defn xgfx [xform g-cmd keep-width?]
  "Transforms a single graphics command.
   xform is how the component is transformed; use this to draw transformed components.
   TODO: implement this keep-width? false mean lines get thinner/thicker (using transparancy when < 1 pixel).
   true will keep lines at the same width, so a 1-pixel line stays 1 pixel."
  (let [kwd (first g-cmd) cf (get gfxcustom/custom-xforms kwd)]
    (if cf (cf xform g-cmd keep-width?)
      (if (= kwd :java) g-cmd ; custom commands, no way to try to parse them.
        (let [g-cmdl (assoc g-cmd 1 (new-location xform (first g-cmd) (second g-cmd)))]
          (cond (= kwd :drawString)
            (let [sz (* (if-let [sz (:FontSize (get g-cmdl 2))] sz 11) (nth xform 2))]
              (assoc-in g-cmdl [2 :FontSize] sz))
            :else g-cmdl))))))

(defn gfx-local2global [gfx cam position]
  "Converts local to global graphics, given a clojure datastructure for the graphics."
  (let [xfrm (xform/xx cam (xform/pos-xform position))]
    (mapv #(xgfx xfrm % true) gfx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Rendering runtime ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn convert-commands [cmds last-cmd-atom]
 "Converts commands from functional drawing format to java's mutable-state format.
  Returns :intro and :cmds
  The input commands have an optional third argument that specifies (i.e. color).
  We pull out the third argument into i.e. :Color and reverse the setColor command for the next run.
  :intro is :getColor, etc commands for finding out what the defaults are, :cmds are the main command.
  The :default keyword as a second argument means use the defaultinstead of the second arg itself."
  (let [_ (if last-cmd-atom (reset! last-cmd-atom "<No command hit>"))
        n (count cmds)
        acf #(if-let [x (do (if last-cmd-atom (reset! last-cmd-atom %)) (get % 2))] (keys x) [])
        ever-changed (apply hash-set (apply concat (mapv acf cmds))) ; #{:Color, :Stroke, etc}
        intro (mapv #(keyword (str "get" (subs (str %) 1))) ever-changed)]
    {:intro intro :cmds
      (loop [acc [] non-defaults {} ix 0]
        (if (= ix (count cmds)) acc
          (let [cmd (nth cmds ix)
                _ (if last-cmd-atom (reset! last-cmd-atom cmd))
                specs (if-let [x (get cmd 2)] x {}) ; The second argument is stuff like color, etc that mutates the java class.
                ; Works for both java and non-java objects:
                changes (reduce #(let [sp (get specs %2) old (get non-defaults %2)]
                                   (cond (or (identical? sp old) (= sp old)) ; identical? used as well to avoid reflection when we have java objects (assuming references are reused).
                                     (dissoc %1 %2) (not sp) (assoc %1 %2 :default) :else (assoc %1 %2 sp)))
                          specs (keys non-defaults))] ; changes to the :Color, etc.
            (recur (conj (apply conj acc (mapv vector (keys changes) (vals changes))) [(first cmd) (second cmd)]) ; changes then
              specs (inc ix)))))})) ; at each step the non-defaults going in are the specs from the last step.

(defn update-graphics! [java-ob old-clj-gfx new-clj-gfx & force-repaint]
  "Updates the graphics, calling a .repaint if needed. See doc at top of this file for the format of clj-gfx."
  (if (or (not= old-clj-gfx new-clj-gfx) (first force-repaint)) ; change detected or forced.
    (jthread/swing-later ; Never wait for a repaint, see https://stackoverflow.com/questions/5133429/waiting-for-swings-elements-repaint-completion.
      (do (.putClientProperty java-ob "repaintCmds" new-clj-gfx)
                     (.revalidate java-ob) (.repaint java-ob)))))

(defn report-e [m-extra e] "Reports an exception and throws it, with m-extra as extra message."
  (throw (RuntimeException. (str m-extra " " (.getMessage e)) e)))

(defonce dbat (atom [])) ; debugging.
;(do (require '[clooj.coder.repl :as repl]) (repl/clc))
;(println (nth (:post @dbat) 500))

(defn draw1! [^Graphics2D g cmd defaults]
   (let [k (first cmd) bank cmd-bank sg-map set-get-map ; kc (k bank)
         g-code (fn [k] (binding [*print-meta* true] (pr-str (k _cmd-bank))))] ; for error reporting.
     ; sg-map is Setting keywords -> getting keywords.
     (if (not (keyword? k)) (throw (Exception. (str "Gfx option keyword not a keyword:" k))))
     (if (= k :java)
       (try ((second cmd) g) (catch Exception e (report-e "Custom user graphics call error: " e)))
       (let [kc (k bank)]
         (if kc
           (let [arg (second cmd) f (k bank)]
             (cond (nil? arg) (throw (RuntimeException. (str "Nil value for draw command: " k)))
               (nil? f) (throw (RuntimeException. (str "Unrecognized draw command or setting: " k)))
               (= arg :default)
               (try ((k bank) g (get defaults (sg-map k)))
                 (catch Exception e (report-e (str "Error (when resetting to defaults) for command: " (k bank) " given: " (get defaults (sg-map k)) " on: " (g-code k)))))
               :else
               (try ((k bank) g arg)
                 (catch Exception e (report-e (str "Error for command: " (k bank) " given: " arg " on: " (g-code k)) e)))))
           (throw (Exception. (str "Unrecognized draw command: " k))))))))

(defn _paint-gfx-fairweather! [^Graphics2D g camera cmds need-convert?]
  "Shine (but not rain): Fails if the cmds are bad."
  (let [cmds (if (nil? camera) cmds (mapv #(xgfx camera % true) cmds))
        cmds (if need-convert? (convert-commands cmds nil) cmds)
        intro (:intro cmds)
        body (:cmds cmds)
        _ (reset! dbat {:pre cmds :post body}) ; Debug atom.
        defaults (zipmap intro (mapv #(if-let [get-default (get cmd-bank %)] (get-default g)) ; Settings that will be changed, requiring us to store defaults.
                                 intro))]
    (mapv #(try (draw1! g % defaults)
             (catch Exception e
               (throw (Exception. (str "Converted command failed to draw: " %))))) body)))
(defn paint-gfx! [^Graphics2D g camera cmds]
  "Paints graphics given a vector of cmds in the format described at the top of this file."
  (let [get-err (fn [] (throw (Exception. "Error was thrown when painting.")))
        err-report-cmds (fn [hint] ; These commands draw basic text and will not generate an error.
                          (gfxcustom/err-gfx (get-err) hint))]
   (cond (nil? cmds) (_paint-gfx-fairweather! g camera (err-report-cmds "Nil GFX cmd list") true)
     (not (sequential? cmds)) (_paint-gfx-fairweather! g camera (err-report-cmds "Gfx cmds must be a vector.") true)
     :else
     (let [cmds (into [] cmds)
           e-atom (atom {})
           cmds-xf-or-err-ix (try (mapv #(xgfx camera % true) cmds)
                               (catch Exception e
                                 (first (filter identity (mapv #(try (do (xgfx camera (nth cmds %) true) false)
                                                                       (catch Exception e
                                                                         (do (reset! e-atom e) %))) (range (count cmds)))))))]
       (if (number? cmds-xf-or-err-ix)
         (_paint-gfx-fairweather! g camera (gfxcustom/err-gfx @e-atom (str "Applying camera xform failed on: " (nth cmds cmds-xf-or-err-ix))) true)
         (let [debug-atom (atom nil)
               cmds-or-err (try (convert-commands cmds-xf-or-err-ix debug-atom) (catch Exception e e))]
           (if (map? cmds-or-err)
             (try (_paint-gfx-fairweather! g nil cmds-or-err false)
               (catch Exception e
                 (_paint-gfx-fairweather! g camera (gfxcustom/err-gfx e "GFX cmds could be converted, but rendering failed.") true)))
             (_paint-gfx-fairweather! g camera (gfxcustom/err-gfx cmds-or-err
                                                  (str "GFX cmd could not be converted: " @debug-atom)) true))))))))

(defn make-imagep [cmds]
  "Makes a single image's gfx cmds along with it's xform. This is before any camera translation.
   Returns [image xf] where xf is the xform that is applied to the image's graphics (xk needs to be inverted)."
  (let [xxyy (xform/xxyy-gfx-bound cmds)
        pad 5 ; padding since gfx-bound isn't perfect. Accuracy vs performance.
        x0 (- (first xxyy) pad) x1 (+ (second xxyy) pad) y0 (- (nth xxyy 2) pad) y1 (+ (nth xxyy 3) pad)

        pixels (float (* (max 1 (- x1 x0)) (max 1 (- y1 y0))))
        scaleup (min *image-scaleup* (Math/sqrt (/ *max-image-pixels* pixels))) ; scaleup so the image is higher fidelity, or scale down to not have huge images.
        xf [(- (* x0 scaleup)) (- (* y0 scaleup)) scaleup scaleup] ; xform to not cut off the up or left and to scale up.

        sx (* scaleup (- x1 x0)) sy (* scaleup (- y1 y0))

        ; Not sure how much createCompatibleImage helps, but seems to be the standard practice:
        ; https://stackoverflow.com/questions/2374321/how-can-i-create-a-hardware-accelerated-image-with-java2d?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
        ^GraphicsEnvironment ge (GraphicsEnvironment/getLocalGraphicsEnvironment);
        ^GraphicsDevice gd (.getDefaultScreenDevice ge)
        ^GraphicsConfiguration gc (.getDefaultConfiguration gd);
        ^BufferedImage img (.createCompatibleImage gc (int (+ sx 0.5)) (int (+ sy 0.5)) (int Transparency/TRANSLUCENT))
        ;(BufferedImage. (int sx) (int sy) (int BufferedImage/TYPE_INT_ARGB))
        ^Graphics2D g (.createGraphics img)
        _ (healthy-stroke! g scaleup)]
    (cp/fixed-width-font! g)
    (paint-gfx! g xf cmds) [img xf]))

(defn add-image! [^Graphics2D g ^BufferedImage sprite-im im-xform camera]
  "Adds the image to g with the xform applied from the camera."
  ; TODO: only draw images that partially or fully fit on the screen [not needed if the gfx does this optimization for us].
  (let [^AffineTransform affineXform (AffineTransform.)
        ;im-xform [(first im-xform) (second im-xform) 1 1]
        xf (xform/xx camera im-xform)
        ; AffineXforms work backwards from our system, thus the translate bf scale.
        _ (.translate affineXform (double (nth xf 0)) (double (nth xf 1)))
        _ (.scale affineXform (double (nth xf 2)) (double (nth xf 3)))
        ^ImageObserver null-observer nil]
    (.drawImage g sprite-im affineXform null-observer)))

(defn global-paint! [^Graphics2D g sprites-oldp cmds-old cmds]
  "Cmds-old and cmds are maps of sprite, each sprite is a map with vectors and other information. Cam determines how to position everything.
   cmds-old and sprites are for precomputation, set to {} to recalculate everything.
   Returns the map of sprites to use for the precompute."
  (cp/fixed-width-font! g) (interpolate-img! g)
  (let [kys (sort-by #(get-in cmds [% :z] 0.0) (keys cmds)) vls (mapv #(get cmds %) kys) ; z-sort
        ; code here a bit messy...
        cameras (zipmap kys (mapv :camera vls))
        gfx-cmds (zipmap kys (mapv :gfx vls))
        wait-ns (* *sprite-cache-ms* 1e6)
        t-now (System/nanoTime)
        ; TODO: does flushing when deleting help with memory?
        sprites1p (zipmap kys
                    (mapv #(let [cmd-old (get cmds-old %1) cmd (get cmds %1)
                                 gc-old (:gfx cmd-old) gc (:gfx cmd)
                                 sp-old (get sprites-oldp %1)
                                 t-old (if-let [t (::paint-time cmd)] t t-now) ; no ::paint-time = fresh.
                                 enough-for-sprite? (> (- t-now t-old) wait-ns)
                                 sp? (and (:bitmap-cache? %2) enough-for-sprite?)]
                             (if (and sp? sp-old (= gc-old gc)) sp-old
                               (if sp? (make-imagep gc)))) kys vls))]
    (mapv #(let [sp (get sprites1p %) cam (get cameras %)]
             (if sp
               (add-image! g (first sp) (xform/x-1 (second sp)) cam)
               (let [cmds-this-box (get gfx-cmds %)]
                 (paint-gfx! g cam cmds-this-box)))) kys)
    sprites1p))

(defn defaultPaintComponent! [g this-obj]
  "Allows us to use our gfx/paint! function on paint-component.
   Uses the repaintCmds client property.
   does NOT include the proxy-super, so you need to:
   (proxy-super paintComponent g) (gfx/defaultPaintComponent! g this)"
  (if-let [cmds (.getClientProperty this-obj "repaintCmds")]
    (let [^Graphics2D g g
          _ (.setColor g ^Color (Color. (float 0) (float 0) (float 0) (float 1)))
          _ (.fillRect g (int 0) (int 0) (int 2000) (int 2000))
          old-spritesp (if-let [x (.getClientProperty this-obj "precomputeGfxSpritesp")] x {})
          old-cmds (if-let [x (.getClientProperty this-obj "precomputeGfxCmds")] x {})
          ; Put the timestamps on the new commands, restamping those that change:
          now (System/nanoTime)
          cmds (zipmap (keys cmds)
                 (mapv (fn [k cmd]
                         (let [cmd-old (get old-cmds k) ptime-old (::paint-time cmd-old)
                               ptime (if (and ptime-old (= (:gfx cmd) (:gfx cmd-old))) ptime-old now)] ; use the old cmd ptime if things havent changed.
                           (assoc cmd ::paint-time ptime))) (keys cmds) (vals cmds)))
          new-spritesp (if cmds (global-paint! g old-spritesp old-cmds cmds) {})]
      (.putClientProperty this-obj "precomputeGfxCmds" cmds)
      (.putClientProperty this-obj "precomputeGfxSpritesp" new-spritesp))))
