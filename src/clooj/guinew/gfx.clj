; TODO: examples are out of date slightly in thier format.

; The default graphics updating that is made to be more functional.
; Performance is favored over a super flexible API, as performace for graphics was found to be the bottleneck.
; As a rule of thumb, light classes like Color are represented by vectors of numbers 1:1 with the constructor.
; Heavy classes such as arrays are represented by the java object themselves (the user must pass in a Java object).
; TODO: handle affine transforms (partially implemented, need to handle the rotate scale etc cases).
; TODO: have a compiler for maximum performance that creates code for a :java command.
(ns clooj.guinew.gfx)

; Each object can have :Graphics which is a vector of commands:
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
(ns clooj.guinew.gfx
  (:import [java.awt.image BufferedImage]
    [java.awt Graphics2D])
  (:require [clojure.set :as set] [clooj.guinew.getset :as getset]
    [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Specific code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def tform-names "Hash-set of transform names, they are like setters but have a non-standard syntax."
  #{"translate" "rotate" "scale" "shear" "transform"})

(def simple-constructors
  "Map from keywords to constructor functions."
  {:Color '(java.awt.Color. (float (first x)) (float (second x)) (float (nth x 2)) (float (nth x 3)))
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

(def custom-cmds ; Custom commands for the java argument.
  {:FontSize (fn [^java.awt.Graphics2D g sz] (.setFont g (.deriveFont (.getFont g) (float sz))))
   :getFontSize (fn [^java.awt.Graphics2D g] (.getSize2D (.getFont g)))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Code generation (compile time w/type hints).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
  (zipmap (concat (keys _cmd-bank) (keys custom-cmds)) 
          (concat (mapv eval (vals _cmd-bank)) (vals custom-cmds))))

(def set-get-map ; map from :Color to :getColor, etc.
  (let [vs (filterv #(.startsWith (str %) ":get") (keys cmd-bank))]
    (zipmap (mapv #(keyword (subs (str %) 4)) vs) vs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;; Rendering runtime.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn convert-commands [cmds]
 "Converts commands from functional drawing format to java's mutable-state format.
  Returns :intro and :cmds
  The input commands have an optional third argument that specifies (i.e. color).
  We pull out the third argument into i.e. :Color and reverse the setColor command for the next run.
  :intro is :getColor, etc commands for finding out what the defaults are, :cmds are the main command.
  The :default keyword as a second argument means use the defaultinstead of the second arg itself."
  (let [n (count cmds)
        ever-changed (apply hash-set (apply concat (mapv #(if-let [x (get % 2)] (keys x) []) cmds))) ; #{:Color, :Stroke, etc}
        intro (mapv #(keyword (str "get" (subs (str %) 1))) ever-changed)] 
    {:intro intro :cmds
      (loop [acc [] non-defaults {} ix 0]
        (if (= ix (count cmds)) acc
          (let [cmd (nth cmds ix)
                specs (if-let [x (get cmd 2)] x {}) ; The second argument is stuff like color, etc that mutates the java class.
                ; Works for both java and non-java objects:
                changes (reduce #(let [sp (get specs %2) old (get non-defaults %2)]
                                   (cond (or (identical? sp old) (= sp old)) ; identical? used as well to avoid reflection when we have java objects (assuming references are reused).
                                     (dissoc %1 %2) (not sp) (assoc %1 %2 :default) :else (assoc %1 %2 sp)))
                          specs (keys non-defaults))] ; changes to the :Color, etc.
            (recur (conj (apply conj acc (mapv vector (keys changes) (vals changes))) [(first cmd) (second cmd)]) ; changes then 
              specs (inc ix)))))})) ; at each step the non-defaults going in are the specs from the last step.

(defn update-graphics! [java-ob old-clj-value new-clj-value]
  "Updates the graphics, calling a .repaint if needed."
  (if (or (not= (:Graphics old-clj-value) (:Graphics new-clj-value)) (:Repaint? new-clj-value)) ; update detected.
    (do (.putClientProperty java-ob "repaintCmds" (:Graphics new-clj-value)) (.repaint java-ob))))

(defn report-e [m-extra e] "Reports an exception and throws it, with m-extra as extra message."
  (throw (RuntimeException. (str m-extra " " (.getMessage e)) e)))

(defonce dbat (atom [])) ; debugging.
;(do (require '[clooj.coder.repl :as repl]) (repl/clc))
;(println (nth (:post @dbat) 500))

(defn paint! [^Graphics2D g cmds0]
  (let [cmds (convert-commands cmds0)
        intro (:intro cmds) ; defaults we need to store.
        body (:cmds cmds) ; the drawing itself.
        bank cmd-bank
        sg-map set-get-map ; Setting keywords -> getting keywords.
        g-code (fn [k] (binding [*print-meta* true] (pr-str (k _cmd-bank)))) ; for error reporting.
        defaults (zipmap intro (mapv #(try ((get cmd-bank %) g) ; Settings that will be changed, requiring us to store defaults.
                                        (catch Exception e (report-e (str "Error getting default:" % " on: " (g-code %)) e))) intro))]
    (reset! dbat {:pre cmds0 :post body})
    (mapv #(let [k (first %)] ; kc (k bank)
             (if (= k :java)
               (try ((second %) g) (catch Exception e (report-e "Custom user graphics call error: " e)))
               (let [kc (k bank)]
                 (if kc 
                   (let [arg (second %) f (k bank)]
                     (cond (nil? arg) (throw (RuntimeException. (str "Nil value for draw command: " k)))
                       (nil? f) (throw (RuntimeException. (str "Unrecognized draw command or setting: " k)))
                       (= arg :default)
                       (try ((k bank) g (get defaults (sg-map k)))
                         (catch Exception e (report-e (str "Error (when resetting to defaults) for command: " (k bank) " given: " (get defaults (sg-map k)) " on: " (g-code k)))))
                       :else
                       (try ((k bank) g arg)
                         (catch Exception e (report-e (str "Error for command: " (k bank) " given: " arg " on: " (g-code k)) e)))))
                   (throw (Exception. (str "Unrecognized draw command: " k)))))))
      body)))

(defn defaultPaintComponent! [g this-obj]
  "Allows us to use our gfx/paint! function on paint-component.
   does NOT include the proxy-super, so you need to:
   (proxy-super paintComponent g) (defaultPaintComponent g this)"
  (if-let [cmds (.getClientProperty this-obj "repaintCmds")] (paint! g cmds)))