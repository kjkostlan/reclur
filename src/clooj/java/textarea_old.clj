; Text areas are fairly easy to use and thus need no abstraction for most functions.
; However, these are a few useful ones that you should consider, along with the trivial abstractions
; so you don't have to import two files.
; TODO: move everything form the utils class that has to do with textareas.

(ns clooj.java.textarea_old
  (:require [clooj.utils :as utils] [clojure.string :as string] [clooj.coder.io :as cio]
    [clooj.coder.grammer :as grammer])
  (:import (java.awt KeyboardFocusManager Graphics2D Color Font RenderingHints Point Rectangle)
           (javax.swing SwingUtilities JOptionPane JPanel JLabel SpringLayout JScrollPane JTextArea)
           (java.nio.file Files Paths)
           (java.nio.charset StandardCharsets)
           (java.awt.event MouseListener MouseAdapter)
           (javax.swing.event UndoableEditListener CaretListener)
           (javax.swing.undo UndoManager CompoundEdit)
           (java.util.prefs Preferences)
           (org.fife.ui.rsyntaxtextarea RSyntaxTextArea SyntaxConstants TokenMakerFactory)    
           (org.fife.ui.rtextarea RTextScrollPane)
           (java.awt.image BufferedImage)
           jcode.TabToSpaceFilter))

;; The retnia likes to draw half resolution images. Set this to 2 to maintain high resolution.
(def superes (int 2))

(def parsy (fn [text] (try (let [p (grammer/basic-parse text false false)]
                                {:level (into [] (:level p)) :mode (into [] (:mode p))})
                           (catch Exception e (do (println "parse problems: " e)
                                                (cio/parse-summary text))))))
;; Code analysis.

(defn _slow-calc [txt f]
  ; add more slow stuff here as time goes on!
  (if (nil? f) {:text txt :levels (into [] (repeat (count txt) 0)) :formats (into [] (repeat (count txt) 0))} ;the default pattern for which nothing special occurs.
    (let [summary (f txt)]
      {:text txt :levels (into [] (:level summary)) :formats (into [] (:mode summary))})))

;; Color rendering

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
;(println "k " k " " (= k 0) " " [1 p t])
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

(defn _get-char-boxes [txt fontmetrix]
 ;Format is [[x y w h], [x y w h], ...]
 ; optimized for constant-width, as this is suprisingly slow.
  (let [n (count txt)
        w (* superes (.charWidth fontmetrix \A )) ; its a fixed-width so it's all the same, we have no tabs.
        h (* superes (.getHeight fontmetrix))]
    (if (= n 0) []
      (let [isnew (into [] (map #(= (str %) "\n") txt))]; into [] makes performace increase from ~600ms to ~3 ms!
        (loop [x 0 y 0 ix 0 acc []]
         (if (= ix n) acc
           (let [ex (if (nth isnew ix) 0 w) newacc (conj acc [x y ex h])];
             (if (nth isnew ix) ; newlines bump the y, otehrwise the x gets bumped.
               (recur 0 (+ y h) (inc ix) newacc)
               (recur (+ x ex) y (inc ix) newacc)))))))))

(defn _float01 [x] (float (max 0 (min 1 x)))) ; colors are picky.
(defn _rgb [gfx triple] 
  (.setColor gfx (Color. (_float01 (first triple)) (_float01 (second triple)) (_float01 (nth triple 2)))))

(defn _render-formatted-line [gfx line yval cols bcols rects ishighlight isselected islink x0 x1 y0 y1]
; cols and bcols of each character.
; gfx = Graphics2D, line = string, yval = where to draw line
; cols = array of rgb triples [0 1], tw = width of text, th = height of text
; x0 x1 y0 y1 = bounds (speeds us up, we do nothing if out of bounds).
; TODO: optimize for blanck colors and groups of colors!
  (if (and (>= yval y0) (<= yval y1)) ; bounds check.
    (let [n (count line)
         r (range n)
         xs (map #(- (nth % 0) (int x0)) rects)
         ys (map #(- (nth % 1) (int y0)) rects)
         ws (map #(nth % 2) rects)
         hs (map #(nth % 3) rects)]
      (doall (map #(do (_rgb gfx %1) (.fillRect gfx %2 %3 %4 %5)) bcols xs ys ws hs))
      (doall (map #(do (_rgb gfx %1) (.drawString gfx (str %2) %3 (- yval (int y0)))) cols line xs))
      (_rgb gfx [1 1 0]) ; what highlight color should we use?
      (doall (map #(if %1 (.drawRect gfx %2 %3 %4 %5)) ishighlight xs ys ws hs))
      (_rgb gfx [0 0 1]) ; what link color should we use?
      (doall (map #(if (= %1 1) (.drawLine gfx %2 (int (dec (+ %3 %5))) (int (+ %2 %4)) (int (dec (+ %3 %5))))) islink xs ys ws hs))
      )))

(defn _level2col [level]
  ; a nice color scheme.
  ;(let [sat (min 1 (* 0.125 (Math/ceil (/ (- level 0.00001) 6.0)))) ]
  ;  (hsv2rgb [(mod (/ (+ level 5.0) 6.0) 1.0) sat 1]))
  ; Perceptual distances of hues:
  ; http://jov.arvojournals.org/data/Journals/JOV/932812/i1534-7362-13-7-1-f03.jpeg
  (let [hvals (into [] (map #(/ % 360) [0 37 55 80 115 153 180 220 320]))
        b (if (= level 0) 1 (+ 0.87 (* 0.08 (mod level 2))))
        h (nth hvals (mod (dec level) (count hvals)))
        s (if (<= level (count hvals)) 1 0.5)] (hsl2rgb [h s b])))

(defn _render-line-by-level-format! [gfx line yval levels formats rects isselected ishighlight islink x0 x1 y0 y1]
  (let [n (count line)
        qc [0.7 0 0.5] bk [0 0 0] cc [0.3 0.5 0.3]
        cols (map #(if (= % 1) qc (if (= % 2) cc bk)) formats) ;quote comment normal. 
        cols (map #(if (= %1 0) %2 [0 0 1]) islink cols) ; link is blue.
        bcols0 (map #(if %2 (vector 1 1 1) %1) (map _level2col levels) (isedge line)); ignore initial and final whitespace.
        bcols1 (map #(if (= %2 2) (vector 1 1 1) %1) bcols0 formats); dont color comments.
        bcols (map #(if %2 (vector 0.5 0.75 0.75) %1) bcols1 isselected)]; text selection color
   (_render-formatted-line gfx line yval cols bcols rects ishighlight isselected islink x0 x1 y0 y1)))

(defn _render-caret! [gfx rects caret x0 y0 texth]
  (let [g0 (> (count rects) caret) r0 (> (count rects) 0) ; annoying off-by-one errors.
        endingnewline (and r0 (not g0) (= (nth (last rects) 2) 0))
        r (if g0 (nth rects caret) (if r0 (last rects) [0 0 0 texth]))
        x (if g0 (- (nth r 0) x0) (- (+ (nth r 0) (nth r 2)) x0))
        ya (- (nth r 1) y0) yb (+ ya (nth r 3))
        x (if endingnewline 0 x)
        ya (if endingnewline (+ ya texth) ya)
        yb (if endingnewline (+ yb texth) yb)
        x (max x 1) ya (max ya 1)]
    (_rgb gfx [0 0 0])
    (.drawRect gfx (int (dec x)) (int (dec ya)) 2 (int (+ (- yb ya) 2)))
    (_rgb gfx [1 1 1])
    (.drawLine gfx (int x) (int ya) (int x) (int yb))))

; TODO: refactory this large aritiness into a nice package.
(defn _slow-render! [gfx txt levels formats isselect ishighlight links font rects caret bounds hasfocus]
  "Renders graphics to the text to simulate syntax hilighting, etc.
   bounds is [x0 x1 y0 y1]. tw and th are the text width and height."
  ;(let [rc #(_rgb gfx (rand) (rand) (rand))]
  ;  (doall (map #(do (rc) (.drawRect gfx (nth % 0) (nth % 1) (nth % 2) (nth % 3))) rects)))
  (.setFont gfx (.deriveFont font (float (* superes (.getSize2D font)))))
     ; super res font. Note we never .getFont on gfx, instead we use the text font for size calculations.
 ;(.setFont gfx font)
  (.setRenderingHint gfx RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_LCD_HRGB)
  (let [zeros (into [] (repeat (count txt) 0))
        fontmetrix (.getFontMetrics gfx font)
        texth (* superes (.getHeight fontmetrix))
        islink (into [] (reduce #(do (utils/set-range %1 (:lo %2) (:hi %2) 1)) zeros links)) ;(println %1 %2 (:lo %2) (:hi %2))
        asc (* superes (.getAscent fontmetrix))
        newlines (into [] (utils/which #(= % \newline) txt)) ; dividers between different areas.
        sub0 (into [] (cons 0 (map inc newlines)))
        sub1 (into [] (conj newlines (count txt))) ; the indexes of each line to render.
        lines (into [] (map #(subs txt %1 %2) sub0 sub1))
        rects1 (into [] rects)
        links-pieces (into [] (map #(subvec islink %1 %2) sub0 sub1))
        rects-pieces (into [] (map #(subvec rects1 %1 %2) sub0 sub1))
        select-pieces (into [] (map #(subvec isselect %1 %2) sub0 sub1))
        levels-pieces (into [] (map #(subvec levels %1 %2) sub0 sub1))
        formats-pieces (into [] (map #(subvec formats %1 %2) sub0 sub1))
        highlight-pieces (into [] (map #(subvec ishighlight %1 %2) sub0 sub1))
        yvalues (map #(+ asc (* %1 texth)) (range (count lines)))]
    (doall (map #(_render-line-by-level-format! gfx %1 %2 %3 %4 %5 %6 %7 %8 (nth bounds 0) (nth bounds 1)
                 (nth bounds 2) (nth bounds 3)) lines yvalues levels-pieces formats-pieces rects-pieces select-pieces highlight-pieces links-pieces))
    (if hasfocus (_render-caret! gfx rects caret (nth bounds 0) (nth bounds 2) texth))))

(defn _make-image [txt f isselect ishighlight links font caret bounds hasfocus]
 ; Creates a buffered image for the txt and draws to it. bounds is x0 x1 y0 y1.
 ; this is slow so should be wrappe din a future.
 (let [w (- (nth bounds 1) (nth bounds 0)) h (- (nth bounds 3) (nth bounds 2))
       img (BufferedImage. w h BufferedImage/TYPE_INT_RGB)
       gfx (.createGraphics img)
       calc (_slow-calc txt f)
       fontmetrix (.getFontMetrics gfx font)]
   (.setPaint gfx (Color. 255 255 255)) ;White background.
   (.fillRect gfx 0 0 (.getWidth img) (.getHeight img));
   (_slow-render! gfx txt (:levels calc) (:formats calc) isselect ishighlight links font
     (_get-char-boxes txt fontmetrix) caret bounds hasfocus)
    img))

(defn _visible-bounds [ta]
  ; x0 x1 y0 y1. Draw stuff within this region to see stuff.
  (let [loc (.getLocation ta) sz (.getSize (.getParent ta))
        x0 (* (* (.getX loc) -1) superes) y0 (* (* (.getY loc) -1) superes)
        x1 (+ x0 (* (.getWidth sz) superes)) y1 (+ y0 (* (.getHeight sz) superes))]
    (vector x0 x1 y0 y1)))

(defn _get-highlighted [ta]
  ; java wins the golden cocoon award.
  (let [hil (.getHighlighter ta) out (into [] (repeat (count (.getText ta)) false))]
    (if (nil? hil) 
      out; no highlighter means no highlights.
      (let [his (.getHighlights hil)]
        (into [] (reduce #(utils/set-range %1 (.getStartOffset %2) (.getEndOffset %2) true) out his))))))

(defn painter-textbox [state]
  "Try to maximize responsivness despite slow calculations/render."
  (proxy [RSyntaxTextArea] [] ;RSyntaxTextArea JTextArea
    (paintComponent [gfx]
        (proxy-super paintComponent gfx)
        (let [redo (fn [] (SwingUtilities/invokeLater #(.repaint this))) ; if the future has different parameters.
              fut (:fut @state)
              futcomplete (and (future-done? fut) (not (future-cancelled? fut)))]
          (if futcomplete ; the slow stuff is done.
            (let [; all these have to match for the graphics to agree:
                  txt (.getText this)
                  isselect (get-selected-indicator this)
                  ishighlight (_get-highlighted this)
                  caret (.getCaretPosition this) 
                  bounds (_visible-bounds this)
                  hasfocus (.hasFocus this)
                  font (.getFont this) ; except this, equality is hard to test since it can be mutated.
                  fc @fut
                  ; forcerepaint makes agreement false.
                  futagree (and (= (:hasfocus fc) hasfocus) (= (:txt fc) txt) (= (:bounds fc) bounds) (= (:isselect fc) isselect) (= (:caret fc) caret) (not (:forcerepaint @state)))
                  imfn #(_make-image txt (:parsefn @state) isselect ishighlight (:links @state) font caret bounds hasfocus)]
              (swap! state assoc :forcerepaint false)
              (if futagree
                (let [x0 (int (first bounds)) y0 (int (nth bounds 2))
                      w (int (- (nth bounds 1) x0)) h (int (- (nth bounds 3) y0))]
                  (.drawImage gfx (:img fc) (int (/ x0 superes)) (int (/ y0 superes)) (int (/ w superes)) (int (/ h superes)) nil)) ; the future is great! Its draw time.
                ; disagree. redo future and have it repaint us.
                (do (swap! state assoc :fut (future {:img (imfn) :hasfocus hasfocus :txt txt :bounds bounds :caret caret :isselect isselect}))
                    (utils/when-done (:fut @state) redo)))
              ))
            ; do nothing if the slow stuff is not done, as we must wait.
          ))))

;; Setup.

(defn add-edit! [undoMgr undoEdit]
  ;(println "adding edit")
  (.addEdit undoMgr undoEdit))

;; Work around for set-text! making there be two edit steps: skip over blanck text if possible.

(defn undo! [ta undoMgr]
   (let [caret (.getCaretPosition ta)]
     (if (.canUndo undoMgr)
       (do (.undo undoMgr)
         (if (and (.canUndo undoMgr) (= (count (.getText ta)) 0))
           (do (.undo undoMgr) (.setCaretPosition ta (int (min (count (.getText ta)) caret)))))))))

(defn redo! [ta undoMgr]
   (let [caret (.getCaretPosition ta)]
     (if (.canRedo undoMgr)
       (do (.redo undoMgr)
         (if (and (.canRedo undoMgr) (= (count (.getText ta)) 0))
           (do (.redo undoMgr) (.setCaretPosition ta (int (min (count (.getText ta)) caret)))))))))

(defn make-undoable! [ta undostate undoMgr]
  ;This fancy :combine-with-next-edit DOES NOT WORK! I don't know why.
  (.setLimit undoMgr 1000)
  (.. ta getDocument (addUndoableEditListener
      (reify UndoableEditListener
        (undoableEditHappened [this evt]
          ;(println "some edit" (:combine-with-next-edit @undostate))
          ; We can set a flag to colesce this event with the next one.
          (if (:combine-with-next-edit @undostate) ;first or second of two edits.
            (if (nil? (:last-edit @undostate))
              (swap! undostate assoc :last-edit (.getEdit evt)) ; first of two edits to combine.
              (let [edit0 (:last-edit @undostate) ; edit0 was the first, now we are the second of two edits.
                    edits (CompoundEdit. )]
                (.addEdit edits edit0)
                (.addEdit edits (.getEdit evt))
;(println "combine " (.addEdit edit0 (.getEdit evt)))
                (add-edit! undoMgr edits)
                (swap! undostate assoc :combine-with-next-edit false :last-edit nil)))
            (add-edit! undoMgr (.getEdit evt))))))) ;normal edit with no combining.
   (utils/attach-action-keys! ta
    ["cmd1 Z" #(undo! ta undoMgr)]
    ["cmd1 Y" #(redo! ta undoMgr)]
    ["cmd1 shift Z" #(redo! ta undoMgr)]))

(defn mouse-press [ta paintstate evt]
 "Mouse presses on the link will trigger the link function."
 (let [caret (.getCaretPosition ta) links (:links @paintstate) n (count links)
      f (loop [ix 0] (if (>= ix n) 
                       #() 
                       (if (and (>= caret (:lo (nth links ix))) (<= caret (:hi (nth links ix))))
                         (:f (nth links ix))
                         (recur (inc ix)))))] (f)))

(defn _make-text-area [editable wrap paintstate]
  (let [out (doto (painter-textbox paintstate); (RSyntaxTextArea.)
      (.setHighlightCurrentLine false)
      ;(.setAnimateBracketMatching false)
      ;(.setBracketMatchingEnabled false)
      ;(.setAutoIndentEnabled false)
      ;(.setAntiAliasingEnabled true)
      ;(.setSyntaxEditingStyle SyntaxConstants/SYNTAX_STYLE_CLOJURE)
      (.setFont (Font. "Courier" Font/PLAIN 12))
      (.setEditable editable)
      (.setLineWrap wrap))]
    ; Blinking forces a redraw, which is expensive for us.
    (.setBlinkRate (.getCaret out) 0) out))

(defn boxed-new [label editable wrap]
  "Makes a new JTextArea, boxed inside of a scroll pane and panel. all inputs are strings"
  (let [panel (JPanel.)
        label (JLabel. label)
        ;paintstate (atom {:fut (future {:txt ""}) :parsefn cio/parse-summary :links []})
        paintstate (atom {:fut (future {:txt ""}) :parsefn parsy :links []})
        textarea (_make-text-area editable wrap paintstate)
        scrollpane (RTextScrollPane. textarea)
        undoMgr (UndoManager.)
        undostate (atom {})] ;RTextScrollPane JScrollPane
    (make-undoable! textarea undostate undoMgr)
    (doto panel (.setLayout (SpringLayout.))
      (.add scrollpane)
      (.add label))  
    (.setLineNumbersEnabled scrollpane true)
    (.setDocumentFilter (.getDocument textarea) (TabToSpaceFilter. 4 textarea))
    (.addMouseListener textarea
      (proxy [MouseAdapter] [] (mousePressed [evt] (mouse-press textarea paintstate evt))))
    {:panel panel :label label :textarea textarea :scrollpane scrollpane :undoMgr undoMgr
      :undostate undostate :paintstate paintstate
      :springs [[:N label -3 :N panel]
                [:N scrollpane 0 :S label]
                [:S scrollpane 0 :S panel]
                [:E label 0 :E panel]
                [:E scrollpane 0 :E panel]
                [:W scrollpane 0 :W panel]]
      :root panel}))

;(defn add-to-frame! [frame area]
;  "TODO Adds the text area to the frame")

(defn get-focus []
  "gets the object that has focus."
  (.getFocusOwner (KeyboardFocusManager/getCurrentKeyboardFocusManager)))

(defn _score [string sub]
  ; Best match (returns index where it is > 0). All inputs must be vectors.
  ; the beginning of sub starts it index.
    (let [k (count sub)
          n (count string)
          scfn (fn [ix] (reduce + 0 (map #(if (= %1 %2) 1 0) (subvec string ix (+ ix k)) sub)))
          maxix (- n k)]
      (utils/argmax scfn (range 0 (+ maxix 1)))))

; TODO: this is SLOW!
(defn str-match [old0 new0 indold]
  "Returns indnew, which is where indold 'goes', uses simple method not the best.
   Useful when you change the string but don't want the user to expirance a jump in cursor position."
  (let [old (apply vector old0) snew (apply vector new0)
        ; pick the median match among several strings. Large offsets are less useful as jumping is enevitable.
        offsets [-50 -30 -10 0 10 20 50 -100 100] ; odd # is better for median.
        lengths [10  10  10 10 10 10 10 20 20]
        pieces (map #(utils/ssubvec old (+ %1 indold) (+ %1 %2 indold)) offsets lengths)
        ; this gets non-sensical at boundaries, but median should take care of that.
        news (map #(- (_score snew %2) %1) offsets pieces)]
    (utils/median (into [] news))))

(defn get-text [atomtext]
  "Same as digging out the textarea and getting it's text.'"
    (let [ta (:textarea @atomtext)] (.getText ta)))

(defn get-caret-position [atomtext]
"Same as digging out the textarea and getting it's caretPosition.'"
(let [ta (:textarea @atomtext)] (.getCaretPosition ta)))

(defn _simultab [text] ; does the filter override append text, making this unnessessary?
  (string/replace text "\t" "    "))

(defn get-coords [atomtext offset]
  (let [row (.getLineOfOffset (:textarea @atomtext) offset)
        col (- offset (.getLineStartOffset (:textarea @atomtext) row))]
    {:row row :col col}))

(defn get-caret-coords [atomtext]
  (get-coords atomtext (.getCaretPosition (:textarea @atomtext))))

(defn set-caret! [atomtext newcaret]
  (let [ta (:textarea @atomtext) n (count (.getText ta))]
    (.setCaretPosition ta (min n (max 0 newcaret)))))

(defn rect-to-vector [r] ; keep things immutable, minimize outside classes getting mutable stuff.
  [(.getX r) (.getY r) (.getWidth r) (.getHeight r)])

(defn vector-to-rect [v] ; keep things immutable.
  (Rectangle. (int (first v)) (int (second v)) (int (nth v 2)) (int (nth v 3))))

(defn get-place [atomtext]
  "gets {:rect :caret}"
  (let [ta (:textarea @atomtext)]
  {:rect (rect-to-vector (.getVisibleRect ta)) :caret (.getCaretPosition ta)}))

(defn set-place! [atomtext place]
"sets the place to a previous get-place"
  (let [ta (:textarea @atomtext)]
    (.scrollRectToVisible ta (vector-to-rect (:rect place)))
    (set-caret! atomtext (:caret place))))

(defn add-caret-listener! [atomtext f]
  (.addCaretListener (:textarea @atomtext)
                     (reify CaretListener (caretUpdate [this evt] f (:textarea @atomtext)))))

(defn set-text! [atomtext ntext newcaret]
  "The change is a simple undo and you also set the caret position."
    (let [ta (:textarea @atomtext) newtext (_simultab ntext)]
;(if (> (count (.getText ta)) 0) ; Non-empty text triggers two edit events.
;       (swap! (:undostate @atomtext) assoc :combine-with-next-edit true))
      (.setText ta newtext)
      (set-caret! atomtext newcaret)))
      ;(swap! (:undostate @atomtext) assoc :combine-with-next-edit false :last-edit nil)))

(defn _redraw-link [atomtext] ; links change, we must show this.
  (let [p (:paintstate @atomtext)]
    (swap! p assoc :forcerepaint true)
    (.repaint (:textarea @atomtext))))

(defn add-link! [atomtext f lo hi]
  "The zero-arg function fs is called"
  (let [p (:paintstate @atomtext) ln (:links @p)] ;fun with nested atoms.
    ;(println "b4: " ln " afr: " (conj ln {:lo lo :hi hi :f f}))
    (swap! p assoc :links (conj ln {:lo lo :hi hi :f f}))
    (.repaint (:textarea @atomtext)) (_redraw-link atomtext)))

(defn append-text!
  ([atomtext text  scroll-to-end?]
    (.append (:textarea @atomtext) (_simultab text))
    (set-caret! atomtext (count (get-text atomtext))))
  ([atomtext text]
    (append-text! atomtext (_simultab text) true)))

(defn append-link-text! [atomtext text fs los his scroll-to-end?]
  "appends text with multible links."
  (let [offset (count (.getText (:textarea @atomtext)))]
    (append-text! atomtext text scroll-to-end?)
    (doall (map #(add-link! atomtext %1 %2 %3) fs (map #(+ % offset ) los) (map #(+ % offset ) his)))))

(defn append-link-text1! [atomtext text f lo hi scroll-to-end?]
  "appends text with one link."
  (let [offset (count (.getText (:textarea @atomtext)))]
    (append-text! atomtext text scroll-to-end?)
    (add-link! atomtext f (+ lo offset) (+ hi offset))))

(defn requestFocusInWindow! [atomtext]
  (.requestFocusInWindow (:textarea @atomtext)))

(defn request-focus! [atomtext]
  (.requestFocus (:textarea @atomtext)))

(defn scroll-to-pos! [atomtext offset]
  (let [r (.modelToView (:textarea @atomtext) offset)
        v (.getParent (:textarea @atomtext))
        l (.. v getViewSize height)
        h (.. v getViewRect height)]
    (when r
      (.setViewPosition v
                        (Point. 0 (min (- l h) (max 0 (- (.y r) (/ h 2)))))))))

(defn scroll-to-line! [atomtext line]
    (let [text (.getText (:textarea @atomtext))
          pos (inc (.length (string/join "\n" (take (dec line) (string/split text #"\n")))))
          n (count (.getText (:textarea @atomtext)))]
      (if (or (< pos 0) (> pos n)) nil
        (do (.setCaretPosition (:textarea @atomtext) pos)
               (scroll-to-pos! atomtext pos)))))

(defn scroll-to-caret! [atomtext]
  (scroll-to-pos! atomtext (.getCaretPosition (:textarea @atomtext))))

(defn move-caret-to-line! [atomtext]
  "Move caret to choosen line in textarea"
  (defn current-line []
    (inc (.getLineOfOffset (:textarea @atomtext) (.getCaretPosition (:textarea @atomtext)))))

  (let [line-str (utils/ask-value "Line number:" "Go to Line")
        line-num  (Integer.
                    (if (or (nil? line-str) (nil? (re-find #"\d+" line-str)))
                      (current-line)
                      (re-find #"\d+" line-str)))]
  (scroll-to-line! atomtext line-num)
  (.requestFocus (:textarea @atomtext))))

(defn clear-undo! [atomtext]
  "Don't allow undoing before this point."
  (.discardAllEdits (:undoMgr @atomtext)))

(defn set-parse! [atomtext f]
  "sets the parse function to use"
  (let [p (:paintstate @atomtext)] ;fun with nested atoms.
    (swap! p assoc :parsefn f)))

(defn clear-links! [atomtext]
  (let [p (:paintstate @atomtext)] ;fun with nested atoms.
    (swap! p assoc :links []) (_redraw-link atomtext)))

(defn append-link1! [gotofn!! atomtext text textix file fx]
  "gotofn!! takes in a file name and a caret and navs you there.
  atomtext is the textarea that you make the function in, which  gotofn!!'s on file and fx.
  text is the string to add, textix is [st en]."
  ; the second arg is an array of functions.
  (append-link-text1! atomtext text #(gotofn!! file fx) (first textix) (second textix) true))
    