; Text editing with multiple pieces.
;   This makes editing more complex data easier.
; There is no "right" way to handle multible pieces. Defaults are all provided,
; but there are two ways to override them:
  ; Change what happens when a mouse is pressed, etc.
  ; Change the functions that handle specific things, like a deletion (see default functions below).
      ; These are encoded as :xyz-fn in the editor.

(ns app.rtext
  (:require [javac.clipboard :as clipboard]
    [globals] ; a slight kludge using the state.
    [clojure.string :as string]
    [app.stringdiff :as stringdiff]))

; Variables stored here:
; :pieces = a vector of 
   ; :text = the pieces rendered text value.
   ; <other stuff> user data.
; :cursor-ix is the current cursor index, counting the space between the pieces.
; :font-size is the font size. The actual font size can be different because of physics-induced compression, etc.
; :selection-start and :selection-end are the selected text range, inclusive.
  ; Dragging the cursor to location x makes :selection-end x-1. 
; :scroll-top and :scroll-left = upper left corner scroll position.
; :size is a two element vector in pixels.
; TODO: maybe use inclusive-exclusive patterns for ranges.
; TODO: an editor that handles extremly long text.

(defn ctrl+ [key-evt]
  "If the user hits control-a returns the character a, etc. Returns nil otherwise."
  (if (:MetaDown key-evt) (:KeyChar key-evt)))

(defn arrow-key [key-event]
  "Returns the character ^ v < or > for arrow keys. Returns nil otherwise."
  (let [kc (:KeyCode key-event)] 
    (cond (= kc 37) \< (= kc 39) \> (= kc 38) \^ (= kc 40) \v)))

(defn typed-key [key-event]
  "Returns the character typed, including case, tabs, backspaces, and newlines. Returns nil otherwise."
  (let [ch (:KeyChar key-event)
        ^String s "`1234567890-=\b\tqwertyuiop[]\\asdfghjkl;'\nzxcvbnm,./~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>? "]
    (if (.contains s (str ch)) ch)))

(defn esc? [key-event]
  "Did we hit the escape key?"
  (= (int (:KeyChar key-event)) 27))

;;;;;;;;;;;;;;;;;;;; Defaults ;;;;;;;;;;;;;;;;

(def ^{:dynamic true} *text-params* 
  {:margin 2 ; Padding when there is no scrollbar.
   :font-linespace-to-size 0.85 ; Not quite as high as advertized.
   :font-width-to-size 0.6; could also be gotten from the font metrics.
   :max-autofit-size-chars 30 ; automatically fit small text (this appers deprecated).
   :font-xshift-to-size 0 ; Don't know where this comes from.
   :font-yshift-to-size 0.25 ; center the text vertically.
   :line-no-standoff-chars 2.5
   :line-fontsz-mult 0.8
   :fit-to-text-margin 1}) ; extra space so that the text doesn't get cutoff. Only used in the fit-to-text fn.

(defn remove-empty [pieces]
  "Removes empty pieces, always keeping one piece. 
   This is only used for the defult edit functions, you may want different behavior."
  (let [p1 (filterv #(> (count (:text %)) 0) pieces)]
    (if (> (count p1) 0) p1 [(first pieces)])))

(defn default-insert [box x stats str?]
  "inserts x (a string or vector of pieces) into box with the help of stats and maybe in place of a selection. 
   str? is true when x is a string."
  (let [px-0 (:piece-0 stats) px+0 (:piece+0 stats) px-1 (:piece-1 stats) px+1 (:piece+1 stats)
        jx-0 (:jx-0 stats) jx+0 (:jx+0 stats) jx-1 (:jx-1 stats) jx+1 (:jx+1 stats)
        same-piece? (>= px+0 px-1) ; within the same piece (or between two pieces).
        xv (if str? [{:text x}] x) cpixs (:copy-piece-ixs stats)
        copy11? (and (= (count cpixs) 1) ; copied from the same index.
                  (or (= px+0 (first cpixs)) (= px-1 (first cpixs))))
        np (count (:pieces box))
        ;overflow? (>= (px+0 stats) np)
        ]
((fn [bx] (assoc (update bx :pieces remove-empty) :cursor-ix (apply + (:ix0 stats) (mapv #(count (:text %)) xv)))) ; cursor to end of insert.
   (if (and (>= px+0 np) (< px-1 0)) ; corner case with empty box, both directions overflow.
      (if str? (assoc-in box [:pieces 0 :text] (str x)) (assoc box :pieces xv))
      (if (and same-piece? (or str? copy11?)) ; in-place modification of the string.
        (let [px (max 0 px-1) ; can use :piece+0 and :piece-1, which will allocate the insertion differently on the boundaries. 
              jx0 (cond (= px-0 px) jx-0 (>= px+0 px) jx+0 :else (throw (Exception. "coding error here bad choice of px")))
              jx1 (cond (= px-1 px) jx-1 (>= px+1 px) jx+1 :else (throw (Exception. "coding error here bad choice of px.")))]
        (update-in box [:pieces px :text]
          #(str (subs % 0 jx0) (apply str (mapv :text xv)) (subs % jx1))))
        (assoc box :pieces (into [] (concat (:b4 stats) xv (:afr stats)))))))))

(defn default-delete [box stats]
  "Delete pressed on selection from ix0 to ix1, inclusive-exlcuisve pattern.
   Stats is the same as it is in default-insert."
   (default-insert box "" stats true)) ; empty insert.

(defn default-doubleclick [box c-ix p-ix jx]
  "c-ix = cursor ix, p-ix = piece ix at cursor, jx = location within piece at cursor. 
   The indexes round up. We select the piece."
  (if-let [p (:text (get (:pieces box) p-ix))]
    (let [sel-end (+ (- c-ix jx) (count p))] 
     (assoc box :selection-start (- c-ix jx) :selection-end sel-end :cursor-ix sel-end)) box))

(defn default-partial-grab [x txt ix01] 
  "Gets a single piece given a piece, the grabbed text, and the indexes of the text."
  (assoc x :text txt))

;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;

(defn v [box]
  "make the pieces a vector."
  (if (vector? (:pieces box)) box (update box :pieces #(into [] %))))

(defn default-colorize [s pieces piece-ix char-ix0 char-ix1]
  "Simple alternating colors."
  (mapv #(if (even? %) [1 1 1 1] [0.7 0.7 1 1]) piece-ix))

(defn pieces-digest [pieces]
  "Piece-based version of string digest.
    :counts = # chars/piece. :num-b4[i] = # chars before piece[i], it has one more element than length of lines."
  (let [texts (mapv :text pieces) counts (mapv count texts)]
    {:counts counts :num-b4 (into [] (reductions + 0 counts)) ; no need for the \n.
     :nchars (apply + counts) :npieces (count counts)}))

(defn ixjx-pieces [pieces ix0 ix1]
  "[piece ix0, piece ix1, jx0 jx1] wherejx is the cursor within the piece.
   piece-ix0 rounds up and piece-ix1 rounds down when it lands on the boundary.
   This means piece-ix0 can be length and piece-ix1 can be -1. The j's are zero in those cases."
  (let [pd (pieces-digest pieces) nb4 (:num-b4 pd)
        n (:npieces pd) counts (:counts pd) 
        mx (:nchars pd) ix0 (min (max ix0 0) mx) ix1 (min (max ix1 0) mx) ; clamp.
        cix0 (last (filter #(> % -1) (map #(if (<= (nth nb4 %) ix0) % -1) (range (inc n)))))
        cix1 (if (= ix1 0) -1
               (first (filter #(> % -1) 
                        (map #(if (>= (+ (nth nb4 %1) %2) ix1) %1 -1) (range (inc n)) (conj counts 1e200)))))
        ;cix1 (inc (last (filter #(> % -1) (map #(if (<= (nth nb4 %) ix1) % -1) (range (inc n)))))) ; thus the 1e100 conj to nb4
        jx0 (if (< cix0 n) (- ix0 (nth nb4 cix0)) 0); subs indexes, jx0 incusive jx1 exclusive.
        jx1 (if (> cix1 -1) (- ix1 (nth nb4 cix1)) 0)]
   [cix0 cix1 jx0 jx1]))

(defn _grab-selection-by [pieces ix0 ix1 partial-grab-f]
   "Returns the selection given ix0 and ix1 on the rendered string 
      ix0 and ix1 correspond to the standard inclusive, exclusive pattern which is equivalent to the cursor being at ix0 and ix1.
    partial-grab-fn tells us how to handle partial selections of x, i.e. what to do to the user data.
      first arg = the thing being selected, second arg = the piece of the :text 
      third arg = the [ix0 ix1] within the text that is being selected.
      Returns the modified x."
  (if (= (count pieces) 0) (throw (Exception. "# of pieces must be > 0.")))
  (if (or (< ix0 0) (< ix1 0)) (throw (Exception. "The indexes must be >= 0")))
  (let [pd (pieces-digest pieces)
        counts (:counts pd) n (:npieces pd) nb4 (:num-b4 pd) 
        cij (ixjx-pieces pieces ix0 ix1) cix0 (first cij) cix1 (second cij) jx0 (nth cij 2) jx1 (nth cij 3) ; indexes.
        pgf (fn [piece j0 j1] (partial-grab-f piece (subs (:text piece) j0 j1) [j0 j1]))] 
    (cond (= (count counts) 0) (throw (Exception. "zero-length :pieces array."))
      (> cix0 cix1) [] ; both cursors on a boundary (eq to ix0 = ix1), grab nothing.
      (= cix0 cix1) ; part of a single selection, pieces[cix0] is never empty.
      [(pgf (nth pieces cix0) jx0 jx1)]
      :else ; cix0 < cix1, can do selection safely w/o out of bounds.
      (into [] (concat [(pgf (nth pieces cix0) jx0 (nth counts cix0))] 
                 (subvec pieces (inc cix0) cix1) 
                 [(pgf (nth pieces cix1) 0 jx1)])))))
(defn grab-selection-by 
  "Optional partial grab function, the default is used otherwise."
  ([pieces ix0 ix1] (_grab-selection-by pieces ix0 ix1 default-partial-grab))
  ([pieces ix0 ix1 partial-grab-f] (_grab-selection-by pieces ix0 ix1 partial-grab-f)))

(defn index-stats [pieces ix0 ix1 partial-grab-f]
  "Use this function instead of rolling your own tedious stat tracking.
   :b4, :in, :afr = everything before, inside, and after the selection (both vectors of pieces). 
   :ix0, :ix1 = cursor indexes, if ix1>ix0 this means the stuff between should be deleted.
   :piece-0, :piece-1 = index of which pieces are affected, rounding down when the cursor is between pieces, can be -1
   :piece+0, :piece+1 is rounding up, digit is beginning 0 vs end 1 of cursor, can be length, and more than one more the the -0 and -1 case.
   :jx-0, :jx-1, :jx+0, :jx+1 = cursor indexes within each piece (1 = the cursor is after the first char of a piece).
      Zero when things are out of bounds.
   :copy-piece-ixs is [] except for pasted-in content."
  (let [; Stuff before:
        stuff-before (grab-selection-by pieces 0 ix0 partial-grab-f)
        stuff-inside (grab-selection-by pieces ix0 ix1 partial-grab-f)
        stuff-after (grab-selection-by pieces ix1 1e100 partial-grab-f)
        pd (pieces-digest pieces)
        numb4 (:num-b4 pd)
        counts (:counts pd) n (count counts)

        ; Are we on a boundary or not?
        ;boundary-0? (boolean (first (filter #(= (nth numb4 %) ix0) (range (count numb4))))) 
        ;boundary-1? (boolean (first (filter #(= (nth numb4 %) ix1) (range (count numb4)))))
        ; Edge pieces (may be nil if no selection):
        last0 (last stuff-before) first1 (first stuff-after)
        ;ninsert (cond (map? insert) (count (:text insert)) ; number of characters inserted. 
        ;          (coll? insert) (apply + (mapv count (mapv :text insert))) :else (count (str insert)))

        ; full-last before and after (i.e. the cursor is not cutoff)
        ij (ixjx-pieces pieces ix0 ix1) ct #(if % (count (:text %)) -1)
        flb4? (or (= (nth ij 2) 0) (= (nth ij 2) (ct (get pieces (first ij)))))
        flafr? (or (= (nth ij 3) 0) (= (nth ij 3) (ct (get pieces (second ij)))))

        p+0 (first ij) p-1 (second ij) ; the easy boundaries that follow the same ij rules.
        p-0 (if flb4? (first (filter #(or (< % 0) (> (nth counts %) 0)) (range (dec p+0) -2 -1))) p+0)
        p+1 (if flafr? (first (filter #(or (= % n) (> (nth counts %) 0)) (range (inc p-1) (inc n)))) p-1)]
    {:b4 stuff-before :in stuff-inside :afr stuff-after :ix0 ix0 :ix1 ix1
     :piece-0 p-0 :piece+0 p+0 :piece-1 p-1 :piece+1 p+1
     :jx-0 (if flb4? (if (= p-0 -1) 0 (nth counts p-0)) (nth ij 2))
     :jx+0 (nth ij 2) :jx-1 (nth ij 3)
     :jx+1 (if flafr? 0 (nth ij 3))
     :copy-piece-ixs []}))

(def empty-text ; default values for the text editor.
  {:cursor-ix 0 :font-size 14 :selection-start 0 :selection-end 0
   :scroll-top 0 :scroll-left 0 :pieces [{:text ""}] :size [600 400] :read-only? #{}
   :partial-grab-fn default-partial-grab
   :selection-color [0.5 0.8 1 1]
   :outline-color [0 0 1 1] :background-color [0 0 0 0.5] 
   :path "nopath"
   :line-num-start 1
   :show-line-nums? true
   :optimize {:pure-gfx? true} ; not quite true (the cursor blink) but one day we will fix this. 
   :insert-fn default-insert :delete-fn default-delete
   :colorize-fn default-colorize :double-click-fn default-doubleclick})
   
;;;;;;;;;;;;;;;;;;;; Mutable:

; On copy: store here and put the rendered string into the clipboard (for other apps).
; On paste: inserts pieces if we copied multiple pieces, or text if we copied from something else.
; Keep in mind that all instances of rtext share the clip-atom, like a regular clipboard.
(defonce clip-atom (atom {}))

(defn clipboard-read [always-as-str?] ; returns a string or vector, the vector puts the copy indexes in the meta.
  (let [cpa @clip-atom txt (clipboard/get-as-string) ; the clipboard takes precedence if the atom disagrees.
        k (first (keys cpa)) v (:x (get cpa k)) ; will be nil if nothing was copied.
        ixs (get cpa :ixs)
        agree? (= (apply str (mapv :text v)) txt)]
    (if (and agree? (not always-as-str?)) (with-meta v {:ixs ixs}) txt)))

;;;;;;;;;;;;;;;;;;;; String and font:

(defn rendered-string [box]
  "Gets the string that is rendered (for i.e. code folding the actual string we mean is different).
   box is a textbox such as (place-holder-text), same for all arguments in this file called box.
   Does not account for scrolling."
  (apply str (mapv :text (:pieces box))))

(defn inserted-string [value]
  "Converts something that could be inserted into a string, useful to see how the rendered string will change."
  (if (string? value) value (apply str (mapv :text value))))

(defn string-digest [^String s]
  "Newline analysis of a given string s.
    :counts = # chars/line. :num-b4[i] = # chars before line[i], it has one more element than length of lines."
  (let [lines (into [] (.split ^String (str s " ") "\n")) ;each line.
        lines (update lines (dec (count lines)) #(subs % 0 (dec (count %)))) ; remove the extra space
        line-counts (mapv count lines)]
    {:counts line-counts :nlines (count lines)
     :num-b4 (into [] (reductions + 0 (mapv inc line-counts))) ; puts the \n on the previous line, and an extra at the end.
     :nchars (count s)})); [0 123 456, etc], inc to include line feeds.

(defn gran2 [box]
  "[horizontal vertical] size needed per character in pixels.
   This determinies the grid size, the actual character size depends on the 
   graphic's rendering and *text-params* that scale it for a fit."
  (if (nil? (:font-size box)) (throw (Exception. (str "you gave us: " (keys box) " instead of the box."))))
  [(* (:font-size box) (:font-width-to-size *text-params*)) 
   (* (:font-size box) (:font-linespace-to-size *text-params*))])

;;;;;;;;;;;;;;;;;;;; Node sizing:

(defn view-wh [box]
  "How many characters across and down can we fit?"
  (let [pix-x (first (:size box)) pix-y (second (:size box))
        g2 (gran2 box) mr (:fit-to-text-margin *text-params*)]
    ; the 1e-5 makes sure that rounding errors in fit-to-text don't affect us.
    [(long (Math/floor (+ 1e-5 (/ (- pix-x (* mr 2)) (first g2)))))
     (long (Math/floor (+ 1e-5 (/ (- pix-y (* mr 2)) (second g2)))))]))

(defn view-range [box]
  "gets the ix of [top bottom left right] of a node based on it's size, inclusive range of chars that are visible.
   It's size may be larger than that of the text."
  (let [wh (view-wh box)
        top (:scroll-top box) left (:scroll-left box)
        bottom (max 0 (dec (long (Math/floor (+ top (second wh)))))) right (max 0 (dec (long (Math/floor (+ left (first wh))))))]
    [top bottom left right]))

(defn scroll-amounts [box]
  "How much we can scroll [top bottom left right].
   negative numbers => extra space."
  (let [vr (view-range box)
        lc (:counts (string-digest (rendered-string box)))]
    [(first vr) (- (dec (count lc)) (second vr))
     (nth vr 2) (- (dec (apply max lc)) (nth vr 3))]))

;;;;;;;;;;;;;;;;;;;; String analysis:
(defn string-grid [box add-one-on-right? allow-overflow?]
  "Grid map from the 2D [ix iy] integer cursor coords to the cursor on the rendered string.
   On the grid 0 is placing the cursor all the way left/top on the screen (scrolling will affect this).
   On the string 0 is the placing the cursor before the first char.
   add-one-on-right?:
   	Add an extra column on the right side for the keys of the map.
   allow-overflow? true:
      left-overflow is mapped to the end of the line, bottom overflow is mapped to the length of string.
   allow-overflow? false:
      Any keys with overflow are discarded.
   DOES account for scrolling."
  (let [box (v box) d (string-digest (rendered-string box))
        v-range (view-range box) top (first v-range) bottom (second v-range) 
        add-me (if add-one-on-right? 1 0) ; add 1 because the cursor can be after the end of the string.
        left (nth v-range 2) right (+ (nth v-range 3) add-me)
        kys (into [] (apply concat 
                       (mapv (fn [l] (mapv #(vector (- % left) (- l top)) (range left (inc right)))) 
                         (range top (inc bottom)))))
        nc (:nchars d) num-b4 (:num-b4 d) nlines (:nlines d) line-counts (:counts d)
        vls (mapv #(let [x (+ (first %) left) y (+ (second %) top)] ; location on the string.
                     (cond (< y 0) (if allow-overflow? 0 -1)
                       (>= y nlines) (if allow-overflow? nc -1)
                       :else (+ (nth num-b4 y) ; add how many chars are before us to get to the char index.
                               (let [nl (nth line-counts y)]
                                 (if (and (>= x nl) (not allow-overflow?)) -1e100 (min nl x)))))) kys)]
    (reduce #(let [k (get kys %2) v (get vls %2)] ; keep non-zero keys.
               (if (>= v 0) (assoc %1 k v) %1)) {} (range (count kys)))))

;;;;;;;;;;;;;;;;;;;; Cursor conversion:
; ix = cursor index in the string (stored in the node for functions that convert from it).
; piece = which piece we are on.
; grid = 2D location within the component's text. Scrolling needs to be subtracted out to compute this.
; ugrid = not adding scrolling.
; pixel = 2D location of cursor in pixels (local coords, camera is moved/zoomed and physics).

(defn cursor-pixel-to-ugrid [box pixel-x pixel-y]
  "Gets the cursor integer cursor grid position [x y] where 0 is upper left corner.
   Doesn't apply scrolling. Clamped to the bounds of the grid index."
  (let [box (v box) g2 (gran2 box)
        mr (:margin *text-params*)
        v-range (view-range box)
        ; unclamped values:
        cx0 (/ (- pixel-x mr) (first g2))
        cy0 (/ (- pixel-y mr) (second g2))
        ; clamped values:
        cx (Math/round (double (min (max cx0 0.0) (- (nth v-range 3) (nth v-range 2)))))
        cy (int (Math/floor (double (min (max cy0 0.0) (- (nth v-range 1) (nth v-range 0))))))]
    [cx cy]))

(defn cursor-pixel-to-grid [box pixel-x pixel-y]
  "More useful than the ugrid version."
  (let [xy (cursor-pixel-to-ugrid box pixel-x pixel-y)] 
    [(+ (first xy) (:scroll-left box)) (+ (second xy) (:scroll-top box))]))

(defn cursor-ugrid-to-pixel [box grid-x grid-y]
  "Gets where the draw commands should go. Ignores scrolling."
  (let [g2 (gran2 box) sx (first g2) sy (second g2) m (:margin *text-params*)
        naive-mid [(+ m (* sx grid-x)) (+ m (* sy grid-y))] ft-pts (:font-size box)
        naive-bottom (vector (first naive-mid) (+ (second naive-mid) (* 0.5 ft-pts)))
        ; shx and shy are in addition to the margin.
        shx (* (:font-xshift-to-size *text-params*) ft-pts) ; tweak all characters the same slight amount. 
        shy (* (:font-yshift-to-size *text-params*) ft-pts)]
    [(+ (first naive-bottom) shx) (+ (second naive-bottom) shy)]))

;(defn cursor-grid-to-pixel [box grid-x grid-y]
;  "Gets the cursor-midpoint pixel location. Add half of the fontsize to get to the bottom-right char. Yes scrolling."
;  (let [ft-sz (gran2 box) sx (first ft-sz) sy (second ft-sz) m (:margin *text-params*)]
;  DOES NOT WORK FOR NOW, may not be useful to use.
;    [(+ m (* sx (- grid-x (:scroll-left box)))) (+ m (* sy (- grid-y (:scroll-top box))))]))

(defn cursor-ugrid-to-ix [box x y]
  (let [gr (string-grid box true true)]
    (get gr [x y])))

(defn cursor-pixel-to-ix [box pixel-x pixel-y]
  "Gets the cursor index from a node and a cursor position.
   For a string of length n, the cursor ix ranges from 0 to n inclusive (n+1 different values)."
  (let [xy (cursor-pixel-to-ugrid box pixel-x pixel-y) ; don't do scrolling yet.
        gr (string-grid box true true)] ; scrolling is accounted for here.
    (get gr xy)))

(defn cursor-ix-to-ugrid [box] 
  "Gets the x and y global grid, never nil and out-of-bounds cursors are clamped.
   no scrolling is applied."
  (let [d (string-digest (rendered-string box)) nb4 (:num-b4 d)
        cursor-ix (max 0 (min (:nchars d) (:cursor-ix box)))
        lix (second (last (filterv #(<= (first %) cursor-ix) (mapv vector nb4 (range)))))]
    [(- cursor-ix (nth nb4 lix)) lix]))

(defn cursor-ix-to-lix [box] (second (cursor-ix-to-ugrid box))) ; convience.

(defn cursor-ix-to-grid [box] 
  "Gets the x and y grid, returns nil if they aren't on the screen."
  (let [xy (cursor-ix-to-ugrid box) sl (:scroll-left box) st (:scroll-top box)]
    (if xy
      (let [gx (- (first xy) sl) gy (- (second xy) st)
            wh (view-wh box) w (first wh) h (second wh)]
        (if (and (>= gx 0) (>= gy 0) (<= gx (inc w)) (<= gy (inc h))) [gx gy])))))

(defn cursor-ix-to-pixel [box]
  "The cursor's coordinates [x y] center, before translation and scaling. nil if the cursor is not rendered."
  (let [ixjx (cursor-ix-to-grid box) sz (gran2 box) margin (:margin *text-params*)]
    (if ixjx [(+ margin (* (first sz) (first ixjx))) (+ margin (* (second sz) (+ 0.5 (second ixjx))))])))

(defn cursor-ix-to-piece [box]
  "Converts the nodes cursor-ix to [piece-ix loc-on-piece].
   Piece-ix is which piece we are. Loc-on-piece is the location on the piece, 0 = left edge of piece.
   We round right but never go off the bounds of the array."
  (let [box (v box) ix (:cursor-ix box) ix (if (< ix 0) 0 ix)
        pd (pieces-digest (:pieces box)) counts (:counts pd) nc (:npieces pd)
        each-start (:num-b4 pd) each-end (mapv + each-start counts) ; inclusive cursor ranges.
        piece-ix (first (filterv #(and (>= ix (nth each-start %)) (<= ix (nth each-end %))) (range (count (:pieces box)))))
        piece-ix (if piece-ix piece-ix (dec nc))
        loc-on-piece (if (= nc 0) 0 (min (- ix (nth each-start piece-ix)) (nth counts piece-ix)))]
    [piece-ix loc-on-piece]))

(defn cursor-piece-to-ix [box piece-ix]
  "The start of the piece-ix'th piece."
  (if (or (< piece-ix 0) (>= piece-ix (count (:pieces box))))
    (throw (Exception. "piece-ix is out-of-bounds")))
  (let [pieces (mapv :text (:pieces box))] (apply + (mapv count (subvec pieces 0 piece-ix)))))

(defn get-selected-char-rects [box]
  "Gets the character rectangles, vector of [x,y width height]'s, local coords with respect to us"
  (let [ix0 (:selection-start box)
        ix1 (:selection-end box)]
    (if (> ix1 ix0)
      (let [g2 (gran2 box) sx (first g2) sy (second g2) v-range (view-range box)
            gr (string-grid box true false)
            igr (zipmap (vals gr) (keys gr))
            m (:margin *text-params*)
            xys (filterv identity (mapv #(get igr %) (range ix0 ix1)))]
        (mapv #(vector (+ m (* sx %1)) (+ m (* sy %2)) (+ sx 1.001) (+ sy 1.001)) (mapv first xys) (mapv second xys))))))

(defn exon [box] 
  "Gets the excised region upon a keypress, [ix0 ix1] inclusive-exclusive pattern.
   [:selection-start :selection-end] in some cases, but the :cursor-ix also can influence things."
  (let [sel0 (:selection-start box) sel1 (:selection-end box) ; inclusive.
        valid-sel? (> sel1 sel0)
        c-ix (:cursor-ix box)
        ; Not sure the best case for sel1>sel0 and c-mismatch, which can be created accidently.
        c-mismatch? (and (not= sel0 c-ix) (not= sel1 c-ix)) ;(> sel1 sel0)
        sel0 (if c-mismatch? c-ix sel0) sel1 (if c-mismatch? c-ix sel1)]
    [sel0 sel1]))

(defn carry-cursor [pieces0 pieces1 cursor-ix0 within-piece-f]
  "Calculates the new cursor index if we change the lenghts of the various pieces.
   pieces can be a number (the count), a string, or a map (using :text).
   (within-piece-f [old-piece new-piece jx]) returns the new jx within the piece; this fn sets
     the policy of how to handle changing the cursor when modifying the piece."
  (let [get-n #(cond (number? %) % (string? %) (count %) (map? %) (count (:text %)))
        n0s (mapv get-n pieces0) n1s (mapv get-n pieces1)
        baux {:pieces (mapv #(hash-map :text (apply str (repeat % "."))) n0s) :cursor-ix cursor-ix0}; refactoring to accept the counts would be a good idea.
        ij (cursor-ix-to-piece baux) ix (first ij) 
        jx (if (= (count pieces1) 0) 0 (within-piece-f (nth pieces0 ix) (nth pieces1 ix) (second ij)))]
    (+ jx (apply + (if (= (count n1s) 0) [0] (subvec n1s 0 ix)))))) ; sum up all lines before our ix.

(defn pixel-to-selected-char-ix [box x y] ; useful for external functions. 
  (let [half-char-width (* (first (gran2 box)) 0.5) ; half-shift since we select on chars not between chars.
        c-ix (cursor-pixel-to-ix box (+ x half-char-width) y)] (dec c-ix)))

;;;;;;;;;; Scrolling

(defn scroll-bound [box]
  "Makes sure the node doesn't have excess space, if it is possible to do so."
  (let [box (v box) d (string-digest (rendered-string box))
        vr (view-range box) vr (update (update vr 1 inc) 3 inc) ; [top bottom left right]
        x1 (apply max (:counts d))
        y1 (:nlines d)
        box (cond (> (nth vr 3) x1) (update box :scroll-left #(max 0 (- % (- (nth vr 3) x1))))
               (< (:scroll-left box) 0) (assoc box :scroll-left 0) :else box)
        box (cond (> (nth vr 1) y1) (update box :scroll-top #(max 0 (- % (- (nth vr 1) y1))))
               (< (:scroll-top box) 0) (assoc box :scroll-top 0) :else box)] box))

(defn scroll [box d-ix d-iy]
 "Scrolls the text inside a node, by an integer amount."
  (scroll-bound (assoc box :scroll-top (+ (:scroll-top box) (long d-iy)) :scroll-left (+ (:scroll-left box) (long d-ix)))))

(defn scroll-to-see-cursor [box]
  "Scrolls just enough to make the cursor visible."
  (let [box (v box) xy (cursor-ix-to-ugrid box) x (first xy) y (second xy)
        scx (fn [n dx] (update n :scroll-left #(+ % (long dx))))
        scy (fn [n dy] (update n :scroll-top #(+ % (long dy))))
        vr (view-range box) ; [top bottom left right].
        t (first vr) b (second vr) l (nth vr 2) r (nth vr 3)
        boxx (cond (< x l) (scx box (- x l)) (> (dec x) r) (scx box (- (dec x) r)) :else box)
        boxxy (cond (< y t) (scy boxx (- y t)) (> y b) (scy boxx (- y b)) :else boxx)] 
    (scroll-bound boxxy)))

(defn cursor-scroll-update [box box1 vis-edits]
  "box is the old box, box1 is the new box, and vix-edits gets from box to box1."
  (let [cur-ix (:cursor-ix box) line0 (cursor-ix-to-lix box)
        cur-ix1 (reduce (fn [cix ed]
                          (let [ix0 (:ix0 ed) ix1 (:ix1 ed) delta (stringdiff/length-delta ed)
                                drag-frac (cond (<= cix ix0) 0 (>= cix ix1) 1
                                            :else (/ (- cix ix0) (- ix1 ix0)))]
                            (int (+ cix 0.5 (* drag-frac delta))))) cur-ix vis-edits)
        box2 (assoc box1 :cursor-ix cur-ix1)
        line1 (cursor-ix-to-lix box2)]
    (scroll-bound (update box2 :scroll-top #(+ % (- line1 line0))))))

;;;;;;;;;;;;;;;;;;;; Box modification functions:

(defn fit-to-text [box x? y?]
  "Fits the node's size to the text in it."
  (let [box (v box) g2 (gran2 box) m (:fit-to-text-margin *text-params*)
        mns (:margin *text-params*)
        d (string-digest box)
        n-l (:nlines d) max-l (apply max (:counts d))
        box (assoc box :scroll-top 0 :scroll-left 0)]
    (assoc box :size 
      [(if x? (+ (* mns 2) m (* max-l (first g2))) (first (:size box)))
       (if y? (+ (* mns 2) m (* n-l (second g2))) (second (:size box)))])))

(defn fit-to-text-if-small [box]
  "Only calls fit-to-text if the box is small."
  (let [box (v box) d (string-digest box) xx (apply max (:counts d)) yy (:nlines d)
        n (:max-autofit-size-chars *text-params*)]
    (if (or (<= xx n) (<= yy n)) (fit-to-text box (<= xx n) (<= yy n)) box)))

(defn edit [box ix0 ix1 insert copy-piece-ixs]
  "Replaces the stuff between cursor ix0 and cursor ix1 (which makes ix0 inclusive and ix1 exclusive). 
   with insert and sets the cursor index to the end of the inserted stuff.
   IF insert is a string, it simply modifies the corresponding string.
   IF it is an object or array thereof it adds a new object.
   ix0 = ix1 with insert bieng a letter, etc is a normal typing insertion.
   copy-piece-ixs = sets :copy-piece-ixs in stats, use [] when not pasting in."
  (let [box (v box) 
        stats (assoc (index-stats (:pieces box) ix0 ix1 (:partial-grab-fn box)) :copy-piece-ixs copy-piece-ixs)
        
        pure-deleting? (and (= insert "") (< ix0 ix1))
        
        box1 (if pure-deleting? ((:delete-fn box) box stats) 
               ((:insert-fn box) box insert stats (string? insert)))]
    (scroll-to-see-cursor (assoc box1 :selection-start 0 :selection-end 0))))

;;;;;;;;;;;;;;;;;;;;;;;;; Editing mouse and key inputs

(defn key-to-edit [box key-evt]
  "Converts the key press to an edit.
   :type is :type :backspace :select-all :cut :copy :paste :save :arrow :type :ignore
   :ix0 and :ix1 are the range of relevent selections. For :select-all it is the entire node.
   :value is the char typed."
  (let [box (v box) ck (ctrl+ key-evt) ak (arrow-key key-evt)
        tk (if-let [x (typed-key key-evt)] (str x)) ek (esc? key-evt)
        ix01 (exon box) ix0 (first ix01) ix1 (second ix01)]
    (cond (= ck \a) {:type :select-all :ix0 0 :ix1 (dec (count (rendered-string box)))}
      (= ck \x) {:type :cut :ix0 ix0 :ix1 ix1 :value ""}
      (= ck \c) {:type :copy :ix0 ix0 :ix1 ix1 :value ""}
      (= ck \v) {:type :paste :ix0 ix0 :ix1 ix1 :value (clipboard-read false)}
      (= ck \s) {:type :save :ix0 0 :ix1 0 :value ""} ; :ix0 and :ix1 have less meaning.
      ck {:type :ignore :ix1 ix1 :value ""}
      ak {:type :arrow :value ak :ix0 ix0 :ix1 ix1}
      (= tk (str \backspace)) {:type :backspace :value "" :ix0 (if (= ix0 ix1) (max 0 (dec ix0)) ix0) :ix1 ix1}
      tk {:type :type :value (string/replace tk #"\t" "    ") :ix0 ix0 :ix1 ix1}
      :else {:type :ignore})))

(defn arrow-cursor [box arrow-code shifting?] 
  "The user moves the cursor. The node scrolls to keep the cursor in view if it moves out of view.
   Arrow codes are: < > v ^. Shift + arrow means select."
  (let [box (v box) global-xy-grid (cursor-ix-to-ugrid box)
        global-x-grid (first global-xy-grid) global-y-grid (second global-xy-grid)
        arrow-code (str arrow-code)
        gxy (cond (= arrow-code "<") [(dec global-x-grid) global-y-grid] ; it wasn't even on the screen.
                  (= arrow-code ">") [(inc global-x-grid) global-y-grid]
                  (= arrow-code "v") [global-x-grid (inc global-y-grid)]
                  (= arrow-code "^") [global-x-grid (dec global-y-grid)]
                  :else (throw (Exception. (str "Unrecognized arrow code: " arrow-code))))
        d (string-digest (rendered-string box)) lc (:counts d) nl (count lc) nb4 (:num-b4 d) nc (:nchars d) 
        cl #(max 0 (min % (dec nl))) gx (first gxy) gy (second gxy)
        gxy (cond (or (< gy 0) (and (< gx 0) (= gy 0))) [0 0] ; various overflow and wrap-around rules in this cond.
              (< gx 0) [(nth lc (dec gy)) (dec gy)]
              (or (>= gy nl) (and (= gy (dec nl)) (> gx (last lc)))) [(last lc) (dec nl)]
              (and (> gx (nth lc gy)) (or (= arrow-code "^") (= arrow-code "v"))) [(nth lc gy) gy]
              (> gx (nth lc gy)) [0 (inc gy)]
              :else [gx gy])
        cur-ix0 (:cursor-ix box) ; old cursor index.
        cur-ix (+ (first gxy) (nth nb4 (second gxy)))]; new cursor index after arrowing. 
  (scroll-to-see-cursor 
    (assoc 
      (if shifting? ; arrow key selections.
        (if (> (:selection-end box) (:selection-start box)) ; init selection.
          (update (update box :selection-start #(if (= % cur-ix0) cur-ix %))
            :selection-end #(if (= % cur-ix0) cur-ix %))
         (assoc box :selection-start (min cur-ix0 cur-ix) :selection-end (max cur-ix0 cur-ix)))
        (assoc box :selection-start 0 :selection-end 0)) :cursor-ix cur-ix))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Mouse and key interaction itself

(defn mouse-press [mouse-evt box]
  "Mouse :X and :Y are local points and not necessarily integers."
  (let [box (v box) x (:X mouse-evt) y (:Y mouse-evt)
        sh? (:ShiftDown (:external-state @globals/one-atom))
        i1 (:cursor-ix box) i2 (cursor-pixel-to-ix box x y)]
    (if (= (:ClickCount mouse-evt) 2) ; double-click select.
      (let [ij (ixjx-pieces (:pieces box) i2 i2)]
        ((:double-click-fn box) box i2 (first ij) (nth ij 2)))
      (assoc box :cursor-ix i2
        :selection-start (if sh? (min i1 i2) 0) 
        :selection-end (if sh? (max i1 i2) 0)))))

(defn mouse-drag [mouse-evt box]
  (let [box (v box) cursor1 (cursor-pixel-to-ix box (:X0 mouse-evt) (:Y0 mouse-evt))
        cursor2 (cursor-pixel-to-ix box (:X mouse-evt) (:Y mouse-evt))]
    (assoc box :selection-start (min cursor1 cursor2) 
      :selection-end (max cursor1 cursor2) :cursor-ix cursor2)))

(defn mouse-wheel [wheel-evt box]
  "Mouse wheel scrolling. Only vertical is supported, sorry mac trackpads."
  ;(println (:WheelRotation wheel-evt) (:PreciseWheelRotation wheel-evt))
  (let [horiz? (:ShiftDown wheel-evt) delta (:WheelRotation wheel-evt)]
    (scroll-bound (scroll box (if horiz? delta 0) (if horiz? 0 delta)))))

(defn dispatch-edit-event [box ed]
  (let [box (v box) ty (:type ed)
        copy!! (fn [] ; maybe make this customizable?
                 (let [x0 (:ix0 ed) x1 (:ix1 ed)] ; selection indexes.
                   (if (<= x1 x0) false ; nothing selected.
                     (let [slice (grab-selection-by (:pieces box) x0 x1 (:partial-grab-fn box))
                           txt (apply str (mapv :text slice)) ; txt is the lookup key on paste.
                           i0 (first (ixjx-pieces (:pieces box) x0 x1)) 
                           copy-ixs (mapv #(+ % i0) (range (count slice)))] ; which index each comes from.
                       (clipboard/put-as-string!! txt) 
                       (reset! clip-atom (hash-map txt {:x slice :ixs copy-ixs}))))))]
    (cond (= ty :backspace) (edit box (:ix0 ed) (:ix1 ed) "" [])
      (= ty :select-all) (assoc box :selection-start 0 :selection-end (:ix1 ed))
      (= ty :cut) (if (copy!!) (edit box (:ix0 ed) (:ix1 ed) "" []) box) ; remove and store if there is a selection.
      (= ty :copy)  (do (copy!!) box)
      (= ty :paste) (let [x (:value ed) agree? (not (string? x))]
                      (edit box (:ix0 ed) (:ix1 ed)
                        x (if agree? (:ixs (meta x)) [])))
      (= ty :save) box; We don't handle saves here, just do nothing.
      (= ty :arrow) (arrow-cursor box (:value ed) (:ShiftDown (:external-state @globals/one-atom)))
      (= ty :type) (edit box (:ix0 ed) (:ix1 ed) (:value ed) [])
      :else box)))

(defn key-press [key-evt box]
  "Key press mode. Trying to save needs a savefunction."
  (dispatch-edit-event box (key-to-edit box key-evt)))

(defn key-release [key-evt box] box)

;;;;;;;;;;;;;;;;;;;;;;;;; Graphics ;;;;;;;;;;;;;;;;;;;;;;;;;

(defn render-scroll [box]
  "Renders scrollbars if need be."
  (let [sc (scroll-amounts box) ;[top bottom left right]
        t (first sc) b (second sc) l (nth sc 2) r (nth sc 3)
        vr (view-range box) ;[top bottom left right]
        nv (max 1 (inc (- (second vr) (first vr))))
        nh (max 1 (inc (- (nth vr 3) (nth vr 2))))
        m 1 ; margin so that the line is visible.
        x0 m x1 (- (first (:size box)) m)
        y0 m y1 (- (second (:size box)) m)
        opts {:Color [1 1 0 1]}
        h-cmd (if (> (+ l r) 0) [(+ x0 (* (- x1 x0) l (/ 1.0 (+ l nh r)))) y1 (- x1 (* (- x1 x0) r (/ 1.0 (+ l nh r)))) y1])
        v-cmd (if (> (+ t b) 0) [x0 (+ y0 (* (- y1 y0) t (/ 1.0 (+ t nv b)))) x0 (- y1 (* (- y1 y0) b (/ 1.0 (+ t nv b))))])]
    (mapv #(vector :drawLine % opts) (filterv identity [h-cmd v-cmd]))))

(defn render-selection [box]
  "Renders the selection. It can be semi-transparent but goes under the text without affecting it's color at all."
  (let [col (:selection-color box)
        rects (get-selected-char-rects box)]
    ; we could lump rects...
    ;(do (require '[clooj.app.gauitest.ctest :as gtest]) (eval (list 'gtest/print1 :id03 [(:selection-start box) (:selection-end box)])))
    (mapv #(vector :drawRect [(first %) (second %) (nth % 2) (nth % 3)]
             {:Color col}) rects)))

(defn render-cursor-by-color [box col col-edge]
  (let [pix (cursor-ix-to-pixel box)] ; nil if not rendered, center of cursor.
    (if pix 
      (let [x (first pix) y1 (- (second pix) (* (:font-size box) 0.5))
            y0 (+ (second pix) (* (:font-size box) 0.5))]
        [[:drawLine [(dec x) y0 (dec x) y1] {:Color col-edge}] [:drawLine [(inc x) y0 (inc x) y1] {:Color col-edge}]
         [:drawLine [x y0 x y1] {:Color col}]])))) ; draw the main cursor last

(defn render-cursor [box] 
  (let [col (if (> (mod (System/nanoTime) 1e9) 5e8) [1 1 1 1] [1 0 1 1])]
    (render-cursor-by-color box col [0 0 0.5 1])))

(defn render-text [box]
  "Renders the characters of box with the line numbers, using :line-num-start and :hidden-nlines to have line nums be properly calculated."
  (let [gr (string-grid box false false) ; [ix iy] => i.
        
        x (pieces-digest (:pieces box)) s (rendered-string box) ft-pts (:font-size box)
        piece-ix (into [] (apply concat (mapv #(repeat %1 %2) (:counts x) (range))))
        vals-gr (into [] (vals gr))
        grid-chars (mapv #(subs s % (inc %)) vals-gr)
        min-c (if (= (count vals-gr) 0) 0 (apply min vals-gr)) max-c (inc (apply max -1 (vals gr)))
        cols ((:colorize-fn box) box (subs s min-c max-c) (subvec piece-ix min-c max-c) min-c max-c) gcols (mapv #(get cols (- % min-c)) (vals gr))
        ;_ (if (not= (count cols) (count s))
        ;    (throw (Exception. (str "Colorize fn returns colors of the wrong length, " (count cols) " instead of " (count s)))))
        gchar-locations (mapv #(cursor-ugrid-to-pixel box (first %) (second %)) (keys gr))
        new-way? true
        char-gfx (if new-way?
                   [[:grid-string [ft-pts (apply str grid-chars) (mapv first gchar-locations) (mapv second gchar-locations) 
                                (mapv first gcols) (mapv second gcols) (mapv #(nth % 2) gcols) (mapv #(nth % 3) gcols)] {}]]
                   (mapv #(vector :drawString [(str %2) (first %1) (second %1)] {:Color %3 :FontSize ft-pts}) gchar-locations grid-chars gcols))]
     (if (:show-line-nums? box)
       (let [ladd (:line-no-standoff-chars *text-params*)
             _line-xsm (reduce (fn [acc xy] (update acc (second xy) #(max (if % % 0) (first xy)))) 
                                                {} (keys gr))
             _line-xsm1 (zipmap (keys _line-xsm) (mapv #(+ ladd %) (vals _line-xsm)))

             line-xs (mapv #(if-let [x (get _line-xsm1 %)] x ladd) (range (inc (apply max -1 (keys _line-xsm1)))))

             n (count line-xs)
             line-locations (mapv #(cursor-ugrid-to-pixel box (nth line-xs %) %) (range n))
             
             ; vector "map" from vis to real lines:
             _ps (conj (:pieces box) {:text "\n"}) _n (count _ps)
             lnum-vis2rel (loop [acc [] ix 0 line-ix (:line-num-start box)]
                            (if (= ix _n) acc
                              (let [p (nth _ps ix)
                                    nfeed (dec (count (string/split (str " " (:text p) " ") #"\n")))
                                    xtra (if-let [x (:hidden-nlines p)] x 0)]
                                (recur (apply conj acc (mapv #(+ % xtra line-ix) (range nfeed)))
                                  (inc ix) (+ line-ix nfeed xtra)))))
             ;_ (println "lnum-vis2rel long: "  lnum-vis2rel)
             line-strs (mapv #(str (max 0 (nth lnum-vis2rel (+ (:scroll-top box) %)))) (range n))
             max-visible-x (+ (:scroll-left box) (first (view-wh box)))
             lft-pts (* ft-pts (:line-fontsz-mult *text-params*))
             cute-overflow? false
             lineno-gfx (filterv identity 
                          (mapv #(if (<= (+ %3 ladd (count %2) (if cute-overflow? -1e100 0)) max-visible-x) 
                                   (vector :drawString [%2 (first %1) (second %1)] {:Color [1 1 1 0.5] :FontSize lft-pts}))
                            line-locations line-strs line-xs))]
         (into [] (concat char-gfx lineno-gfx)))
       char-gfx)))

(defn render-border [box] [[:drawRect [0 0 (first (:size box)) (second (:size box))] {:Color (:outline-color box)}]])

(defn render-path [box]
  (let [box1 (assoc box :font-size 15) s (:path box1) sz (:size box1) g2 (gran2 box1) bc (:outline-color box1)
        s (if (string? s) s (apply str (interpose "/" s)))
        n (count s) places (mapv #(cursor-ugrid-to-pixel box1 % 0) (range n))
        m (:margin *text-params*) 
        tc (mapv #(+ (* % 0.5) 0.5) bc) width (+ (* m 2) (* (first g2) n))
        height (+ (* m 2) (second g2)) ft-pts (:font-size box1)
        put-above? true
        x0 (- (* (first sz) 0.5) (* width 0.5)) y0 (if put-above? (- height) 0)]
    (into [] (concat 
      [[:drawRect [x0 y0 width height] {:Color (:outline-color box1)}]]
      (mapv #(vector :drawString [(str %2) (+ (first %1) x0) (+ (second %1) y0)] {:Color tc :FontSize ft-pts}) places s)))))

(defn render-background [box]
  [[:fillRect [0 0 (first (:size box)) (second (:size box))] {:Color (:background-color box)}]])

(defn render [box & show-cursor?]
  (if (nil? box) (throw (Exception. "Somewhere in the guts of the render loop the box got lost.")))
  (let [box (v box)]
    (concat (render-background box) (render-border box) (render-path box)
      (render-scroll box) (render-selection box) (render-text box)
        (if (= (first show-cursor?) false) [] (render-cursor box)))))
