; Text editing with multiple pieces that can hold user data/functions.

(ns clooj.app.gaui.rtext
  (:require [clooj.java.clipboard :as clipboard]
    [clooj.app.claytreeevt :as clevt]
    [clojure.string :as string]))

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

;;;;;;;;;;;;;;;;;;;; Defaults:

(def ^{:dynamic true} *text-params* 
  {:margin 2 ; Padding when there is no scroll.
   :font-linespace-to-size 0.85 ; Not quite as high as advertized.
   :font-width-to-size 0.54; could also be gotten from the font metrics.
   :max-autofit-size-chars 30 ; automatically fit small text.
   :font-xshift-to-size -0.1 ; Don't know where this comes from.
   :font-yshift-to-size 0.125 ; center the text vertically.
   :shifting? false ; the keyboard is a singleton.
   :fit-to-text-margin 1}) ; extra space so that the text doesn't get cutoff. Only used in the fit-to-text fn.

(defn duster [box]
  "Removes empty pieces, always keeping one pieces. 
   Within rtext, thsi is only used in the defult functions, you may want different behavior."
  (update box :pieces 
    (fn [pieces] 
      (let [p1 (filterv #(> (count (:text %)) 0) pieces)]
       (if (> (count p1) 0) p1 (first pieces))))))

(defn v [box]
  "make the pieces a vector."
  (if (vector? (:pieces box)) box (update box :pieces #(into [] %))))

(defn default-colorize [s grid piece-ix]
  "Simple alternating colors."
  (mapv #(if (even? %) [1 1 1 1] [0.7 0.7 1 1]) piece-ix))

(defn default-insert [box x stats str?] ; unify default-text-insert and default-pieces-insert.
  "inserts x (a string or vector of pieces) into box with the help of stats and maybe in place of a selection. 
   str? is true when x is a string.
   stats:
     :b4, :afr = everything before and after the selection (both vectors of pieces). 
     :ix0, :ix1 = cursor indexes, if ix1>ix0 this means the stuff between should be deleted.
     :piece-0, :piece-1 = index of which pieces are affected, rounding down when the cursor is between pieces, can be -1
     :piece+0, :piece+1 is rounding up, digit is beginning 0 vs end 1 of cursor, can be length.
     :jx-0, :jx-1, :jx+0, :jx+1 = cursor indexes within each piece (1 = the cursor is after the first char of a piece).
        Zero when things are out of bounds.
     :copy-piece-ixs = which piece indexes that were grabbed by the last clipboard copy."
  (let [same-piece? (>= (:piece+0 stats) (:piece-1 stats)) ; withing the same piece (or between two pieces).
        x (if str? [{:text x}] x) cpixs (:copy-piece-ixs stats)
        copy11? (and (= (count cpixs) 1) ; copied from the same index.
                  (or (= (:piece+0 stats) (first cpixs)) (= (:piece-1 stats) (first cpixs))))
        overflow? (>= (:piece+0 stats) (count (:pieces box)))]
    ; use duster since we remove stuff between ix0 and ix1.
    ((fn [bx] (assoc (duster bx) :cursor-ix (apply + (:ix0 stats) (mapv #(count (:text %)) x)))) ; cursor to end of insert.
      (if (and same-piece? (or str? copy11?)) ; in-place modification.
        ; can use :piece+0 and :piece-1, will be different on the boundaires, no obvious "better". 
        (update-in box [:pieces (if overflow? (max 0 (:piece-0 stats)) (:piece+0 stats)) :text]
          #(str (subs % 0 ((if overflow? :jx-0 :jx+0) stats)) 
             (apply str (mapv :text x)) (subs % ((if overflow? :jx-1 :jx+1) stats))))
        (assoc box :pieces (into [] (concat (:b4 stats) x (:afr stats))))))))

(defn default-save-fn!!! [box]
 "Must return the box, even if it is different."
 (do (println "no save function specified.") box))

(defn default-delete [box ix0 ix1 b4 afr multi?]
  "Delete pressed on selection from ix0 to ix1, inclusive-exlcuisve pattern.
   b4 and afr are the stuff before and after the excision. 
   multi? if the selection is forced to cover multible indexes.
   when ix0 = ix1 > 0, remove one char. When ix1 > ix0, remove the stuff between."
  (duster
    (cond (= ix0 ix1 0) box ; no change, cursor at beginning and no selection.
      (= ix0 ix1) (assoc box :cursor-ix (dec (:cursor-ix box)) :pieces ; backspace when there is no selection => remove one char.
                    (let [b4d (:pieces (duster {:pieces b4})) afrd (:pieces (duster {:pieces afr}))
                          b41 (update-in b4d [(dec (count b4d)) :text] ; b41 is never empty
                                #(str (subs % 0 (dec (count %)))))
                          boundary? (and (> (count b4d) 0) (> (count afrd) 0)
                                      (= (+ (count b4d) (count afrd)) (count (:pieces (duster box)))))]
                      (into [] (if boundary? (concat b41 afrd) 
                                 (concat (butlast b4d) [(update (last b41) :text #(str % (:text (first afr))))]
                                   (rest afr))))))
      :else
      (assoc box :cursor-ix ix0 :pieces
        (if multi? (into [] (concat b4 afr)) ; no need to repair the traumtized piece.
          ; if it's within a single piece merge back that split piece.
          (into [] (concat (butlast b4) [(update (last b4) :text #(str % (:text (first afr))))] (rest afr)))))))) 

(defn default-doubleclick [box c-ix p-ix jx]
  "c-ix = cursor ix, p-ix = piece ix at cursor, jx = location within piece at cursor. 
   The indexes round up. We select the piece."
  (if-let [p (:text (get (:pieces box) p-ix))]
    (assoc box :selection-start (- c-ix jx) :selection-end (+ (- c-ix jx) (count p))) box))

(def empty-text ; default values for the text editor.
  {:cursor-ix 0 :font-size 18 :selection-start 0 :selection-end 0
   :scroll-top 0 :scroll-left 0 :pieces [{:text ""}] :size [200 200] :read-only? #{}
   :partial-grab-f (fn [x txt ix01] (assoc x :text txt)) ; get an x given an x, the new text and the indexes of the text.
   :selection-color [0.5 0.8 1 1]
   :outline-color [0 0 1 1] 
   :insert-fn default-insert :delete-fn default-delete
   :colorize-fn default-colorize :double-click-fn default-doubleclick
   ; TODO: implement this :move-cursor-fn default-move-cursor
   :save-fn!!! default-save-fn!!!})

;;;;;;;;;;;;;;;;;;;; Mutable:

; On copy: store here and put the rendered string into the clipboard (for other apps).
; On paste: if text matches use what is stored, otherwise make an empty one as defined by :paste-fn
; Keep in mind that all instances of rtext share the clip-atom, like a regular clipboard.
(defonce clip-atom (atom {}))

;;;;;;;;;;;;;;;;;;;; String and font:

(defn rendered-string [box]
  "Gets the string that is rendered (for i.e. code folding the actual string we mean is different).
   box is a textbox such as (place-holder-text), same for all arguments in this file called box.
   Does not account for scrolling."
  (apply str (mapv :text (:pieces box))))

(defn string-digest [^String s]
  "Newline analysis of a given string s.
    :counts = # chars/line. :num-b4[i] = # chars before line[i], it has one more element than length of lines."
  (let [lines (into [] (.split s "\n")) ;each line.
        lines (if (= (count lines) 0) [""] lines) ; have at least one line.
        line-counts (mapv count lines)]
    {:counts line-counts :nlines (count lines)
     :num-b4 (into [] (reductions + 0 (mapv inc line-counts))) ; puts the \n on the previous line, and an extra at the end.
     :nchars (count s)})); [0 123 456, etc], inc to include line feeds.

(defn piece-digest [box]
  "Piece-based version of string digest.
    :counts = # chars/piece. :num-b4[i] = # chars before piece[i], it has one more element than length of lines."
  (let [pieces (mapv :text (:pieces box))
        counts (mapv count pieces)]
    {:counts counts :num-b4 (into [] (reductions + 0 counts)) ; no need for the \n.
     :nchars (apply + counts) :npieces (count counts)}))

(defn font-size2 [box]
  "[horizontal vertical] character size for the font, i.e. the 2D size.
   This determinies the grid size, the actual character size depends on the 
   graphic's rendering, and *text-params* tries to scale it."
  [(* (:font-size box) (:font-width-to-size *text-params*)) 
   (* (:font-size box) (:font-linespace-to-size *text-params*))])

;;;;;;;;;;;;;;;;;;;; Node sizing:

(defn view-wh [box]
  "How many characters across and down can we fit?"
  (let [pix-x (first (:size box)) pix-y (second (:size box))
        ft-sz (font-size2 box) mr (:fit-to-text-margin *text-params*)]
    ; the 1e-5 makes sure that rounding errors in fit-to-text don't affect us.
    [(long (Math/floor (+ 1e-5 (/ (- pix-x (* mr 2)) (first ft-sz)))))
     (long (Math/floor (+ 1e-5 (/ (- pix-y (* mr 2)) (second ft-sz)))))]))

(defn view-range [box]
  "gets the ix of [top bottom left right] of a node based on it's size, inclusive range of chars that are visible.
   It's size may be larger than the text."
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
   On the grid 0 is placing the cursor all the way left/top on the screen.
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

(defn spacer-to-cursor-ix [box piece-ix]
  "Returns the cursor location on the string (0 being before the entire string) 
   that puts the cursor at the beginning of the piece-ix's piece."
  (let [strs (mapv :text (:pieces box))] (apply + (mapv count (subvec strs 0 piece-ix)))))

(defn ixjx [box ix0 ix1]
  "[piece ix0, piece ix1, jx0 jx1] wherejx is the cursor within the piece.
   piece-ix0 rounds up and piece-ix1 rounds down when it lands on the boundary.
   This means piece-ix0 can be length and piece-ix1 can be -1. The j's are zero in those cases."
  (let [box (v box) pd (piece-digest box) nb4 (:num-b4 pd)
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

(defn grab-selection [box ix0 ix1]
   "Returns the selection given ix0 and ix1 on the rendered string 
      ix0 and ix1 correspond to the standard inclusive, exclusive pattern which is equivalent to the cursor being at ix0 and ix1.
    partial-grab-f tells us how to handle partial selections of x, i.e. what to do to the user data.
      first arg = the thing being selected, second arg = the piece of the :text 
      third arg = the [ix0 ix1] within the text that is being selected.
      Returns the modified x."
  (if (= (count (:pieces box)) 0) (throw (Exception. "# of pieces must be > 0.")))
  (if (or (< ix0 0) (< ix1 0)) (throw (Exception. "The indexes must be >= 0")))
  (let [box (v box) pd (piece-digest box) pieces (:pieces box)
        counts (:counts pd) n (:npieces pd) nb4 (:num-b4 pd) 
        cij (ixjx box ix0 ix1) cix0 (first cij) cix1 (second cij) jx0 (nth cij 2) jx1 (nth cij 3) ; indexes.
        pgf (fn [piece j0 j1] ((:partial-grab-f box) piece (subs (:text piece) j0 j1) [j0 j1]))] 
    (cond (= (count counts) 0) (throw (Exception. "zero-length :pieces array."))
      (> cix0 cix1) [] ; both cursors on a boundary (eq to ix0 = ix1), grab nothing.
      (= cix0 cix1) ; part of a single selection, pieces[cix0] is never empty.
      [(pgf (nth pieces cix0) jx0 jx1)]
      :else ; cix0 < cix1, can do selection safely w/o out of bounds.
      (into [] (concat [(pgf (nth pieces cix0) jx0 (nth counts cix0))] 
                 (subvec pieces (inc cix0) cix1) 
                 [(pgf (nth pieces cix1) 0 jx1)])))))

;;;;;;;;;;;;;;;;;;;; Cursor conversion:
; ix = cursor index in the string (stored in the node for functions that convert from it).
; piece = which piece we are on.
; grid = 2D location within the component's text. Scrolling needs to be subtracted out to compute this.
; ugrid = not adding scrolling.
; pixel = 2D location of cursor in pixels (local coords, camera is moved/zoomed and physics).

(defn cursor-pixel-to-ugrid [box pixel-x pixel-y]
  "Gets the cursor integer cursor grid position [x y] where 0 is upper left corner.
   Doesn't apply scrolling. Clamped to the bounds of the grid index."
  (let [box (v box) ft-sz (font-size2 box)
        mr (:margin *text-params*)
        v-range (view-range box)
        ; unclamped values:
        cx0 (/ (- pixel-x mr) (first ft-sz))
        cy0 (/ (- pixel-y mr) (second ft-sz))
        ; clamped values:
        cx (Math/round (double (min (max cx0 0.0) (- (nth v-range 3) (nth v-range 2)))))
        cy (int (Math/floor (double (min (max cy0 0.0) (- (nth v-range 1) (nth v-range 0))))))]
    [cx cy]))

(defn cursor-pixel-to-grid [box pixel-x pixel-y]
  "More useful than the ugrid version."
  (let [xy (cursor-pixel-to-ugrid box pixel-x pixel-y)] 
    [(+ (first xy) (:scroll-left box)) (+ (second xy) (:scroll-top box))]))

(defn cursor-ugrid-to-pixel [box grid-x grid-y]
  "Gets the cursor-midpoint pixel location. Add half of the fontsize to get to the bottom-right char. No scrolling."
  (let [ft-sz (font-size2 box) sx (first ft-sz) sy (second ft-sz) m (:margin *text-params*)]
    [(+ m (* sx grid-x)) (+ m (* sy grid-y))]))

(defn cursor-grid-to-pixel [box grid-x grid-y]
  "Gets the cursor-midpoint pixel location. Add half of the fontsize to get to the bottom-right char. Yes scrolling."
  (let [ft-sz (font-size2 box) sx (first ft-sz) sy (second ft-sz) m (:margin *text-params*)]
    [(+ m (* sx (- grid-x (:scroll-left box)))) (+ m (* sy (- grid-y (:scroll-top box))))]))

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

(defn cursor-ix-to-grid [box] 
  "Gets the x and y grid, returns nil if they aren't on the screen."
  (let [xy (cursor-ix-to-ugrid box) sl (:scroll-left box) st (:scroll-top box)]
    (if xy
      (let [gx (- (first xy) sl) gy (- (second xy) st)
            wh (view-wh box) w (first wh) h (second wh)]
        (if (and (>= gx 0) (>= gy 0) (<= gx (inc w)) (<= gy (inc h))) [gx gy])))))

(defn cursor-ix-to-pixel [box]
  "The cursor's coordinates [x y] center, before translation and scaling. nil if the cursor is not rendered."
  (let [ixjx (cursor-ix-to-grid box) sz (font-size2 box) margin (:margin *text-params*)]
    (if ixjx [(+ margin (* (first sz) (first ixjx))) (+ margin (* (second sz) (+ 0.5 (second ixjx))))])))

(defn cursor-ix-to-piece [box]
  "Converts the nodes cursor-ix to [piece-ix loc-on-piece].
   Piece-ix is which piece we are. Loc-on-piece is the location on the piece, 0 = left edge of piece.
   in-space? is whether we are inside (not at the edge of) the spacers. We are assigned to the piece before the spacer if we are in the spacer."
  (let [box (v box) ix (:cursor-ix box) ix (if (< ix 0) 0 ix)
        pd (piece-digest box) counts (:counts pd) nc (:npieces pd)
        each-start (:num-b4 pd) each-end (mapv + each-start counts) ; inclusive cursor ranges.
        piece-ix (first (filterv #(and (>= ix (nth each-start %)) (<= ix (nth each-end %))) (range (count (:pieces box)))))
        piece-ix (if piece-ix piece-ix (dec nc))
        loc-on-piece (min (- ix (nth each-start piece-ix)) (nth counts piece-ix))]
    [piece-ix loc-on-piece]))

(defn cursor-piece-to-ix [box piece-ix]
  "The start of the piece-ix'th piece"
  (let [pieces (mapv :text (:pieces box))] (apply + (mapv count (subvec pieces 0 (inc piece-ix))))))

(defn get-selected-char-rects [box]
  "Gets the character rectangles, vector of [x,y width height]'s, local coords with respect to us"
  (let [ix0 (:selection-start box)
        ix1 (:selection-end box)]
    (if (> ix1 ix0)
      (let [fs (font-size2 box) fx (first fs) fy (second fs) v-range (view-range box)
            gr (string-grid box true false)
            igr (zipmap (vals gr) (keys gr))
            m (:margin *text-params*)
            xys (filterv identity (mapv #(get igr %) (range ix0 ix1)))]
        (mapv #(vector (+ m (* fx %1)) (+ m (* fy %2)) (+ fx 1.01) (+ fy 1.01)) (mapv first xys) (mapv second xys))))))

(defn exon [box] 
  "Gets the excised region upon a keypress, [ix0 ix1] inclusive-exclusive pattern.
   [:selection-start :selection-end] in some cases, but the :cursor-ix also can influence things."
  (let [sel0 (:selection-start box) sel1 (:selection-end box) ; inclusive.
        valid-sel? (> sel1 sel0)
        c-ix (:cursor-ix box)
        ; Not sure the best for sel1>sel0 and c-mismatch, which is normally impossible
        c-mismatch? (and (not= sel0 c-ix) (not= sel1 c-ix))
        sel0 (if c-mismatch? c-ix sel0) sel1 (if c-mismatch? c-ix sel1)]
    [sel0 sel1]))

;;;;;;;;;;;;;;;;;;;; Box modification functions:

(defn fit-to-text [box x? y?]
  "Fits the node's size to the text in it."
  (let [box (v box) ft-sz (font-size2 box) m (:fit-to-text-margin *text-params*)
        mns (:margin *text-params*)
        d (string-digest box)
        n-l (:nlines d) max-l (apply max (:counts d))
        box (assoc box :scroll-top 0 :scroll-left 0)]
    (assoc box :size 
      [(if x? (+ (* mns 2) m (* max-l (first ft-sz))) (first (:size box)))
       (if y? (+ (* mns 2) m (* n-l (second ft-sz))) (second (:size box)))])))

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
   round-down? = for strings whether we add to the end of the before stuff or beginning of the after stuff."
  (let [box (v box) 
        ; Stuff before:
        stuff-before (grab-selection box 0 ix0)
        stuff-after (grab-selection box ix1 1e100)
        pd (piece-digest box) pieces (:pieces box)
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
        ij (ixjx box ix0 ix1) ct #(if % (count (:text %)) -1)
        flb4? (or (= (nth ij 2) 0) (= (nth ij 2) (ct (get pieces (first ij)))))
        flafr? (or (= (nth ij 3) 0) (= (nth ij 3) (ct (get pieces (second ij)))))

        p+0 (first ij) p-1 (second ij) ; the easy boundaries that follow the same ij rules.
        p-0 (if flb4? (first (filter #(or (< % 0) (> (nth counts %) 0)) (range (dec p+0) -2 -1))) p+0)
        p+1 (if flafr? (first (filter #(or (= % n) (> (nth counts %) 0)) (range (inc p-1) (inc n)))) p-1)]
    ((:insert-fn box)
      box insert 
      {:b4 stuff-before :afr stuff-after :ix0 ix0 :ix1 ix1
       :piece-0 p-0 :piece+0 p+0 :piece-1 p-1 :piece+1 p+1
       :jx-0 (if flb4? (if (= p-0 -1) 0 (nth counts p-0)) (nth ij 2))
       :jx+0 (nth ij 2) :jx-1 (nth ij 3)
       :jx+1 (if flafr? 0 (nth ij 3))
       :copy-piece-ixs copy-piece-ixs} (string? insert))))

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

;;;;;;;;;;;;;;;;;;;;;;;;; Editing mouse and key inputs

(defn key-to-edit [box key-evt]
  "Converts the key press to an edit.
   :type is :type :backspace :select-all :cut :copy :paste :save :arrow :type :ignore
   :ix0 and :ix1 are the range of relevent selections. For :select-all it is the entire node.
   :value is the char typed."
  (let [box (v box) ck (clevt/ctrl+ key-evt) ak (clevt/arrow-key key-evt) 
        tk (clevt/typed-key key-evt) ek (clevt/esc? key-evt)
        ix01 (exon box) ix0 (first ix01) ix1 (second ix01)]
    (cond (= ck \a) {:type :select-all :ix0 0 :ix1 (dec (count (rendered-string box)))}
      (= ck \x) {:type :cut :ix0 ix0 :ix1 ix1 :value ""}
      (= ck \c) {:type :copy :ix0 ix0 :ix1 ix1 :value ""}
      (= ck \v) {:type :paste :ix0 ix0 :ix1 ix1 :value ""}
      (= ck \s) {:type :save :ix0 0 :ix1 (count (rendered-string box)) :value ""} ; :ix0 and :ix1 have less meaning.
      ck {:type :ignore :ix1 ix1 :value ""}
      ak {:type :arrow :value ak :ix0 ix0 :ix1 ix1}
      tk {:type :type :value tk :ix0 ix0 :ix1 ix1}
      :else {:type :ignore})))

(defn type-char [box chr] 
  "The user types a char in a given node, which replaces the selected region, if any.
   Does not handle arrow keys."
  (let [box (v box) sel01 (exon box) sel0 (first sel01) sel1 (second sel01)
        valid-sel? (> sel1 sel0)
        bk? (= (str chr) (str \backspace))] ; delete a char instead of adding one.
    (scroll-to-see-cursor
      (assoc (if bk? 
               (let [b4 (grab-selection box 0 sel0) afr (grab-selection box sel1 1e100)
                     ij (ixjx box sel0 sel1)
                     multi? (< (first ij) (second ij))] ; note that first rounds up and second rounds down.
                 ((:delete-fn box) box sel0 sel1 b4 afr multi?)) 
               (edit box sel0 sel1 (str (if (= (str chr) "\t") "    " chr)) []))
        :selection-start 0 :selection-end 0))))

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
        sh? (:shifting? *text-params*)
        i1 (:cursor-ix box) i2 (cursor-pixel-to-ix box x y)]
    (if (= (:ClickCount mouse-evt) 2) ; double-click select.
      (let [ij (ixjx box i2 i2)]
        ((:double-click-fn box) box i2 (first ij) (nth ij 2)))
      (assoc box :cursor-ix i2
        :selection-start (if sh? (min i1 i2) 0) 
        :selection-end (if sh? (max i1 i2) 0)))))

(defn mouse-drag [mouse-evt box]
  ":X0 and :X1 for initial and final points."
  (let [box (v box) cursor1 (cursor-pixel-to-ix box (:X0 mouse-evt) (:Y0 mouse-evt))
        cursor2 (cursor-pixel-to-ix box (:X1 mouse-evt) (:Y1 mouse-evt))]
    (assoc box :selection-start (min cursor1 cursor2) 
      :selection-end (max cursor1 cursor2) :cursor-ix cursor2)))

(defn mouse-wheel [wheel-evt box]
  "Mouse wheel scrolling. Only vertical is supported, sorry mac trackpads."
  ;(println (:WheelRotation wheel-evt) (:PreciseWheelRotation wheel-evt))
(scroll-bound (scroll box 0 (:WheelRotation wheel-evt))))

(defn key-press [key-evt box]
  "Key press mode. Trying to save generates an error here."
  (let [box (v box) ed (key-to-edit box key-evt) ty (:type ed)
        copy!! (fn [] ; maybe make this customizable?
                 (let [x0 (:ix0 ed) x1 (:ix1 ed)] ; selection indexes.
                   (if (<= x1 x0) false ; nothing selected.
                     (let [slice (grab-selection box x0 x1)
                           txt (apply str (mapv :text slice)) ; txt is the lookup key on paste.
                           i0 (first (ixjx box x0 x1)) 
                           copy-ixs (mapv #(+ % i0) (range (count slice)))] ; which index each comes from.
                       (clipboard/put-as-string!! txt) 
                       (reset! clip-atom (hash-map txt {:x slice :ixs copy-ixs}))))))]
    (alter-var-root #'*text-params* #(assoc % :shifting? (:ShiftDown key-evt)))
    (cond (= ty :backspace) (type-char box \backspace)
      (= ty :select-all) (assoc box :selection-start 0 :selection-end (:ix1 ed))
      (= ty :cut) (if (copy!!) (type-char box \backspace) box) ; remove and store if there is a selection.
      (= ty :copy)  (do (copy!!) box)
      (= ty :paste) (let [cpa @clip-atom txt (clipboard/get-as-string) ; the clipboard takes precedence if the atom disagrees.
                          k (first (keys cpa)) v (:x (get cpa k)) ; will be nil if nothing was copied.
                          agree? (= (apply str (mapv :text v)) txt)
                          ix01 (exon box)]
                      (edit box (first ix01) (second ix01)
                        (if agree? v txt) (if agree? (:ixs (get cpa k)) [])))
      (= ty :save) ((:save-fn!!! box) box)
      (= ty :arrow) (arrow-cursor box (:value ed) (:ShiftDown key-evt))
      (= ty :type) (type-char box (:value ed))
      :else box)))

(defn key-release [key-evt box]
  (alter-var-root #'*text-params* #(assoc % :shifting? false)) box)

;;;;;;;;;;;;;;;;;;;;;;;;; Graphics ;;;;;;;;;;;;;;;;;;;;;;;;;

;(defn clojure-colorize [s grid]; TODO: move this function
;  (let [^ints idepth (:inter-depth (blitcode/basic-parse s)) ; paranthesis nesting level.
;        char-lev (mapv #(max (aget ^ints idepth %) (aget ^ints idepth (inc %))) (vals grid))] ; Nesting level of characters. 
;    (mapv #(conj % 1) (mapv colorful/level2rgb char-lev))))

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
        h-cmd (if (> (+ l r) 0) [(+ x0 (* (- x1 x0) l (/ (+ l nh r)))) y1 (- x1 (* (- x1 x0) r (/ (+ l nh r)))) y1])
        v-cmd (if (> (+ t b) 0) [x0 (+ y0 (* (- y1 y0) t (/ (+ t nv b)))) x0 (- y1 (* (- y1 y0) b (/ (+ t nv b))))])]
    (mapv #(vector :drawLine % opts) (filterv identity [h-cmd v-cmd]))))

(defn render-selection [box]
  "Renders the selection. It can be semi-transparent but goes under the text without affecting it's color at all."
  (let [col (:selection-color box)
        rects (get-selected-char-rects box)]
    ; we could lump rects...
    ;(do (require '[clooj.app.gauitest.ctest :as gtest]) (eval (list 'gtest/print1 :id03 [(:selection-start box) (:selection-end box)])))
    (mapv #(vector :drawRect [(first %) (second %) (nth % 2) (nth % 3)]
             {:Color col}) rects)))

(defn render-cursor [box]
  "Renders the cursor of a box."
  (let [tick? (> (mod (System/nanoTime) 1e9) 5e8)
        col (if tick? [1 1 1 1] [1 0 1 1]) ; flashing.
        col-edge [0 0 0.5 1] ; for contrast.
        pix (cursor-ix-to-pixel box)] ; nil if not rendered, center of cursor.
    (if pix 
      (let [x (first pix) y1 (- (second pix) (* (:font-size box) 0.5))
            y0 (+ (second pix) (* (:font-size box) 0.5))]
        [[:drawLine [(dec x) y0 (dec x) y1] {:Color col-edge}] [:drawLine [(inc x) y0 (inc x) y1] {:Color col-edge}]
         [:drawLine [x y0 x y1] {:Color col}]])))) ; draw the main cursor last

(defn render-text [box]
  "Renders the characters of a node.
   (colorize-fn s grid piece-ix) gives us the color.
     s is the rendered string, all pieces mashed together (visible and invisible).
     grid is the string-grid.
     piece-ix is which piece each character on the string belongs to.
   With (colorize-fn s grid) -> [[rgba] [rgba] ...] how to colorize it."
  (let [colorize-fn (:colorize-fn box)
        gr (string-grid box false false) ; [ix iy] => i.
        x (piece-digest box)
        s (rendered-string box)
        piece-ix (into [] (apply concat (mapv #(repeat %1 %2) (:counts x) (range))))
        grid-chars (mapv #(subs s % (inc %)) (vals gr))
        cols (colorize-fn s gr piece-ix) gcols (mapv #(get cols %) (vals gr))
        ft-pts (:font-size box)
        gchar-left-mids (mapv #(cursor-ugrid-to-pixel box (first %) (second %)) (keys gr))         
        gchar-left-bottoms (mapv #(vector (first %) (+ (second %) (* 0.5 ft-pts))) gchar-left-mids)
        m (:margin *text-params*)
        ; shx and shy are in addition to the margin.
        shx (* (:font-xshift-to-size *text-params*) ft-pts) ; tweak all characters the same slight amount. 
        shy (* (:font-yshift-to-size *text-params*) ft-pts)]
    (mapv #(vector :drawString [%2 (+ (first %1) shx m) (+ (second %1) shy m)] {:Color %3 :FontSize ft-pts}) gchar-left-bottoms grid-chars gcols)))


(defn render [box] 
  (let [box (v box)]
    (concat [[:drawRect [0 0 (first (:size box)) (second (:size box))] {:Color (:outline-color box)}]] ; outer rectangle.
      (render-scroll box) (render-selection box) (render-text box) (render-cursor box))))
