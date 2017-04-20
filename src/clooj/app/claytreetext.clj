; Text editing.
(ns clooj.app.claytreetext
  (require [clooj.java.clipboard :as clipboard]
    [clooj.app.claytreeevt :as clevt]
    [clojure.string :as string]))

; Text fields:
; :pieces = an array of strings.
  ; If there are n children "pulled out" there are n+1 elements.
; :cursor-ix is the current cursor index, counting the space between the pieces.
; :font-size0 is the font size. The actual font size can be smaller because of physics-induced compression.
; :selection-start and :selection-end are the selected text range, inclusive.
  ; Dragging the cursor to location x makes :selection-end x-1. 
; :scroll-top and :scroll-left = upper left corner scroll position.
; TODO: maybe use inclusive-exclusive patterns for ranges.

;;;;;;;;;;;;;;;;;;;; Defaults:

(def ^{:dynamic true} *text-params* 
  {:margin 2 ; Padding when there is no scroll.
   :font-linespace-to-size 0.85 ; Not quite as high as advertized.
   :font-width-to-size 0.46; could also be gotten from the font metrics.
   :segment-space 4 ; space between the head and tail to show a gap.
   :max-autofit-size-chars 30 ; automatically fit small text.
   :font-xshift-to-size -0.1 ; Don't know where this comes from.
   :font-yshift-to-size -0.08 ; center the text vertically.
   :fit-to-text-margin 1}) ; extra space so that the text doesn't get cutoff.

(def place-holder-text ; default values for the text editor.
  {:cursor-ix 0 :font-size0 18 :selection-start 0 :selection-end -1
   :scroll-top 0 :scroll-left 0 :pieces [] :read-only? #{} :visible-spacers? true})

;;;;;;;;;;;;;;;;;;;; Node sizing:

(defn font-size [node compress?]
  "Gets the [h v pt] size, where h and v is the size of the character rectangle
   and pt is how large a fontsize to render each character as.
   compress? = include compression effects."
  (let [phy (:physics node)
        horiz-ratio (if compress? (/ (- (:x1 phy) (:x0 phy)) (:width0 phy)) 1.0) ; compression!
        verta-ratio (if compress? (/ (- (:y1 phy) (:y0 phy)) (:height0 phy)) 1.0)
        sz (:font-size0 (:tbox node))]
    [(* sz horiz-ratio (:font-width-to-size *text-params*)) 
     (* sz verta-ratio (:font-linespace-to-size *text-params*)) 
     (* 0.5 sz (+ horiz-ratio verta-ratio))]))

(defn margin [node] (:margin *text-params*)) ; TODO: two margins with optional compression.

(defn view-wh [node]
  "Gets the scroll size, i.e the number of characters of the node."
  (let [tbox (:tbox node)
        ft-sz (font-size node false) ; don't compress
        mr (margin node)
        top (:scroll-top tbox) left (:scroll-left tbox)
        phy (:physics node)]
    ; the 1e-5 makes sure that rounding errors in fit-to-text don't affect us.
    [(long (Math/floor (+ 1e-5 (/ (- (:width0 phy) (* mr 2)) (first ft-sz)))))
     (long (Math/floor (+ 1e-5 (/ (- (:height0 phy) (* mr 2)) (second ft-sz)))))]))

(defn view-range [node]
  "gets the ix of [top bottom left right] of a node based on it's size, inclusive range of chars that are visible.
   It's size may be larger than the text."
  (let [wh (view-wh node) tbox (:tbox node)
        top (:scroll-top tbox) left (:scroll-left tbox)
        bottom (max 0 (dec (long (Math/floor (+ top (second wh)))))) right (max 0 (dec (long (Math/floor (+ left (first wh))))))]
    [top bottom left right]))

;;;;;;;;;;;;;;;;;;;; String analysis:

(def tr-ch (str \u00A7)) ; a spacer character that is nice looking and different from the standard characters.

(defn rm-sp [s] "Removes spacers."
  (string/replace s tr-ch ""))
(defn sp-sp [s] "Replaces spacers with space."
  (string/replace s tr-ch " "))

(defn rendered-string [node]
  "Gets the string that is rendered (as opposed to the string that is actually in the node)."
  (let [v? (:visible-spacers? (:tbox node))
        spc #(if v? % (string/replace % tr-ch " "))]
    (spc (apply str (interpose (apply str (repeat (:segment-space *text-params*) tr-ch)) (:pieces (:tbox node)))))))

(defn string-digest [s-or-node]
  "Gets several properties that are useful."
  (let [^String s (if (string? s-or-node) s-or-node (rendered-string s-or-node))
        lines (into [] (.split s "\n")) ;each line.
        lines (if (= (count lines) 0) [""] lines) ; have at least one line.
        line-counts (mapv count lines)]
    {:line-counts line-counts :nlines (count lines)
     :num-b4-line (into [] (reductions + 0 (mapv inc line-counts))) ; puts the \n on the previous line, and an extra at the end.
     :nchars (count s)})); [0 123 456, etc], inc to include line feeds.

(defn string-grid [node add-one-on-right?]
  "Grid map from the 2D [ix iy] integer cursor coords to the cursor on the rendered string.
   On the grid 0 is placing the cursor all the way left/top on the screen.
   On the string 0 it the placing the cursor at the beginning.
   left-overflow is mapped to the end of the line, bottom overflow is mapped to the length of string."
  (let [d (string-digest node)
        v-range (view-range node) top (first v-range) bottom (second v-range) 
        add-me (if add-one-on-right? 1 0)  ; add 1 because the cursor can be after the end of the string.
        left (nth v-range 2) right (+ (nth v-range 3) add-me)
        kys (apply concat 
              (mapv (fn [l] (mapv #(vector (- % left) (- l top)) (range left (inc right)))) 
                (range top (inc bottom))))
        nc (:nchars d) num-b4 (:num-b4-line d) nlines (:nlines d) line-counts (:line-counts d)
        vals (mapv #(let [x (+ (first %) left) y (+ (second %) top)] ; location on the string.
                      (cond (< y 0) 0 (>= y nlines) nc
                        :else (+ (nth num-b4 y) (min (nth line-counts y) x)))) kys)]
    (zipmap kys vals)))

(defn spacer-to-cursor-ix [node child-ix]
  "Returns the cursor ix of the beginning of the spacer for a given child."
  (let [pieces (:pieces (:tbox node)) ssp (:segment-space *text-params*)]
    (apply + (mapv #(+ % ssp) (subvec pieces 0 child-ix)))))

(defn selection-of-chunks [chunks ix0 ix1 pad-at-end]
   "Which chunks are selected. ix0 and ix1 are on the original rendered string.
    pad-at-end = how many characters as spacers to add at the end of each chunk.
   :ix0, :ix1 = inclusive indexes of which chunks are selected.
   :begin-stub, :end-stub = the part (maybe whole) of the first and last chunk that has been selected, not including the padding.
     They will be the same if only one line is selected.
   the pad after one below ix1 is the last pad to be fully-selected (this means :end-stub can have zero length).
   WHY BOTH inclusive? Because it is equiv to putting the caret at ix0 and dragging it to ix1."
  (let [counts (map #(+ (count %) pad-at-end) chunks) n (count chunks) 
        nb4 (butlast (reductions + 0 counts)) ; How many characters are below each line.
        cix0 (last (filter #(> % -1) (map #(if (<= (nth nb4 %) (max ix0 0)) % -1) (range n)))); what chunk the selection ix0 starts on.
        cix1 (last (filter #(> % -1) (map #(if (<= (nth nb4 %) (max ix1 0)) % -1) (range n))))
        jx0 (min (- ix0 (nth nb4 cix0)) (dec (count (nth chunks cix0)))); subs indexes, jx0 incusive jx1 exclusive.
        jx1 (min (inc (- ix1 (nth nb4 cix1))) (count (nth chunks cix1)))
        out {:ix0 cix0 :ix1 cix1}]
    (if (= cix0 cix1) (let [s (subs (nth chunks cix0) jx0 jx1)] (assoc out :begin-stub s :end-stub s)) ; same line.
      (assoc out :begin-stub (subs (nth chunks cix0) 0 jx0) :end-stub (subs (nth chunks cix1) 0 jx1)))))

(defn real-string 
  "Returns the actual value of the string for the node or a selection. 
   Children have to be fully selected to be included."
  ([node] (real-string node 0 1e100))
  ([node ix0 ix1] ;ix = cursor-indexes on the rendered-string.
    (let [pieces (:pieces (:tbox node)) n-sp (:segment-space *text-params*)
          sel (selection-of-chunks pieces ix0 ix1 (:segment-space *text-params*))
          bodies (subvec pieces (:ix0 sel) (inc (:ix1 sel)))
          ch-strs (mapv #(if-let [x (nth (:children node) %)] (real-string x) "") (range (:ix0 sel) (:ix1 sel)))]
      (apply str (interleave bodies (conj ch-strs ""))))))

;;;;;;;;;;;;;;;;;;;; Cursor conversion:
; ix = cursor index in the string (stored in the node for functions that convert from it).
; grid = 2D location of cursor, (0,0) is the upper left.
; screengrid = 2D location within the component, when we subtract out scrolling.
; world = 2D location of cursor in pixels.

(defn scroll-amounts [node]
  "How much we can scroll [top bottom left right].
   negative numbers => extra space."
  (let [vr (view-range node)
        lc (:line-counts (string-digest node))]
    [(first vr) (- (dec (count lc)) (second vr))
     (nth vr 2) (- (dec (apply max lc)) (nth vr 3))]))

(defn cursor-screengrid-to-ix [node x y]
  "Cursor grid to 1D index."
  (get (string-grid node true) [x y]))

(defn cursor-world-to-screengrid [node world-x world-y]
  "Gets the cursor integer cursor grid position [x y] where 0 is upper left corner.
   Doesn't apply scrolling. Clamped to the bounds of the grid index."
  (let [tbox (:tbox node)
        ft-sz (font-size node true)
        mr (margin node)
        v-range (view-range node)
        rel-x (- world-x (:x0 (:physics node))) rel-y (- world-y (:y0 (:physics node)))
        cx (Math/round (double (min (max (/ (- rel-x (* mr 2.0)) (first ft-sz)) 0.0) (- (nth v-range 3) (nth v-range 2)))))
        cy (Math/round (double (min (max (/ (- rel-y (* mr 2.0) (* 0.5 (second ft-sz))) (second ft-sz)) 0) (- (nth v-range 1) (nth v-range 0)))))]
    [cx cy]))

(defn cursor-screengrid-to-world [node grid-x grid-y]
  "Gets the cursor-midpoint world location. Add half of the fontsize to get to the bottom-right char."
  (let [ft-sz (font-size node true) sx (first ft-sz) sy (second ft-sz) m (margin node)]
    [(+ (:x0 (:physics node)) m (* sx grid-x)) (+ (:y0 (:physics node)) m (* sy grid-y) (* 0.5 (nth ft-sz 2)))]))

(defn cursor-world-to-ix [node world-x world-y]
  "Gets the cursor index from a node and a cursor position.
   For a string of length n, the cursor ix ranges from 0 to n inclusive (n+1 values)."
  (let [xy (cursor-world-to-screengrid node world-x world-y)
        tbox (:tbox node)
        v-range (view-range node)
        gr (string-grid node true)]
    (get gr xy)))

(defn get-selected-char-rects [node]
  "Gets the character rectangles, vector of [x,y width height]'s."
  (let [ix0 (:selection-start (:tbox node))
        ix1 (:selection-end (:tbox node))]
    (if (>= ix1 ix0)
      (let [fs (font-size node true) fx (first fs) fy (second fs) v-range (view-range node)
            gr (string-grid node true)
            igr (zipmap (vals gr) (keys gr))
            m (margin node)
            xc (:x0 (:physics node)) yc (:y0 (:physics node))
            xys (filterv identity (mapv #(get igr %) (range ix0 (inc ix1))))]
        (mapv #(vector (+ xc m (* fx %1)) (+ yc m (* fy %2)) (+ fx 1.01) (+ fy 1.01)) (mapv first xys) (mapv second xys))))))

(defn cursor-ix-to-ggrid [node] 
  "Gets the x and y global grid, never nil and out-of-bounds cursors are clamped."
  (let [d (string-digest node) nb4 (:num-b4-line d)
        cursor-ix (max 0 (min (:nchars d) (:cursor-ix (:tbox node))))
        lix (second (last (filterv #(<= (first %) cursor-ix) (mapv vector nb4 (range)))))]
    [(- cursor-ix (nth nb4 lix)) lix]))

(defn cursor-ix-to-screengrid [node] 
  "Gets the x and y grid, returns nil if they aren't on the screen."
  (let [xy (cursor-ix-to-ggrid node) sl (:scroll-left (:tbox node)) st (:scroll-top (:tbox node))]
    (if xy
      (let [gx (- (first xy) sl) gy (- (second xy) st)
            wh (view-wh node) w (first wh) h (second wh)]
        (if (and (>= gx 0) (>= gy 0) (<= gx (inc w)) (<= gy (inc h))) [gx gy])))))

(defn cursor-ix-to-world [node]
  "The global cursor's coordinates [x y] center. nil if the cursor is not rendered."
  (let [ixjx (cursor-ix-to-screengrid node)
        sz (font-size node true)
        margin (margin node)]
    (if ixjx [(+ margin (:x0 (:physics node)) (* (first sz) (first ixjx))) (+ margin (:y0 (:physics node)) (* (second sz) (+ 0.5 (second ixjx))))])))

(defn cursor-ix-to-piece [node]
  "Converts the nodes cursor-ix to [piece-ix loc-on-piece in-space?].
   Piece-ix is which piece we are. Loc-on-piece is the location on the piece, 0 = left edge of piece.
   in-space? is whether we are inside (not at the edge of) the spacers. We are assigned to the piece before the spacer if we are in the spacer."
  (let [ix (:cursor-ix (:tbox node)) ix (if (< ix 0) 0 ix)
        pieces (:pieces (:tbox node)) sp (:segment-space *text-params*)
        counts (mapv #(+ (count %) sp) pieces) nc (count counts)
        each-start (into [] (reductions + 0 counts)) each-end (mapv + each-start counts) ; inclusive cursor ranges.
        piece-ix (first (filterv #(and (>= ix (nth each-start %)) (<= ix (nth each-end %))) (range (count pieces))))
        piece-ix (if piece-ix piece-ix (dec nc))
        loc-on-piece (min (- ix (nth each-start piece-ix)) (nth counts piece-ix))]
    [piece-ix loc-on-piece (and (< piece-ix (dec nc)) (> loc-on-piece (- (nth counts piece-ix) sp)))]))

(defn cursor-child-ix-to-ix [node child-ix]
  "Where the child is (cursor ix is at the center of the spacer)."
  (let [pieces (:pieces (:tbox node))
        nsp (:segment-space *text-params*)]
    (apply + (int (/ nsp 2)) (- nsp) (mapv #(+ (count %) nsp) (subvec pieces 0 (inc child-ix))))))

;;;;;;;;;;;;;;;;;;;; Node modification functions:

(defn fit-to-text [node x? y?]
  "Fits the node's size to the text in it. Uasually used with :folder or :file nodes
   where the string is small. Also gets rid of any scrolling."
  (let [ft-sz (font-size node false) m (:fit-to-text-margin *text-params*)
        mns (margin node)
        d (string-digest node)
        n-l (:nlines d) max-l (apply max (:line-counts d))
        node (update node :tbox #(assoc % :scroll-top 0 :scroll-left 0))]
    (update node :physics #(assoc % :width0 (if x? (+ (* mns 2) m (* max-l (first ft-sz))) (:width0 %))
                            :height0 (if y? (+ (* mns 2) m (* n-l (second ft-sz))) (:height0 %))))))

(defn fit-to-text-if-small [node]
  (let [d (string-digest node) xx (apply max (:line-counts d)) yy (count (:line-counts d))
        n (:max-autofit-size-chars *text-params*)]
    (if (or (<= xx n) (<= yy n)) (fit-to-text node (<= xx n) (<= yy n)) node)))

(defn on-text-change [node]
  "Call this after the text is updated by one of the above functions. We must update
   two nodes in the tree: one to tell us that the file is changed and one for the physics."
  (fit-to-text-if-small node)) ; can be a bit jumpy so maybe soften it a bit.

(defn edit-string [node ix0 ix1 ^String str-insert] 
  "Replaces the part of the string between ix0 and ix1 (inclusive) with str-insert and
   sets the cursor index to the end of the inserted string.
   All indexes are on the string with :pieces unwrapped and spaced.
   This editing does not changes the number of :pieces even if :pieces are emptied out."
  (let [str-insert (.replace str-insert "\t" "    ")
        pc (:pieces (:tbox node)) nsp (:segment-space *text-params*)
        each-string-start (into [] (reductions + 0 (mapv #(+ % nsp) (mapv count pc)))) ; Starting char of each piece, including one for when the piece after the last would start.
        ; Remove the selection (this may affect multiple segments).
        ; Even if we empty pieces out we don't remove them from the vector:
        pc-removed (mapv #(let [rix0 (- ix0 %3) rix1 (- ix1 %3)] ; inclusive deletion indexes on this string.
                            (if (or (>= rix0 %2) (< rix1 0) (= %2 0)) %1 ; both indexes out of range or string is empty.
                              (let [rcix0 (max rix0 0) rcix1 (min rix1 (dec %2))]
                                (str (subs %1 0 rcix0) (subs %1 (inc rcix1)))))) pc (mapv count pc) each-string-start)
        ; Read-only pieces don't change:
        ro-enforce (fn [pieces] (mapv #(if (get (:read-only? node) %3) %1 %2) pc pieces (range)))
        ; [Which element, where on the string] the ix0 is:
        cur-find (mapv #(if (and (>= ix0 %1) (< ix0 %2)) [%3 (min (- ix0 %1) %4)] false) ; 
                   each-string-start (conj (into [] (rest each-string-start)) 1e100) (range) (mapv count pc-removed))
        cur-find (first (filterv identity cur-find))
        cur-find (cond cur-find cur-find (< ix0 0) [0 0] :else [(dec (count pc)) (count (last pc-removed))]) ; out of bounds cases.
        ca (first cur-find) cb (second cur-find) ; location: which string and where in the string the cursor is.
        tbox1 (assoc (:tbox node) :cursor-ix (+ (nth each-string-start ca) cb (count str-insert)) ; the length changing doesn't affect stuff before ix0.
               :pieces (ro-enforce (update pc-removed ca #(str (subs % 0 cb) str-insert (subs % cb)))))]
   (on-text-change (assoc node :tbox tbox1))))

;;;;;;;;;; Scrolling

(defn scroll-bound [node]
  "Makes sure the node doesn't have excess space, if it is possible to do so."
  (let [d (string-digest node)
        vr (view-range node) vr (update (update vr 1 inc) 3 inc) ; [top bottom left right]
        x1 (apply max (:line-counts d))
        y1 (:nlines d)
        tbox (:tbox node)
        tbox (cond (> (nth vr 3) x1) (update tbox :scroll-left #(max 0 (- % (- (nth vr 3) x1))))
               (< (:scroll-left tbox) 0) (assoc tbox :scroll-left 0) :else tbox)
        tbox (cond (> (nth vr 1) y1) (update tbox :scroll-top #(max 0 (- % (- (nth vr 1) y1))))
               (< (:scroll-top tbox) 0) (assoc tbox :scroll-top 0) :else tbox)]
    (assoc node :tbox tbox)))

(defn scroll [node d-ix d-iy]
 "Scrolls the text inside a node, by an integer amount."
  (scroll-bound (update node :tbox 
                  #(assoc % :scroll-top (+ (:scroll-top %) (long d-iy)) :scroll-left (+ (:scroll-left %) (long d-ix))))))

(defn scroll-to-see-cursor [node]
  "Scrolls just enough to make the cursor visible."
  (let [xy (cursor-ix-to-ggrid node) x (first xy) y (second xy)
        scx (fn [n x] (update-in n [:tbox :scroll-left] #(+ % (long x))))
        scy (fn [n y] (update-in n [:tbox :scroll-top] #(+ % (long y))))
        vr (view-range node) ; [top bottom left right].
        t (first vr) b (second vr) l (nth vr 2) r (nth vr 3)
        nodex (cond (< x l) (scx node (- x l)) (> (dec x) r) (scx node (- (dec x) r)) :else node)
        nodexy (cond (< y t) (scy nodex (- y t)) (> y b) (scy nodex (- y b)) :else nodex)] 
    (scroll-bound nodexy)))

;;;;;;;;;; Editing

(defn key-to-edit [node key-evt]
  "Converts the key press to an edit.
   :type is :type :backspace :select-all :cut :copy :paste :save :arrow :type :ignore
   :ix0 and :ix1 are the range of relevent selections. For :select-all it is the entire node.
   :value is the char typed."
  (let [ck (clevt/ctrl+ key-evt) ak (clevt/arrow-key key-evt) 
        tk (clevt/typed-key key-evt) ek (clevt/esc? key-evt)
        ix0 (:selection-start (:tbox node)) ix1 (:selection-end (:tbox node))]
    (cond (= ck \a) {:type :select-all :ix0 0 :ix1 (dec (count (rendered-string node)))}
      (= ck \x) {:type :cut :ix0 ix0 :ix1 ix1 :value ""}
      (= ck \c) {:type :copy :ix0 ix0 :ix1 ix1 :value ""}
      (= ck \v) {:type :paste :ix0 ix0 :ix1 ix1 :value ""}
      (= ck \s) {:type :save :ix0 0 :ix1 (count (rendered-string node)) :value ""} ; :ix0 and :ix1 have less meaning.
      ck {:type :ignore :ix1 ix1 :value ""}
      ak {:type :arrow :value ak :ix0 ix0 :ix1 ix1}
      tk {:type :type :value tk :ix0 ix0 :ix1 ix1}
      :else {:type :ignore})))

(defn type-char [node chr] 
  "The user types a char in a given node, which replaces the selected region, if any."
  (let [sel0 (:selection-start (:tbox node)) sel1 (:selection-end (:tbox node)) ; inclusive.
        valid-sel? (>= sel1 sel0)
        bk? (= (str chr) (str \backspace)) ; delete a char instead of adding one.
        c-ix (:cursor-ix (:tbox node))]
    (scroll-to-see-cursor
      (update (edit-string node (if valid-sel? sel0 (- c-ix (if bk? 1 0))) 
                (if valid-sel? sel1 (dec c-ix)) (if bk? "" (str chr)))
        :tbox #(assoc % :selection-start 0 :selection-end -1)))))

(defn insert-text [node str-insert]
  "Inserts text txt at the node. Doesn't update the physical properties or invalidate the file.
   The cursor is moved to the end of the insert."
  (let [c-ix (:cursor-ix (:tbox node))] (edit-string node c-ix (dec c-ix) str-insert)))
  
(defn arrow-cursor [node arrow-code] 
  "The user moves the cursor. The node scrolls to keep the cursor in view if it moves out of view.
   Arrow codes are: < > v ^."
  (let [global-xy-grid (cursor-ix-to-ggrid node)
        global-x-grid (first global-xy-grid) global-y-grid (second global-xy-grid)
        arrow-code (str arrow-code)
        gxy (cond (= arrow-code "<") [(dec global-x-grid) global-y-grid] ; it wasn't even on the screen.
                  (= arrow-code ">") [(inc global-x-grid) global-y-grid]
                  (= arrow-code "v") [global-x-grid (inc global-y-grid)]
                  (= arrow-code "^") [global-x-grid (dec global-y-grid)]
                  :else (throw (Exception. (str "Unrecognized arrow code: " arrow-code))))
        d (string-digest node) lc (:line-counts d) nl (count lc) nb4 (:num-b4-line d) nc (:nchars d) 
        cl #(max 0 (min % (dec nl))) gx (first gxy) gy (second gxy)
        gxy (cond (or (< gy 0) (and (< gx 0) (= gy 0))) [0 0] ; various overflow and wrap-around rules in this cond.
              (< gx 0) [(nth lc (dec gy)) (dec gy)]
              (or (>= gy nl) (and (= gy (dec nl)) (> gx (last lc)))) [(last lc) (dec nl)]
              (and (> gx (nth lc gy)) (or (= arrow-code "^") (= arrow-code "v"))) [(nth lc gy) gy]
              (> gx (nth lc gy)) [0 (inc gy)]
              :else [gx gy])]
  (scroll-to-see-cursor (assoc-in node [:tbox :cursor-ix] (+ (first gxy) (nth nb4 (second gxy)))))))

(defn key-press [node key-evt]
  "Key press mode. Trying to save generates an error here."
  (let [edit (key-to-edit node key-evt) ty (:type edit)
        copy!! (fn [x0 x1]
                 (if (< x1 x0) false ; nothing selected.
                    (let [s (rendered-string node) x0 (min (max x0 0) (count s)) x1 (min (max x1 0) (count s))]
                      (do (clipboard/put-as-string!! (real-string node (:ix0 edit) (:ix1 edit))) true))))]
    (cond (= ty :backspace) (type-char node \backspace)
      (= ty :select-all) (assoc (:tbox node) :selection-start 0 :selection-end (:ix1 edit))
      (= ty :cut) (if (copy!! node) (type-char node \backspace) node) ; remove and store if there is a selection.
      (= ty :copy)  (do (copy!! node) node)
      (= ty :paste) (insert-text (if (<= (:ix0 edit) (:ix1 edit)) (type-char node \backspace) node) (clipboard/get-as-string))
      (= ty :save) (do (throw (Exception. "Saving must be handled at a higher level")) node)
      (= ty :arrow) (arrow-cursor node (:value edit))
      (= ty :type) (type-char node (:value edit))
      :else node)))