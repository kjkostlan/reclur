; View and edit code. Includes code folding as well as code expansion to children.
(ns app.codebox
 (:require [clojure.string :as string] [clojure.walk :as walk]
   [app.rtext :as rtext]
   [app.fbrowser :as fbrowser]
   [app.stringdiff :as stringdiff]
   [javac.file :as jfile]
   [clojure.string :as string]
   [coder.crosslang.langs :as langs]
   [coder.cnav :as cnav]
   [layout.colorful :as colorful]
   [coder.cbase :as cbase]))

; The global rtext contains a language protocol in :langkwd that is used for text coloring and 
; contraction/expansion, etc.
; There isnt much of a tree, the exported id are in order and there is only one element
; which is the code itself's real string sans exported children.

(declare interact-fns) ; Possible dependency cycle with the new function being used by some interact fns.

(def spacer \u26ac) ; a circle for code folding.

(defn _merge-leaf-pieces [pieces] ; keeps the total piece count down.
  (let [pieces (into [] pieces) n (count pieces)
        squishy?s (mapv #(not (:children %)) pieces)]
    (filterv #(not= % {:text ""})
      (loop [acc [] ix 0 piece {:text []}]
        (if (= ix n) (conj acc (update piece :text #(apply str %)))
          (let [pi (nth pieces ix) si? (nth squishy?s ix)]
            (recur (if si? acc (conj acc (update piece :text #(apply str %)) pi))
              (inc ix) (if si? (update piece :text #(conj % (:text pi))) {:text []}))))))))
(defn _rml [pieces] 
  (rtext/remove-empty (_merge-leaf-pieces pieces)))

;;;;;;;;;;;;;;;;;;;;;;; Updating the precomputation ;;;;;;;;;;;;;;;;;;;

(defn set-precompute [box]
  (let [inter-levels (langs/interstitial-depth (rtext/rendered-string box) (:langkwd box))
        levels-inclusive (mapv max (butlast inter-levels) (rest inter-levels))]
    (assoc box :precompute {:levels levels-inclusive :inter-levels inter-levels})))

(defn edits-update [box box1 edits]
  ; For now we don't implement editing.
  (rtext/cursor-scroll-update box
    (set-precompute (update box1 :pieces _rml)) edits))

(defn generic-update [box box1]
  "Updates the precompute, making the best attempt to drag the cursor along.
   If the edits are easy to compute by hand it's better use this instead of generic-update."
  (let [edits (stringdiff/edits-between (rtext/rendered-string box) (rtext/rendered-string box1))]
    (edits-update box box1 edits)))

;;;;;;;;;;;;;;;;;;;;;;; Piece-handling and paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ... (let [ndots 3] (apply str (repeat ndots spacer))))

(defn folded? [piece]
  "true for folded and not exported to a child."
 (boolean (:children piece)))

(defn _splay-out [cljpath0 pieces] 
  (into [] (apply concat (mapv #(concat [(conj cljpath0 %2)] (if (:children %1) (_splay-out (conj cljpath0 %2 :children) (:children %1)) [])) pieces (range)))))
(defn splay-out [box]
  "All paths to various pieces, including nested pieces, in reading order."
  (_splay-out [:pieces] (:pieces box)))
(defn splay-out+ [box]
  "The splayed-out pieces themselves, with a :tmp-path key."
  (let [uspaths (splay-out box)]
    (into [] (concat [{:text (get box :head "") :tmp-path [:head]}] 
               (mapv #(assoc (get-in box %) :tmp-path %) uspaths) 
               [{:text (get box :foot "") :tmp-path [:tail]}]))))

(defn piece-real-string [piece]
  "For folded pieces, digs down into children."
  (if (folded? piece) (apply str (mapv piece-real-string (:children piece))) (:text piece)))

(defn real-string [box]
  "What would be saved to a file."
  (if (not box) (throw (Exception. "Nil box")))
  (if (not (:pieces box)) (throw (Exception. "Nil pieces")))
  (str (get box :head "") (apply str (mapv piece-real-string (:pieces box))) (get box :foot "")))

(defn num-newlines [piece]
  "Uses precomputed values here for folded or exported pieces, otherwise uses the string."
  (dec (count (string/split (piece-real-string piece) #"\n"))))

(defn contain-ixs [box] 
 "Indexes on the rendered str that contain the index
  grab the string using an inclusive, exclusive pattern to select the internal region and the brackets. 
  rtext/cursor-ix-to-piece can get the piece index.
  Used for cold-folding, etc."
  (let [levels (:inter-levels (:precompute box))
        cur-ix (:cursor-ix box)
        n (dec (count levels)) cur-ix (max 0 (min cur-ix n))
        lev (nth levels cur-ix)
        lo (loop [ix cur-ix]
             (if (or (= ix 0) (< (nth levels ix) lev)) ix
               (recur (dec ix))))
        hi (loop [ix cur-ix]
             (if (or (= ix n) (< (nth levels ix) lev)) ix
               (recur (inc ix))))]
    [lo hi]))    
(defn contain-ixs1 [box cur-ix]
  "We commonly want to test modified cursor-ixs, so this is a convenience function."
  (contain-ixs (assoc box :cursor-ix cur-ix)))

(defn colorize [box s piece-ix char-ix0 char-ix1]
  "Level based colorization, with exported pieces counting differently."
  (let [levels (subvec (:levels (:precompute box)) char-ix0 char-ix1)
        mx (apply max 0 levels) cols (mapv #(conj (colorful/level2rgb %) 1) (range (inc mx)))]
    (mapv #(nth cols (if (>= % 0) % 0)) levels)))

(defn new-codebox []
  (assoc (merge rtext/empty-text (interact-fns)) :outline-color [0.8 0 0 1] :path [] :head "" :foot ""
    :type :codebox :langkwd :clojure :precompute {:levels [] :inter-levels [0]} :colorize-fn (fn [& args] (apply colorize args))))

(defn code-fold-toggle [cur-pieceix folding? ixs complement? box]
  "Folds or unfolds. complement? true to hilight instead of fold, i.e. in stead of hiding the
   part to be folded we hide the part to not fold."
  (let [pieces (_rml (:pieces box))

        ; Equivalent edit (needed for updating the tokens incrementally instead of recomputing everything):
        ;insert (if folding? ... 
        ;         (apply str (mapv :text (:children (nth pieces cur-pieceix)))))
        ;edit {:type :misc :ix0 (first ixs) :ix1 (second ixs) :value insert}
        ;box1 (edits-update box [edit])
        ;add-edit #(edits-update box %1 [edit])

        stats (rtext/index-stats (:pieces box) (first ixs) (second ixs) rtext/default-partial-grab)
       
        ;set-pieces #(assoc box :pieces (_rml (into [] %)))
        folded-piece (if folding? {:text ... :children (:in stats)})
        pieces-b4 (:b4 stats) pieces-afr (:afr stats)]
   (cond (and complement? folding?) ; fold it up, but put it it into the exported child.
     (let [new-head (str (get box :head "") (apply str (mapv piece-real-string pieces-b4)))
           new-foot (str (apply str (mapv piece-real-string pieces-afr)) (get box :foot ""))
           ch-pieces (:in stats)]
       (set-precompute (assoc box :head new-head :foot new-foot :pieces ch-pieces :cursor-ix 0)))
     folding?
     (let [new-pieces (_rml (into [] (concat pieces-b4 [folded-piece] pieces-afr)))]
       (set-precompute (assoc box :pieces new-pieces :cursor-ix (first ixs))))
     complement?
     (throw (Exception. "Unfolding + complement is harder to make sense of."))
     :else (let [new-pieces (_rml (into [] (concat pieces-b4 (:children (first (:in stats))) pieces-afr)))] ; remove the dots.
             (set-precompute (assoc box :pieces new-pieces :cursor-ix (first ixs)))))))

(defn code-fold-toggle-at-cursor [box folding? complement?]
  "Fold up the outer-level paren level of the code, or unfold folded code.
   Folded code splits the :pieces, unfolded code merges the :pieces.
   complement? true: folds everything else that is not the target."
  (let [cur-ix (:cursor-ix box)
        pieces (_rml (:pieces box))
        ; The cursor may be between two pieces:
        cur-pieceix0 (first (rtext/cursor-ix-to-piece (assoc box :pieces pieces)))
        cur-pieceix1 (first (rtext/cursor-ix-to-piece (assoc box :cursor-ix (inc cur-ix) :pieces pieces)))

        folded?0 (folded? (nth (:pieces box) cur-pieceix0))
        folded?1 (folded? (nth (:pieces box) cur-pieceix1))

        cur-pieceix (if (or (and folding? (not folded?0)) (and (not folding?) folded?0)) cur-pieceix0 cur-pieceix1)

        ; Cursor ixs to base the edit on and to divide up the pieces for folding:
        ixs (if folding? (let [ix0s (contain-ixs1 box cur-ix)] ; contain-ixs include the ().
                           (if (> (second ix0s) (+ (first ix0s) 2)) [(inc (first ix0s)) (dec (second ix0s))] ix0s))
              (let [cur-ix-piece-begin (rtext/cursor-piece-to-ix box cur-pieceix)]
                [cur-ix-piece-begin (+ cur-ix-piece-begin (count (:text (nth pieces cur-pieceix))))]))]
    (code-fold-toggle cur-pieceix folding? ixs complement? box)))

(defn code-unfold [piece-ix box]
   "Unfolds the piece at piece-ix where the code is folded."
   (let [cur-ix-piece-begin (rtext/cursor-piece-to-ix box piece-ix)
         ixs [cur-ix-piece-begin (+ cur-ix-piece-begin (count (:text (nth (:pieces box) piece-ix))))]]
      (if (not (folded? (nth (:pieces box) piece-ix))) (throw (Exception. "Attempted to unfold an unfolded piece.")))
      (code-fold-toggle piece-ix false ixs nil box)))

(defn diff-edit [s0 s1]
 "An edit applied to s0 giving us s1."
  (let [n0 (count s0) n1 (count s1) n- (min n0 n1)
       ; Equivalent cursor ixs for the edit.
       ix0 (if-let [i (first (filter #(not= (nth s0 %) (nth s1 %)) (range n-)))] i n-)
       min-ix1 (max ix0 (+ ix0 (- n0 n1)))
       ix1 (loop [i (dec n0)] ; annoying array stuff.
             (cond (<= i min-ix1) min-ix1 (< (+ i (- n1 n0)) 0) i
               (= (nth s0 i) (nth s1 (+ i (- n1 n0)))) (recur (dec i)) :else (inc i)))
       insert (subs s1 ix0 (+ ix1 (- n1 n0)))]
    {:type :misc :ix0 ix0 :ix1 ix1 :value insert}))

(defn keep-fe-length [box] "Always 3 spacers"
  (let [ndots 3 pieces0 (:pieces box) 
        pieces1 (mapv #(if (folded? %) (assoc % :text ...) %) pieces0)]
    (if (= pieces0 pieces1) box
      (let [ccf (fn [p0 p1 jx0] jx0) cix (:cursor-ix box)]
        ; the spacers count as an e either way, so don't factor into the edit we use:
        (set-precompute
          (assoc box :pieces (mapv (fn [p] (if (folded? p) p (update p :text #(string/replace % (str spacer) "e")))) pieces1)
            :cursor-ix (rtext/carry-cursor pieces0 pieces1 cix ccf)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Other ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; string -> tokens -> either vis-levels or inter-levels.

(defn from-text [txt lang-kwd]
  "Sets up a text-editor from a given text and language keyword."
  (set-precompute (assoc (new-codebox) :pieces [{:text txt}]
                   :langkwd lang-kwd)))

(defn token-cur-ix01 [strings allowed?s ix]
   "Returns the cursor indexes that select the token enclosing index ix.
    allowed?s determine whether each token is allowed to be selected."
  (let [cumsum (into [] (reductions + (mapv count strings)))
        cumsum1 (assoc cumsum (dec (count cumsum)) 1e100)
        tix-in (first (filter #(> (nth cumsum1 %) ix) (range)))
        tix (first (filterv #(nth allowed?s %) (range tix-in (count strings))))]
    [(if (= tix 0) 0 (nth cumsum (dec tix))) (nth cumsum tix) tix]))

(defn select-twofour-click [box four?] 
  "double click with no shift, so selects text instead of code folding."
  (let [st (rtext/rendered-string box)
        c-ix (:cursor-ix box)
        ils (:inter-levels (:precompute box))
        sel-start (:selection-start box)
        sel-end (:selection-end box)
        l0 (if (> sel-end (inc sel-start)) 
             (apply min (mapv #(nth ils %) (range (inc sel-start) sel-end)))
             (nth ils (max 0 c-ix)))
        n (count ils)
        c-ix0 (loop [ix c-ix] ; has this code been written somewhere else?
                (if (<= ix 0) 0
                  (if (and (> l0 0) (= (nth ils ix) (dec l0))) ix (recur (dec ix)))))
        c-ix1 (loop [ix c-ix] ; has this code been written somewhere else?
                (if (>= ix (dec n)) (dec n)
                  (if (and (> l0 0) (= (nth ils ix) (dec l0))) ix (recur (inc ix)))))
        toksty (cbase/tokenize (subs st c-ix0 c-ix1) (:langkwd box))
        toks (first toksty) ty (second toksty)
        
        cur-jx (- c-ix c-ix0)
        jx01-tix (token-cur-ix01 toks (repeat true) cur-jx)
        tix-in (nth jx01-tix 2)
        t-in (get toks tix-in)
        jx01 (if (and (= (get ty tix-in) 0) (re-find #"[a-zA-Z0-9]+" t-in)) ; Trigger for in-comment mode which uses human language instead.
               (let [jx0 (first jx01-tix)
                     piecesty (cbase/tokenize t-in :human)
                     pieces (first piecesty) ty (second piecesty)
                     kx01-tix (token-cur-ix01 pieces (repeat true) (inc (- cur-jx jx0)))]
                [(+ jx0 (first kx01-tix) -1) (+ jx0 (second kx01-tix) -1)])
                [(first jx01-tix) (second jx01-tix)])
        jx0 (first jx01) jx1 (second jx01)]
    (if four? (assoc box :cursor-ix c-ix1 :selection-start c-ix0 :selection-end c-ix1) ; selects the whole enclosing form.
      (assoc box :cursor-ix (+ c-ix0 jx1) :selection-start (+ c-ix0 jx0) :selection-end (+ c-ix0 jx1)))))

;;;;;;;;;;;;;;;;;;;;;;; Interaction functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interactions beyond the uasual rtext interactions.

(defn hint-sym-qual [box]
  "Most useful in the repl, where a symbol gets qualed."
  (let [box1 (select-twofour-click box false)
        sel0 (:selection-start box1)
        sel1 (:selection-end box1)]
    (if (> sel1 sel0)
      (let [sel-str (subs (rtext/rendered-string box) sel0 sel1)
            sym? (not (re-find #"[\[\]{}\(\) ]" sel-str))]
        (if sym? 
          (let [strs (sort (mapv str (cbase/symaverse)))
                matches (filterv #(string/includes? % sel-str) strs)]
            (if (= (count matches) 0) 
              (do (println "No matching fully qualified symbols [in loaded namespaces] to" sel-str) box)
              (do (if (> (count matches) 1) (println "Multible matches to" sel-str "taking fist one."))
                (rtext/edit box1 sel0 sel1 (first matches) []))))
          (do (println "The selection at+ the cursor is not a symbol.") box)))
      (do (println "No selection can be made from cursor's location.") box))))

(defn key-press [key-evt box]
  "Key-based interaction 101. Tab indents (shift-tab dedents) rather than whoops where did the block of code go.
   For now just indent the hilighted lines (and any lines in the children), not consious of the code itself.
   Other fns work normally."
  (let [ed (rtext/key-to-edit box key-evt) bk-sp? (= (str (:value ed) (str \backspace)))
        render-str (rtext/rendered-string box)
        shifting? (:ShiftDown key-evt)
        get-xy #(rtext/cursor-ix-to-ugrid (assoc box :cursor-ix %))]
    (keep-fe-length
    (cond (and (= (:type ed) :type) (or (= (:value ed) "    ") (= (:value ed) "\t"))) ; line-by line indent or dedent.
      (let [y01 (if (< (:selection-start box) (:selection-end box))
                  (let [xy0 (get-xy (:selection-start box))
                        xy1 (get-xy (:selection-end box))]
                    [(second xy0) (second xy1) (first xy0)])
                  (let [c-ix (:cursor-ix box)
                        c-range (#(vector (max (first %1) (first %2)) (min (second %1) (second %2))) 
                                  (contain-ixs1 box c-ix) (contain-ixs1 box (inc c-ix)))
                        xy0 (get-xy (first c-range))
                        xy1 (get-xy (second c-range))]
                    [(second xy0) (second xy1) (first xy0)])) ; sneak in an x0.
            y0 (first y01) y1 (second y01) x0 (nth y01 2)
            indent "  " nind (count indent)
	           line-begin-ixs (mapv #(rtext/cursor-ugrid-to-ix box 0 %) (range y0 y1))
            lines (if shifting? (string/split (rtext/rendered-string box) #"\n"))
            n-space (fn [l] (count (re-find #"[ \t]*" l)))
            ; Slow line-by-line, can be improved:
            box1 (reduce 
                   (fn [bx y] 
                    (let [ix0 (rtext/cursor-ugrid-to-ix bx 0 y)
                          ix1 (if shifting? (+ ix0 (min nind (n-space (nth lines y)))) ix0)]
                      (rtext/edit bx ix0 ix1 (if shifting? "" indent) [])))
                    box (range y0 (inc y1)))
            box2 (assoc box1 :cursor-ix 
                   (rtext/cursor-ugrid-to-ix box1 
                     (max 0 (+ x0 (if shifting? (- nind) nind))) y0))]
         (set-precompute box2))
      (= (:KeyCode key-evt) 10) ; indent current line as far.
      (let [box1 (rtext/key-press key-evt box) cix (:cursor-ix box)
            ix0 (first (contain-ixs1 box cix))
            indent-add (if (= (try (subs (rtext/rendered-string box) ix0 (inc ix0)) (catch Exception e false)) "(") 2 1)
            n-indented (+ (first (get-xy ix0)) indent-add)
            spacer (apply str (repeat n-indented " "))
            box2 (rtext/edit box1 (:cursor-ix box1) (:cursor-ix box1) spacer [])]
        (set-precompute box2))
      :else (set-precompute (rtext/key-press key-evt box))))))

(defn mouse-press [m-evt box] ; shift+double click = code folding.
  (cond (and (= (:ClickCount m-evt) 2) (:ShiftDown m-evt))
    (let [cur-ix (rtext/cursor-pixel-to-ix box (:X m-evt) (:Y m-evt))          
          cur-pieceix (first (rtext/cursor-ix-to-piece (assoc box :cursor-ix cur-ix)))
          cur-pieceix1 (first (rtext/cursor-ix-to-piece (assoc box :cursor-ix (inc cur-ix))))
          folding? (and (not (folded? (nth (:pieces box) cur-pieceix)))
                     (not (folded? (nth (:pieces box) cur-pieceix1))))]
      (code-fold-toggle-at-cursor (assoc box :cursor-ix cur-ix) folding? nil)) 
    (= (:ClickCount m-evt) 2)
    (select-twofour-click box false)
    (= (:ClickCount m-evt) 4)
    (select-twofour-click box true)
    :else (rtext/mouse-press m-evt box)))

;;;;;;;;;;;;;;;;;;;;;;;; Codebox code navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ensure-visible [box path]
  "Unfolds code to make the path visible, drags the cursor along if possible. Does not scroll to said piece."
  (if (= (count path) 2) box
      (let [expand-ixs (mapv #(nth path %) (range 1 (- (count path) 2) 2)) n (count expand-ixs)]
        (loop [acc box n-b4 0 ix 0]
          (if (= ix n) acc
              (let [e-ix (nth expand-ixs ix) ex-ix (+ e-ix n-b4)
                    acc1 (code-unfold ex-ix acc)
                    ; Did the expanded piece compact back down:
                    compact? (and (> ex-ix 0)
                                  (not= (get-in acc [:pieces (dec ex-ix)])
                                        (get-in acc1 [:pieces (dec ex-ix)])))]
                (recur acc1 ; code-unfold keeps things updated.
                       (+ ex-ix (if compact? -1 0)) (inc ix))))))))

(defn cursor-to-real-string [box]
  "Returns where the cursor is on the real string."
  (let [box-chop (assoc (rtext/edit box 0 (:cursor-ix box) "" []) :head "")]
    (- (count (real-string box)) (count (real-string box-chop)))))

(defn real-string-to-cursor [box real-ix folded-rule]
  "Returns where the cursor-ix should go to match the real index.
   Clamps at the ends of the rendered string.
   use -1 or 1 for folded-rule."
  (let [counts (mapv #(count (piece-real-string %)) (:pieces box))
        n (count (:pieces box))]
    (loop [ix 0 cx 0 rx (count (get box :head ""))]
      (if (= ix n) cx
        (let [piece (nth (:pieces box) ix) ch? (:children piece)
              +real (nth counts ix) +vis (count (:text piece))]
          (cond (> real-ix (+ rx +real)) (recur (inc ix) (+ cx +vis) (+ rx +real))
            ch? (if (<= folded-rule 0) cx (+ cx +vis)) (<= real-ix rx) cx
            :else (+ cx (- real-ix rx))))))))

(defn select-on-real-string [box real-ix0 real-ix1]
  "Selects the text on the as-real-as-we-know string, scrolling to the selection and expanding if necessary.
   pix is the ix of the real string, jx0 and jx1 are the ixs within the real string.
   If the jxs go off the end it just maps them to the end.
   The selection includes folded pieces it is in."
  (let [selection0 (real-string-to-cursor box real-ix0 -1) 
        selection1 (real-string-to-cursor box real-ix1 1)
        cur-ix (:cursor-ix box)
        hi-ix? (> cur-ix (+ (* 0.5 selection0) (* 0.5 selection1)))
        scrolled-box (-> box (assoc :cursor-ix (if hi-ix? selection0 selection1))
                       rtext/scroll-to-see-cursor 
                       (assoc :cursor-ix (if hi-ix? selection1 selection0))
                       rtext/scroll-to-see-cursor)]
    (assoc box :selection-start selection0 :selection-end selection1
      :cursor-ix (if hi-ix? selection1 selection0))))

(defn real-string-ixs-for-thing-at-cursor [box]
  "Whatever symbol, etc is at cursor, string indexes for this."
  (let [real-s (real-string box)
        rcur-ix (cursor-to-real-string box)
        cs0 (first (if (<= rcur-ix 0) " " 
                     (subs real-s (dec rcur-ix) rcur-ix)))
        cs1 (first (if (< rcur-ix (count real-s)) 
                     (subs real-s rcur-ix (inc rcur-ix)) " "))
        sp? (fn [c] (contains? #{\ \newline \tab} c)) 
        open? (fn [c] (contains? #{\{ \( \[} c))
        closed? (fn [c] (contains? #{\} \) \]} c))
        land? (or (and (not (open? cs0)) (not (closed? cs0)) (not (sp? cs0)))
                (and (not (open? cs1)) (not (closed? cs1)) (not (sp? cs1))))
        box1 (assoc box :cursor-ix rcur-ix :pieces [{:text real-s}])]
    (cond 
      (and (not land?) (open? cs0))
      (contain-ixs box1)
      (and (not land?) (open? cs1))
      (contain-ixs1 box1 (inc (:cursor-ix box1)))
      (and (not land?) (closed? cs0))
      (contain-ixs1 box1 (dec (:cursor-ix box1)))
      (and (not land?) (closed? cs1))
      (contain-ixs box1)
      :else
      (let [box1 (if (or (sp? cs1) (open? cs1) (closed? cs1)) (update box1 :cursor-ix dec) box1)
            box1 (select-twofour-click (set-precompute box1) false)
            cix-real0 (cursor-to-real-string (assoc box1 :cursor-ix (:selection-start box1)))
            cix-real1 (cursor-to-real-string (assoc box1 :cursor-ix (:selection-end box1)))]
        [cix-real0 cix-real1]))))

(defn generic-ns-sym [box]
  "Not just codeboxes"
  (let [ty (:type box)]
    (if (= ty :codebox) 
      (langs/file2ns (fbrowser/devec-file (:path box)))
      'app.orepl)))

(defn cursor2cpath [box]
    "Returns a complete path where the cursor in a codebox is.
     For the first element of the cpath, uses what is stored in the file if read-string fails.
     Example path: [my.ns/my-sym 4 2 5 1]."
  (let [txt (real-string box) ix (cursor-to-real-string box)
        wpath (cbase/stringlang-to-wpath txt ix (:langkwd box))
        codes (langs/reads-string txt (:langkwd box))
        codes (if codes codes (langs/reads-string (jfile/open (fbrowser/devec-file (:path box))) (:langkwd box)))
        subdef-path (cnav/path2subdef-path codes wpath)
        def-sym-qual (langs/resolved (generic-ns-sym box) (cnav/path2defsym codes wpath))
        def-sym-qual (if def-sym-qual def-sym-qual (symbol (str "Unknown_cpath_head" (first wpath))))]
    (into [] (concat [def-sym-qual] subdef-path))))

(defn x-qual-at-cursor [box]
  "The object at the box's cursor, qualified if possible. Not just codeboxes.
   Tries to return a valid result even if the code is broken (TODO: it will try to qualify local symbols)."
  (let [ns-sym (generic-ns-sym box)
        txt (real-string box) ix (cursor-to-real-string box)
        thingy (cbase/x-at-stringlang txt ix (:langkwd box))]
    (if (symbol? thingy) (if-let [xq (langs/resolved ns-sym thingy)] xq thingy) thingy)))

;;;;;;;;;;;;;;;;;;;;; other child UI functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn expandable? [mouse-evt box]
  "Test whether we are expandable first!"
  (let [cur-ix (rtext/cursor-pixel-to-ix box (:X mouse-evt) (:Y mouse-evt))
        ixs (contain-ixs box cur-ix) n (apply + (mapv #(count (:text %)) (:pieces box)))]
    (or (> (first ixs) 0) (< (second ixs) n))))

(defn expand-child [mouse-evt box]
  "Hilights a section of code. Since we don't really have children we return the unmodified parent.
   Not to be confused with code-unfolding."
  (let [cur-ix (rtext/cursor-pixel-to-ix box (:X mouse-evt) (:Y mouse-evt))
        windowed-box (code-fold-toggle-at-cursor (assoc box :cursor-ix cur-ix) true true)]
    (if (not= (real-string windowed-box) (real-string box)) (throw (Exception. "Bug in code-fold-toggle-at-cursor"))) ; check.
    [box windowed-box]))

(defn contract-child [box child]
  "Since expanding children doesn't really create a child, we don't need to modify the parent."
  (if (not= (real-string box) (real-string child))
    (throw (Exception. "Disagreement detected between larger and narrower code views of the same file. Coding bug somewhere..."))))

(defn apply-edits-to-real-string [box edits]
  "Applies the edits to the real-string, dragging the cursor along.
   Order of edits matters."
  (if (= (count edits) 0) box
    (let [map2vec (fn [m] (mapv #(get m % {:text ""}) (range (inc (apply max (keys m))))))
          piecesu (splay-out+ box)
          piecesu (mapv #(if (:children %) (assoc % :text "") %) piecesu) ; real string 1:1 with rendered string.
          piecesu (mapv #(dissoc % :children) piecesu) ; children will be re-added.
          piecesu (mapv #(assoc % :tmp-path1 (:tmp-path %)) piecesu)
          shadow-box (reduce #(rtext/dispatch-edit-event %1 %2) (assoc box :pieces piecesu :head "" :foot "") edits)
          piecesu1 (splay-out+ shadow-box)
          piecesu1 (mapv #(dissoc (assoc % :tmp-path (:tmp-path1 %)) :tmp-path1) piecesu1)
          
          box1 (reduce #(assoc-in %1 (:tmp-path %2) (dissoc %2 :tmp-path)) (dissoc box :pieces) 
                  (subvec piecesu1 1 (dec (count piecesu1))))
          box1 (update box1 :pieces map2vec)
          box1 (walk/postwalk #(if (:children %) (update (assoc % :text ...) :children map2vec) %) box1)
          box1 (assoc box1 :head (:text (first piecesu1)) :foot (:text (last piecesu1)))
          pr #(vector (:tmp-path %) (count (:text %)))]
    #_(println "paths:" (mapv pr piecesu) "|" (mapv pr piecesu1) "|"
      (mapv pr (splay-out+ box1)))
    #_(println "Real-str edit test:" 
        "Edits needed:" (pr-str edits)
        "Should = needed (shadow test):" (pr-str (stringdiff/edits-between (real-string box) (real-string shadow-box)))
        "Should = needed (piecesu1 test):" (pr-str (stringdiff/edits-between (real-string box) (apply str (mapv :text piecesu1))))
        "Should = needed (box1 test):" (pr-str (stringdiff/edits-between (real-string box) (real-string box1))))
      (generic-update box box1)))) ;lazy way out.

;;;;;;;;;;;;;;;;;;;;;;;; Compiling interaction events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def dispatch 
  {:mousePressed mouse-press
     :keyPressed key-press
     :keyReleased rtext/key-release
     :mouseDragged rtext/mouse-drag
     :mouseWheelMoved rtext/mouse-wheel})

(defn interact-fns []
  {:dispatch dispatch
   :render (fn [box & show-cursor?] 
             (let [head (get box :head "") foot (get box :foot "")
                   title (str (fbrowser/devec-file (:path box))
                           (if (> (count (str head foot)) 0)
                             (str " (" (count head) ":-" (count foot) ")") ""))]
               (apply rtext/render (assoc box :path title) show-cursor?)))
   :expandable? expandable?
   :expand-child expand-child :contract-child contract-child
   :is-child? (fn [box] false)}) 
