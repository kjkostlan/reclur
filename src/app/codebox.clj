; View and edit code. Includes code folding as well as code expansion to children.
; Note: export-markers pasted in identical to copying.
(ns app.codebox
 (:require [clojure.string :as string]
   [coder.rcode :as rcode]
   [app.rtext :as rtext]
   [app.fbrowser :as fbrowser]
   [coder.lang.clojure :as clojurelang]
   [app.colorful :as colorful]
   [javac.file :as jfile]
   [clojure.string :as string]
   [app.stringdiff :as stringdiff]))

; The global rtext contains a language protocol in :lang that is used for text coloring and 
; contraction/expansion, etc.
; There isnt much of a tree, the exported id are in order and there is only one element
; which is the code itself's real string sans exported children.

(declare interact-fns) ; Possible dependency cycle with the new function being used by some interact fns.

(def spacer \u26ac) ; a circle for code folding.

(defn _merge-leaf-pieces [pieces] ; keeps the total piece count down.
  (let [pieces (into [] pieces) n (count pieces)
        squishy?s (mapv #(and (not (:children %)) (not (:export-marker %))) pieces)]
    (filterv #(not= % {:text ""})
      (loop [acc [] ix 0 piece {:text []}]
        (if (= ix n) (conj acc (update piece :text #(apply str %)))
          (let [pi (nth pieces ix) si? (nth squishy?s ix)]
            (recur (if si? acc (conj acc (update piece :text #(apply str %)) pi))
              (inc ix) (if si? (update piece :text #(conj % (:text pi))) {:text []}))))))))
(defn _rml [pieces] 
  (rtext/remove-empty (_merge-leaf-pieces pieces)))

;;;;;;;;;;;;;;;;;;;;;;; The clojure language ;;;;;;;;;;;;;;;;;;;

(defmacro with-lang-reload [body kwd-code]
  `(try ~body (catch Exception e# (do (reload-lang ~kwd-code) ~body))))

(defn _get-lang [kwd]
  "Switchyard for languages."
  (cond (not (keyword? kwd)) (throw (Exception. "get-lang must be given a keyword such as :clojure :java, etc"))
  (= kwd :clojure) (clojurelang/clang)
  (= kwd :java) (throw (Exception. "TODO: java support"))
  (= kwd :python) (throw (Exception. "TODO: python support"))
  (= kwd :matlab) (throw (Exception. "TODO: matlab support"))
  (= kwd :haskell) (throw (Exception. "TODO: haskell support (no, we won't be an enemy of haskell's type system)."))
  (= kwd :c) (throw (Exception. "TODO: C support (C++ may be too complex to reasonably support)."))
  :else (throw (Exception. (str "Lang not supported now or planned to in near future:" kwd)))))

(defn reload-lang [kwd]
  (println "Reload request for:" kwd)
  (cond (not (keyword? kwd)) (throw (Exception. "reload-lang must be given a keyword such as :clojure :java, etc"))
  (= kwd :clojure) (require '[coder.lang.clojure :as clojurelang])
  :else (throw (Exception. (str "Not supported yet: " kwd)))))

(defn tokenize [txt lang-kwd]
  (with-lang-reload 
    (let [pipeline (rcode/fn-pipeline (_get-lang lang-kwd) identity)]
      (-> txt ((first pipeline)) ((second pipeline)))) lang-kwd))

(defn vis-levels [tokens]
  "Visual levels of the char, which include () 
   tokens as being the same level as the stuff inside."
  (let [n (count tokens) types (mapv :type tokens)
        strs (mapv #(apply str (:strings %)) tokens)]
    (loop [levels [] ix 0 lev 0]
      (if (= ix n) (into [] (apply concat levels))
        (let [ty (nth types ix) k (count (nth strs ix)) 
              r #(repeat k %) ix1 (inc ix)]
          (cond (= ty 4) (recur (conj levels (r (inc lev))) ix1 (inc lev))
            (= ty 5) (recur (conj levels (r lev)) ix1 (dec lev))
            :else (recur (conj levels (r lev)) ix1 lev)))))))

(defn vis-sp?s [tokens]
  (let [out (mapv #(repeat (count (apply str (:strings %))) (= (:type %) 0)) tokens)]
    (into [] (apply concat out))))

;;;;;;;;;;;;;;;;;;;;;;; Updating the precomputation ;;;;;;;;;;;;;;;;;;;

(defn inter-levels [tokens]
  "Indentation at cursor locations, interstitial to the text.
   The array is one longer than the length of the total rendered string.
   For ()-like tokens longer than 1 char, the level doesn't increase partway inside."
  (let [n (count tokens) types (mapv :type tokens)
        strs (mapv #(apply str (:strings %)) tokens)]
    (loop [levels [] ix 0 lev 0]
      (if (= ix n) (conj (into [] (apply concat levels)) 1)
        (let [ty (nth types ix) k (count (nth strs ix)) 
              r #(repeat k %) ix1 (inc ix)]
          (cond (= ty 4) (recur (conj levels (r lev)) ix1 (inc lev))
            (= ty 5) (recur (conj levels (r lev)) ix1 (dec lev))
            :else (recur (conj levels (r lev)) ix1 lev)))))))

(defn _tapply-edit [tokens edit lang] ; some edits don't do anything.
  (let [ty (:type edit)]
    (if (or (= ty :copy) (= ty :save) (= ty :ignore) (= ty :arrow) (= ty :select-all)) tokens ; nothing.
      (rcode/parse-string-diff tokens (:ix0 edit) (:ix1 edit) 
        (rtext/inserted-string (:value edit)) lang))))
(defn update-precompute [box edit]
  ; Updates the tokens. A nil edit means calculate tokens from scratch.
  (let [lk (:lang box)]
    (with-lang-reload 
      (let [ts1 (if edit (_tapply-edit (:tokens (:precompute box)) edit (_get-lang lk)) 
                  (rcode/parse-string (rtext/rendered-string box) (_get-lang lk)))
            levs (vis-levels ts1) sp?s (vis-sp?s ts1)]
        ;(if edit (println "edit: " edit "counts: " (rtext/rendered-string box)
        ;           (apply str (mapv rcode/_tok2str ts1))))
        (assoc box :precompute {:tokens ts1 :levels levs :sp?s sp?s})) lk)))

(defn complete-update [box box1 edits]
  (rtext/cursor-scroll-update box
    (if (< (count edits) 50) (reduce update-precompute box1 edits)
      (update-precompute (update box1 :pieces _rml) nil))
    edits))

(defn generic-update [box box1]
  "Updates the precompute, making the best attempt to drag the cursor along.
   If the edits are easy to compute by hand it's better do that instead."
  (let [edits (stringdiff/edits-between (rtext/rendered-string box) (rtext/rendered-string box1))]
    (complete-update box box1 edits)))

;;;;;;;;;;;;;;;;;;;;;;; Piece-handling and paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn folded? [piece]
  "true for folded and not exported to a child."
 (boolean (:children piece)))

(defn exported? [piece]
  "Whether the children are exported."
  (boolean (:export-marker piece)))

(defn _splay-out [cljpath0 pieces] ; list of all paths that go to pieces, in reading order. 
  (into [] (apply concat (mapv #(concat [(conj cljpath0 %2)] (if (:children %1) (_splay-out (conj cljpath0 %2 :children) (:children %1)) [])) pieces (range)))))
(defn splay-out [box] (_splay-out [:pieces] (:pieces box)))

(defn uspaths-with-export [box]
  "all paths with an :export-marker."
  (filterv #(:export-marker (get-in box %)) (splay-out box)))

(defn child-paths [box]
  (zipmap (mapv #(:export-marker (get-in box %)) (uspaths-with-export box))
     (map #(conj (:path box) %) (range))))

(defn real-str-ixs [box all-uspaths]
  "Array of [ix0 ix1] for each real-string ix.
   Doesn't include any exported paths but does include folded parents ixs (which don't contribute to the real-string)."
  (let [n (count all-uspaths)]
    (loop [acc [] ix 0 lastix 0]
      (if (= ix n) (conj acc [lastix ix])
        (if (exported? (get-in box (nth all-uspaths ix)))
              (recur (conj acc [lastix ix]) (inc ix) (inc ix))
              (recur acc (inc ix) lastix))))))

(defn real-strings [box]
  "Real-string but split into groups that aren't spacers."
  (let [uspaths (splay-out box)
        real-str-contrib (mapv #(let [p (get-in box %)] (if (or (folded? p) (exported? p)) "" (:text p))) uspaths)
        rs-ixs (real-str-ixs box uspaths)]
    (if (not (:pieces box)) (throw (Exception. "Nil pieces")))
    (mapv #(apply str (subvec real-str-contrib (first %) (second %))) rs-ixs)))

(defn contain-ixs [box cur-ix] 
 "Indexes on the rendered str that contain the index
  grab the string using an inclusive, exclusive pattern to select the internal region and the brackets. 
  rtext/cursor-ix-to-piece can get the piece index.
  Used for cold-folding, etc."
  (let [tokens (:tokens (:precompute box)) ; all tokens mashed together. 
        levels (inter-levels tokens)
        n (dec (count levels)) cur-ix (max 0 (min cur-ix n))
        lev (nth levels cur-ix)
        dbgStrs (mapv #(apply str (:strings %)) tokens)
        lo (loop [ix cur-ix]
             (if (or (= ix 0) (< (nth levels ix) lev)) ix
               (recur (dec ix))))
        hi (loop [ix cur-ix]
             (if (or (= ix n) (< (nth levels ix) lev)) ix
               (recur (inc ix))))]
    [lo hi]))

(defn num-newlines [piece]
  "Uses precomputed values here for folded or exported pieces, otherwise uses the string."
  (if (or (folded? piece) (exported? piece)) (:hidden-nlines piece)
    (dec (count (string/split (:text piece) #"\n")))))    

(defn colorize [box s piece-ix char-ix0 char-ix1]
  "levels and sp?s are one element per character. 
    non-white chars in sp?s are rendered the same as comments."
  (let [levels (subvec (:levels (:precompute box)) char-ix0 char-ix1)
              piece-ix (subvec piece-ix char-ix0 char-ix1)
              exported?s (mapv exported? (:pieces box))
              mx (apply max 0 levels) cols (mapv #(conj (colorful/level2rgb %) 1) (range (inc mx)))]
    (mapv #(if (nth exported?s %2) [1 1 1 0.3] (nth cols (if (>= %1 0) %1 0))) levels piece-ix)))

(defn new-codebox []
  (assoc rtext/empty-text :interact-fns (interact-fns) :outline-color [0.3 0.5 0.9 1] :path []
    :type :codebox :lang :clojure :precompute {:tokens [] :levels []} :colorize-fn (fn [& args] (apply colorize args))))

(defn code-fold-toggle [cur-pieceix folding? ixs export-marker box]
  "Both (un)exporting and code (un)folding."
  (let [ndots 3 pieces (_rml (:pieces box))

        ; Equivalent edit (needed for updating the tokens incrementally instead of recomputing everything):
        insert (if folding? (apply str (repeat ndots spacer)) (apply str (mapv :text (:children (nth pieces cur-pieceix)))))
        edit {:type :misc :ix0 (first ixs) :ix1 (second ixs) :value insert}

        stats (rtext/index-stats (:pieces box) (first ixs) (second ixs) rtext/default-partial-grab)
       
        boxy1 #(assoc box :pieces (_rml (into [] %)))
        num-nlines (if folding? (apply + (mapv num-newlines (:in stats))))
        folded-piece (if folding? {:text insert :children (:in stats) :hidden-nlines num-nlines})
        cud #(complete-update box %1 [edit])]
   (cond (and export-marker folding?) ; fold it up, but put it it into the exported child.
     (let [box1 (boxy1 (concat (:b4 stats) [(dissoc (assoc folded-piece :export-marker export-marker) :children)] (:afr stats)))
           child (update-precompute (assoc (new-codebox) :pieces (:children folded-piece) :path (get (child-paths box1) export-marker)) nil)
           ; Calculate total line nums before:
           box1p (splay-out box1)
           n-linesb4 (loop [acc 0 ix 0] 
                       (let [piece (get-in box1 (nth box1p ix))]
                         (cond (and (exported? piece) (= (:export-marker piece) export-marker)) acc
                           (not (folded? piece)) ; only count leaves, as we are looping through the splayed-out paths.
                           (recur (+ acc (num-newlines piece)) (inc ix))
                           :else (recur acc (inc ix)))))
           child (assoc child :line-num-start (inc n-linesb4))]
       [(cud box1) child])
     folding?
     (cud (boxy1 (concat (:b4 stats) [folded-piece] (:afr stats))))
     export-marker
     (throw (Exception. "Bad case, can't unfold with the release of a child"))
     :else (cud (boxy1 (concat (subvec pieces 0 cur-pieceix) (:children (nth pieces cur-pieceix)) (subvec pieces (inc cur-pieceix))))))))

(defn code-fold-toggle-at-cursor [cur-ix folding? export-marker box]
  "Fold up the outer-level paren level of the code, or unfold folded code.
   Folded code splits the :pieces, unfolded code merges the :pieces.
   export-marker non-nil: give an output of [parent child] for folding true."
  (let [pieces (_rml (:pieces box))
        ; The cursor may be between two pieces:
        cur-pieceix0 (first (rtext/cursor-ix-to-piece (assoc box :cursor-ix cur-ix :pieces pieces)))
        cur-pieceix1 (first (rtext/cursor-ix-to-piece (assoc box :cursor-ix (inc cur-ix) :pieces pieces)))

        folded?0 (folded? (nth (:pieces box) cur-pieceix0))
        folded?1 (folded? (nth (:pieces box) cur-pieceix1))

        cur-pieceix (if (or (and folding? (not folded?0)) (and (not folding?) folded?0)) cur-pieceix0 cur-pieceix1)

        ; Cursor ixs to base the edit on and to divide up the pieces for folding:
        ixs (if folding? (let [ix0s (contain-ixs box cur-ix)] ; contain-ixs include the ().
                           (if (> (second ix0s) (+ (first ix0s) 2)) [(inc (first ix0s)) (dec (second ix0s))] ix0s))
              (let [cur-ix-piece-begin (rtext/cursor-piece-to-ix box cur-pieceix)]
                [cur-ix-piece-begin (+ cur-ix-piece-begin (count (:text (nth pieces cur-pieceix))))]))]
    (code-fold-toggle cur-pieceix folding? ixs export-marker box)))

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
  (let [ndots 3 pieces0 (:pieces box) txt (apply str (repeat ndots spacer))
        ooo? #(or (folded? %) (exported? %))
        pieces1 (mapv #(if (ooo? %) (assoc % :text txt) %) pieces0)]
    (if (= pieces0 pieces1) box
      (let [ccf (fn [p0 p1 jx0] jx0) cix (:cursor-ix box)
            edit (diff-edit (apply str (mapv :text pieces0)) (apply str (mapv :text pieces1)))]
        ; the spacers count as an e either way, so don't factor into the edit we use:
        (update-precompute
          (assoc box :pieces (mapv (fn [p] (if (ooo? p) p (update p :text #(string/replace % (str spacer) "e")))) pieces1)
            :cursor-ix (rtext/carry-cursor pieces0 pieces1 cix ccf)) edit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Other ;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;; string -> tokens -> either vis-levels or inter-levels.

(defn from-text [txt lang-kwd]
  "Sets up a text-editor from a given text and language keyword."
  (update-precompute (assoc (new-codebox) :pieces [{:text txt}]
                   :lang lang-kwd) nil))

(defn load-from-file [fname]
  (let [txt (jfile/open fname)]
    (if (not txt) (throw (Exception. (str  "Attempted to load non-existant file: " fname))))
    ; The entire fname goes into one path:
    (assoc (from-text txt :clojure) :path [fname])))

;;;;;;;;;;;;;;;;;;;;;;; Line number updating ;;;;;;;;;;;;;;;;;;;;;;;

(defn update-lineno-info [box num-lines-b4-each-realstring] "Updates the info that is used rendertime to calc line-nos."
  ; TODO: refactor where we get num-lines-in-each-export rather than num-lines-b4-each-realstring, easier to understand code.
  (let [piecesu-paths (splay-out box) nu (count piecesu-paths)
        n-b4su (loop [acc [] ix 0 n-b4 (first num-lines-b4-each-realstring) pix 0]
                 (if (= ix nu) acc
                   (let [piece (get-in box (nth piecesu-paths ix))
                         export? (exported? piece) leaf? (and (not export?) (not (folded? piece)))
                         next-nb4 (cond export? (nth num-lines-b4-each-realstring (inc pix))
                                   leaf? (+ n-b4 (num-newlines piece)) :else n-b4)
                         next-pix (if export? (inc pix) pix)]
                     (recur (conj acc n-b4) (inc ix) next-nb4 next-pix))))
        
        pieces (:pieces box)  n (count pieces)
        line0 (first num-lines-b4-each-realstring) path2nb4 (zipmap piecesu-paths n-b4su)
        n-total (let [last-piece (get-in box (last piecesu-paths))] ; # before a hypothetical empty piece added to the end.
                  (if (exported? last-piece) (last num-lines-b4-each-realstring)
                    (+ (last n-b4su) (num-newlines (get-in box (last piecesu-paths))))))
        pieces1 (loop [acc [] ix 0]
                  (if (= ix n) acc
                    (let [piece (nth pieces ix)]
                      (if (and (not (folded? piece)) (not (exported? piece)))
                        (recur (conj acc piece) (inc ix))
                        (let [n-b4 (get path2nb4 [:pieces ix])
                              n-b41 (if-let [nb (get path2nb4 [:pieces (inc ix)])] nb n-total)] 
                          (recur (conj acc (assoc piece :hidden-nlines (- n-b41 n-b4))) (inc ix)))))))]
    (assoc box :pieces pieces1 :line-num-start (inc line0))))

;;;;;;;;;;;;;;;;;;;;;;; Interaction functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interactions beyond the uasual rtext interactions.

(defn key-press [key-evt box]
  "Key-based interaction 101. Tab indents (shift-tab dedents) rather than whoops where did the block of code go.
   For now just indent the hilighted lines (and any lines in the children), not consious of the code itself.
   Other fns work normally."
  (let [ed (rtext/key-to-edit box key-evt) bk-sp? (= (str (:value ed) (str \backspace)))
        render-str (rtext/rendered-string box)
        shifting? (:ShiftDown key-evt)]
    (keep-fe-length
    (if (and (= (:type ed) :type) (= (:value ed) "    ")) ; line-by line indent or dedent.
      (let [c-ix (:cursor-ix box)
            c-range (contain-ixs box c-ix)
            xy0 (rtext/cursor-ix-to-ugrid box)
            y0 (second (rtext/cursor-ix-to-ugrid (assoc box :cursor-ix (first c-range))))
	        y1 (second (rtext/cursor-ix-to-ugrid (assoc box :cursor-ix (second c-range))))
	        line-begin-ixs (mapv #(rtext/cursor-ugrid-to-ix box 0 %) (range y0 y1))
            lines (if shifting? (string/split (rtext/rendered-string box) #"\n"))
            n-space (fn [l] (count (re-find #"[ \t]*" l)))
            ; Slow line-by-line, can be improved:
            box1 (reduce 
                   (fn [bx y] 
                    (let [ix0 (rtext/cursor-ugrid-to-ix bx 0 y)
                          ix1 (if shifting? (+ ix0 (min 4 (n-space (nth lines y)))) ix0)]
                      (rtext/edit bx ix0 ix1 (if shifting? "" "    ") [] shifting?)))
                    box (range y0 (inc y1)))
            box2 (assoc box1 :cursor-ix 
                   (rtext/cursor-ugrid-to-ix box1 
                     (max 0 (+ (first xy0) (if shifting? -4 4))) (second xy0)))]
         (update-precompute box2 nil)) ; for now at least recalc everything.
      (rtext/key-press key-evt (update-precompute box ed))))))

(defn mouse-press [m-evt box] ; double click = code folding.
  (if (= (:ClickCount m-evt) 2)
    (let [cur-ix (rtext/cursor-pixel-to-ix box (:X m-evt) (:Y m-evt))
          vis-sp?s (vis-sp?s (:tokens (:precompute box)))
          
          cur-pieceix (first (rtext/cursor-ix-to-piece (assoc box :cursor-ix cur-ix)))
          cur-pieceix1 (first (rtext/cursor-ix-to-piece (assoc box :cursor-ix (inc cur-ix))))
          folding? (and (not (folded? (nth (:pieces box) cur-pieceix)))
                     (not (folded? (nth (:pieces box) cur-pieceix1))))]
      (code-fold-toggle-at-cursor cur-ix folding? nil box)) (rtext/mouse-press m-evt box)))

;;;;;;;;;;;;;;;;;;;;; other child UI functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn expandable? [mouse-evt box]
  "Test whether we are expandable first!"
  (let [cur-ix (rtext/cursor-pixel-to-ix box (:X mouse-evt) (:Y mouse-evt))
        ixs (contain-ixs box cur-ix) n (apply + (mapv #(count (:text %)) (:pieces box)))]
    (or (> (first ixs) 0) (< (second ixs) n))))

(defn expand-child [mouse-evt marker box]
  "Expands a child, when the user clicks on x,y. Works a lot like folding code.
   Returns [modified-parent new-child].
   the modified-parent must store the unique-id in order to retrieve it when we use contract-child.
   Not to be confused with code-unfolding"
   (let [cur-ix (rtext/cursor-pixel-to-ix box (:X mouse-evt) (:Y mouse-evt))]
     (code-fold-toggle-at-cursor cur-ix true marker box)))

(defn contract-child [box child]
  "Contracts a child that has unique-id, returns the modified box.
   Not to be confused with code-folding."
   (let [uspath (nth (uspaths-with-export box) (last (:path child)))
         box1 (update-in box uspath #(assoc (dissoc % :export-marker) :children (:pieces child))) ; no visual changes yet.
         vis-change? (= (count uspath) 2)] ; not bound up in some :children folded up somewhere.
     (if vis-change? (let [cix-land (inc (rtext/cursor-piece-to-ix box1 (second uspath)))]
                       (code-fold-toggle-at-cursor cix-land false nil box1))
       box1)))

(defn unwrapped-tree [box]
   "A 1 element tree: simply our path, with the exported ids attached."
  (let [reals (real-strings box)
        export-markers (mapv #(:export-marker (get-in box %)) (uspaths-with-export box))]
    {(:path box) {:real-string reals :export-marker export-markers}}))

(defn apply-edits-to-real-string [box edits pix]
  "Applies the edits to the pix'th real-string, dragging the cursor along. 
   Note: There are one more real strings than exported children.
   An edit with :ix0 = 0 will edit the beginning of the pix'th string, etc."
  (if (= (count edits) 0) box
  (let [uspaths (splay-out box)
        rsix (real-str-ixs box uspaths)
        uspath-ix0 (first (nth rsix pix)) uspath-ix1 (second (nth rsix pix)) ; inclusive exclusive for pix. Doesn't include the exported cases.
        
        ; Window edits for each piece:
        editss (loop [acc {} uix uspath-ix0 char-ix 0] ; char-ix is the index on the real string, NOT the visible index.
                 (if (= uix uspath-ix1) acc
                   (let [piece (get-in box (nth uspaths uix))]
                     (if (and (not (folded? piece)) (not (exported? piece))) ; folded pieces are ignored, but any unfolded children aren't.
                       (let [char-ix1 (+ char-ix (count (:text piece)))
                             last-piece-to-crunch? (= uix (dec uspath-ix1))
                             edits1 (stringdiff/window-edits edits char-ix char-ix1 last-piece-to-crunch?)]
                         (recur (assoc acc uix edits1) (inc uix) char-ix1))
                       (recur acc (inc uix) char-ix)))))
        box1 (reduce (fn [bx uix]
                       (let [eds (get editss uix)]
                         (update-in bx (conj (nth uspaths uix) :text)
                           #(stringdiff/apply-edits % eds))))
               box (keys editss))]
    (generic-update box box1)))) ;lazy way out.

(defn split-real-string [box pix jx0 jx1]
  "Splits the real string at pix into two pieces, adding a unique :export-marker.
   The stuff between jx0 and jx1 is discarded (it should represent the exported stuff)."
  (let [box (update box :pieces _rml)
        uspaths-all (splay-out box)
        marker (gensym "split") ndots 3
        all-rstixs (real-str-ixs box uspaths-all)
        
        uix0 (first (nth all-rstixs pix)) uix1 (second (nth all-rstixs pix))
        
        uspathsw (subvec uspaths-all uix0 uix1) ; uspaths windowed, windowed = all uspaths that map to pix.
        nu (count uspathsw)

        leaf?s (mapv #(let [p (get-in box %)] (not (or (folded? p) (exported? p)))) uspathsw)
        nreal-charsw (mapv #(let [p (get-in box %1)] (if %2 (count (:text p)) 0)) uspathsw leaf?s)
        
        nreal-chars-b4-eachw (into [] (reductions + 0 nreal-charsw))

        ; for not splitting, we will create a zero-length piece somewhere:
        ix-split0 (first (filter #(and (> (nth nreal-charsw %) 0) (>= (+ (nth nreal-chars-b4-eachw %) (nth nreal-charsw %)) jx0)) (range nu))) 
        ix-split1 (first (filter #(and (> (nth nreal-charsw %) 0) (>= (+ (nth nreal-chars-b4-eachw %) (nth nreal-charsw %)) jx1)) (range nu)))
        
        piecesw0 (mapv #(get-in box %) uspathsw)
        piecesw1 (concat (subvec piecesw0 0 ix-split0)
                   [(update (nth piecesw0 ix-split0) :text
                     #(subs % 0 (- jx0 (nth nreal-chars-b4-eachw ix-split0))))
                    {:export-marker marker :text (apply str (repeat ndots spacer))}
                   (update (nth piecesw0 ix-split1) :text
                     #(subs % (- jx1 (nth nreal-chars-b4-eachw ix-split1))))]
                     (subvec piecesw0 (inc ix-split1)))
        
        nstub (dec (if-let [x (first (filterv (fn [ix] (> (count (apply hash-set (mapv #(get % ix) uspathsw))) 1))
                                       (range (count (first uspathsw)))))] x (count (first uspathsw))))

        p-new (let [p (nth uspathsw ix-split0)] (update p (dec (count p)) inc)) ; same as ix-split0 but with the last indexed upped by 1.
        bump? (fn [p] (let [nu (count p-new)] ; use this bump fn when p is after p-new
                        (and (>= (count p) nu) (= (subvec p 0 (dec nu)) (subvec p-new 0 (dec nu))))))
        bump (let [p1 (nth uspathsw ix-split1)]
               (if (bump? p1) (- (last p-new) (dec (last p1))))) ;only defined if we bump the first after piece, if we don't no pieces are bumped.

        pathsw+ (mapv (fn [p] (if (bump? p) (update p (dec (count p-new)) #(+ % bump)) p)) (subvec uspathsw ix-split1))
        uspathsw1 (concat (subvec uspathsw 0 (inc ix-split0)) [p-new] pathsw+)

        ; Remove the old paths:
        box1 (reduce #(assoc-in %1 %2 {:text ""}) box (subvec uspathsw ix-split1))
        ; Add in the new paths:
        box2 (reduce #(assoc-in %1 (first %2) (second %2)) box1 (mapv vector uspathsw1 piecesw1))]
    ; lazy way out:
    (generic-update box (update box2 :pieces _rml))))

(defn implement-diffs [box diffs]
  (throw (Exception. "Codebox doesn't have a traditional diff system, use ")))

;;;;;;;;;;;;;;;;;;;;;;;; Compiling interaction events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro updaty-fns [code] 
  (let [a1 (gensym 'args)] 
    (zipmap (keys code) (mapv #(list `fn ['& a1] (list `apply % a1)) (vals code)))))
(defn interact-fns [] (updaty-fns
  {:mousePressed mouse-press
   :mouseDragged rtext/mouse-drag
   :keyPressed key-press
   :keyReleased rtext/key-release
   :mouseWheelMoved rtext/mouse-wheel
   :everyFrame (fn [_ box] box)
   :render rtext/render
   :mouseMoved (fn [_ box] box)
   :expandable? expandable?
   :expand-child expand-child :contract-child contract-child
   :is-child? (fn [box] (> (count (:path box)) 1))
   :unwrapped-tree unwrapped-tree :implement-diffs implement-diffs})) 