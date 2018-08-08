; Browse files and folders with a text-editor interface.
; :path is always treated as a vector to the file, even if just given the string.
(ns app.fbrowser
 (:require [javac.file :as jfile]
   [app.rtext :as rtext]
   [clojure.string :as string]
   [app.stringdiff :as stringdiff]
   [coder.plurality :as plurality]))

; Each :piece of the our app.gaui.rtext contains:
 ; :text = indentation, an arrow for folders, the filename itselfm and a newline
 ; :fullname0 = original name loaded from disk or last name saved to disk. false for new filders.
 ; :folder? = we show folders differently. Trying to make a file a child of a file will TODO.
 ; :children (if not expanded). If we are expanded we infer the hierarchy from the indentation.
 ; :exported? = are we exported.

;;;;;;;;;;;;;;;;;;;;;;;;;;; Other ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare interact-fns) ; Possible dependency cycle with the new function being used by some interact fns.

; spacers at the beginning:
(def folder-closed \u25b9) ; a triangle that points right.
(def folder-open \u25bc) ; a triangle that points down.
(def spacer \u26ac) ; a circle and how the number of levels are coded.

(defn rm-decor [line]
  (-> line (string/replace (re-pattern (str folder-closed)) "")
    (string/replace (re-pattern (str folder-open)) "")
    (string/replace (re-pattern (str spacer)) "")
    (string/replace (re-pattern "\n") "")))

(defn foldermark? [char-or-1-char-str] 
  "the open or closed char tells us whether it is a folder or not."
  (or (= (str char-or-1-char-str) (str folder-closed)) (= (str char-or-1-char-str) (str folder-open))))

(defn folder? [line]
  (or (.contains ^String (:text line) ^String (str folder-closed))
    (.contains ^String (:text line) ^String (str folder-open))))

(defn devec-file [v] 
  (if (string? v) v (apply str (interpose "/" (mapv str v)))))

(defn vec-file [s]
  (if (sequential? s) s (string/split s #"/")))

(defn pixel-to-line [box x y]
  "There is always one line per piece."
  (second (rtext/cursor-pixel-to-grid box x y)))

(defn level-of [line]
  "How many indent levels are we in?"
  (let [s (if (string? line) line (:text line))] (count (filterv #(= % spacer) s))))

(defn set-level [line lev]
  "The level is encoded in the :text itself."
  (assoc line :text (str (apply str (repeat lev spacer)) (string/replace (:text line) (str spacer) ""))))

(defn shift-levels [lines delta]
  (mapv (fn [line] (if (:children line) (update line :children #(shift-levels % delta)) line))
    (mapv #(set-level % (+ (level-of %) delta)) lines)))

(defn str-assoc [s ix c]
  "pretend a string is a vector. Still O(n), so don't have filenames millions of chars long."
  (str (subs s 0 ix) c (subs s (inc ix))))

(defn expanded? [line]
  "Children expanded out into some other component DONT count."
  (and (folder? line) (not (:exported? line)) #_(not (:children line))
    (.contains ^String (:text line) ^String (str folder-open))))

(defn exported? [line] (:exported? line))

(defn _cursor-advect [box new-lines lix old-cursor]
  (let [cij (rtext/cursor-ix-to-piece (assoc box :cursor-ix (inc old-cursor)))
        lines (:pieces box) 
        clix (first cij) jx (second cij) del (- (count new-lines) (count lines))
        box1 (assoc box :pieces new-lines :cursor-ix (inc old-cursor))]
    (if (> clix lix) ; move the cursor function.
      (if (> del 0) (dec (+ (rtext/cursor-piece-to-ix box1 (+ clix del)) jx)) ; expand
        (let [clix1 (max (+ clix del) lix) ; contract
              jx1 (if (< (+ clix del) lix) (dec (count (nth lines lix))) (dec jx))]
          (+ (rtext/cursor-piece-to-ix box1 clix1) jx1))) old-cursor)))
(defn _folder-open-toggle [box lix]
  ; TODO: figure out how deep folders are handled.
  (let [lines (:pieces box) 
        line (nth lines lix) level (level-of line) txt (:text line)
        ; add 1 to the cursor so that newline is like treated as bieng on the next piece.
        new-lines (cond (expanded? (nth (:pieces box) lix)) ; Contract all children, but do not contract grandchildren into children.
                    (let [lines-b4 (subvec lines 0 lix) lines1 (conj lines "")
                          first-notchix (first (filter #(<= (level-of (nth lines1 %)) level) (range (inc lix) (count lines1))))
                          lines-mid (subvec lines (inc lix) first-notchix)]
                       (into [] (concat lines-b4 [(assoc line :children lines-mid :text (str-assoc txt level folder-closed))] 
                                  (subvec lines first-notchix))))
                     (not (exported? (nth (:pieces box) lix))) ; Closed with internal children. Expand these children and remove the :children.
                     (into [] (concat (subvec lines 0 lix) [(assoc (dissoc line :children) :text (str-assoc txt level folder-open))]
                                (:children line) (subvec lines (inc lix)))) ; :children are already indented more than the parent.
                     :else lines)
        cur-fn #(_cursor-advect box new-lines lix %)]
    (-> box (assoc :pieces new-lines) (update :cursor-ix cur-fn)
      (update :selection-start cur-fn) (update :selection-end cur-fn))))

(defn clear-empty-files [box]
  "Clears files with no filename text (i.e. just the newline and indentation/ the folder arrow mark). 
   Eventually this will integrate in some way with some form of 'are your sure you want to delete'?.
   Subfilders are deleted iff the folder is compact."
  (let [keep-line?s (mapv #(>= (count (:text %)) (+ 2 (if (folder? %) 1 0))) (:pieces box))]
    (update (assoc box :pieces (reduce #(if (first %2) (conj %1 (second %2)) %1) [] (mapv vector keep-line?s (:pieces box))))
     :cursor-ix (fn [cix] (rtext/carry-cursor (:pieces box) (mapv #(if %1 %2 0) keep-line?s (:pieces box)) cix (fn [old new ix] ix))))))

;;;;;;;;;;;;;;;;;;;;; Customization functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn colorize-file-vs-folder [box s piece-ix char-ix0 char-ix1]
  "File = white, folder = yellow. Exported folder = a little darker. Bad levels trigger red/brown coloring."
  (let [pieces (:pieces box)
        folder?s (mapv folder? pieces)
        levels (mapv level-of pieces)
        expanded?s (mapv expanded? pieces)
        exported?s (mapv exported? pieces)
        ; Allowed depth changes:
        ; Any negative value (I think so at least).
        ; +1 if the folder is expanded. 
        bad-level?s (mapv (fn [l l0 e0?] (not (or (<= l l0) (and (= l (inc l0)) e0?)))) 
                      levels (concat [0] levels) (concat [false] expanded?s)) ; concat [0] means previous. 
        color-per-piece (mapv (fn [f b e] (mapv * (if f [1 1 0.4 1] [1 1 1 1]) (if b [0.8 0 0 1] [1 1 1 1]) (if e [1 0.7 1 1] [1 1 1 1]))) 
                          folder?s bad-level?s exported?s)
        fioe (mapv #(nth color-per-piece %) piece-ix)]
   (mapv (fn [col cr] (if (= cr spacer) (assoc col 3 0.5) col)) fioe s)))

(defn valid-str [is-folder? is-expanded? str-want]
  "Makes sure the string is a valid format."
 (let [level (count (filter #(= % spacer) str-want)) ; level = total # of spacers in str-want.
       str-core (apply str (filter #(not (or (= % spacer) (= % \newline) (= % folder-closed) (= % folder-open))) str-want))]
   (str (apply str (repeat level spacer)) 
     (cond (and is-folder? is-expanded?) folder-open is-folder? folder-closed :else "") str-core \newline)))

(defn insert-fn [box x stats str?]
  "Used for both pasting and regular typing. Can be a string or vector of pieces."
  (let [store-tmps (fn [p] ; we shuffle the array around. Store temp variables to ensure a valid state after typing.
                     (assoc p :tmp-folder? (folder? p) :tmp-expand? (expanded? p)))
        clear-tmps (fn [p] (dissoc p :tmp-folder? :tmp-expand?))
        box1 (update box :pieces #(mapv store-tmps %))
        box2 (if str? (rtext/default-insert box1 x stats str?) ; this rearranges stuff!
               (let [p0 (:piece-0 stats)
                     delta-level (- (level-of (nth (:pieces box) p0)) (level-of (first x)))
                     x1 (shift-levels x delta-level)
                     box-after-deletion (rtext/default-insert box1 "" stats true)
                     cursor-delta (dec (apply + (- (count (:text (nth (:pieces box) p0))) (:jx+1 stats)) (mapv #(count (:text %)) x)))]
                 (update (update box-after-deletion :cursor-ix #(+ % cursor-delta)) :pieces
                   #(into [] (concat (subvec % 0 (inc p0)) x1 (subvec % (inc p0)))))))
        box3 (clear-empty-files box2)
        pieces4 (mapv #(clear-tmps (if (nil? (:tmp-folder? %)) % (assoc % :text (valid-str (:tmp-folder? %) (:tmp-expand? %) (:text %))))) (:pieces box3))]
    (assoc (assoc box3 :pieces pieces4) :cursor-ix ; :cursor-ix also may move.
      (rtext/carry-cursor (:pieces box3) pieces4 (:cursor-ix box3) (fn [old new ix] ix)))))

(defn partial-grab-fn [piece txt ix01] 
  piece ;(if (< (second ix01) (count piece)) piece {:text ""}) ; this would be better but causes other bugs.
  )

(defn delete-fn [box stats]
   (insert-fn box "" stats true))

(defn new-fbrowser [pieces]
  ; leave pieces empty for an empty file browser.
  (assoc rtext/empty-text :outline-color [0.8 0.5 0.3 1] :show-line-nums? false 
    :colorize-fn colorize-file-vs-folder
    :partial-grab-fn partial-grab-fn :insert-fn insert-fn :delete-fn delete-fn
    :type :fbrowser :pieces pieces :path [] ; don't forget to update the path.
    :interact-fns (interact-fns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Disk file and pathing ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn paths-of-lines [box] "subtracts out the global level, doesn't include children."
  (let [lines (:pieces box) n (count lines)
        levels (let [l (mapv level-of lines)] (mapv #(- % (first l)) l))
        _ (if (and (first lines) (not (map? (first lines))))
            (throw (Exception. (str "non-map line, it is a " (type (first lines))))))
        names (mapv #(rm-decor (:text %)) lines) 
        open?s (mapv #(< %1 %2) levels (conj (into [] (rest levels)) -1))
        va #(if (= %2 (count %1)) (conj %1 %3) (assoc %1 %2 %3))]
    (mapv #(into [] (concat (vec-file (:path box)) %))
      (loop [acc [] ix 0 last-opens []]
        (if (= ix n) acc
          (recur (conj acc (conj (subvec last-opens 0 (min (max 0 (nth levels ix)) (count last-opens))) (nth names ix))) (inc ix)
            (if (nth open?s ix) (va last-opens (nth levels ix) (nth names ix)) last-opens)))))))

(defn _all-paths [cljpath0 diskpath0 pieces] ; cljpath0 goes to pieces.
  (let [r (range (count pieces))
        discs+ (paths-of-lines {:path diskpath0 :pieces pieces})
        out1 (zipmap (mapv #(conj cljpath0 %) r) discs+) 
        outrs (mapv #(if-let [ch (:children (nth pieces %1))] 
                       (_all-paths (conj cljpath0 %1 :children) %2 ch) {}) r discs+)]
    (apply merge (concat [out1] outrs))))
(defn all-paths [box]
  "All file paths. Map from path within box (path to a piece) to filename."
  (_all-paths [:pieces] (vec-file (:path box)) (:pieces box)))

(defn exported-paths [box]
  "like all-paths but restricted to only the paths with exported children."
  (let [us2disk (all-paths box)
        us-export-only (filterv identity (mapv #(if (get-in box (conj % :exported?)) %) (keys us2disk)))]
    (zipmap us-export-only (mapv #(get us2disk %) us-export-only))))

(defn _text-to-leaf0 [x level] ; recursively sets :text to the leaf of the :fullname0 based on it's level. All folders start closed.
  (let [ch (:children x)]
    (assoc (if ch (assoc x :children (mapv #(_text-to-leaf0 % (inc level)) ch)) x) 
      :text (str (apply str (repeat level spacer)) (if ch folder-closed "") (jfile/full-to-leaf (:fullname0 x)) "\n"))))

(defn new-file [name] ; no fullname0.
  {:text (str name "\n") :fullname0 false :timestamp 0})

(defn _load-from-folder [foldername-full] 
  "loads the names, doesn't load the files themselves."
  (let [filders (sort (jfile/visible-children foldername-full false)) nf (count filders)
        arbor {:fullname0 foldername-full}] ; children start off invisible.
    (assoc arbor :children ; everything starts out unexpanded.
      (mapv #(let [ffull (str foldername-full (jfile/sep) %)] ; this filder converted to a full-path.
               (if (jfile/dir? ffull) (_load-from-folder ffull)
                 {:fullname0 ffull})) filders))))

(defn load-from-folder [foldername-full]
  "All children startout contracted. The last element of foldername-full goes into the root folder holding everything.
   TODO: lazy filesystem that only operates at need, probably manual implementation b/c the mechanics are complex."
  (assoc (new-fbrowser [(_text-to-leaf0 (_load-from-folder foldername-full) 0)]) 
    :path (into [] (butlast (vec-file foldername-full)))))

(defn _recursive-indent [x] ; any :children are also indented.
  (update (if-let [ch (:children x)] (assoc x :children (mapv _recursive-indent ch)) x) 
    :text #(str spacer %)))
(defn _recursive-dedent [x] ; any :children are also dedented. Does nothing if can't dedent.
  (update (if-let [ch (:children x)] (assoc x :children (mapv _recursive-dedent ch)) x) 
    :text #(if (= (first %) spacer) (subs % 1) %)))

(defn recursive-unwrap [folder]
  "Recursively lists and unwraps all files in folder."
  (let [folder (devec-file (if (= folder "") "." folder))]
    (mapv #(devec-file (second %))
                (all-paths (load-from-folder folder)))))

;;;;;;;;;;;;;;;;;;;;;;; Interaction functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Interactions beyond the usual rtext interactions.

(defn non-folder-file-click? [mouse-evt box]
  (let [lix (pixel-to-line box (:X mouse-evt) (:Y mouse-evt))
        piece (get (:pieces box) lix)]
    (and piece (not (expanded? piece)) (not (exported? piece)))))

(defn fullfile-click [mouse-evt box]
  (let [lix (pixel-to-line box (:X mouse-evt) (:Y mouse-evt))]
    (if-let [fvec (get (paths-of-lines box) lix)] (devec-file fvec))))

(defn folder-open-toggle [mouse-evt box]
  "Standard folder opening toggle, will do nothing when applied to a file.
   This is different from expand-child and contract-child because it doesn't create other components."
 (let [x (:X mouse-evt) y (:Y mouse-evt)
       ln (pixel-to-line box x y)]
   (if (and (>= ln 0) (< ln (count (:pieces box))) (folder? (nth (:pieces box) ln)))
    (_folder-open-toggle box ln) box)))
    
(defn selection-snap [box]
  "Makes sure the selection selects text within a file or whole files."
  (let [sel-start (:selection-start box) sel-end (:selection-end box)]
    (if (and sel-start sel-end (> sel-end sel-start))
      (let [p-s0 (rtext/cursor-ix-to-piece (assoc box :cursor-ix (inc sel-start))) ; [piece ix, loc within piece.]
            p-s1 (rtext/cursor-ix-to-piece (assoc box :cursor-ix sel-end)) ; round left for this.
            ix0 (first p-s0) jx0 (dec (second p-s0))
            ix1 (first p-s1) jx1 (second p-s1)
            n (count (:text (nth (:pieces box) ix0)))]
        (cond 
          (and (= ix0 ix1) (= jx0 0) (>= jx1 (dec n))) box
          (= ix0 ix1)
          (let [n0 (level-of (nth (:pieces box) ix0))
                start1 (+ sel-start (cond (< jx0 n0) (- n0 jx0) (= jx0 n) -1 :else 0))
                end1 (+ sel-end (cond (< jx1 n0) (- n0 jx1) (= jx1 n) -1 :else 0))]
            (-> box (assoc :selection-start start1)
              (assoc :selection-end end1)
              (update :cursor-ix #(if (< % start1) start1 end1))))
          :else
          (let [n1 (count (:text (nth (:pieces box) ix1)))
                start1 (rtext/cursor-piece-to-ix box ix0)
                end1 (+ 1 (dec n1) (rtext/cursor-piece-to-ix box ix1))]
            (-> box (assoc :selection-start start1) (assoc :selection-end end1)
             (update :cursor-ix #(if (< % start1) start1 end1))))))
      box)))

(defn mouse-press [m-evt box] ; did we click on the arrow? If so folder-open-toggle.
  (let [c-ix (rtext/pixel-to-selected-char-ix box (:X m-evt) (:Y m-evt))
        r-str (rtext/rendered-string box) ch (get r-str c-ix)]
    (if (or (= ch folder-open) (= ch folder-closed))
      (folder-open-toggle m-evt box)
      (rtext/mouse-press m-evt box))))

(defn key-press [key-evt box]
  "Key-based interaction 101.
   indent and dedent affect children, enter makes a new filder, other key events are sent to rtext."
  (let [ed (rtext/key-to-edit box key-evt) bk-sp? (= (str (:value ed) (str \backspace)))
        render-str (rtext/rendered-string box)]
    (cond (and (= (:type ed) :type) (= (:value ed) "    ")) ; TAB indent or dedent all selected lines and children thereof.
      (let [indent? (not (:ShiftDown key-evt)) sel-range (rtext/exon box)
            lix0 (rtext/cursor-ix-to-lix (assoc box :cursor-ix (first sel-range)))
            lix1 (rtext/cursor-ix-to-lix (assoc box :cursor-ix (second sel-range)))
            xdent (if indent? _recursive-indent _recursive-dedent) lines (:pieces box)
            new-lines (into [] (concat (subvec lines 0 lix0) (mapv xdent (subvec lines lix0 (inc lix1))) (subvec lines (inc lix1))))
            new-cix (fn [cix] (rtext/carry-cursor (:pieces box) new-lines cix
                                (fn [old new jx] ; jx bieng the length of the piece => beginning of next line => don't move if dedent.
                                  (if (and (not indent?) (= jx (count (:text old)))) jx (+ jx (if indent? 1 -1))))))]
        (-> box (assoc :pieces new-lines) (update :cursor-ix new-cix)
          (update :selection-start new-cix) (update :selection-end new-cix)))
     (= (:KeyCode key-evt) 10) ; ENTER add a newline below a cursor, shift to make a folder.
     (let [lix (rtext/cursor-ix-to-lix box) folder? (:ShiftDown key-evt)
           lev (level-of (nth (:pieces box) lix))
           nm (if folder? "newFolder" "newFile")
           new-fio (update (new-file nm) :text
                     #(str (apply str (repeat lev spacer)) (if folder? folder-open "") %))
           pieces (into [] (concat (subvec (:pieces box) 0 (inc lix)) [new-fio] (subvec (:pieces box) (inc lix))))
           box1 (assoc box :pieces pieces)]
       (assoc box1 :selection-start 0 :selection-end 0
         :cursor-ix (+ (rtext/cursor-piece-to-ix box1 (+ lix 1)) lev (if folder? 1 0) (count nm))))
     :else 
       (clear-empty-files (rtext/key-press key-evt box))))) ; normal keypress but deleting any files.

(defn reset-fullname0s [box]
  ":fullname0 is reset, use this after a disk update."
  (let [us2disk (all-paths box)]
    (reduce #(assoc-in %1 (conj %2 :fullname0) (get us2disk %2)) box (keys us2disk))))

;;;;;;;;;;;;;;;;;;;;; Child UI functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn expandable? [mouse-evt box]
  "Test whether we are expandable first!"
  (let [x (:X mouse-evt) y (:Y mouse-evt)
        lix (pixel-to-line box x y)
        box (if (and (get (:pieces box) lix) (expanded? (get (:pieces box) lix))) 
              (_folder-open-toggle box lix) box)] 
    (and (> (count (:pieces box)) 1) (boolean (:children (get (:pieces box) lix))))))

(defn expand-child [mouse-evt box]
 (let [x (:X mouse-evt) y (:Y mouse-evt)
       lix (pixel-to-line box x y)
       ; make sure the folder is contracted:
       box (if (expanded? (nth (:pieces box) lix)) (_folder-open-toggle box lix) box)
       path (nth (paths-of-lines box) lix)
       line (get-in box [:pieces lix])
       new-children (shift-levels (:children line) (dec (- (level-of line))))]
   [(update-in box [:pieces lix] #(dissoc (assoc % :exported? true) :children))
    (assoc (new-fbrowser new-children) :path (vec-file path))]))

(defn parent? [box child]
  (and (= (:type box) :fbrowser)
       (= (:type child) :fbrowser)
       (let [p0 (vec-file (:path box))
             p1 (vec-file (:path child))]
         (and (> (count p1) (count p0))
              (= (subvec p1 0 (count p0)) p0)))
       (let [us2disk-ex (exported-paths box)
             disk2us-ex (zipmap (vals us2disk-ex) (keys us2disk-ex))]
         (boolean (get disk2us-ex (vec-file (:path child)))))))

(defn contract-child [box child]
  "Returns the modified box."
  (let [;the count prevents an infinite loop when you try to acces an id that isn't there.
        us2disk-ex (exported-paths box)
        disk2us-ex (zipmap (vals us2disk-ex) (keys us2disk-ex))
        ks (get disk2us-ex (vec-file (:path child)))
        _ (if (not ks) (throw (Exception. "Can't find the child path, it may have been deleted in the parent and the child should have gotten deleted.")))
        ch-contract (if (expanded? (first (:pieces child))) (_folder-open-toggle child 0) child)
        ch-pieces (:pieces ch-contract)]
    (update-in box ks #(assoc (dissoc % :exported?)
                              :children (shift-levels ch-pieces (inc (level-of %)))))))

(defn unwrapped-tree [box]
  "Map from disk path to an element, for now the element is simply {:folder? :fullname0}."
  (let [us2disk (all-paths box)
        disk2us (zipmap (vals us2disk) (keys us2disk)) ; dup loss: folders can't share the same name with files either so OK.
        out (zipmap (keys disk2us) 
             (mapv #(let [x (get-in box %) y {:folder? (folder? x) :fullname0 (:fullname0 x)}] 
                      y) (vals disk2us)))]
    out))

(defn vassoc-in [m ks v] ; creates vectors when given a number.
  (let [n (count ks)]
    (reduce 
      (fn [acc i]
        (let [ksi (subvec ks 0 (inc i)) ksi- (butlast ksi) num (if (number? (nth ks i)) (nth ks i))
              num0 (if-let [v0 (get-in acc ksi-)] (if (vector? v0) (count v0) 1e100) -1)]
          ; v only will count for the last element, it is a placeholder that gets overwritten otherwise.
          (cond (and num (< num0 0)) (assoc-in acc ksi- (conj (into [] (repeat num [])) v))
            (and num (>= num num0)) (let [vi (get-in acc ksi-) vi1 (into [] (concat vi (repeat (- num (count vi)) []) [v]))]
                                      (assoc-in acc ksi- vi1))
            (= i (dec n)) (assoc-in acc ks v) :else acc))) 
      m (range (count ks)))))

(defn _inc-last [p] (update p (dec (count p)) inc))
(defn _push-ixs [box usstub]
  ;clears the way for (assoc-in box (_inc-last usstub)), putting a nil here.
  (let [ps0 (get-in box (butlast usstub)) ; vector of stuff.
        ix-to-clear (inc (last usstub))
        ps1 (into [] (concat (subvec ps0 0 ix-to-clear) [{:text :TMP}] (subvec ps0 ix-to-clear)))]
    (assoc-in box (butlast usstub) ps1)))
(defn _paths-to-add [shortest-path-to-add total-num-added]
  ; All the paths to add in a given add diff.
  (mapv #(into [] (apply concat shortest-path-to-add (repeat % [:children 0]))) (range total-num-added)))
(defn _implement-diff [box us2disk disk2us diff] ; Inefficient O(tree-size*accepted-diffs), which may be a problem for changing siblings.
  (let [ty (first diff) diskpath (second diff)]
    (cond
      (= ty :add) ; convert val and add by taking a bunch of closed folders.
        (let [val (nth diff 2) ph (vec-file (:path box))]
          (if (and (= ph (subvec diskpath 0 (min (count ph) (count diskpath)))) 
                (not (get disk2us diskpath))) ; make sure the edit is even relevant.
            ; i.e. stub goes to [:pieces 8 :children 2]
            (let [nstub (last (filter #(get disk2us (subvec diskpath 0 %)) (range (count diskpath))))]
              (if nstub
                (let [stub (subvec diskpath 0 nstub) ; the longest subvec of diskpath we can find.
                      usstub (get disk2us stub) line-stub (get-in box usstub)]
                  (if (exported? line-stub) box ; the change happens to the children, not here.
                    (let [nindent-stub (level-of (get-in box usstub)) ; how many spaces get us to stub level.
                          left-overs-disk (subvec diskpath nstub) 
                          n (count diskpath) n+ (- (count diskpath) (count stub))
                          r (range nstub n)
                          ; add the leaf names at each level beyond the stub:
                          xs-to-add (mapv (fn [ix] (let [folder? (or (< ix (dec n)) (:folder? val))
                                                         fname (nth diskpath ix) indent (+ nindent-stub (- ix nstub) 1)
                                                         new-text (apply str (concat (repeat indent spacer) (if folder? [folder-closed] []) [fname "\n"]))
                                                         ] (assoc (dissoc val :folder?) :text new-text))) r)]
                      (if (expanded? line-stub) ; clear the way.
                        (let [box1 (_push-ixs box usstub)
                              path-add0 (_inc-last usstub) ; the path to add for the first of xs-to-add 
                              paths-to-add (_paths-to-add path-add0 n+)]
                          (reduce #(vassoc-in %1 (first %2) (second %2)) box1
                            (mapv vector paths-to-add xs-to-add)))
                        (let [num-children (if-let [x (get-in box (conj usstub :children))] (count x) 0) ; num-children at the first level we need to add.
                              path-add0 (conj usstub :children num-children) ; add at the end of all the children.
                              paths-to-add (_paths-to-add path-add0 n+)]  
                          (reduce #(vassoc-in %1 (first %2) (second %2)) box
                            (mapv vector paths-to-add xs-to-add)))))))
                  ; No nstub, add a non-indented path at the end: 
                  (let [leaf (str (if (:folder? val) folder-closed "") (last diskpath) "\n")] 
                    (update box :pieces #(conj % (assoc (dissoc val :folder?) :text leaf))))))
            box))
      (= ty :remove)  ; remove val and anything deeper in the us2disk path.
      (if-let [uspath (get disk2us diskpath)] ; make sure the edit is even relevant.
        (let [uspath- (into [] (butlast uspath))
              ; the deletion may make us remove multiple lines:
              first-ix-gone (last uspath)
              r (range first-ix-gone (count (get-in box uspath-)))
              last-ix-gone (last (filter #(let [dp (get us2disk (conj uspath- %))]
                                            (and (>= (count dp) (count diskpath))
                                              (= (subvec dp 0 (count diskpath)) diskpath))) r))]
          (update-in box uspath- #(into [] (concat (subvec % 0 first-ix-gone) (subvec % (inc last-ix-gone)))))) box)
      :else ; change diffs change the last element of the path.
        (let [val (nth diff 2) pth (get disk2us diskpath)]
          (update-in box pth 
            #(let [n-indent (level-of %) folder? (:folder? val) fname (:fname val)
                   new-text (apply str (concat (repeat n-indent spacer) (if folder? [folder-closed] []) [fname "\n"]))]
              (assoc % :text new-text)))))))
(defn implement-diffs [box diffs]
  "Only implement diffs if they havent already been implemented and the children aren't exported
   (if the children are exported the changes will be implemented there).
   the path is in vectorized form.
   [:add path val] (val may be a tree).
   [:remove path] (removes all children, grandchildren, etc).
   [:change path newval]"
  ;(if (> (count diffs) 0) (println "Diffs applies: " (:path box) diffs))
  (let [diffs (filterv #(> (count (second %)) (count (vec-file (:path box)))) diffs) ; only diffs that are deeper than our path do anything.
        _d0 (first diffs) _d1 (second diffs)
        d0 (if (= (first _d0) :remove) _d1 _d0) d1 (if (= d0 _d0) _d1 _d0)
        n (count diffs) us2disk (all-paths box) 
        disk2us (zipmap (vals us2disk) (keys us2disk))
        scu #(rtext/cursor-scroll-update box % ; lazy way.
               (stringdiff/edits-between (rtext/rendered-string box) (rtext/rendered-string %)))]
    (if (and (= (count diffs) 2) ; simplification in place modification for typing.
          (= (first d0) :add) (= (first d1) :remove) 
          (= (butlast (second d0)) (butlast (second d1))))
      (scu (_implement-diff box us2disk disk2us [:change (second d1) (assoc (nth d0 2) :fname (last (second d0)))]))
      (loop [acc box ix 0 us2disk us2disk disk2us disk2us]
        (if (= ix n) (scu acc)
          (let [box1 (_implement-diff acc us2disk disk2us (nth diffs ix))
                us2disk1 (if (= acc box1) us2disk (all-paths box1))
                disk2us1 (if (= acc box1) disk2us (zipmap (vals us2disk1) (keys us2disk1)))]
            ; removal diffs need to be ran multiple times when there are duplicate filenames:
            (recur box1 (if (and (not= acc box1) (= (first (nth diffs ix)) :remove)) ix (inc ix))
              us2disk1 disk2us1)))))))

;;;;;;;;;;;;;;;;;;;;;;;; Compiling interaction events ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def dispatch 
  (plurality/->simple-multi-fn
    {:mousePressed mouse-press
     :mouseDragged rtext/mouse-drag
     :keyPressed key-press
     :keyReleased rtext/key-release
     :mouseWheelMoved rtext/mouse-wheel
     :mouseReleased (fn [m-evt comp] (selection-snap comp))}
     (fn [e-clj comp] comp)
     (fn [e-clj comp] (:type e-clj))))

(defmacro updaty-fns [code] 
  (let [a1 (gensym 'args)] 
    (zipmap (keys code) (mapv #(list `fn ['& a1] (list `apply % a1)) (vals code)))))
(defn interact-fns [] (updaty-fns
  {:dispatch dispatch
   :render rtext/render
   :expandable? expandable?
   :expand-child expand-child :contract-child contract-child
   :is-child? (fn [box] (> (count (vec-file (:path box))) 1))}))
