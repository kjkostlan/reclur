; Implements JTree-like file systems with a much easier API than the horrid JTree's.

(ns clooj.app.claytreearbor
  (:require [clooj.app.claytreetext :as cltext]
    [clooj.app.claytreephysics :as claphy]
    [clojure.set :as set]
    [clooj.app.claytreetree :as clatre]
    [clooj.app.claytreewalk :as clwalk]
    [clooj.java.file :as jfile]
    [clooj.java.clipboard :as clipboard]
    [clojure.string :as string]))

(def idt (str \u037A)) ; a small symbol that is mostly empty space.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Initialization functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-arbor-node []
  "Sets up an empty node. The node has it's vanilla :children as well as an internal :arbor.
   The :arbor has similar structure to the vanilla :children.
   :children starts out empty and is only populated when the user vanilla-expands the node.
   The user can also arbor-expand the node to get JTree-like behavior where no new node is added.
   Subfields of :arbor:
     :name = folder or filename (leaf, not full path).
     :contents = filedata in string form.
     :name0, :contents0 = state at file open or last file save.
     :timestamp = when the file was read, see jfile's atomistic file reading. Files only, not folders.
     :vanilla? = Do we correspond to a vanilla child. The vanilla?=true elements, when the arbor is unwrapped and sorted, are 1:1 with the node's :children.
     :children = further arbor children. Mutually exclusive with :vanilla."
  (assoc (clatre/make-node :arbor "") :arbor {:name "" :name0 "" :contents "" :contents0 "" :timestamp -1 :vanilla? false :children []}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Conversion functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn line-to-indent [line]
  "How many spaces at the beginning of line."
  (count (take-while #(= % (first idt)) line)))

(defn line-is-vanilla-child? [^String line]
  "Whether or not the line is a stand-in for a vanilla node child"
  (boolean (.contains line (str cltext/tr-ch))))

(defn arbor-to-lines [arbor]
  "Converts the arbor into an indented using idt vector of lines.
   Only visible lines are shown.
   Children use the spacers (cltext/tr-ch)."
  (let [au (clwalk/assign-paths arbor true) sp (apply str (repeat (:segment-space cltext/*text-params*) cltext/tr-ch))
        names (mapv :name au) paths (mapv :path au) chs (mapv :vanilla? au)]
    (mapv #(str (apply str (repeat (count %1) idt)) (if %3 sp %2)) paths names chs)))

(defn deep-line-count [arbor]
  "How many lines are contained in the arbor. Only visible children"
  (inc (if (and (:children-visible? arbor) (:children arbor)) 
         (apply + (mapv deep-line-count (:children arbor))) 0)))

(defn assign-lines [arbor ix0]
  "Assigns line numbers to us and visible descendents recursively in the correct order.
   ix0 = index of the first line. Use 0 uasually. Does not expand vanilla children.
   The spacers must be converted to array breaks in the :children of :tbox."
  (let [a1 (assoc arbor :tmp-line-num ix0)]
    (if (not (:children-visible? a1)) a1
      (let [offsets (reductions + (inc ix0) (butlast (mapv deep-line-count (:children a1))))]
        (update a1 :children (fn [c] (mapv assign-lines c offsets)))))))

(defn sort-unwrap [arbor add-invis-children?]
  "Sorts (in the order that it appears) and unwraps an arbor. Visible only, of course.
   add-invis-children? allows us to add-back invisible children within the arbor.
   Does not expand vanilla children or invisible children."
  (let [au (clwalk/vis-tree-unwrap (assign-lines arbor 0))
        au-map (reduce #(assoc %1 (:tmp-line-num %2) %2) {} au) ; map from line-num to val.
        au-sort (mapv #(get au-map %) (range (count (vals au-map)))) ; vector.
        au-sort (if add-invis-children?
                  (mapv #(if (not (:children-visible %)) ; add children when we don't have visible children.
                             (assoc % :children (get-in arbor (concat (:path %) [:children]))) %) 
                    au-sort) au-sort)]
    (mapv #(dissoc % :tmp-line-num) au-sort)))

(defn child-ix-of-lines [lines]
  "Lines can be stand-ins for vanilla children (see arbor-to-lines)
   If a line cooresponds to a vanilla child the entry is the index of said child. If not, it's -1.
   Both arbor and non-arbor children are included."
  (let [is-child-shorthand? (mapv #(not= (string/replace % (str cltext/tr-ch) "") %) lines)
        is-child-shorthand01 (mapv #(if % 1 0) is-child-shorthand?)
        child-ix (into [] (reductions + 0 is-child-shorthand01))]
    (mapv #(if %2 %1 -1) child-ix is-child-shorthand?)))

(defn node-to-lines [node]
  "Lines as shown on the screen."
  (let [s (cltext/rendered-string (assoc-in node [:tbox :visible-spacers?] true))]
    (string/split s #"\n")))

(defn tuck-in-children [lines arbor children]
  "Converts any vanilla :children (if they are :arbor) into the node's :arbor, returning the fulled :arbor.
   Acts recursivly. Used to ensure we copy the vanilla children when we ctrl+C.
   Lines must be 1:1 with arbor."
  (let [child-ix (child-ix-of-lines lines)
        au-sort (sort-unwrap arbor)
        ; Only uses node-children when there are lines that are placeholders:
        vanilla-to-arbor (fn [nd] (tuck-in-children (node-to-lines nd) (:arbor nd) (:children nd)))
        au-sort-ch (mapv #(if (> %2 -1) (assoc %1 :children (vanilla-to-arbor (nth children %2))) %1) 
                     au-sort child-ix)]
      (clwalk/tree-wrap au-sort-ch)))

(defn sub-arbor [arbor all-lines line-indexes & vanilla-children]
  "Creates a sub-arbor from arbor, the indexes of visible lines, and the text of each line.
   The root of the sub-arbor is the common ancestor of all line-indexes. 
   Children (as represented in all-lines by placeholder cltext/tr-ch) are tucked in if node-children is provided.
   Note: there is no lines-to-arbor feature since we need to store :contents and :timestamp"
  (let [line-indexes (sort line-indexes)
        arbor1 (if (= (count vanilla-children) 1) (tuck-in-children all-lines arbor (first vanilla-children)) arbor)
        au-sort (sort-unwrap arbor1 true)
        au-piece (mapv #(nth au-sort %) line-indexes)
        root-path (clwalk/common-root (mapv :path au-piece)) nr (count root-path)
        au-rpiece (mapv (fn [u] (update u :path #(subvec % nr))) au-piece)]    
    (clwalk/tree-wrap au-rpiece)))

(defn unindent-line [line]
  "Remove indents from lines. Also fixes lines with indents midway through them."
  (string/replace (string/replace line cltext/tr-ch "") idt ""))

(defn arbor-name-update [arbor lines]
  "Uses the values of lines to update the arbor.
   The arbor does not have it's children tucked in.
   Arbor and lines must be 1:1."
  (let [au (sort-unwrap arbor true)
        au1 (mapv #(assoc %1 :name (unindent-line %2)) au lines)]
    (clwalk/tree-wrap au1)))

(defn set-text-to-arbor [node]
  "Sets the text to match the node's arbor."
  (let [lines (arbor-to-lines (:arbor node)) ; Convert to lines.
        ch-ix (child-ix-of-lines lines)
        lines (mapv #(string/replace % cltext/tr-ch "") lines) ; get rid of children spacers (they are automatically put in by cltext).
        ; Aggregate lines into blocks. Vanilla children end old blocks.
        blocks (reduce #(if (> (nth ch-ix (dec %2)) -1) (conj %1 [(nth lines %2)])
                            (update %1 (dec (count %1)) (fn [x] (conj x (nth lines %2))))) 
                 [[(first lines)]] (range 1 (count ch-ix)))
        ; Convert the blocks into strings with newlines between them:
        blocks (mapv #(apply str (interpose "\n" %)) blocks)
        ; Put a newline at the beginning of each block (except the first block):
        blocks (mapv (fn [b ix] (if (= ix 0) b (str "\n" b))) blocks (range))]
  (cltext/on-text-change (assoc-in node [:tbox :pieces]  blocks))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; File IO functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _load-from-disk [foldername-full]
  (let [filders (sort (jfile/visible-children foldername-full false)) nf (count filders)
        arbor {:name (jfile/full-to-leaf foldername-full) :children-visible? false :vanilla? false}] ; children start off invisible.
    (assoc arbor :children
      (mapv #(let [ffull (str foldername-full (jfile/sep) %)] ; this filder converted to a full-path.
               (if (jfile/dir? ffull) (_load-from-disk ffull)
                 (let [x (jfile/load-textfile-timestamp ffull)] ; we know the file was modified iff the date-modified > :file-mode-time
                   {:name % :name0 % :contents (:text x) :contents0 (:text x) :timestamp (:last-modified x) :vanilla -1}))) filders))))
(defn load-from-disk []
  "Creates a fully-collapsed :arbor node from the files on the disk.
   No :vanilla children. TODO: not load everything at once."
  (let [a (_load-from-disk (jfile/absolute-project-folder))]
    (cltext/on-text-change 
      (set-text-to-arbor (assoc (new-arbor-node) :arbor a))))) ; only one level is visible.

(defn _mt? [x] (or (not x) (= (count x) 0)))
(defn _file-node? [nd]
  (or (= (:type nd) :text) (and (= (:type nd) :arbor) (_mt? (:children nd)) (_mt? (:children (:arbor nd))))))
(defn _save-to-disk-file!!! [text-or-leafarbor-node filename-full]
  ; Does not undo external changes. See update-from-disk.
  (if (and (= (:name0 text-or-leafarbor-node) (:name0 text-or-leafarbor-node))
       (= (:contents0 text-or-leafarbor-node) (:contents text-or-leafarbor-node))) text-or-leafarbor-node ; up to date (unless external changes happened).
    (let [ar? (= (:type text-or-leafarbor-node) :arbor)
          val (if ar? (:contents text-or-leafarbor-node) (cltext/real-string text-or-leafarbor-node))
          date (jfile/save-textfile-timestamp!!! filename-full val)]
      ((if ar? #(assoc % :contents val) identity) (assoc text-or-leafarbor-node :contents0 val :timestamp date)))))
(defn _delete-other-filders!!! [leaf-list-to-keep full-folder-path]
  (let [ch (apply hash-set (jfile/visible-children full-folder-path true))
        full-list-to-keep (apply hash-set (mapv #(str full-folder-path (jfile/sep) %) leaf-list-to-keep))
        byby (set/difference ch full-list-to-keep)]
    (mapv jfile/delete-filder!!! byby)))
(defn _save-to-disk-folder!!! [arbor-node full-folder-path]
  (let [lines (node-to-lines arbor-node) arb (:arbor arbor-node)
        paths (mapv :path (sort-unwrap arb false))
        vanilla-ix (child-ix-of-lines lines)
        rines (mapv #(hash-map :line %1 :path %2 :ix %3) lines paths vanilla-ix)
        vanilla-ch (:children arbor-node)
        one-lev-filders (mapv #(:name (get-in arb %)) (filter #(= (count %) 2) paths))]
    (_delete-other-filders!!! (apply hash-set one-lev-filders) full-folder-path)
    (reduce #(let [folder-levels (mapv (fn [p] (:name (get-in arb p))) (take-nth 2 (:path %2))) ; don't take the :children parts of the path.
                   disk-path (apply str full-folder-path (interpose (jfile/sep) folder-levels))]
               (if (> (:ix %2) -1) ; vanilla child.
                 (let [ch (nth vanilla-ch (:ix %2))]
                   (assoc-in %1 [:children (:ix %2)]
                     ((if (_file-node? ch) _save-to-disk-file!!! _save-to-disk-folder!!!) ch disk-path)))) 
                  arbor-node rines))))
(defn save-to-disk!!! [root-arbor-node]
  "Saves any text edits to the disk, reloading (TODO implement this) the repl files.
   Returns the node with up-to-date timestamps, etc.
   Removes files that aren't in root-arbor-node."
  (throw (Exception. "saving disabled for safety reasons"))
  (_save-to-disk-folder!!! root-arbor-node (jfile/absolute-project-folder)))

(defn update-from-disk [root-arbor-node]
  "Responds to disk changes.
   Opens a warning box for files the user prompted."
  (throw (Exception. "TODO"))) ; Don't know the best UI for when the disk changes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; Editing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn copy-to-clipboard!! [node]
  "Copies the node's arbor to the clipboard as a pr-str arbor, converting any vanilla :children into the arbor."
  (let [sel-start (:selection-start (:tbox node)) sel-end (:selection-end (:tbox node))]
    (if (and sel-start sel-end (>= sel-end sel-start)) ; non-empty selection.
      (let [lines (node-to-lines node)
            sel (cltext/selection-of-chunks lines sel-start sel-end)
            ix0 (:ix0 sel) ix1 (:ix1 sel) ; inclusive.
            full-begin? (= (:begin-stub sel) (nth lines (:ix0 sel)))
            full-end? (= (:end-stub sel) (nth lines (:ix1 sel)))
            ch? #(= (last (nth lines %)) (first cltext/tr-ch))
            ix0+ (if (and (not full-begin?) (ch? ix0)) (inc ix0) ix0) ; Children lines must be fully selected.
            ix1- (if (and (not full-end?) (ch? ix1)) (dec ix1) ix1) ; ranges are inclusive.
            lines1 (assoc lines ix0 (:begin-stub sel) ix1 (:end-stub sel))] ; modify the stubs.
         (if (> ix0+ ix1-) nil ; no selection as it was partial on a children node.
           (let [a1 (sub-arbor (:arbor node) lines1 (range ix0+ (inc ix1-)) (:children node))]
             (clipboard/put-as-string!! (pr-str a1)))))))) ; store as a string.

(defn insert-at [node insert-ix xs]
  "Inserts a string or an arbor object encoded as a string in the node at index insert-ix.
   Returns the modified node. Sets the cursor ix to the end of the inserted stuff."
  (let [xs (str xs) x (try (read-string xs) (catch Exception e false)) ; strings or a specific data-structure.
        lines (node-to-lines node)
        s0 (cltext/selection-of-chunks lines 0 insert-ix 1) ; 1 = extra newline at end of each line.
        lix (:ix0 s0) ; same as :ix1
        linel (unindent-line (:end-stub s0)) ; split the line at insert x. Splitting mid-indent is fixed by unindent.
        liner (unindent-line (:begin-stub (cltext/selection-of-chunks lines insert-ix 1e100 1)))
        lines (mapv unindent-line lines)
        au (sort-unwrap (:arbor node) true) nm (mapv :name au)
        set-c (fn [nd lx] (let [ls (node-to-lines nd) cix (+ (apply + (mapv count (subvec ls 0 lx))) lix (count (:begin-stub s0)))] 
                            (assoc-in node [:tbox :cursor-ix] cix)))
        nu #(set-text-to-arbor (assoc node :arbor (clwalk/tree-wrap %)))]
    (if (and (map? x) (:name x)) ; Maps are arbors where children has been tucked in. It includes file contents (copying large files will take time and memory).
      (let [aui (sort-unwrap x true) ; last arg true/false probably doesn't matter.
            clix (+ lix (count aui)) ; cursor line index (at the end of the insert).
            cv #(into [] (concat %1 %2 %3)) anm (fn [au ln] (mapv #(assoc %1 :name %2) au ln))]
        (cond (or (= (count linel) 0) (first (child-ix-of-lines [(nth lines linel)]))) ; Selection at beginning of line OR split a child line.
          (set-c (nu (anm (cv (subvec au 0 lix) aui (subvec au lix)) (cv (subvec lines 0 lix) nm (subvec lines lix)))) clix)
          (= (count liner) 0) ; Selection at end of line.
          (set-c (nu (anm (cv (subvec au 0 (inc lix)) aui (subvec au (inc lix))) (cv (subvec lines 0 (inc lix)) nm (subvec lines (inc lix))))) clix)
          :else ; split up a regular line. Less common.
          (set-c (nu (anm (cv (conj (subvec au 0 lix) (nth au lix)) aui (subvec au lix)) 
                   (cv (conj (subvec lines 0 lix) linel) nm (concat [liner] (subvec lines (inc lix)))))) clix)))
      (set-c (nu (assoc-in au [lix :name] (str linel (string/replace xs "\n" "") liner))) lix)))) ; Strings: we can only modify one line's :name as we don't know the non :name date.

(defn remove-sel [node ix0 ix1]
  "Removes between ix0 and ix1, inclusive on the rendered string.
   Does not remove children (TODO: this is a limitation in the current editing code flow, it can be changed)."
  (let [lines (node-to-lines node) ch (child-ix-of-lines lines)
        sel (cltext/selection-of-chunks lines ix0 ix1 1) ; 1 = extra newline at end of each line.
        lix0 (:ix0 sel) lix1 (:ix1 sel) line1 (unindent-line (:end-stub sel))
        line0 (unindent-line (subs (nth lines lix0) 0 (- (count (nth lines lix0)) (count (:begin-stub sel)))))]
    (if (< line1 line0) node ; incluseve indexes, so negative difference means no overlap.
        (let [lines (mapv unindent-line (assoc lines lix0 line0 lix1 line1)); the two stub lines can be in-place modified.
              ixs (filterv #(or (<= % lix0) (>= % lix1) (> (nth ch %) -1)) (range (count lines)))]
          (set-text-to-arbor (assoc node :arbor (sub-arbor (:arbor node) lines ixs)))))))

(defn override-keytype [node key-evt]
  "Overrides the typing event (called in from cltool).
   Does not handle saving, that must be handled at cltool."
  (let [edit (cltext/key-to-edit node key-evt) ty (:type edit)]
    (if (or (= ty :select-all) (= ty :arrow) (= ty :save)) (cltext/key-press node key-evt) ; same as vanilla.
      (let [edit (cltext/key-to-edit node key-evt) ty (:type edit) cix (:cursor-ix (:tbox node))
            sel0 (:selection-start (:tbox node)) sel1 (:selection-end (:tbox node))
            set-cix #(assoc-in %1 [:tbox :cursor-ix] %2)
            node0 (fn [] (set-cix (remove-sel node sel0 sel1) sel0))]
        (if (>= sel1 sel0) ; non-zero selection.
          (cond (= ty :cut) (do (copy-to-clipboard!! node) (node0))
            (= ty :copy) (do (copy-to-clipboard!! node) node)
            (= ty :paste) (insert-at (node0) sel0 (clipboard/get-as-string))
            (= ty :backspace) (node0)
            (= ty :type) (insert-at (node0) (str (:value edit)))
            :else node)
          (cond (or (= ty :cut) (= ty :copy)) node ; empty selection region.
            (= ty :paste) (insert-at node sel0 (clipboard/get-as-string))
            (= ty :backspace) (set-cix (remove-sel node sel0 sel0) (dec sel0))
            (= ty :type) (insert-at node sel0 (str (:value edit)))))))))

(defn pos-child [node child-node]
  "Positions the child."
  (let [l0 (claphy/parent-child-l0 node child-node)]
    (clatre/position-relative-to node child-node l0 0.0 :parent true)))

; Refactor out these & fix bug: 45 work-min, started 11:30.

(defn _vanilla-expand [panel node line-ix child-ixs nv-folder? au path au-path]
  (let [insert-ix (inc (nth (reductions max child-ixs) line-ix))
        child (pos-child node 
                (if nv-folder? (set-text-to-arbor (assoc (clatre/make-node :arbor "") :arbor (nth au line-ix)))
                  (let [x (get-in panel au-path)]
                    (assoc (clatre/make-node :text (:contents x)) :name0 (:name0 x) :contents0 (:contents0 x)))))] ; two types of children.
    (-> panel (assoc-in (concat au-path [:vanilla?]) true) 
      (assoc-in (concat path [:children-visible?]) true) ; make sure children are visible (we do NOT ever set this to false upon contraction, the children are removed).
      (update-in au-path #(if nv-folder? (assoc % :children []) ; arbor child pop-out.
                            (dissoc % :name0 :contents0 :contents :timestamp))) ; arbor wipe contents.
      (update-in (concat path [:children]) ; insert the child.
        #(into [] (concat (subvec % 0 insert-ix) [child] (subvec % insert-ix)))))))


(defn _vanilla-contract [panel node ci path au-path]
  (let [a 1]
    (-> panel (assoc-in (concat au-path [:vanilla?]) false)
      (update-in au-path #(let [ch (get-in panel (concat path [:children ci]))]
                            (if (= (:type ch) :arbor) (:arbor ch) ; back into the arbor
                              (assoc % :name0 (:name0 ch) :contents0 (:contents0 ch) :contents (cltext/real-string ch) :timestamp (:timestamp ch))))) ; put back the arbor contents.
      (update-in (concat path [:children]) #(into [] (concat (subvec % 0 ci) (subvec % (inc ci))))))))

(defn _deep-contract [panel node line-ix child-ixs path au au-path]
  (let [a-path (subvec au-path (inc (count path))) nap (count a-path) ; all stuff after the :arbor.
        ; all contracting line indexes, including ourselves:
        contract-line-ixs (filterv #(let [ap (:path (nth au %))]
                                      (and (>= (count ap) nap) (= (subvec ap 0 nap) a-path))) 
                            (range (count au))) 
        contract-line-paths (mapv #(:path (nth au %)) contract-line-ixs)
        which-vanilla-contract (mapv #(nth child-ixs %) contract-line-ixs)
        nc (count contract-line-ixs)]
    (loop [panel1 panel ix 0 which-vanilla-contract1 which-vanilla-contract]
      (if (= ix nc) panel1
        (let [au-pathi (concat path [:arbor] (nth contract-line-paths ix))
              cixi (nth which-vanilla-contract1 ix)  
              node1 (get-in panel1 path)]
          ; Thankfully the contract-line-paths aren't affected by the index shift.
          (if (> cixi -1)
            (recur (_vanilla-contract panel1 node1 cixi path au-pathi)
              (inc ix) (mapv #(cond (< % cixi) % (= % cixi) -1 (> % cixi) (dec %)) which-vanilla-contract1)) ; index shift on vanilla children.
            (recur (assoc-in panel1 (concat au-pathi [:children-visible?]) false) (inc ix) which-vanilla-contract1)))))))

;TODO ; vanilla children don't disappear with grandparent collapse of the node.
; Empty node is allowed, it shouldn't be.
(defn override-inflate-toggle [panel world-pt]
  "Opening and closing the arbor without adding child nodes to the physics.
   For now: First half of line will toggle like Jtrees do, second half of line will use vanilla children.
   This is not ideal but a simple way to make it compatable with our current editing paradigm."
  (let [path (first (get-in panel [:edit-state :selected-paths]))
        node (get-in panel path)
        cix (:cursor-ix (:tbox node)) ; the cursor was updated by the last click.
        lines (node-to-lines node)
        s0 (cltext/selection-of-chunks lines 0 cix 1)
        lix (:ix1 s0) len (count (nth lines lix)) 
        within-line-ix (count (:end-stub s0)) ; 0 = cursor at begining of line.
        first-half? (<= within-line-ix (/ len 2.0))
        au (sort-unwrap (:arbor node) true)
        nv-folder? (boolean (first (:children (nth au lix)))) ; are our line a folder (excluding vanilla folders expansion)?
        au-path (into [] (concat path [:arbor] (:path (nth au lix))))
        ch-ixs (child-ix-of-lines lines)
        chvis-path (concat au-path [:children-visible?])]
    (update-in
      (if (and first-half? nv-folder?); JTree-like toggling that doesn't create another node.
        (if (get-in panel chvis-path)
          (_deep-contract panel node lix ch-ixs path au au-path)
          (assoc-in panel chvis-path true))
        ; Children toggling.
        (let [ci (nth ch-ixs lix)]
          (if (= ci -1)
            (_vanilla-expand panel node lix ch-ixs nv-folder? au path au-path)
            (_vanilla-contract panel node ci path au-path))))
      path set-text-to-arbor)))