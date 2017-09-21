; Browses files and folders.
(ns clooj.app.gaui.fbrowser
 (:require [clooj.java.file :as jfile]
   [clooj.app.gaui.rtext :as rtext]
   [clooj.app.claytreeevt :as clevt]))

; We work on a text box.
; :pieces = a vector of 
   ; :text = the pieces rendered text value.
   ; <other stuff> user data.
; :cursor-ix is the current cursor index, counting the space between the pieces.
; :font-size is the font size. The actual font size can be different because of physics-induced compression, etc.
; :selection-start and :selection-end are the selected text range, inclusive.
  ; Dragging the cursor to location x makes :selection-end x-1. 
; :scroll-top and :scroll-left = upper left corner scroll position.
; :size is a two element vector in pixels.


; Each :piece of the box contains:
 ; :leafname0 = original name loaded from disk or last name saved to disk. The current leafname will be added.
 ; :fullname0 = same idea but the full-path. Cut-n-paste will always make a unique name out of this. TODO: is this useful?
 ; :folder? = we show folders differently. Trying to make a file a child of a file will TODO.
 ; :children (if not expanded). If we are expanded we infer the hierarchy from the indentation.
 ; :contents (the file contents).
 ; :timestamp (for atomistic file modifications, files only).
 ; :children-id = unique id for children when expanded to another node.

;;;;;;;;;;;;;;;;;;;;;;;;;;; Other ;;;;;;;;;;;;;;;;;;;;;;;;;;;

; spacers at the beginning:
(def folder-spacer \u25b9) ; a triangle that points right.
(def file-spacer \u26ac) ; a circle.

(defn pixel-to-line [box x y]
  "There is always one line per piece."
  (second (rtext/cursor-pixel-to-ugrid box x y)))

(defn level-of [x]
  "How many indent levels are we in?"
  (let [s (if (string? x) x (:pieces x))]
    (+ (count (filterv #(= % folder-spacer) s)) (count (filterv #(= % file-spacer) s)))))

(defn _folder-open-toggle [box lix]
  (let [lix (pixel-to-line box x y) lines (:pieces box) line (nth lines lix)
        level (level-of line)]
    (assoc box :pieces
      (if (:children line) ; expand these children.
        (into [] (concat (subvec lines 0 lix) [(dissoc line :children)]
                   (mapv #(set-level % (inc level)) (:children line)) (subvec lines (inc lix))))
        ; Contract all children, but do not contract grandchildren into children.
        (let [lines-b4 (subvec lines 0 lix)
              first-notchix (+ lix (first (filter #(<= (level-of %) level) (conj lines-b4 ""))))
              lines-mid (subvec lines (inc lix) first-notchix)]
          (into [] (concat lines-b4 [(assoc line :children lines-mid)] (subvec lines first-notchix))))))))
(defn folder-open-toggle [box x y]
  "Standard folder opening toggle, will do nothing when applied to a file.
   This is different from expand-child and contract-child because it doesn't create other components."
  (_folder-open-toggle box (pixel-to-line box x y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Disk file ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn _text-to-leaf [x level] ; recursively sets :text to :leafname0 and level.
  (let [ch (:children x)]
    (assoc (if ch (assoc x :children (mapv #(_text-to-leaf % (inc level)) ch)) x) 
      :text (str (apply str (repeat level (if ch folder-spacer file-spacer))) (:leafname0 x) "\n"))))

(defn _load-from-disk [foldername-full]
  (let [filders (sort (jfile/visible-children foldername-full false)) nf (count filders)
        arbor {:leafname0 (jfile/full-to-leaf foldername-full) :fullname0 foldername-full}] ; children start off invisible.
    (assoc arbor :children ; everything starts out unexpanded.
      (mapv #(let [ffull (str foldername-full (jfile/sep) %)] ; this filder converted to a full-path.
               (if (jfile/dir? ffull) (_load-from-disk ffull)
                 (let [x (jfile/load-textfile-timestamp ffull)] ; we know the file was modified iff the date-modified > :file-mode-time
                   {:leafname0 % :contents (:text x) :contents0 (:text x) :timestamp (:last-modified x)
                    :fullname0 (str ffull (jfile/sep) %)}))) filders))))
(defn load-from-disk []
  "All children startout contracted."
  (assoc (rtext/place-holder-text) :outline-color [0.8 0.5 0.3]
    :pieces (_text-to-leaf (_load-from-disk (jfile/absolute-project-folder)))))

(defn save-to-disk!!! [] 
  (throw (Exception. "Saving not implemented yet TODO")))

(defn update-from-disk [] ; not sure the best way to implement this.
  (throw (Exception. "Updating from external modifications not implemented yet TODO")))


;;;;;;;;;;;;;;;;;;;;;;; Boss support functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def tool-modes 
  "Tool modes are added to mouse and key events to allow additional functionality beyond
   the standard usage and expanding/contracting node children."
  #{:default :toggle-folder-open})

(defn expandable? [box x y]
  "Test whether we are expandable first!"
  (let [lix (pixel-to-line box x y) line (nth (:pieces box) lix)] (boolean (:children line))))
(defn contractable? [box x y]
  "Sometimes for us both expandable and contractable are false, but they are never both true."
  (let [lix (pixel-to-line box x y) lines (:pieces box)] 
   (and (< lix (dec (count lines))) 
     (> (level-of (nth lines (inc lix))) (level-of (nth lines lix))))))
     
(defn expand-child [box x y child-unique-id]
  "Expands a child, when the user clicks on x,y.
   Returns [modified-parent new-child].
   the modified-parent must store the unique-id in order to retrieve it when we use contract-child."
 (let [lix (pixel-to-line box x y)]
   [(update-in box [:pieces lix]
      #(assoc (dissoc % :children) :children-id child-unique-id))
    (:children (get-in box [:pieces lix]))]))

(defn contract-child [box unique-id child]
  "Contracts a child that has unique-id, returns the modified box"
  (let [;the count prevents an infinite loop when you try to acces an id that isn't there.
        lix (first (filter #(= (:children-id %) unique-id) (range (count (:pieces box)))))
        box-contract (_folder-open-toggle box lix)]
    (update-in box [:pieces lix] #(dissoc (assoc % :children) :children-id)))) 

(defn mouse-pressed [box mouse-evt tool-mode]
  "Mouse-based interaction 101. Expand-contract is an optional tool mode."
  (if (= tool-mode :default) (rtext/mouse-press box mouse-evt)
    (folder-open-toggle box (:X mouse-evt) (:Y mouse-evt)))) ; expand and contract boxes.

(defn _recursive-indent [x] ; any :children are also indented.
  (update (if-let [ch (:children x)] (assoc x :children (mapv _recursive-indent ch))) 
    :text #(str (if (:folder? x) folder-spacer file-spacer) %)))
(defn _recursive-dedent [x] ; any :children are also dedented.
  (update (if-let [ch (:children x)] (assoc x :children (mapv _recursive-dedent ch))) 
    :text #(if (or (= (first %) file-spacer) (= (first %) folder-spacer)) (subs % 1) %)))
(defn key-pressed [box key-evt tool-mode]
  "Key-based interaction 101.
   Tabs always indent, keys typed within the padding region change the file itself.
   Backspace can dedent which will affect all children.
   Paste doesn't have to worry about interstitial indent chars."
  (let [ed (rtext/key-to-edit key-evt)]
    (cond (not= (:type ed) :type) (rtext/key-press box key-evt)
      (= (str (:value ed) "\t")) ; indent or dedent all selected lines and children thereof.
      (let [indent? (throw "TODO: is-shifting check when tap is typed (inverse)")]
        (let [sel0 (if (> (:selection-start box) -1) (:selection-start box) (:cursor-ix box))
              lix0 (second (rtext/cursor-pixel-to-ugrid box sel0))
              lix1 (second (rtext/cursor-pixel-to-ugrid box (:selection-end box)))
              xdent (if indent? _recursive-indent _recursive-dedent)
              lines (:pieces box)]
          (assoc box :pieces (into [] (concat (subvec pieces 0 lix0) (mapv xdent (subvec pieces lix0 (inc lix1))) (subvec pieces (inc lix1))))))
     (and (= (str (:value ed) (str \backspace))) ; dedent all children if we backspace one of the characters.
       (> (:selection-start box) (:selection-end box)) ; only dedent if no selection.
       (let [char (get (rtext/rendered-string box) (dec (:cursor-ix box)))] 
         (or (= char folder-spacer) (= char file-spacer)))) ; only dedent if we are an indentation character.
     (update-in (update box :cursor-ix dec) [:pieces (second (rtext/cursor-pixel-to-ugrid box (:cursor-ix box)))] _recursive-dedent)
     :else (rtext/key-press box key-evt))))) ; normal keypress.

(defn mouse-dragged [box mouse-evt tool-mode] box)

; Only pressed and released events matter. The typed and clicked events are more trouble then their worth.
(defn mouse-moved [box mouse-evt tool-mode] box)
(defn mouse-released [box mouse-evt tool-mode] box)
(defn key-released [box key-evt tool-mode] box)


(defn cmd-input [box cmd]
  "Commands also include menu options. Synergize gui and cli!"
  (throw (Exception. "TODO: support commands.")))
