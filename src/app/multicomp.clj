; Multiple component manager.
; Trying not to have too much component-specific code here, but the coupling is just so tight for the structural editor.

(ns app.multicomp
  (:require 
    [clojure.set :as set] [clojure.string :as string]
    [app.singlecomp :as singlecomp]
    [app.codebox :as codebox]
    [app.xform :as xform]
    globals
    [javac.file :as jfile]
    [javac.warnbox :as warnbox]
    [app.multisync :as multisync]
    [app.stringdiff :as stringdiff]
    [app.fbrowser :as fbrowser]))

(defn rootfbrowser? [box] (and (= (:type box) :fbrowser) (= (count (:path box)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Searching ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn who-has [s filename real-char-ix]
  "Returns pairs of [key, cursor-ix], empty vector if it can't find anything."
  (let [comps (:components s) box-ks (filterv #(= (:type (get comps %)) :codebox) (keys comps))
        box-ks1 (filterv #(= (:path (get comps %)) filename) box-ks)]
    (mapv #(vector % (codebox/real-string-to-cursor (get comps %) real-char-ix)) box-ks1)))

(defn codebox-keys [comps fname] (filterv #(and (= (:path (get comps %)) fname) (= (:type (get comps %)) :codebox)) (keys comps)))

(defn cursor-locate [s k]
  "Returns the [filename, char-ix within file] of the cursor given a k."
  (let [comps (:components s) comp (get comps k) 
        _ (if (nil? comps) (throw (Exception. "Bad state")))
        _ (if (nil? comp) (throw (Exception. (str k " doesn't exist within the :components"))))
        _ (if (not= (:type comp) :codebox) (throw (Exception. "Not a codebox")))
        filename (:path comp)
        real-char-ix (codebox/cursor-to-real-string comp)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Modifying the cached tree within s ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _wrap-tree [paths]
  "Only used for updating the filetree, it would be nice to refactor this away."
  (let [paths (mapv fbrowser/vec-file paths)
        children (apply hash-set (mapv first paths))
        out {:text (first (first paths))}]
    (if (> (count children) 0)
      (let [paths-uproot (filterv #(> (count %) 0) (apply hash-set (mapv #(into [] (rest %)) paths)))
            ; map from first key to other paths:
            paths-uprootm (reduce (fn [acc k] (update acc (first k) #(if % (conj % k) [k]))) {} paths-uproot)]
        (assoc out :children
          (mapv _wrap-tree (vals paths-uprootm)))))))
(defn wrap-tree [paths] [(_wrap-tree paths)])

(defn get-filelist [s old? allow-false-old?]
  (let [comps (:components s)
        fbrowserk (filterv #(= (:type (get comps %)) :fbrowser) (keys comps))
        filepath2elem (apply merge (mapv #(fbrowser/unwrapped-tree (get comps %)) fbrowserk))
        ffil (if allow-false-old? identity #(filterv identity %))]
    (mapv #(if % (fbrowser/devec-file %) %) (if old? (ffil (mapv :fullname0 (vals filepath2elem))) (keys filepath2elem)))))

(defn new2?old-files [s]
  "map from new to old files, both fullpath. Nil values mean no old files."
  (zipmap (get-filelist s false nil) (get-filelist s true true)))

(defn set-filetree [s tree reset-fullname0s?]
  "tree is indexes with :children for folders and :text for the filename."
  (let [tmpk (gensym "reference") fb (fbrowser/new-fbrowser tree) ; sync to a temp component.
        comps1 (dissoc (multisync/comprehensive-sync (:components s) (assoc (:components s) tmpk fb)) tmpk)]
    (assoc s :components (if reset-fullname0s? (zipmap (keys comps1) (mapv fbrowser/reset-fullname0s (vals comps1))) comps1))))

(defn open-cache [s fname]
  "Loads fname from s, not the disk. nil if fname isn't found."
  (let [comps (:components s)
        codeboxks (codebox-keys comps fname)]
    (if (> (count codeboxks) 0)
      (codebox/real-string (get comps (first codeboxks))))))

(defn save-cache [s fname new-string]
  "Saves new-string to the file fname within s, not the disk."
  (let [_ (if (not (string? new-string)) (throw (Exception. "The string to save as must be a string.")))
        comps (:components s)
        codeboxks (codebox-keys comps fname)
        old-string (codebox/real-string (get comps (first codeboxks)))
        old-string (if old-string old-string "")
        edits (stringdiff/edits-between old-string new-string)
        
        comps1 (reduce (fn [comp-map k]
                         (assoc comp-map k
                           (codebox/apply-edits-to-real-string (get comps codeboxks) edits))) comps codeboxks)]
    (if (not= (open-cache (assoc s :components comps1) fname) new-string) (throw (Exception. "Bug in multicomp/save-cache"))) ; Extra safety double check.
    (assoc s :components comps1)))

(defn add-file [s fname folder?]
   "Adds a file to the codeboxes in s if need be."
  (let [comps (:components s)
        pieces (string/split fname #"/")
        diff [:add pieces {:folder folder?}]
        do-diff #(fbrowser/implement-diffs % [diff])
        comps1 (reduce #(let [c (get comps %2)]
                          (if (= (:type c) :fbrowser)
                            (assoc %1 %2 (do-diff c)) %1))
                 comps (keys comps))]
    (assoc s :components comps1)))

(defn who-is-open [s]
  "Which files are open in our codeboxes."
  (set (mapv #(:path %) (filterv #(= (:type %) :codebox) (vals (:components s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DISK based file handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn load-from-file [comps filename]
  "Returns a component. It will copy a component if one is already open, to ensure agreement in exported stuff.
   This means be careful with external modifications to the disk."
  (let [kys (filterv #(let [c (get comps %)]
                        (and (= (:type c) :codebox) (= (:path c) filename))) (keys comps))]
    (if (= (count kys) 0) ; first component.
      (let [txt (jfile/open filename)]
        (if (not txt) (throw (Exception. (str  "Attempted to load non-existant file: " filename))))
        ; The entire fname goes into one path:
        (assoc (codebox/from-text txt :clojure) :path [filename]))
      (let [; TODO: better picking of which one.
            ky (first kys)]
        (if ky (assoc (get comps ky) :position [0 0] :size [512 512]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Child expansion and contraction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn expand-child [k-parent k-child mevt-c s]
  "Expands a child if possible."
  (let [comp0 (get (:components s) k-parent) comp (dissoc comp0 :position)
        int-fns (:interact-fns comp)
        s (assoc-in s [:precompute :desync-safe-mod?] true)

        expandable? (:expandable? int-fns)
        mevt (xform/xevt (xform/x-1 (singlecomp/pos-xform (:position comp0))) mevt-c)
        x (if (expandable? mevt comp) 
            ((:expand-child int-fns) mevt comp))]
    (if x 
      (let [new-parent (first x) new-child (second x)
            comps (:components s)
            pos1 (mapv #(+ %1 (* %2 0.75)) (:position comp0) (:size comp0))
            s1 (assoc-in s [:components k-parent] (assoc new-parent :position (:position comp0)))
            s2 ((:add-component (:layout s1)) s1 (assoc new-child :position pos1 :z (inc (:z new-parent))) k-child)] 
        s2) s)))

(defn contract-child [parent child]
  (assoc ((:contract-child (:interact-fns parent)) (dissoc parent :position) (dissoc child :position)) :position (:position parent)))

(defn contract-child1 [comps parent-k child-k]
  "Like contract child but if the component is a codebox, also contracts into the twins and adjusts the numbers of remaining children."
  (let [parent (get comps parent-k) child (get comps child-k) comps1 (dissoc comps child-k)]
    (assoc comps1 parent-k (contract-child (get comps1 parent-k) (get comps1 child-k)))))

(defn contract-all-descendents [comps k]
  "leaf-first contraction, for now only affects fbrowsers."
  (let [comp (get comps k) ty (:type comps)]
    (if (= ty :fbrowser) ; only fbrowsers for now.
      (let [ch (multisync/fbrowser-children comps k)
            comps1 (reduce #(contract-all-descendents %1 %2) comps ch)
            ; Remove twins:
            ch-u (filterv #(get comps1 %) (vals (zipmap (mapv #(:path (get comps1 %)) ch) ch)))
            ch-x (set/difference (apply hash-set ch) (apply hash-set ch-u))
            ; Needed for codeboxes: sort high to low so contracting multiple children works properly:
            ch-u1 (sort-by #(- (last (:path (get comps1 %)))) ch-u)]
        (reduce #(contract-child1 %1 k %2) (reduce dissoc comps1 ch-x) ch-u)) comps)))

(defn contract-descendents-if-twinless [comps k]
  "leaf-first contraction, if we don't have twins."
  (if (> (count (multisync/twins comps k)) 0) comps
    (contract-all-descendents comps k)))

(defn close-component-noprompt [s kwd]
  "Contracts into the parent(s) if it has parents."
  (let [cs0 (:components s) ty (get-in cs0 [kwd :type])]
    (if (= ty :fbrowser) 
      (let [cs (contract-descendents-if-twinless cs0 kwd)
            doomed (get cs kwd)
            s (assoc-in s [:precompute :desync-safe-mod?] true)
            twins? (> (count (multisync/twins cs kwd)) 0)
            
            cs1 (dissoc cs kwd)
            
            s (assoc s :components cs1) 
            parent-ks (filterv #(= ty (:type (get cs %))) 
                        (multisync/fbrowser-padres cs kwd))]
        (if (or twins? (= (count parent-ks) 0)) s ; don't contract the child unless it is the last one remaining.
          (reduce #(assoc-in %1 [:components %2] (contract-child (get cs %2) doomed)) s parent-ks))) 
      (assoc s :components (dissoc cs0 kwd)))))

(defn close-component [s kwd]
  "Prompts the user if there are modified files open and the last codebox of a given type is open.
   Closes will fail if the user clicks cancel."
  (let [comp (get (:components s) kwd) ph (:path comp)]
    (cond (and (= (:type comp) :codebox) ; Closing the last open codebox.
            (= (count (multisync/twins (:components s) kwd)) 0))
      (let [fname (:path comp)
            txt0 (if (jfile/exists? fname) (jfile/open fname))
            txt1 (open-cache s fname)]
        (if (not= txt0 txt1)
          (let [opt (warnbox/choice (str "Save file before closing? " fname) [:yes :no :cancel] :yes)]
            (cond (= opt :yes) 
              (do (jfile/save!! fname txt1)
                (close-component-noprompt s kwd)) ; the cache is stored in the components; closing it removes the cache.
              (= opt :no)
              (close-component-noprompt s kwd)
              :else s))
          (close-component-noprompt s kwd)))
      (and (rootfbrowser? comp) (= (count (filterv rootfbrowser? (vals (:components s)))) 1))
      (do (println "Can't close the last root fbrowser.") s)
      :else (close-component-noprompt s kwd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rendering ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn which-tool-hud [s]
  (let [tool (if-let [m (:active-tool s)] (:name m) :OOPS) typing? (:typing-mode? s)
        g-cmd [:drawString [(str "tool = " tool " typing? = " typing? (if (globals/are-we-child?) "CHILD VERSION" "")) 2 15] {:FontSize 18 :Color [0 1 1 0.7]}]]
    [g-cmd]))

(defn draw-select-box [comps k camera]
  (let [f-comp (get comps k) p (:position f-comp) sz (:size f-comp)]
    [(xform/xgfx camera [:drawRect [(dec (first p)) (dec (second p)) (+ (first sz) 2) (+ (second sz) 2)] {:Color [0.9 1 0.7 1.0]}] true)]))
