; Multiple component manager.
; Trying not to have too much component-specific code here, but the coupling is just so tight for the structural editor.

(ns app.multicomp
  (:require 
    [clojure.set :as set] [clojure.string :as string]
    [app.codebox :as codebox]
    [layout.xform :as xform]
    globals
    [javac.file :as jfile]
    [javac.warnbox :as warnbox]
    [javac.gfx :as gfx]
    [app.multisync :as multisync]
    [app.stringdiff :as stringdiff]
    [app.fbrowser :as fbrowser]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Searching ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn who-has [s filename real-char-ix]
  "Returns pairs of [key, cursor-ix], empty vector if it can't find anything.
   Convenience function that may not be all that helpful."
  (let [comps (:components s) box-ks (filterv #(= (:type (get comps %)) :codebox) (keys comps))
        box-ks1 (filterv #(= (:path (get comps %)) (fbrowser/vec-file filename)) box-ks)]
    (mapv #(vector % (codebox/real-string-to-cursor (get comps %) real-char-ix -1)) box-ks1)))

(defn codebox-keys [comps fname] (filterv #(and (= (:path (get comps %)) (fbrowser/vec-file fname)) (= (:type (get comps %)) :codebox)) (keys comps)))

(defn cursor-locate [s k]
  "Returns the [filename, char-ix within file] of the cursor given a k."
  (let [comps (:components s) comp (get comps k) 
        _ (if (nil? comps) (throw (Exception. "Bad state")))
        _ (if (nil? comp) (throw (Exception. (str k " doesn't exist within the :components"))))
        _ (if (not= (:type comp) :codebox) (throw (Exception. "Not a codebox")))
        filename (fbrowser/devec-file (:path comp))
        real-char-ix (codebox/cursor-to-real-string comp)]
    [filename real-char-ix]))

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

(defn open-fcache [comps fname]
  "Loads fname from s, not the disk. nil if fname isn't found."
  (let [codeboxks (codebox-keys comps fname)]
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
    (if (not= (open-fcache comps1 fname) new-string) (throw (Exception. "Bug in multicomp/save-cache"))) ; Extra safety double check.
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
  (set (mapv #(fbrowser/devec-file (:path %)) (filterv #(= (:type %) :codebox) (vals (:components s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Child expansion and contraction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn expand-child [k-parent k-child mevt-c s]
  "Expands a child if possible."
  (let [comp0 (get (:components s) k-parent) comp (dissoc comp0 :position)
        s (assoc-in s [:precompute :desync-safe-mod?] true)

        expandable? (:expandable? comp)
        mevt (xform/xevt (xform/x-1 (xform/pos-xform (:position comp0))) mevt-c)
        x (if (expandable? mevt comp) 
            ((:expand-child comp) mevt comp))]
    (if x 
      (let [new-parent (first x) new-child (second x)
            comps (:components s)
            pos1 (mapv #(+ %1 (* %2 0.75)) (:position comp0) (:size comp0))
            s1 (assoc-in s [:components k-parent] (assoc new-parent :position (:position comp0)))
            s2 ((:add-component (:layout s1)) s1 (assoc new-child :position pos1 :z (inc (:z new-parent))) k-child)] 
        s2) s)))

(defn contract-child [parent child]
  (assoc ((:contract-child parent) (dissoc parent :position) (dissoc child :position)) :position (:position parent)))

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

(defn close-component-noprompt [comps kwd]
  "Contracts into the parent(s) if it has parents."
  (let [ty (get-in comps [kwd :type])]
    (if (= ty :fbrowser) 
      (let [cs (contract-descendents-if-twinless comps kwd)
            doomed (get cs kwd)
            twins? (> (count (multisync/twins cs kwd)) 0)
            cs1 (dissoc cs kwd)
            
            parent-ks (filterv #(= ty (:type (get cs %))) 
                        (multisync/fbrowser-padres cs kwd))]
        (if (or twins? (= (count parent-ks) 0)) cs1 ; don't contract the child unless it is the last one remaining.
          (reduce #(assoc %1 %2 (contract-child (get comps %2) doomed)) cs1 parent-ks))) 
      (dissoc comps kwd))))

(defn close-component-vanilla-layout [comps kwd]
  "Prompts the user if there are modified files open and the last codebox of a given type is open.
   Closes will fail if the user clicks cancel."
  (let [comp (get comps kwd)]
    (cond (and (= (:type comp) :codebox) ; Closing the last open codebox.
            (= (count (multisync/twins comps kwd)) 0))
      (let [fname (fbrowser/devec-file (:path comp))
            txt0 (if (jfile/exists? fname) (jfile/open fname))
            txt1 (open-fcache comps fname)]
        (if (not= txt0 txt1)
          (let [opt (warnbox/choice (str "Save file before closing? " fname) [:yes :no :cancel] :yes)]
            (cond (= opt :yes) 
              (do (jfile/save!! fname txt1)
                (close-component-noprompt comps kwd)) ; the cache is stored in the components; closing it removes the cache.
              (= opt :no)
              (close-component-noprompt comps kwd)
              :else comps))
          (close-component-noprompt comps kwd)))
      (and (fbrowser/rootfbrowser? comp) (= (count (filterv fbrowser/rootfbrowser? (vals comps))) 1))
      (do (println "Can't close the last root fbrowser.") comps)
      :else (close-component-noprompt comps kwd))))

(defn close-component [s kwd]
  "Includes the visual layout function."
  (let [clf (:close-components (:layout s)) comps (:components s)
        comps1 (close-component-vanilla-layout comps kwd)
        kys-gone (set/difference (set (keys comps)) (set (keys comps1)))
        s1 (assoc (assoc-in s [:precompute :desync-safe-mod?] true) :components comps1)] ; we handled all synching during the close.
    (if clf 
      (let [s2 (assoc s1 :components (merge comps comps1))
            s3 (clf s2 kys-gone)] ; The comps that were removed are doomed anyway.
        (if (not= (keys (:components s3)) (keys comps1))
          (throw (Exception. "The layout close function didn't behave properly.")))
        s3) 
      s1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rendering ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn which-tool-hud [s]
  (let [tool (if-let [m (:active-tool s)] (:name m) :OOPS) typing? (:typing-mode? s)
        g-cmd [:drawString [(str "tool = " tool " typing? = " typing? (if (globals/are-we-child?) "CHILD VERSION" "")) 2 15] {:FontSize 18 :Color [0 1 1 0.7]}]]
    [g-cmd]))

(defn draw-select-box [comps k camera]
  (let [f-comp (get comps k) p (:position f-comp) sz (:size f-comp)]
    [(gfx/xgfx camera [:drawRect [(dec (first p)) (dec (second p)) (+ (first sz) 2) (+ (second sz) 2)] {:Color [0.9 1 0.7 1.0]}] true)]))
