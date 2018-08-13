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
  "Returns [keys, zone], the latter is in a format
   that codebox/select-our-who-has understands.
   Rounds right."
  (let [comps (:components s) boxes (filterv #(= (:type (get comps %)) :codebox) (keys comps))
        boxes1 (filterv #(= (first (:path (get comps %))) filename) boxes)
        sp-tuples (multisync/string-path+ comps filename) ; ordered.
        n (count sp-tuples) ; Look for paths that are in our range:
        ix-p (loop [nb4 0 ix 0]
               (if (= ix n) false
                   (let [s-p (nth sp-tuples ix) nc (count (first s-p))]
                     (if (> (+ nb4 nc) real-char-ix) [(- real-char-ix nb4) (second s-p)]
                         (recur (+ nb4 nc) (inc ix))))))]
    (if ix-p
      (let [path (into [] (butlast (second ix-p)))
            boxes2 (filterv #(= (:path (get comps %)) path) boxes1)]
        [boxes2 [(last (second ix-p)) (first ix-p)]]) [[] [0 0]])))

(defn codebox-keys [comps fname] (filterv #(and (= (first (:path (get comps %))) fname) (= (:type (get comps %)) :codebox)) (keys comps)))

(defn cursor-locate [s k]
  "Returns the [filename, char-ix within file] of the cursor given a k."
  (let [comps (:components s) comp (get comps k) 
        _ (if (nil? comps) (throw (Exception. "Bad state")))
        _ (if (nil? comp) (throw (Exception. (str k " doesn't exist within the :components"))))
        _ (if (not= (:type comp) :codebox) (throw (Exception. "Not a codebox")))
        filename (first (:path comp))
        sp-tuples (multisync/string-path+ comps filename)
        nums (mapv #(count (first %)) sp-tuples)
        ij (codebox/cursor-to-real-string comp)
        stop-path (conj (:path comp) (first ij)) ; Add up all paths b4 this.
        contrib-b4 (loop [acc 0 ix 0]
                     (let [ph (get-in sp-tuples [ix 1])]
                       (if (and ph (not= ph stop-path))
                         (recur (+ acc (nth nums ix)) (inc ix))
                         acc)))]
    [filename (+ contrib-b4 (second ij))]))

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
  "Loads fname from s, not the disk."
  (let [string-pathP-tuples (multisync/string-path+ (:components s) fname)]
    (apply str (mapv first string-pathP-tuples))))

(defn save-cache [s fname new-string]
  "Saves new-string to the file fname within s, not the disk."
  (let [_ (if (not (string? new-string)) (throw (Exception. "The string to save as must be a string.")))
        comps (:components s) string-pathP-tuples (multisync/string-path+ comps fname)
        
        codeboxks (codebox-keys comps fname)
        path-to-kys (reduce (fn [m k] (let [p (:path (get comps k))] ; to vector of keys.
                                        (update m p #(if % [k] (conj % k))))) {} codeboxks)
        
        nb4s (zipmap (mapv second string-pathP-tuples) (reductions + 0 (mapv #(count (first %)) string-pathP-tuples)))
        old-string (apply str (mapv first string-pathP-tuples))
        edits (stringdiff/edits-between old-string new-string)

        comps1 (reduce 
                 (fn [acc p] 
                   (let [ks (get path-to-kys p) real-s (codebox/real-strings (get comps (first ks)))
                         nchars (mapv count real-s) nr (count real-s)
                         p+s (mapv #(conj p %) (range nr)) ix0s (mapv #(get nb4s %) p+s)
                         ix1s (mapv + ix0s nchars)
                         editss (mapv #(stringdiff/window-edits edits %1 %2 (= %3 (dec nr))) ix0s ix1s (range nr))
                         update1 (fn [acci k]
                                   (assoc acci k
                                     (reduce #(codebox/apply-edits-to-real-string
                                                %1 (nth editss %2) %2) (get acci k) (range nr))))]
                     (reduce update1 acc ks))) comps (keys path-to-kys))]
    ;(println "edits are:" (pr-str edits))
    ;(println "new string vv:" new-string "new string redux:" (open-cache (assoc s :components comps1) fname))
    (if (not= (open-cache (assoc s :components comps1) fname) new-string) (throw (Exception. "Bug in multicomp/save-cache"))) ; TODO: DEBUG remove when trusted.
    (assoc s :components comps1)))

(defn who-is-open [s]
  "Which files are open in our codeboxes."
  (set (mapv #(first (:path %)) (filterv #(= (:type %) :codebox) (vals (:components s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DISK based file handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn load-from-file [comps filename]
  "Returns a component. It will copy a component if one is already open, to ensure agreement in exported stuff.
   This means be careful with external modifications to the disk."
  (let [kys (filterv #(= (first (:path (get comps %))) filename) (keys comps))]
    (if (= (count kys) 0) ; first component.
      (let [txt (jfile/open filename)]
        (if (not txt) (throw (Exception. (str  "Attempted to load non-existant file: " filename))))
        ; The entire fname goes into one path:
        (assoc (codebox/from-text txt :clojure) :path [filename]))
      (let [ky (first (filterv #(= (count (:path (get comps %))) 1) kys))]
        (if ky (assoc (get comps ky) :position [0 0] :size [512 512])
          (throw (Exception. "Missing root component.")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Child expansion and contraction ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn codebox-sibling-exchild [parent-sib export-ix jx0 jx1]
  (codebox/split-real-string parent-sib export-ix jx0 jx1))

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
            s1 (if (= (:type (get comps k-parent)) :codebox)
                 (let [sibs (multisync/twins comps k-parent)
                       export-ix (last (:path new-child)) ; ix on the new parent = last of path of child.
                       str-b4-split (nth (codebox/real-strings comp) export-ix) ; on the old parent.
                       new-strs (codebox/real-strings new-parent)
                       afr-split0 (nth new-strs export-ix) afr-split1 (nth new-strs (inc export-ix))
                       jx0 (count afr-split0) jx1 (- (count str-b4-split) (count afr-split1)) ; indexes on the new string.
                       
                       ; Expanding stuff that contains expanded stuff means that descendents paths get one longer:
                       nexport0 (count (filterv codebox/exported? (:pieces comp0)))
                       nexport1 (count (filterv codebox/exported? (:pieces new-parent)))
                       n-embed-ch (inc (- nexport0 nexport1))
                       p2k (zipmap (mapv #(let [c (get comps %)] (if (= (:type c) :codebox) (:path c) false)) (keys comps)) (keys comps))
                       comps (reduce (fn [cs ix]  
                                       (let [k (p2k (conj (:path comp0) ix))
                                             ksi (apply hash-set (concat [k] (multisync/twins cs k) (multisync/codebox-descendents cs k)))]
                                         (reduce (fn [csj kj] (update-in csj [kj :path] #(conj (into [] (butlast %)) export-ix (- ix export-ix)))) cs ksi))) 
                               comps (range export-ix (+ export-ix n-embed-ch)))
                       
                       ; Siblings of the parents must change in concordance for codeboxes.
                       s1 (assoc s :components 
                            (reduce (fn [acc k] (update acc k #(codebox-sibling-exchild % export-ix jx0 jx1))) comps sibs))
                       
                       ; Expanding above already expanded territory means child paths may need to be changed to make room or due to expanding stuff containing expanded stuff:
                       desc (multisync/codebox-descendents (:components s1) k-parent)
                       bump-ix (count (:path (get comps k-parent)))
                       shift (- 1 n-embed-ch)]
                 (reduce (fn [acc k] (update-in acc [:components k :path bump-ix]
                                       #(if (>= % (+ export-ix n-embed-ch)) (+ % shift) %))) s1 desc)) s)
              s2 (assoc-in s1 [:components k-parent] (assoc new-parent :position (:position comp0)))
              s3 ((:add-component (:layout s2)) s2 (assoc new-child :position pos1 :z (inc (:z new-parent))) k-child)] 
        s3) s)))

(defn contract-child [parent child]
  (assoc ((:contract-child (:interact-fns parent)) (dissoc parent :position) (dissoc child :position)) :position (:position parent)))

(defn contract-child-cb [comps parent-k child-k]
  "Like contract child but if the component is a codebox, also contracts into the twins and adjusts the numbers of remaining children."
  (let [parent (get comps parent-k) child (get comps child-k) comps1 (dissoc comps child-k)]
    (if (= (:type parent) :codebox)
      (let [sibs (apply hash-set (concat [parent-k] (multisync/twins comps parent-k)))]
        (reduce (fn [acc k] (update acc k #(contract-child % child))) comps1 sibs))
      (assoc comps1 parent-k (contract-child (get comps1 parent-k) (get comps1 child-k))))))

(defn contract-all-descendents [comps k]
  "leaf-first contraction."
  (let [ty (:type (get comps k))
        ch (filterv #(= (:type (get comps %)) ty) 
             (if (= ty :codebox) (multisync/codebox-children comps k)
               (multisync/fbrowser-children comps k)))
        comps1 (reduce #(contract-all-descendents %1 %2) comps ch)
        ; Remove twins:
        ch-u (filterv #(get comps1 %) (vals (zipmap (mapv #(:path (get comps1 %)) ch) ch)))
        ch-x (set/difference (apply hash-set ch) (apply hash-set ch-u))
        ; Needed for codeboxes: sort high to low so contracting multiple children works properly:
        ch-u (sort-by #(- (last (:path (get comps1 %)))) ch-u)]
    (reduce #(contract-child-cb %1 k %2) (reduce dissoc comps1 ch-x) ch-u)))

(defn contract-descendents-if-twinless [comps k]
  "leaf-first contraction, if we don't have twins."
  (if (> (count (multisync/twins comps k)) 0) comps
    (contract-all-descendents comps k)))

(defn close-component-noprompt [s kwd]
  "Contracts into the parent(s) if it has parents."
  (let [cs0 (:components s) ty (get-in cs0 [kwd :type])]
    (if (or (= ty :fbrowser) (= ty :codebox)) 
      (let [cs (contract-descendents-if-twinless cs0 kwd)
            doomed (get cs kwd)
            s (assoc-in s [:precompute :desync-safe-mod?] true)
            twins? (> (count (multisync/twins cs kwd)) 0)
            
            cs1 (dissoc cs kwd)
            
            s (assoc s :components cs1) 
            parent-ks (filterv #(= ty (:type (get cs %))) 
                        (cond (= ty :codebox) (multisync/codebox-padres cs kwd)
                          (= ty :fbrowser) (multisync/fbrowser-padres cs kwd) :else []))]
        (if (or twins? (= (count parent-ks) 0)) s ; don't contract the child unless it is the last one remaining.
          (let [s1 (reduce #(assoc-in %1 [:components %2] (contract-child (get cs %2) doomed)) s parent-ks)
                s2 (if (= (:type doomed) :codebox)
                     (let [ph (:path doomed)
                           leaf-ix (last ph) desc (multisync/codebox-descendents cs1 (first parent-ks))
                           p-ix (dec (count ph))]
                       (reduce (fn [acc k] (update-in acc [:components k :path p-ix] #(if (> % leaf-ix) (dec %) %))) s1 desc)) s1)]
            s2))) (assoc s :components (dissoc cs0 kwd)))))

(defn close-component [s kwd]
  "Prompts the user if there are modified files open and the last codebox of a given type is open.
   Closes will fail if the user clicks cancel."
  (let [comp (get (:components s) kwd) ph (:path comp)]
    (cond (and (= (:type comp) :codebox) (= (count ph) 1) ; Closing the last open dialogue box.
            (= (count (multisync/twins (:components s) kwd)) 0))
      (let [fname (first (:path comp))
            txt0 (if (jfile/exists? fname) (jfile/open fname))
            txt1 (open-cache s fname)]
        (if (not= txt0 txt1)
          (let [opt (warnbox/yes-no-cancel? (str "Save file before closing? " fname))]
            (cond (= opt :yes) 
              (do (jfile/save!!! fname txt1)
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
