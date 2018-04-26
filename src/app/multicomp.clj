; Multiple component manager.

(ns app.multicomp
  (:require 
    [clojure.set :as set] [clojure.string :as string]
    [app.singlecomp :as singlecomp]
    [app.codebox :as codebox]
    [app.xform :as xform]
    [app.multisync :as multisync]
    [app.stringdiff :as stringdiff]
    [app.fbrowser :as fbrowser]))

;;;;;;;;;;;;;;;;; Layouts ;;;;;;;;;;;;;;;;;
; the :position of the top-left corner and the :size.
; TODO: refactor layouts to another folder and improve it.

(defn unique-z [components]
  "Assigns each component a unique z-value."
  (loop [acc (zipmap (keys components) (mapv #(if (:z %) % (assoc % :z 1)) (vals components)))]
    (if (= (count (apply hash-set (mapv :z (vals acc)))) (count acc)) acc
      (recur (zipmap (keys acc) (mapv (fn [c] (update c :z #(+ (* % (+ 1 (* (Math/random) 1e-10))) 1e-100))) (vals acc)))))))

(defn grid-layout [components]
  (let [components (unique-z components) ; just in case.
        vs (vals components) n (count vs)
        sizexs (mapv #(first (:size %)) vs)
        sizeys (mapv #(second (:size %)) vs)
        margin 3 wrap (max 2 (Math/round (Math/sqrt n)))
        setpos (fn [comp x y] (assoc comp :position [x y]))
        xinit 2 yinit 17]
    (loop [acc [] ix 0 xused xinit ymax yinit y0 yinit]
      (if (= ix n) (zipmap (keys components) acc)
        (let [nxtw? (= (mod (dec ix) wrap) 0)
              sx (nth sizexs ix) sy (nth sizeys ix)]
          (recur (conj acc (setpos (nth vs ix) xused y0))
            (inc ix) (if nxtw? 0 (+ xused margin sx))
            (if nxtw? 0 (max ymax (+ margin sy)))
            (if nxtw? (+ ymax y0) y0)))))))

(defn new-position [components pos sz]
  ; It should try to find room. This can be improved quite a bit.
  [(+ (first pos) 610) (+ (second pos) 510)])

;;;;;;;;;;;;;;;;; Child expansion and contraction ;;;;;;;;;;;;

(defn codebox-sibling-exchild [parent-sib export-ix jx0 jx1]
  (codebox/split-real-string parent-sib export-ix jx0 jx1))

(defn expand-child [k-parent k-child mevt-c s]
  "Expands a child if possible."
  (let [comp0 (get (:components s) k-parent) comp (dissoc comp0 :position)
        int-fns (:interact-fns comp)
        s (assoc-in s [:precompute :desync-safe-mod?] true)

        expandable? (:expandable? int-fns)
        mevt (xform/xevt (xform/x-1 (singlecomp/pos-xform (:position comp0))) mevt-c)
        marker (gensym "xyz")
        x (if (expandable? mevt comp) 
            ((:expand-child int-fns) mevt marker comp))]
    (if x 
      (let [new-parent (first x) new-child (second x)
            new-parent (if (= (:type new-parent) :fbrowser) ; add :fname-gui to know to change the children's path upon change.
                         (let [ix (first (filterv #(= (:export-marker (get-in new-parent [:pieces %])) marker) (range (count (:pieces new-parent)))))]
                           (assoc-in new-parent [:pieces ix :fname-gui] (:path new-child))) new-parent)
            comps (:components s)
            ;pos1 (new-position comps (:position comp0) (:size new-child))
            pos1 (mapv #(+ %1 (* %2 0.75)) (:position comp0) (:size comp0))
            s1 (if (= (:type (get comps k-parent)) :codebox)
                 (let [sibs (multisync/twins comps k-parent)
                       export-markers1 (mapv #(get-in new-parent (conj % :export-marker)) (codebox/uspaths-with-export new-parent))
                       export-ix (first (filter #(= (nth export-markers1 %) marker) (range (count export-markers1)))) ; ix on the new parent = last of path of child.
                       str-b4-split (nth (codebox/real-strings comp) export-ix) ; on the old parent.
                       new-strs (codebox/real-strings new-parent)
                       afr-split0 (nth new-strs export-ix) afr-split1 (nth new-strs (inc export-ix))
                       jx0 (count afr-split0) jx1 (- (count str-b4-split) (count afr-split1)) ; indexes on the new string.
                       s1 (assoc s :components ; all parents modified.
                            (reduce (fn [acc k] (update acc k #(codebox-sibling-exchild % export-ix jx0 jx1))) comps sibs))
                       desc (multisync/descendents (:components s1) k-parent)
                       bump-ix (count (:path (get comps k-parent)))]
                 (reduce (fn [acc k] (update-in acc [:components k :path bump-ix]
                                       #(if (>= % export-ix) (inc %) %))) s1 desc)) s)]
        (-> s1 (assoc-in [:components k-parent] (assoc new-parent :position (:position comp0)))
          (assoc-in [:components k-child] (assoc new-child :position pos1 :z (inc (:z new-parent)))))) s)))

(defn contract-child [parent child]
  (assoc ((:contract-child (:interact-fns parent)) (dissoc parent :position) (dissoc child :position)) :position (:position parent)))

(defn contract-child-cb [comps parent-k child-k]
  "Like contract child but if the component is a codebox, also contracts into the twins."
  (let [parent (get comps parent-k) child (get comps child-k) comps1 (dissoc comps child-k)]
    (if (= (:type parent) :codebox)
      (let [sibs (multisync/twins comps parent-k)]
        (reduce (fn [acc k] (update acc k #(contract-child % child))) comps1 sibs))
      (assoc comps1 parent-k (contract-child parent-k child-k)))))

(defn contract-descendents [comps k]
  "leaf-first contraction, if we don't have twins."
  (if (> (count (multisync/twins comps k)) 0) comps
    (let [ch (filterv #(= (:type (get comps %)) (:type (get comps k))) (multisync/children comps k))
          comps1 (reduce #(contract-descendents %1 %2) comps ch)]
      (reduce #(contract-child-cb %1 k %2) comps1 ch))))

(defn close-component [s kwd]
  "Contracts into the parent(s) if it has parents."
  (let [cs0 (:components s) ty (get-in cs0 [kwd :type])]
    (if (or (= ty :fbrowser) (= ty :codebox)) 
      (let [cs (contract-descendents cs0 kwd)
            doomed (get cs kwd)
            s (assoc-in s [:precompute :desync-safe-mod?] true)
        
            cs1 (dissoc cs kwd)
            twins? (> (count (multisync/twins cs kwd)) 0)
            s (assoc s :components cs1) 
            parent-ks (filterv #(= ty (:type (get cs %))) (multisync/padres cs kwd))] ; fbrowser -> codebox connections not handled here, they are handled elsewhere.
        (if (or twins? (= (count parent-ks) 0)) s ; don't contract the child unless it is the last one remaining.
          (let [s1 (reduce #(assoc-in %1 [:components %2] (contract-child (get cs %2) doomed)) s parent-ks)
                s2 (if (= (:type doomed) :codebox)
                     (let [ph (:path doomed)
                           leaf-ix (last ph) desc (multisync/descendents cs1 (first parent-ks))
                           p-ix (dec (count ph))]
                       (reduce (fn [acc k] (update-in acc [:components k :path p-ix] #(if (> % leaf-ix) (dec %) %))) s1 desc)) s1)]
            s2))) (assoc s :components (dissoc cs0 kwd)))))

;;;;;;;;;;;;;;;;;;;; Getting and setting files ;;;;;;;;;;;;;;;

(defn wrap-tree [paths]
  "Only used for updating the filetree, it would be nice to refactor this away."
  (let [paths (mapv fbrowser/vec-file paths)
        children (apply hash-set (mapv first paths))
        out {:text (first (first paths))}]
    (if (> (count children) 0)
      (let [paths-uproot (filterv #(> (count %) 0) (apply hash-set (mapv #(into [] (rest %)) paths)))
            ; map from first key to other paths:
            paths-uprootm (reduce (fn [acc k] (update acc (first k) #(if % (conj % k) [k]))) {} paths-uproot)]
        (assoc out :children
          (mapv wrap-tree (vals paths-uprootm)))))))

(defn get-filelist [s old?]
  (let [comps (:components s)
        fbrowserk (filterv #(= (:type (get comps %)) :fbrowser) (keys comps))
        filepath2elem (apply merge (mapv #(fbrowser/unwrapped-tree (get comps %)) fbrowserk))]
    (if old? (mapv :fullname0 (vals filepath2elem)) (mapv #(apply str (interpose "/" %)) (keys filepath2elem)))))

(defn new2?old-files [s]
  "map from new to old files, both fullpath. Nil values mean no old files."
  (zipmap (get-filelist s false) (get-filelist s true)))

(defn set-filetree [s tree reset-fullname0s?]
  "tree is indexes with :children for folders and :text for the filename."
  (let [tmpk (gensym "reference") fb (fbrowser/new-fbrowser tree) ; sync to a temp component.
        comps1 (dissoc (multisync/iterative-sync (:components s) (assoc (:components s) tmpk fb)) tmpk)]
    (assoc s :components (if reset-fullname0s? (zipmap (keys comps1) (mapv fbrowser/reset-fullname0s (vals comps1))) comps1))))

(defn codebox-keys [comps fname] (filterv #(and (= (first (:path (get comps %))) fname) (= (:type (get comps %)) :codebox)) (keys comps)))

(defn get-filetext [s fname]
  (let [string-pathP-tuples (multisync/string-path+ (:components s) fname)]
    (apply str (mapv first string-pathP-tuples))))

(defn set-filetext [s fname new-string]
  (let [comps (:components s) string-pathP-tuples (multisync/string-path+ comps fname)
        
        codeboxks (codebox-keys comps fname)
        path-to-kys (reduce (fn [m k] (let [p (:path (get comps k))] ; to vector of keys.
                                        (update m p #(if % [k] (conj % k))))) codeboxks)
                                
        nb4s (zipmap (mapv second string-pathP-tuples) (reduce + 0 (mapv #(count (first %)) string-pathP-tuples)))
        old-string (apply str (mapv first string-pathP-tuples))
        edits (stringdiff/edits-between old-string new-string)
        
        comps1 (reduce 
                 (fn [acc p] 
                   (let [ks (get path-to-kys p) real-s (codebox/real-strings (get comps (first ks)))
                         nchars (mapv count real-s) nr (count real-s)
                         p+s (mapv #(conj p %) (range nr)) ix0s (mapv #(get nb4s %) p+s)
                         ix1s (mapv + ix0s nchars)
                         editss (mapv #(stringdiff/window-edits edits %1 %2 (= %3 nr)) ix0s ix1s (range nr))
                         acc1 (fn [acc k]
                                (reduce #(codebox/apply-edits-to-real-string
                                           %1 (nth editss %2) %2)) (get acc k) (range nr))]
                     (reduce acc1 acc ks))) (keys path-to-kys))]
    
    (if (not= (get-filetext (assoc s :components comps1) fname) new-string) (throw (Exception. "Bug in multicomp/set-filetext"))) ; TODO: DEBUG remove when trusted.
    (assoc s :components comps1)))

;;;;;;;;;;;;;;;;; Rendering ;;;;;;;;;;;;

(defn which-tool-hud [s]
  (let [tool (if-let [m (:active-tool s)] (:name m) :OOPS) typing? (:typing-mode? s)
        g-cmd [:drawString [(str "tool = " tool " typing? = " typing?) 2 15] {:FontSize 18 :Color [0 1 1 0.7]}]]
    [g-cmd]))

(defn draw-select-box [comps k camera]
  (let [f-comp (get comps k) p (:position f-comp) sz (:size f-comp)]
    [(xform/xgfx camera [:drawRect [(dec (first p)) (dec (second p)) (+ (first sz) 2) (+ (second sz) 2)] {:Color [0.9 1 0.7 1.0]}] true)]))