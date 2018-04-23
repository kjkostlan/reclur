; Syncs multiple components
; TODO: there is a rare bug that happens when codebox piece(s) are exported and then
  ; an enclosing piece around them is also exported. May need to edit multicomp to fix or just disable this behavior.
(ns app.multisync
  (:require 
    [clojure.set :as set] [clojure.string :as string]
    [app.singlecomp :as singlecomp]
    [app.codebox :as codebox]
    [app.xform :as xform]
    [app.rtext :as rtext]
    [app.stringdiff :as stringdiff]
    [app.fbrowser :as fbrowser]))

;;;;;;;;;;;;;;;;; Simple collection stuff ;;;;;;;;;;;;;;;;;

(defn ?conj [v x] (if v (conj v x) [x]))

(defn nil-assert [comps msg] 
  (filterv #(if (not (get comps %)) (throw (Exception. (str msg)))) (keys comps)))

(defn comp-eq? [c0 c1]
  (= (:pieces c0) (:pieces c1)))
(defn comps-eq? [cs0 cs1]
  (or (= cs0 cs1)
    (= (mapv :pieces (vals cs0))
       (mapv :pieces (vals cs1)))))

(defn ty? [comp]
  (or (= (:type comp) :fbrowser) (= (:type comp) :codebox)))

(defn filterk [comps kys]
  "Keeps kys that are in comps"
  (let [kys (if (set? kys) kys (apply hash-set kys))]
    (reduce #(if (contains? comps %2) %1 (disj %1 %2)) kys kys)))

;;;;;;;;;;;;;;;;; Family functions ;;;;;;;;;;;;;

(defn get-uspaths [comp exported-only?] "In order for codebox. Includes the :fname-gui fbrowser codebox linkage."
  (cond (and exported-only? (= (:type comp) :codebox))
    (codebox/uspaths-with-export comp)  
    (= (:type comp) :codebox)
    (codebox/splay-out comp)
    (and exported-only? (= (:type comp) :fbrowser))
    (let [all-p (keys (fbrowser/all-paths comp))] 
      (filterv #(let [piece (get-in comp %)] (or (:export-marker piece) (:fname-gui piece))) all-p))
    (= (:type comp) :fbrowser)
    (into [] (keys (fbrowser/all-paths comp))
    :else (throw (Exception. "Only :fbrowser and :codebox is supported or needed for synching.")))))

(defn export-markers [comp] "same order as (get-uspaths comp true). Includes the :fname-gui"
  (mapv #(let [p (get-in comp %)]
           (if-let [m (:export-marker p)] m (:fname-gui p))) (get-uspaths comp true)))

(defn export-paths [comp] "Same order as export-markers."
  (cond (= (:type comp) :codebox)
    (let [n (count (export-markers comp)) ph (:path comp)]
      (mapv #(conj ph %) (range n)))
    (= (:type comp) :fbrowser)
    (let [us2disk (fbrowser/all-paths comp) usp (get-uspaths comp true)] 
      ; devec connections:
      (mapv #(let [disk-vec (get us2disk %)]
               (if (:export-marker (get-in comp %)) disk-vec [(fbrowser/devec-file disk-vec)])) usp))
    :else (throw (Exception. "Only :fbrowser and :codebox is supported or needed for synching."))))

(defn children [comps k]
  "Only one level. Includes fbrowser -> codebox connections, where the path devectorizes."
  (let [c (get comps k) ty (:type c) 
        chpaths (apply hash-set (export-paths c))]
    (filterv #(let [ci (get comps %)]
                (or (and (= (:type ci) ty)
                      (get chpaths (:path ci)))
                  (and (= (:type c) :fbrowser)
                    (= (:type ci) :codebox)
                    (= (count (:path ci)) 1)
                    (= (first (:path ci)) (fbrowser/devec-file (:path c))))))
     (keys comps))))

(defn descendents [comps k]
  "Multible levels."
  (let [ch (children comps k)]
    (into [] (apply concat ch (mapv #(descendents comps %) ch)))))

(defn padres [comps k]
  "Only one level, multiple parents when they are copied around, etc. Included fbrowser -> codebox connections."
  (let [c (get comps k) ty (:type c) ph (:path c)]
    (if (not c) (throw (Exception. (str "nil component for key:" k))))
    (if (not (:type c)) (throw (Exception. "Somewhere the :type was vanished.")))
    (filterv #(let [ci (get comps %) phi (:path ci)]
                (or
                  (and (= (:type ci) ty :fbrowser) ; fbrowser -> fbrowser
                    (< (count phi) (count ph))
                    (= phi (subvec ph 0 (count phi)))
                    (let [exp-paths (apply hash-set (export-paths ci))]
                      (contains? exp-paths ph)))
                  (and (= (:type ci) ty :codebox) ; codebox -> codebox.
                    (= (count phi) (dec (count ph))) 
                    (= phi (subvec ph 0 (count phi))))
                  (and (= (:type ci) :fbrowser) ; codebox -> fbrowser
                    (= (:type c) :codebox)
                    (= (count (:path c)) 1)
                    (= (first (:path c)) (fbrowser/devec-file (:path ci))))))
     (keys comps))))

(defn twins [comps k]
  "Twins have the same :type and :path. Does not include ourselves."
  (let [c (get comps k) ty (:type c) ph (:path c)]
    (filterv #(and (= (:path (get comps %)) ph) (= (:type (get comps %)) ty) (not= % k)) (keys comps))))

;;;;;;;;;;;;;;;;; :export-marker handling ;;;;;;;;;;;;;;;
; I tried simply setting the export marker to the key of components, it didn't work well when components were copied, so this extra complexity seems hard to avoid.

(defn path-old2paths-new [comp-old comp-new]
  "Multiple paths in case of pasted text, or [] if the path is deleted."
  (let [path2marker-old (zipmap (export-paths comp-old) (export-markers comp-old)) ; export-markers for the OLD should be unique.
        expm-new (export-markers comp-new) expp-new (export-paths comp-new)
        marker2paths-new (reduce (fn [acc ix] (update acc (nth expm-new ix) #(?conj % (nth expp-new ix)))) {} 
                           (range (count expm-new)))]
    (reduce #(if-let [phs (get marker2paths-new (get path2marker-old %2))] 
               (assoc %1 %2 phs) (assoc %1 %2 [])) {} (keys path2marker-old))))

(defn unique-markers [comp]
  "Run this AFTER updating any children. Only :export-markers are uniqueified, not the :fname-gui pieces."
  (let [uspaths (get-uspaths comp true) markers (export-markers comp)
        markers1 (first (reduce (fn [acc m] [(conj (first acc) (if (get (second acc) m) (gensym "pastetext") m))
                                             (conj (second acc) m)]) [[] #{}] markers))]
    (if (= markers markers1) comp              
      (reduce #(let [uspath (nth uspaths %2)]
                 (if (:export-marker (get-in %1 uspath))
                   (assoc-in %1 (conj (nth uspaths %2) :export-marker) (nth markers1 %2)) %1))
        comp (range (count uspaths))))))

(defn reset-fname-gui [box codebox-files]
  "Run this AFTER updating any children, Resets the fnamegui to match the path."
  (if (= (:type box) :fbrowser)
    (let [us2disk (fbrowser/all-paths box)]
      (reduce #(if (:fname-gui (get-in %1 %2)) 
                 (let [file (fbrowser/devec-file (get us2disk %2))]
                   (if (not (get codebox-files file)) ; we no longer have the codebox open, so remove :fname-gui
                     (assoc-in %1 %2 (dissoc (get-in %1 %2) :fname-gui))
                     (assoc-in %1 (conj %2 :fname-gui) file))) %1) box (keys us2disk))) box))

;;;;;;;;;;;;;;;;; Twin codebox and partially-twin fbrowser updating ;;;;;;;;;;;;;


(defn filepath2export?-1 [box]
  "Map of filepaths to whether the comp is exported"
  (if (= (:type box) :fbrowser) 
    (let [us2disk (fbrowser/all-paths box)]
      (zipmap (vals us2disk) (mapv #(:exported? (get-in box %)) (keys us2disk))))
    {}))

(defn filepath2export? [comps kys] "Multi version, where all have to be exported."
  (let [paths2exs (mapv #(filepath2export?-1 (get comps %)) kys)]
    (reduce (fn [acc p2e] (reduce (fn [acci p] (update acci p #(and % (get p2e p)))) acc (keys p2e))) {} paths2exs)))

(defn _hard-miss? [fpath2export? p]
  "Non-exported deleted p."
  (and (not (contains? fpath2export? p))
    (let [pr (reductions conj [] p) ; stubs of increasing sizes.
          p0 (last (filterv #(contains? fpath2export? %) pr))]
      (and p0 (not (get fpath2export? p0))))))

(defn compute-fbrowser-diffs [box fpath2export?]
  "fpath2export? is on across all components that may have changed."
  (let [disk (apply hash-set (vals (fbrowser/all-paths box)))
        us2disk (fbrowser/all-paths box) ;disk2us (zipmap (vals us2disk) (keys us2disk))
        disk2val (zipmap (vals us2disk) (mapv #(let [x (get-in box %)] {:folder? (fbrowser/folder? x) :fullname0 (:fullname0 x)}) (keys us2disk)))
        ; fbrowser automatically ignores additions that happen to exported paths, so extra additions here wouldn't hurt us.
          ; But their removal makes it easier to reason about and debug.

        us-fpath2export? (filepath2export?-1 box)

        news (mapv #(vector :add % (get disk2val %))
               (filterv #(_hard-miss? us-fpath2export? %)
                 (reduce (fn [acc p]
                           (if (contains? disk p) acc (conj acc p))) [] (keys fpath2export?))))
        dels (mapv #(vector :remove %) 
               (reduce (fn [acc p]
                         (if (_hard-miss? fpath2export? p) (conj acc p) acc)) [] disk))]
    (into [] (concat dels news))))

(defn sync-fbrowsers [comps-new kys-diff]
  (if (= (count (filterv #(= (:type (get comps-new %) :fbrowser)) kys-diff)) 0) comps-new
    (let [gttr #(if (and % (= (:type %) :codebox)) ((:unwrapped-tree (:interact-fns %)) %) [])

          path2ex?-from-diffs (filepath2export? comps-new kys-diff)]
      (reduce (fn [acc k]
                (let [box (get acc k)]
                  (if (= (:type box) :fbrowser)
                    (let [diffs (compute-fbrowser-diffs box path2ex?-from-diffs)
                          box1 ((:implement-diffs (:interact-fns box)) box diffs)]
                      (assoc acc k box1)) acc))) comps-new (keys comps-new)))))
(defn _fnameguifiles [box]
  (let [us2disk (fbrowser/all-paths box) disk2us (zipmap (vals us2disk) (keys us2disk))]
    (filterv #(:fname-gui (get-in box (get disk2us %))) (keys disk2us))))
(defn spread-fname-gui [comps-new kys-diff]
  "Makes sure all fbrowsers have the :fname-gui key."
  (let [fb? #(= (:type (get comps-new %)) :fbrowser) fbks (filterv fb? kys-diff)
        fname-guis (apply hash-set (apply concat (mapv #(_fnameguifiles (get comps-new %)) fbks)))]
    (if (= (count fbks) 0) comps-new
      (reduce (fn [acc k] 
                (let [box (get acc k) us2disk (fbrowser/all-paths box) disk2us (zipmap (vals us2disk) (keys us2disk))
                      box1 (reduce #(if-let [usp (get disk2us %2)] (do #_(println "spread fname-gui: " usp %2) (assoc-in %1 (conj usp :fname-gui) %2)) %1) 
                             box fname-guis)] 
                  (assoc acc k box1)))
        comps-new (filterv fb? (keys comps-new))))))

(defn single-codebox-sync [leader-twin follower-twin]
  "Codeboxes are a bit easier than fbrowsers since only twins will need synching."
  (if (comp-eq? leader-twin follower-twin) follower-twin
    (let [rs-gold (codebox/real-strings leader-twin)
          rs (codebox/real-strings follower-twin) n (count rs)]
      (if (= n (count rs-gold))
        (reduce #(let [eds (stringdiff/edits-between (nth rs %2) (nth rs-gold %2))]
                   (codebox/apply-edits-to-real-string %1 eds %2)) follower-twin (range n))
        ; Less common, occurs when the user copies removes a child piece,
        ; Not ideal just to steamroll everything, TODO: improve this.
        (let [follower1 (assoc follower-twin :pieces (:pieces leader-twin) :precompute (:precompute leader-twin))
              vis-edits (stringdiff/edits-between (rtext/rendered-string follower-twin) (rtext/rendered-string follower1))]
          (rtext/cursor-scroll-update follower-twin follower1 vis-edits))))))

(defn sync-codeboxes [comps-new kys-diff]
  (let [comps-new1 (reduce (fn [acc k]
                             (let [twns (twins acc k) c (get acc k)]
                               (reduce #(assoc %1 %2 (single-codebox-sync c (get %1 %2))) acc twns))) 
                     comps-new (filterk comps-new kys-diff))] comps-new1))

(defn total-sibling-update [comps-new kys-diff]
  "Makes sure comps-new is self-consistent, i.e. twins have the same content."
  (let [getty (fn [ty comps] (let [kys (filterv #(= (:type (get comps %)) ty) (keys comps))]
                               (zipmap kys (mapv #(get comps %) kys))))
        tys (apply hash-set (mapv :type (vals comps-new)))]
        ; Only the fbrowser syncs, the codebox has a seperate sync:
   (apply merge 
     (mapv #(cond (= % :fbrowser) (sync-fbrowsers (getty % comps-new) kys-diff)
              (= % :codebox) (sync-codeboxes (getty % comps-new) kys-diff)
              :else (getty % comps-new)) tys))))

;;;;;;;;;;;;;;;;; Descendent updating (only changes the :path, or copies or deletes comps) ;;;;;;;;;;;;;;;;

(defn also-delete-descendents [comps k-gone]
  "Comps must NOT have k-gone removed just yet."
  (reduce dissoc (dissoc comps k-gone) (descendents comps k-gone)))

(defn also-repath-descendents [comps k-changedpath new-path]
  "Comps must NOT have the path changed just yet."
  (let [comp (get comps k-changedpath) ph (:path comp) n (count ph)
        fb? (= (:type comp) :fbrowser)
        
        repath (fn [box] (update box :path #(if (and fb? (= (:type box) :codebox)) (into [] (fbrowser/devec-file new-path) (rest %)) 
                                              (into [] (concat new-path (subvec % n))))))]
    (reduce (fn [acc k] (update acc k repath)) 
      (assoc-in comps [k-changedpath :path] new-path)
      (descendents comps k-changedpath))))

(defn shift-copies [comps target-ks]
  "Shifts so they arent perfectly on top of eachother. The first copy isnt a copy"
  (let [target-ks (sort-by #(:path (get comps %)) target-ks) ; some reasonable sort.
        pos0 (:position (get comps (first target-ks))) pix 20 n (count target-ks)]
    (reduce (fn [acc ix] (update acc (nth target-ks ix) 
                           #(assoc % :position [(+ (first pos0) (* pix ix)) (+ (second pos0) (* pix ix))])))
      comps (range n))))

(defn make-copies-with-descendents [comps target-k new-paths]
  "Only adds copies when (count new-paths) > 1"
  (let [packets (mapv (fn [new-ph]
                        (let [comps1 (also-repath-descendents comps target-k new-ph)
                              ofks1 (conj (descendents comps1 target-k) target-k)]
                          (zipmap ofks1 (mapv #(get comps1 %) ofks1)))) new-paths)
        n (count packets)
        kys (keys (first packets))
        groups (mapv (fn [k] (let [k1s (conj (mapv (fn [_] (gensym "rabbit")) (range (dec n))) k)]
                               (shift-copies (zipmap k1s (mapv #(get % k) packets)) k1s))) kys)]
    (apply merge comps groups)))

(defn update-descendents [comps-old comps-new k]
  "The effects of changing the comp assoc with k from comps-old to comps-new. k must be in both comps-old and comps-new"
  (let [comp-old (get comps-old k) comp-new (get comps-new k)]
    (if (comp-eq? comp-old comp-new) comps-new
      (let [path-old2news (path-old2paths-new comp-old comp-new)
            ; not sure if can-del? is needed, but seems safer given unique-markers changes the markers in the copy-paste case.
            can-del? (= (count (set/difference (apply hash-set (export-markers comp-new)) (apply hash-set (export-markers comp-old)))) 0)]
        ; Dont use descendents, use path-old2news, since the comps-new can break the descendents path. 
        (reduce (fn [acc ki]
                  (let [comp-new (get acc ki)]
                    (if comp-new ; sometimes the comp is deleted by an earlier change.
                      (let [new-paths (get path-old2news (:path comp-new))
                            np (if new-paths (count new-paths) 0)]
                        (cond (and can-del? new-paths (= np 0)) ; empty means delete, nil means leave alone.
                          (also-delete-descendents acc ki)
                          (= np 0) acc
                          :else (make-copies-with-descendents acc ki new-paths)))
                       acc)))
          comps-new (filterv #(ty? (get comps-new %)) (keys comps-new)))))))

(defn total-descendent-update [comps-old comps-new kys-diff]
  "kys-diff is a superset of changed, added, and deleted elements."
  (if (comps-eq? comps-old comps-new) comps-new
    (let [kys-mod-del (apply hash-set (filterv #(let [old (get comps-old %) new (get comps-new %)]
                                                  (and (not (comp-eq? old new))
                                                    (or (= (:type new) :fbrowser) (= (:type new) :codebox))))
                                        (keys comps-old)))
          comps-new1 (reduce (fn [acc k]
                       (if (get acc k) (update-descendents comps-old acc k)
                         (also-delete-descendents (assoc acc k (get comps-old k)) k))) comps-new kys-mod-del)
          codebox-files (apply hash-set (reduce #(if (= (:type %2) :codebox) (conj %1 (first (:path %2))) %1) [] (vals comps-new1)))
          comps-new2 (reduce (fn [acc k] (update acc k #(reset-fname-gui % codebox-files)))
                       comps-new1 (filterk comps-new1 kys-diff))
          comps-new3 (reduce (fn [acc k] (update acc k #(unique-markers %)))
                       comps-new2 (filterk comps-new2 kys-diff))]
      comps-new3)))

;;;;;;;;;;;;;;;;; Line number sync ;;;;;;;;;;;;;;;;;

(defn string-path+ [comps fname]
   "Ordered [string path], where path extends one beyond the :path with an indexed of 0,1,2,... number of real strings-1
    If multible codeboxes have the same path one is chosen arbitrarily, the choice doesn't matter if we are in a consistant state."
   (let [codeboxks (filterv #(and (= (first (:path (get comps %))) fname) (= (:type (get comps %)) :codebox)) (keys comps))
         n (count codeboxks)
         ; only one ky per path:
         codeboxks (loop [acc [] paths #{} ix 0]
                     (if (= ix n) acc
                       (let [k (nth codeboxks ix) p (:path (get comps k))]
                         (if (get paths p) (recur acc paths (inc ix))
                           (recur (conj acc k) (conj paths p) (inc ix))))))
         stringss (mapv #(codebox/real-strings (get comps %)) codeboxks)
         spath+ (apply concat (mapv (fn [s k] (let [p (:path (get comps k))] 
                                                (mapv vector s (mapv #(conj p %) (range (count s)))))) stringss codeboxks))
         formats (fn [p] (apply str (mapv #(if (number? %) (format "%09d" %) %) p)))]
     (into [] (sort-by #(formats (second %)) spath+)))) ; alphabetical order if numbers are padded with zeros.

(defn compute-linenums [comps fname]
  (let [s-pathP-tuples (string-path+ comps fname)
        codeboxks (filterv #(and (= (first (:path (get comps %))) fname) (= (:type (get comps %)) :codebox)) (keys comps))
        path-to-kys (reduce (fn [m k] (let [p (:path (get comps k))] ; to vector of keys.
                                        (update m p #(if % [k] (conj % k))))) {} codeboxks)
        countnl (fn [s] (count (string/split (str "_" s "_") #"\n")))
        nb4s (zipmap (mapv second s-pathP-tuples) (reductions + 0 (mapv #(countnl (first %)) s-pathP-tuples)))]     
    (reduce 
      (fn [acc p]
        (let [ks (get path-to-kys p) real-s (codebox/real-strings (get comps (first ks)))
              nr (count real-s) b4s (mapv #(get nb4s (conj p %)) (range nr))
              b4s (mapv #(if (= %1 0) 0 (- %1 %2 1)) b4s (range))
              acc1 (fn [acc k] (update acc k #(codebox/update-lineno-info % b4s)))]
          (reduce acc1 acc ks))) comps (keys path-to-kys))))

(defn sync-line-nums [comps-old comps-new]
  "using comps-old is just for optimization. Sets :line-num-start and :hidden-nlines."
  (let [kys (keys comps-new)
        kdiffs (filterv #(let [c0 (get comps-old %) c1 (get comps-new %)] 
                           (and (= (:type c1) :codebox) (not (comp-eq? c0 c1)))) kys)
        fnames (apply hash-set (mapv #(first (:path (get comps-new %))) kys))]
    (reduce compute-linenums comps-new fnames)))

;;;;;;;;;;;;;;;;; The global update function ;;;;;;;;;;;;;;;

(defn iterative-sync [s0 s1]
  "Not really iterativly, as multible iterations of total-descendent-update cause bugs.
   Not sure the best one to go first."
  (let [get-ky-diff (fn [sA sB] (apply hash-set (filterv #(not (comp-eq? (get (:components sA) %) (get (:components sB) %)))
                                                  (apply hash-set (concat (keys (:components sA)) (keys (:components sB)))))))
        s2 (assoc s1 :components (total-sibling-update (:components s1) (get-ky-diff s0 s1)))
        s3 (assoc s2 :components (total-descendent-update (:components s0) (:components s2) (get-ky-diff s0 s2)))
        s4 (assoc s3 :components (total-sibling-update (:components s3) (get-ky-diff s0 s3)))
        ;s5 (assoc s4 :components (spread-fname-gui (:components s4) (get-ky-diff s0 s4)))
        ] s4))
  #_(let [one-step (fn [s0 s1 kys-diff]
                   (if (comps-eq? (:components s0) (:components s1)) s1
                     (let [c0 (:components s0) c1 (:components s1) _ (nil-assert c0 "has nils to start with") _ (nil-assert c1 "has nils to start with")
                           c2 (total-descendent-update c0 c1 kys-diff) _ (nil-assert c2 "descendent step makes nils")
                           c3 (total-sibling-update c2 kys-diff) _ (nil-assert c3 "sibling step makes nils")] (assoc s1 :components c3))))
        maxsteps 200]
    (loop [old s0 new s1 nsteps 0 kys-diff (get-ky-diff s0 s1)]
      (let [very-new (one-step old new kys-diff) 
            _ (if (= nsteps maxsteps) (throw (Exception. "child-sibling-path-sync likely doesn't converge, component tree-deltas may be bugged.")))]
        (if (= new very-new) (update very-new :components #(sync-line-nums (:components s0) %))
          (recur new very-new (inc nsteps) (set/union kys-diff (get-ky-diff new very-new)))))))

;;;;;;;;;;;;;;;;;;;; Getting and setting files ;;;;;;;;;;;;;;;
