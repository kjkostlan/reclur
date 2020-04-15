; Syncs multible components.
; There is slightly more codebox-fbrowser duplicate or near duplicate code then necessary, but
; it may make it easier to handle case-specific bugs and easier to read as the individual fns don't have as many branches in them.
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

(defn comp-eq? [c0 c1]
  (= (:pieces c0) (:pieces c1)))

(defn comps-eq? [cs0 cs1]
  (or (= cs0 cs1)
    (= (mapv :pieces (vals cs0))
       (mapv :pieces (vals cs1)))))

(defn map-filter [m f]
  (reduce #(assoc %1 %2 (get m %2)) {} (filterv #(f (get m %)) (keys m))))

(defn ?conj [v x] (if v (conj v x) [x]))

(defn nil-assert [comps msg] 
  (filterv #(if (not (get comps %)) (throw (Exception. (str msg)))) (keys comps)))

;;;;;;;;;;;;;;;;; More specific collection stuff ;;;;;;;;;;;;;;;;;

(defn getty [ty comps] (let [kys (filterv #(= (:type (get comps %)) ty) (keys comps))]
                                  (zipmap kys (mapv #(get comps %) kys))))

(defn uspath2chpath [comp]
  "Map from internal paths within comp (i.e. for use with assoc-in and get-in)
   To the path that a child component would have."
  (let [ty (:type comp)]
    (cond (= ty :fbrowser) (fbrowser/all-paths comp)
      :else (throw (Exception. "uspath2chpath only for fbrowsers.")))))

(defn chpath2uspath [comp]
  (let [us2ch (uspath2chpath comp)]
    (zipmap (vals us2ch) (keys us2ch)))) ; shouldn't have any duplicates.

(defn uspath2chtag [comp]
  (let [u2c (uspath2chpath comp)]
    (map-filter (zipmap (keys u2c) (mapv #(::childtag (get-in comp %)) (keys u2c))) 
      #(and % (> (count %) 0)))))

(defn chk2chpath [comp multi?]
  "Mapping from child keys to child paths, used to update child paths.
   Multi false: If a child key is mapped to multible paths this fn will only return one of them.
   Multi true: Always returns a vector of paths."
  (let [us2ch (uspath2chpath comp) 
        us2ch-kvecs (uspath2chtag comp)
        
        chk2us (reduce (fn [a uspath] ; map from children's keys to the uspaths (for the ix'th component).
                         (let [cks (get us2ch-kvecs uspath)
                               inner-fn (if multi? (fn [acc k] (update acc k #(?conj % uspath))) #(assoc %1 %2 uspath))]
                           (reduce inner-fn a cks)))
                       {} (keys us2ch-kvecs))        
        chk2ch (if multi? (zipmap (keys chk2us) (mapv (fn [k] (mapv #(get us2ch %) (get chk2us k))) (keys chk2us)))
                 (zipmap (keys chk2us) (mapv #(get us2ch (get chk2us %)) (keys chk2us))))
        chk2ch (map-filter chk2ch #(and % (> (count %) 0)))] chk2ch))

(defn update-paths [comps ck2ch-last ck2ch]
  "Also deletes components if they are found in the old and not the new.
   ck2ch-last and ck2ch are the old and new child -> paths maps."
  (reduce (fn [acc k]
            (let [ch-path (get ck2ch k)] ; if the link is still there make sure the path is up-to-date.
              (cond (and (not ch-path) (not (get ck2ch-last k))) ; our path is not in a changed component.
                acc
                (not ch-path) ; our path is no more.
                (dissoc acc k)
                (not= (get-in acc [k :path]) ch-path) ; changed path.
                (assoc-in acc [k :path] (if (= (:type (get acc k)) :codebox) 
                                          (apply str (interpose "/" ch-path)) ch-path)) 
                :else acc)))
    comps (keys comps)))

(defn twins [comps k]
  "Twins have the same :type and :path. Does not include ourselves.
   These family fns are NOT based on the ::childtag, they are inferred
   from the paths and structure within a component."
  (let [c (get comps k) ty (:type c) ph (:path c)]
    (filterv #(and (= (:path (get comps %)) ph) (= (:type (get comps %)) ty) (not= % k)) (keys comps))))

(defn fbrowser-children [comps k]
  "The parent fbrowser must contain an exported child to count,
   just bieng up the hierarchy is not enough."
  (let [c (get comps k) ph (:path c)]
    (filterv #(fbrowser/parent? c (get comps %))
             (keys comps))))
      
(defn fbrowser-padres [comps k]
  "Same rules as children but what we are a child of."
  (let [c (get comps k) ph (:path c)]
    (filterv #(fbrowser/parent? (get comps %) c)
             (keys comps))))

(defn fbrowser-descendents [comps k]
  "Multible levels."
  (let [ch (fbrowser-children comps k)]
    (into [] (apply concat ch (mapv #(fbrowser-descendents comps %) ch)))))

;;;;;;;;;;;;;;;;;;;;;;;; Updating fns ;;;;;;;;;;;;;;;;;;;;;;;;
; Be careful with which thing is used for the old one, it is explained in the docstring.

(defn copy-with-descendents [components k-copy-me new-kys new-paths]
  "Copies all descendents of comp into comp-copies, adjusting the child id and path."
  (let [comp (get components k-copy-me)
        desc (if (= (:type comp) :fbrowser) (fbrowser-descendents comp k-copy-me)
               (throw (Exception. "copy-with-descendents only works with fbrowsers.")))
        shft #(assoc %1 :position (mapv + (:position %1) [(* 15 %2) (* 15 %2)])
                     :z (+ (:z %1) %2))
        n (count new-kys) nd (count desc)
        old-path-n (count (:path comp))
        components1 (reduce (fn [acc ix] ; outer loop through all new keys.
                              (let [new-k (nth new-kys ix) new-toppath (nth new-paths ix) ; new-toppath is the path at the top.
                                    desc-old2new-ks (zipmap desc (mapv (fn [_] (gensym "src")) (range nd))) ; the desc get these keys.
                                    nus (into [] (vals desc-old2new-ks))
                                    upd-chtag (fn [c] (let [us2t (uspath2chtag c)] ; update ::childtag to reflect the new keys.
                                                        (reduce (fn [ci usph] (update-in ci (conj usph ::childtag)
                                                                                     #(get desc-old2new-ks %)))
                                                                c (keys us2t))))
                                    upd-path #(assoc % :path (into [] (concat new-toppath (subvec (:path %) old-path-n))))
                                    upd-comp #(shft (upd-path (upd-chtag %)) ix) ; all updating needed.
                                    acc1 (assoc acc new-k (upd-comp comp))] ; top level comp.
                                (reduce #(assoc %1 (nth nus %2) (upd-comp (get %1 (nth desc %2))))
                                        acc1 (range nd)))) ;all descendents.
                            components (range n))]
    components1))

(defn copy-export-realize [components kys-diff]
  "If there was a copy of an exported piece in one or more components, then apply the changes to kys-diff.
   Only use for file browsers."
  (reduce
   (fn [acc kdiff]
     (let [comp (get acc kdiff)
           chk2chpaths (chk2chpath comp true) ; will keys map to multible paths?
           ; Only unique paths:
           chk2chpaths (zipmap (keys chk2chpaths) (mapv #(into [] (apply hash-set %)) (vals chk2chpaths)))
           ch2us (chpath2uspath comp)
           n (count chk2chpaths) kys (into [] (keys chk2chpaths))]
       (loop [acci acc compi comp ix 0] ; modify both the comp's ::childtag and add stuff to comps.
         (if (= ix n) (assoc acci kdiff comp)
             (let [k (nth kys ix) phs (get chk2chpaths k)
                   phs (into [] (sort-by (fn [p] (apply + (filterv number? p))) phs))] ; sorting for aesthetics.
               (if (= (count phs) 1) ; only one path => no need to copy.
                 (recur acci compi (inc ix))
                 (let [ni (count phs)
                       new-ks (conj (mapv #(keyword (gensym (str "pop" %)))
                                          (range (dec ni))) k)
                       ch-comp (get acci k)                       
                       compi1 (reduce #(assoc-in %1 (conj (get ch2us (nth phs %2)) ::childtag) (nth new-ks %2)) compi (range ni))
                       ;acci1 (reduce #(assoc %1 (nth new-ks %2) (assoc ch-comp :path (nth phs %2))) acci (range ni))
                       acci1 (copy-with-descendents acci k new-ks phs)] ; copy us and all descendents.
                   (recur acci1 compi1 (inc ix)))))))))
   components kys-diff))

(defn single-codebox-sync [leader-twin follower-twin]
  "Twins are codeboxes with the same :path."
  (if (comp-eq? leader-twin follower-twin) follower-twin
    (let [rs-gold (codebox/real-string leader-twin)
          rs (codebox/real-string follower-twin)
          edits (stringdiff/edits-between rs rs-gold)
          upgraded-box (codebox/apply-edits-to-real-string follower-twin edits)]
      (if (not= rs-gold (codebox/real-string upgraded-box))
        (throw (Exception. "Edits didn't work, but in code most likely codebox/apply-edits-to-real-string.")))
      upgraded-box)))

(defn update-fbrowser-step [fbrowsers-last fbrowsers]
  "One step update on fbrowsers.
   fbrowsers-last is the fbrowsers from one step earlier, either before the state modification or before the last call of this fn.
   Call this until convergence, as descendents may be modified and in turn modify their descendents.
   Use this as the first update step."
  (let [kys0 (apply hash-set (keys fbrowsers-last)) 
        kys1 (apply hash-set (keys fbrowsers))
        kys-new (set/difference kys1 kys0) kys-del (set/difference kys0 kys1)
        kys-diff (apply hash-set (filterv #(not (comp-eq? (get fbrowsers-last %) (get fbrowsers %))) (set/intersection kys0 kys1)))
        
        ; Changed parents causing update paths of children or deleting children:
        ck2ch-last (apply merge (mapv #(chk2chpath (get fbrowsers-last %) false) kys-diff))  
        ck2ch (apply merge (mapv #(chk2chpath (get fbrowsers %) false) kys-diff))          
        fbrowsers1 (update-paths fbrowsers ck2ch-last ck2ch)
        
        ; Deleted parents causing deleted children (must check to ensure the filepath is not in some other component).
        ; Not the most efficient code since we have to check everything but only invokes when stuff is deleted.
        delete-these (if (= (count kys-del) 0) #{}
                       (let [keep-tags (apply hash-set 
                                         (apply concat (apply concat (mapv #(vals (uspath2chtag %)) (vals fbrowsers1)))))]
                         (set/difference (keys fbrowsers1) keep-tags)))
        fbrowsers2 (reduce dissoc fbrowsers1 delete-these)
       
        ; Calculate diffs due to changed components:
        ; A deleted component won't actively delete any files, and we have it set up that closing a component retracts back into the parent.
        ; However, deleting stuff within a changed component WILL cause changes.
        
        ; Changed components imposing diffs (only uses old changes, but this is iterative so should propagate through):
        kys-diff2 (apply hash-set (filterv #(get fbrowsers2 %) kys-diff))
        files-old (apply hash-set (apply concat (mapv #(vals (fbrowser/all-paths (get fbrowsers-last %))) kys-diff2)))
        files-new (apply hash-set (apply concat (mapv #(vals (fbrowser/all-paths (get fbrowsers2 %))) kys-diff2)))
        files-add (set/difference files-new files-old)
        fname2folder? (apply merge
                        (mapv (fn [k] (let [comp (get fbrowsers2 k) us2fl (fbrowser/all-paths comp)]
                                        (zipmap (vals us2fl) 
                                          (mapv #(fbrowser/folder? (get-in comp %)) (keys us2fl))))) kys-diff2))
        folder?s-add (mapv #(get fname2folder? %) files-add)
        files-del (set/difference files-old files-new)
        
        ; Delete b4 add:
        diffs (into [] (concat (mapv #(vector :remove (fbrowser/vec-file %)) files-del)
                         (mapv #(vector :add (fbrowser/vec-file %1) {:folder? %2}) files-add folder?s-add)))
        
        fbrowsers3 (if (= (count diffs) 0) fbrowsers2 ; applying diffs everywhere is not the fastest... 
                      (reduce (fn [acc k] (update acc k #(fbrowser/implement-diffs % diffs))) 
                              fbrowsers2 (keys fbrowsers2)))

        ; Copy children when there are multible paths mapping to the same ::chtag. This occurs when i.e. you copy a section of code with an expanded child in it:
        fbrowsers4 (copy-export-realize fbrowsers3 kys-diff)
        ]
    fbrowsers4))

(defn codebox-topk2kys [codeboxes0 kys]
 "Map from top-level keys to child keys."
  (let [kys (filterv #(get codeboxes0 %) kys)
        get-pA #(first (:path (get %1 %2)))
        all-kys (keys codeboxes0)]
    (zipmap kys
      (mapv (fn [k]
              (let [p (get-pA codeboxes0 k)]
                (filterv #(and (not= % k) (= (get-pA codeboxes0 %) p)) all-kys)))
            kys))))

(defn update-fbrowser-codebox [fbrowsers0 fbrowsers codeboxes0 codeboxes]
  "Updates the fbrowsers -> codeboxes linkages, changing paths or deleting when necessary.
   fbrowser0 and codeboxes0 is before the modification.
   Call this after updating fbrowsers.
   Returns the updated codeboxes."
  (let [kys0 (apply hash-set (keys fbrowsers0)) 
        kys1 (apply hash-set (keys fbrowsers))
        kys-new (set/difference kys1 kys0) kys-del (set/difference kys0 kys1)
        kys-diff (apply hash-set (filterv #(not (comp-eq? (get fbrowsers0 %) (get fbrowsers %))) (set/intersection kys0 kys1)))
        
        ck2ch0 (apply merge (mapv #(chk2chpath (get fbrowsers0 %) false) (set/union kys-del kys-diff)))  
        ck2ch (apply merge (mapv #(chk2chpath (get fbrowsers %) false) kys-diff))
        
        ;_ (if (= (count kys-diff) 1) (println "ck2ch stuff:" kys-del kys-diff ck2ch0 ck2ch "guess stuff:" (chk2chpath (get fbrowsers0 (first kys-diff)) false)))
        
        ; Change these to deveced paths to drag codebox paths along:
        dv (fn [m] (zipmap (keys m) (mapv #(vector (fbrowser/devec-file %)) (vals m))))
        ck2ch0 (dv ck2ch0) ck2ch (dv ck2ch)

        ; Also change children:
        kys0-codebox (keys codeboxes0) kys1-codebox (keys codeboxes)
        ;topk2kys0 (codebox-topk2kys codeboxes0 (keys ck2ch0))
        topk2kys (codebox-topk2kys codeboxes (keys ck2ch))

        ck2ch0 (reduce (fn [acc k] (let [kys (if-let [x (get topk2kys k)] x [])
                                         p0 (get ck2ch0 k)]
                                     (merge acc (zipmap kys (mapv #(into [] (concat p0 (rest (:path (get codeboxes %))))) kys)))))
                       ck2ch0 (keys ck2ch0))
        ck2ch (reduce (fn [acc k] (let [kys (if-let [x (get topk2kys k)] x [])
                                        p0 (get ck2ch k)]
                                     (merge acc (zipmap kys (mapv #(into [] (concat p0 (rest (:path (get codeboxes %))))) kys)))))
                       ck2ch (keys ck2ch))
        
        codeboxes1 (update-paths codeboxes ck2ch0 ck2ch)]
    codeboxes1))

(defn update-codebox-step [codeboxes-last codeboxes]
  "One step update on codeboxes.
   codeboxes-last is the codeboxes from one step earlier, either before the state modification or before the last call of this fn.
   Call this until convergence, as descendents may be modified and in turn modify their descendents.
   Use this after update-fbrowser-codebox."
  (let [kys0 (apply hash-set (keys codeboxes-last)) 
        kys1 (apply hash-set (keys codeboxes))
        kys-new (set/difference kys1 kys0) kys-del (set/difference kys0 kys1)
        kys-diff (apply hash-set (filterv #(not (comp-eq? (get codeboxes-last %) (get codeboxes %))) (set/intersection kys0 kys1)))
         
        ; Twin updatings (same paths, diff only as del has no effect here):
        ph2ks (if (> (count kys-diff) 0) ; path to vector of keys.
                (reduce (fn [acc k] (update acc (:path (get codeboxes k)) #(?conj %1 k))) {} 
                  (keys codeboxes)) {})
         
        codeboxes1 (reduce (fn [acc k]
                             (let [leader (get codeboxes k)
                                   ph (:path leader)
                                   all-with-p (apply hash-set (get ph2ks ph))
                                   twins (disj all-with-p k)]
                               (reduce (fn [acci ki] (update acci ki #(single-codebox-sync leader %))) acc twins))) 
                           codeboxes kys-diff)    
        ] codeboxes1))

(defn update-keytags [comps0 comps1]
  "Use this AFTER synching.
   Comps0 should be fully synched and up to date, can be empty for the initialization.
   Comps1 is the modified and synced comps0, which we use to diff comps0."
; Differential: only non comp-eq? comps, and new comps, need to be updated if they are :fbrowser or :codebox.
; Each ::childtag is a vectors of keys of expanded children and fbrowser opening codebox keys, all children are listed.
;   All pieces, and children thereof, etc get a ::childtag for all children that are relevant to the pieces.
; A [] and nil ::childtag is the same for our purposes.
; This is different from the comp's internal :exported? tags.
                                        ; If structural editing then set everything at once (simpler this way).
  ;(println "update keytags")
  (let [codeboxes0 (getty :codebox comps0) codeboxes1 (getty :codebox comps1)
        fbrowsers0 (getty :fbrowser comps0) fbrowsers1 (getty :fbrowser comps1)
        combined0 (merge codeboxes0 fbrowsers0) combined1 (merge codeboxes1 fbrowsers1)
        
        structural? (or (not= (sort (mapv pr-str (keys combined0)))
                              (sort (mapv pr-str (keys combined1))))
                      (first (filter #(let [c0 (get combined0 %) c1 (get combined1 %)]
                                        (and (not (comp-eq? c0 c1))
                                             (not= (uspath2chpath c0) (uspath2chpath c1))))
                               (keys fbrowsers0))) 
                        (keys fbrowsers1))
        
        comps2 (if structural? ; a low bar to clear to recalculate everything...
                 (let [; Mapping from a component's :path to keys. 
                       ; Always included from vectorized file format to keys (codeboxes don't have it).
                       path2kys1 (reduce (fn [acc k]
                                           (let [c (get combined1 k) ph (:path c)
                                                 acc1 (update acc ph #(?conj % k))
                                                 _ (if (and (= (:type c) :codebox) (coll? (:path c))) (throw (Exception. (str "Codebox got a vector path somehow: " (:path c)))))
                                                 acc2 (if (= (:type c) :codebox) ;Handle fbrowser -> codebox cases.
                                                        (update acc1 (fbrowser/vec-file ph) #(?conj % k)) acc1)] acc2 
                                                )) {} (keys combined1))
                       update1 (fn [comp]
                                 (let [us2ch (uspath2chpath comp)]
                                   (reduce (fn [acc usph]
                                             (let [usph1 (conj usph ::childtag)
                                                   chph (get us2ch usph) ; chph = path of hypothetical children.
                                                   x0 (get-in acc usph1) x1 (get path2kys1 chph)]
                                               (cond (= x0 x1) acc
                                                     (or (not x1) (= (count x1) 0)) (do #_(println "unbinding:" (:type comp) usph) (assoc-in acc (conj usph ::childtag) []))
                                                     :else (assoc-in acc (conj usph ::childtag) x1))))
                                     comp (keys us2ch))))]
                   (reduce (fn [acc k] (update acc k update1)) comps1 (keys fbrowsers1))) comps1)]
    comps2))

(defn comprehensive-sync [comps0 comps1]
  (let [fbrowsers0 (getty :fbrowser comps0) fbrowsers1 (getty :fbrowser comps1)
        fbsync (loop [old fbrowsers0 new fbrowsers1 n 0]
                 (if (= n 512) (throw (Exception. "Iterative fbrowser sync is (probably) not converging."))
                   (let [new1 (update-fbrowser-step old new)]
                     (if (= new new1) new1 (recur new new1 (inc n))))))
        codeboxes0 (getty :codebox comps0) codeboxes1 (getty :codebox comps1)
        cbsync1 (update-fbrowser-codebox fbrowsers0 fbsync codeboxes0 codeboxes1)
        
        cbsync2 (loop [old codeboxes0 new cbsync1 n 0]
                  (if (= n 512) (throw (Exception. "Iterative codebox sync is (probably) not converging."))
                    (let [new1 (update-codebox-step old new)]
                      (if (= new new1) new1 (recur new new1 (inc n))))))        
        kothers (filterv #(let [ty (:type (get comps1 %))]
                            (and (not= ty :codebox) (not= ty :fbrowser))) (keys comps1))
        others (zipmap kothers (mapv #(get comps1 %) kothers))]
    (update-keytags comps0 (merge others fbsync cbsync2))))
