; Stands for "code database". Functions that look over the source code as a database more than working
; on individual bits of code.
(ns clooj.coder.cbase (:require [clooj.java.file :as jfile] [clooj.coder.grammer :as grammer] 
                          [clooj.coder.namespacer :as namespacer]
                          [clojure.string :as string]
                          [clojure.set :as sett]
                          [clooj.coder.io :as cio]
                          [clooj.coder.strmap :as strmap]))

; TODO: precompute delux so only changes in files need to be considered.

; NOTE: getting the actual code out of an anomonous function is super-hard!
  ; Clojure.repl.source only works for one level.
  ; So our fancy optimize system would warn on inner-loops built with anomonous functions.
 
;;;;;;;;;;;;;;;;;;;;;;;; Helper functions.

(defn m+ 
   "Adds k n times to a counted maps that maps from keys to # of times the key is hit."
  ([m k] (m+ m k 1))
  ([m k n] (assoc m k (if (nil? (get m k)) n (+ (get m k) n)))))

(defn m1+m2 [m1 m2]
  "Adds two counting maps together"
  (reduce #(m+ %1 %2 (get m2 %2)) m1 (keys m2)))
  
(defn most-popular [m]
  "For map from stuff to counts, gets the {:key :pop} most popular key and it's popularity"
    (let [mx (apply max (vals m))]
      {:key (first (filter #(= (get m %) mx) (keys m))) :pop mx}))

(defn _sym-use [counts locals code] ; recursive helper fn. counts is a map locals is a set.
  (cond (map? code) (reduce #(_sym-use %1 locals %2) counts (into [] (concat (keys code) (vals code)))); recursive both on keys and values.
        (and (seq? code) (vector? (second code)) (grammer/is-let-statement (first code))) ; let statements
          (let [syms (take-nth 2 (second code)) vals (take-nth 2 (rest (second code))) n-pair (count vals)]
             (loop [out counts locals1 locals ix 0]
               (if (= ix n-pair) out
                   (recur (_sym-use out locals1 (nth vals ix)) (conj locals1 (nth syms ix)) (inc ix)))))
        (coll? code) (reduce #(_sym-use %1 locals %2) counts code) ; bucket for all other collections, even sets.
        (symbol? code) (if (contains? locals code) counts (m+ counts code))  ;Add in only if this symbol isn't locally defined.
        :else counts)) ; no change.

(defn symbol-use-def-block [code]
  "Returns the dependencies, a vector of symbols used in a block of code that are not defined in said code.
   This only works on a single def, defn, etc block."
   ; TODO: make sure this function works for everything.
  (let [code (cio/try-to-read code)
        _ (if (not (grammer/is-def-statement (first code))) (throw (Exception. "Only a def, defn, etc can be passed in here")))
        
        fn-sym (first (filter #(and (symbol? %) (not (grammer/is-def-statement %))) code)) ; one whether single or multi arity.
        arg2set (fn [c] ; deal with funky optional args.
                   (cond (symbol? c) #{c}
                         (vector? c) (apply hash-set c)
                         (map? c)    (apply sett/union (mapv #(cond (symbol? %) % (vector? %) (apply hash-set %) (map? %) (apply (hash-set (vals %)))) (vals c)))))
                         
        get-init-locals #(let [v (first (filter vector? %))] ; initial locals (the def and the vector code), works with or without docstrings.
                           (conj (apply sett/union (mapv arg2set v)) fn-sym))
        ; single vs multiple arity:
        single-arity? (first (filter vector? code)) ; nil iff multi arity.
        ]
    (if (single-arity?) (_sym-use {} (get-init-locals code) (last code)
                        (reduce m1+m2 {}; add up the results from each single-arity sub-function.
                          (mapv (fn [c] (_sym-use {} (get-init-locals c) (last code)))))))))
(defn symbol-counts-ns [code]
  "Counts how many symbols are used in an ns, excluding local ones like those defined in let blocks.
   Symbols that use other symbols DO count."
  (let [codev (cio/try-to-read (str "[" code "]"))]
    (if (nil? codev) {}
        (reduce m1+m2 {} (mapv symbol-use-def-block (rest codev))))))

;;;;;;;;;;;;;;;;;;;;;;;; SMALL database functions.

(defn get-var-loc [varfull]
  "Gets the :file and :char of a variable name/symbol that is fully-qualified."
  (let [pieces (string/split (str varfull) #"\/")
        leaf (second pieces)
        code (strmap/reads-string+ (jfile/load-textfile (jfile/namespace2file (first pieces))))]
     {:file (first pieces)
      :char (first (:pos (filter #(and (coll? %) (= (str (:obj (second %))) leaf)) code)))}))

;;;;;;;;;;;;;;;;;;;;;;;; LARGE database functions.

(defn get-all-vars [] 
  "Map form ns's to variables over all ns's. Does not include java classes."
  (let [files (jfile/get-clj-files)
        nms (mapv #(symbol (jfile/file2namespace %)) files)
        vars-ea-file (mapv #(try (rest (read-string (str "(" (jfile/load-textfile %) ")"))) (catch Exception e '())) files)
        names-ea-file (mapv (fn [vs] (mapv (fn [v] (if (coll? v) (symbol (second v)) v)) vs)) vars-ea-file)] ; vector of lists.
    (zipmap nms names-ea-file)))

(defn nickname-decoder []
  "A map from nicknames to full varaible names and the fetch statement it uses.
   Each key is a nickname and each value is a map of of {:full :fetch} => # of times counted. 
   the same nickname may map to several full names (if differnet files use conflicting conventions).
   Uses SYMBOLS, not strings."
  (let [files (jfile/get-clj-files)
       contents (mapv jfile/load-textfile files)
       sym-counts (mapv symbol-counts-ns contents) ; one per each file, 32 ms as of Jan 6 2016.
       fetches (mapv namespacer/extract-full-fetches contents) ; one group per file.
       ; we can't use ns-resolve b/c we need to know which fetch got what symbol.
       qual-ns (fn [ns-fetches] ; gets the map from short symbols to {:full :fetch} given the fetches in an ns.
                 (let [f2ss (mapv namespacer/single-full2short ns-fetches) ; each full2short map, one per fetch.
                       f2ssf (mapv #(hash-map :m %1 :fetch %2) f2ss ns-fetches)] ; package the map with the fetch.
                   (reduce (fn [acc f2sf] ; acc = the big output. f2sf is the {:m :fetch} object.
                              (let [f2s (:m f2sf) fetch (:fetch f2sf)] ; full2short map for one fetch, and the fetch itself.
                                (reduce #(assoc %1 (get f2s %2) {:full %2 :fetch fetch}) acc (keys f2s)))) {} f2ssf))) ; inverts each map and accumilates. Later fetches displace earlier ones.
       qual-maps (mapv qual-ns fetches) ; short => :full and :fetch, one for each file.
       
       decoder (fn [symc qm] ; decodes a single nickname to return nickname => {:full :fetch :count}
                 (let [c (vals symc) ff (mapv #(get qm %) (keys symc))] ; counts and {:full :fetch} for each variable.
                   (zipmap (keys symc) (mapv #(assoc %1 :count %2) ff c)))) ; each symbol maps to a full fetch mapping to a count.
       
       catmap1 (fn [acc m sym] ; adds a symbol to acc given m, a single decoded map.
                 (let [ffc (get m sym) ; a tuple of {:full :fetch :count}
                       ffcs (get acc sym) ffcs (if (nil? ffcs) {} ffcs)] ; a map of {:full :fetch} => count for all cases for which the short-hand is sym.
                   (assoc acc sym (m+ ffcs (dissoc ffc :count) (:count ffc)))))
       catmap (fn [acc m]  ; adds a single decoder map to acc.
                 (reduce #(catmap1 %1 m %2) acc (keys m)))]
        
       (reduce catmap {} (mapv decoder sym-counts qual-maps))))

(defn guess-full-fetch [sym]
  "Guesses the fully qualified name based on a symbol in a file,
   returns the {:short :full :fetch :pop} to get it. :short will be sym or be similar to sym."
   ; TODO: arity checking.
  (let [sym (symbol sym)
        rosetta (nickname-decoder) ; nested map nicknames => {:full :fetch} => count.
        nicks (keys rosetta) ;nickname symbols. 

        ; Gets the {:full :fetch :pop} of a short symbol, taking the peak popularity one. nil if failed.
        peak-ffp #(let [ffs_c (get rosetta %)] (if (nil? ffs_c) nil 
                    (let [fp (most-popular ffs_c)] (assoc (:key fp) :pop (:pop fp)))))
        multisym (fn [syms]  ; like peak-ffp but picks the most popular symbol for a vector of symbols. nil if given an empty vector.
                   (let [ffps (mapv peak-ffp syms)
                         mx (apply max (mapv #(:pop %) ffps))]
                     (:full (first (filter #(= (:pop %) mx) ffps)))))
        
        ; First try to find our symbol in the map:
        lookup (:full (peak-ffp sym))
        ; If look-up is nil try to find a symbol that contains it (picking the most popular one):
        lookup (if (nil? lookup) (multisym (filter #(.contains (str %) (str sym)) nicks)))
        ; If this is still nil try to find a symbol it contains:
        lookup (if (nil? lookup) (multisym (filter #(.contains (str sym) (str %)) nicks)) lookup)
        ; If this is still nil rerun find a symbol that contains it but with no namespace /:
        sym-leaf (symbol (last (string/split (str sym) #"\/")))
        lookup (if (nil? lookup) (multisym (filter #(.contains (str %) (str sym-leaf)) nicks)) lookup)
        ]
    (if (nil? lookup) nil; TODO: add more robust stuff, i.e. misspellings, here.
        (assoc (peak-ffp lookup) :short lookup))))

(defn guess-ns [hint]
   "Guesses the namespace that matches hint, based on how you nickname the namespace, etc. nil = failed."
   (let [hint (str hint)
         hint (.replace hint "/" ".")
         hint (if (.endsWith hint ".clj") (subs hint 0 (- (count hint) 4)) hint)
         
         files (jfile/get-clj-files)
         namespaces (mapv #(str (jfile/file2namespace %)) files)
         
         ; Earlier guesses are high priority:         
         guess (if (contains? (apply hash-set namespaces) hint) hint nil) ; Specified the entire namespace or clojure file.
         guess (if (nil? guess) ; nicknames as used in the src code.
                   (let [contents (mapv jfile/load-textfile files)
                         fetches-ea-file (mapv namespacer/extract-full-fetches contents)
                         fetches   (into [] (apply hash-set (apply concat fetches-ea-file))) ; all unique fetches.
                         nicknames (mapv str (mapv namespacer/nickname-ns fetches))
                         full-names (mapv str (mapv namespacer/fullname-ns fetches))
                         nicks2full (zipmap nicknames full-names); TODO: counting usages instead of just picking one when they collide.
                         ] (get nicks2full hint)) guess) ; will be nil if it does not contain the hit
         guess (if (nil? guess) ; leaf names.
                   (let [get-leaf #(last (string/split % #"\."))
                         hint-leaf (get-leaf hint)
                         leaves2full (zipmap (mapv get-leaf namespaces) namespaces); TODO: counting usages instead of just picking one when they collide.
                         ] (get leaves2full hint-leaf)))])) ; last chance to not be nil.


(defn guess-fetch [ns]
  "Guesses the correct way to fetch a given ns, based on what kinds of fetches in the file."
  (let [ns (symbol ns)
        files (jfile/get-clj-files)
        contents (mapv jfile/load-textfile files)
        fetches (apply concat (mapv namespacer/extract-full-fetches contents)); ALLOW duplicates.
        
        matching-fetches (filterv #(= (namespacer/fullname-ns %) ns) fetches) ; all fetches that use this symbol.
        fetch-counts (reduce #(m+ %1 %2) {} matching-fetches) ; how many of each fetch match.
        out (most-popular fetch-counts)]
    (if (nil? out) (list :require ns) out))) ; some may not have a fetch yet, just use a require with no :as or anything else.

(defn dependency []
  "Which vars reference which other vars.
   Each key is a fully qualified var symbol. Each val is an array of vars it depends on.
   Said array may includes itself (recursive) other defs in the ns, and other defs from the other ns."
  (let [files (jfile/get-clj-files)
        contents (mapv jfile/load-textfile files)
        namespace-syms (mapv #(symbol (jfile/file2namespace %)) files)
        namespaces (mapv find-ns namespace-syms) ; why can't it just be a symbol?
        defs (mapv #(into [] (rest (cio/try-to-read (str "[" % "]")))) contents) ; function definition code, nested vector 2 levels.
        full-var-sym (apply concat (mapv (fn [ds ns] (mapv #(symbol (str (second %1) ns)) ds)) defs namespace-syms)) ; Flattened the array.
        get-full-deps (fn [ds ns] (let [pends (mapv symbol-use-def-block ds)]
                         (mapv #(ns-resolve ns %) ns)))
        full-uses-sym (apply concat (mapv get-full-deps defs namespaces))] ; Flattened the array, but still is a map within a vector.
     (zipmap full-var-sym (mapv #(apply vector %) full-uses-sym)))) ; remove # information by converting it into a vector.

;;;;;;;;;;;;;;;;;;;;;;;;; Old stuff below. TODO: refactor/remove.

(defn invert-network [network]
 "inverts a network. The network itself is a map between keys and arrays of keys"
  (let [add-to-map (fn [acc k v] (if (nil? (get acc k)) (assoc acc k (vector v)) (assoc acc k (conj (get acc k) v))))
        ; all the contributions to the inverse map that a single function makes.
        ; note the switching of key and value in the # fn here:
        element-invert (fn [acc k vs] (reduce #(add-to-map %1 %2 k) acc vs))]
    (reduce #(element-invert %1 (first %2) (second %2)) {} (map #(vector %1 %2) (keys network) (vals network)))))