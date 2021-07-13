; Pathed macro expansion of clojure.core macros (not all macros set up yet).
; 1. Specify paths on the unexpanded code.
; 2. Expand some sections of code to reduce the effort in writing type infer fns, etc.
 ; 2.1 Replace fn* with fn to ensure all fns are expanded.
; 3. Keep track of "where stuff goes". 

(ns coder.pathedmexp
  (:require 
    [clojure.walk :as walk]
    [collections] 
    [coder.textparse :as textparse]
    [coder.crosslang.langs :as langs]
    [layout.blit :as blit]))


;;;;;;;;;;;;;;;;;;;;;;; Collection functions ;;;;;;;;;;;;;;;;;;;;;;


(defn leaf-path-map [x y]
  "Map from path in x to path in y for any leaf element. If a leaf element appears more than once in y
   only one path is chosen. Using ^:leaf-meta also will let us associcate."
  (let [lget-in (fn [x ph] (let [yi (collections/cget-in x ph)
                                 lm (:leaf-meta (meta yi))]
                             (cond (not (coll? yi)) yi lm lm :else false)))
        leaf2py (reduce #(let [v (lget-in y %2)] 
                           (if v (assoc %1 v %2) %1)) {} (collections/paths y))]
    (reduce #(let [xi (lget-in x %2)]
               (if (get leaf2py xi) (assoc %1 %2 (get leaf2py xi)) %1)) 
      {} (collections/paths x))))

(defn leaf-branch-path-map [x y]
  "Not just leaves."
  (let [lpm (leaf-path-map x y)
        
        max-depth (apply max (mapv count (keys lpm)))
        cut (fn [v n-cut]
              (if (>= (count v) n-cut) (subvec (into [] v) 0 (- (count v) n-cut))))
        no-nil (fn [m] (select-keys m (filter #(and % (get m %)) (keys m))))
        ; Sucessively more stubby branch-maps:
        branch-phms (mapv (fn [n-cut] (no-nil (zipmap (mapv #(cut % n-cut) (keys lpm))
                                                (mapv #(cut % n-cut) (vals lpm))))) 
                      (range max-depth))
        phx (collections/paths x)
        get-ph (fn [px] 
                 (let [phs (mapv #(get % px) branch-phms)]
                   (first (filterv #(and % (= (collections/cget-in x px) (collections/cget-in y %))) phs))))
        paths-x (collections/paths x)]
    (collections/filter-kv
      (fn [k v] v) (zipmap paths-x (mapv get-ph paths-x)))))

(defn unique-leaves [x exclude-f]
  "Makes leaves unuqie, unless in excludes"
  (let [a (atom 0)
        xform! #(let [ix (str @a) _ (swap! a inc)]
                  (cond (exclude-f %) %
                    (symbol? %) (symbol (str "sym" ix))
                    (string? %) (str ix)
                    (keyword? %) (keyword (str "kwd" ix))
                    (coll? %) %
                    (number? %) ix
                    :else (str "leafy" ix)))]
    (walk/postwalk xform! x)))


;;;;;;;;;;;;;;;;;;;;;;; Library functions ;;;;;;;;;;;;;;;;;;;;;;


(def mexpand-these #{'-> '->> 'as-> 'some-> 'some->> 'cond-> 'cond->>
                     'fn 'defn 'defn- 'fn* 'defmulti 'defmethod 'definline 
                     'with-out-str 'with-in-str 'with-precision
                     'let 'letfn 'loop 'when-let 'for 'refer-clojure 'with-bindings 'while
                     'when 'when-not 'if-not 'and 'or 'cond 'condp 'case
                     'pvalues 'locking 'lazy-cat 'dosync
                     'comment})

; Macro-expanding these doesn't help us understand things. 
; Most of these are private or call clojure and/or java internals.
(def dont-bother #{'. '.. 'lazy-seq 'delay 'assert-args 'binding 'sync 'io! 'vswap! 'declare 'doseq 'dotimes
                   'import 'with-open 'doto 'memfn 'def-aset 'with-local-vars 'assert 'amap 'areduce
                   'ns 'defonce 'add-doc-and-meta 'when-class 'future})


;;;;;;;;;;;;;;;;;;;;;;; Core macro functions ;;;;;;;;;;;;;;;;;;;;;;

(defn exclude-f? [x]
  (or (contains? mexpand-these (textparse/unqual x))
      (contains? #{'if '. '.. 'def 'quote 'var 'recur 'throw 'try 'monitor-enter 'monitor-exit '& '&env '&form} x)
      (contains? #{:as :tag :or :pre :post} x))) ; special meaning macro kwds.

(defn pmexpand [code]
  "Macroexpands core constructs when helpful to do so. See 'dont-bother to see what isn't deemed helpful."
  (cond (map? code) (zipmap (keys code) (mapv pmexpand (vals code)))
    (vector? code) (mapv pmexpand code)
    (set? code) (set (mapv pmexpand code))
    (coll? code) (let [code1 (apply list (mapv pmexpand code))
                       symu (textparse/unqual (first code))]
                   (if (contains? mexpand-these symu) 
                     ; Call pmexpand again if the code requires it.
                     (let [macroexpand-1+ #(macroexpand-1 (if (= (first %) '*fn) (apply list 'clojure.core/fn (rest %)) %))
                           code2 (macroexpand-1+ code1)] 
                       (if (= code1 code2) code2 (pmexpand code2)))
                     code1))
    :else code))

(defn path-map [code]
  "Map from nonexpanded code to expanded code. Does not include paths that disappear."
  (let [codeunique (unique-leaves code exclude-f?)]
    (leaf-branch-path-map codeunique (pmexpand codeunique))))
  

(defn inv-path-map [code]
  "Map from expanded code to nonexpanded code. Does not include paths that disappear."
  (let [codeunique (unique-leaves code exclude-f?)]
    (leaf-branch-path-map (pmexpand codeunique) codeunique)))


;;;;;;;;;;;;; Testing ;;;;;;;;;;;;;;;


(defn _testpm [sym-or-code inv?]
  "The most straihtforward test"
  (let [code (if (symbol? sym-or-code) (langs/var-source sym-or-code) sym-or-code)
        pm (if inv? (inv-path-map code) (path-map code))
        code-ex (pmexpand code)
        phs (keys pm)
        report (if inv?
                 (fn [ph] (str ph "->" (get pm ph) " | " (collections/cget-in code-ex ph) " | " (collections/cget-in code (get pm ph))))
                 (fn [ph] (str ph "->" (get pm ph) " | " (collections/cget-in code ph) " | " (collections/cget-in code-ex (get pm ph)))))]
    (println (if inv? "TESTINVPM:" "TESTPM:") "Before and after expansion:")
    (blit/vpu code)
    (blit/vpu code-ex)
    (println "Path map and code pieces:")
    (mapv #(println (report %)) phs) "See printouts"))

(defn testpm [sym-or-code] (_testpm sym-or-code false))
(defn testinvpm [sym-or-code] (_testpm sym-or-code true))