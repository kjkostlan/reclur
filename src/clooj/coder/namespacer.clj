; The name-space system in Clojure is the most complicated that I have ever seen.
; This file handles namespace qualifications and conversions.

(ns clooj.coder.namespacer
  (:require [clooj.coder.io :as cio] [clojure.string :as string]
            [clooj.coder.grammer :as grammer]
            [clooj.utils :as utils]
            [clooj.coder.tracer :as tracer]
            [clooj.collections :as collections]
            [clojure.reflect]))

(defn deep-dequote [code]
  "Recurisivly removes any quotes in the code. For quotes with one argument, remove the extra ().
   This is half of the conversion from repl to non-repl namespaces, the other is replacing require with :require"
  (if (coll? code)
    (let [code (collections/cmap :flatten deep-dequote code)] ; recursive.
      ; strip the extra quote.
      (if (and (seq? code) (= (first code) 'quote))
        (if (= (count code) 2) (second code) (rest code)); can't strip for n>2, shouldn't be an issue in most cases. 
          code))
    code)) ; not a collection.

(defn ns-dequote [code] 
   "Converts a repl (ns ...) code block into file ns code."
   (let [code (deep-dequote code)
         kyfn1 #(if (or (= % 'require) (= % 'refer) (= % 'use) (= % 'import))
                    (keyword %))
         kyfn #(apply list (kyfn1 (first %)) (rest %))] 
     (apply list (map kyfn code))))

(defn ns-quote [code] 
   "Converts a file (ns ...) code block into repl ns code."
   (let [ky? #(or (= % :require) (= % :refer) (= % :use) (= % :import))
         kyfn (fn [g] (if (ky? (first g)) (apply list (map #(quote %) (rest g))) g))] 
     (apply list (map kyfn code))))

(defn expand-ns [code]
  "expands i.e (clojure [string :as string] test) => [[clojure.string :as string] [clojure.test]]
   Also, a statement is packaged into an outer [] even if it is one long."
  (if (and (seq? code) (> (count code) 1)); >= 2 elements and is a sequence.
      (let [root (str (first code) "." ) leaves (rest code)
            prepend #(symbol (str root %))] ; adds the root to the symbol.
        (mapv #(if (vector? %) (assoc % 0 (prepend (first %))) (prepend %)) leaves))
      (vector code))) ; package it up since we will unwrap one level.

(defn ns-publics-robust [ns] 
  "Fickle functions are normally HfA. Not the case for clojure sadly. This fn makes it less fickel"
  (let [ns (symbol ns)]
    (try (ns-publics ns) 
      (catch Exception e 
        (let [c (str "(require '[" ns " :as " (gensym) "_ignore])")]
          (eval (read-string c))
          (ns-publics ns))))))

(defn single-full2short [_code]
  "Returns the mapping form the full variable names to short variable names.
   code is a single fetch i.e. (:require [clojure.java.io :as io :refer ...]) but NOT (:require [...] [...]).
   The mapping is 1:1 unless there are 'warning: ... already referes to ...' or imports with the same leaf fn names,
   in which case multiple full names will target the same short name."
   ; TODO: for duplicate variable names but NOT java classes we need to get rid of the duplicates.
   ; This fn's code is a bit redundant b/c we like to keep things in symbol form. This may be changed.
  (let [ty (first _code)
        code (rest _code)
        ;_ (println code)
        code (if (coll? (first code)) (apply concat code) code); compensate for extra wrapping.
        code (into [] code)
        
        import? (or (= ty 'import) (= ty "import") (= ty :import)) 
        require? (or (= ty 'require) (= ty "require") (= ty :require))
        ; These are identical (I think).
        refer? (or (= ty 'use) (= ty "use") (= ty :use) (= ty 'refer) (= ty "refer") (= ty :refer))
        ns (first code) ; namespace OR the java class. Both are in SYMBOL form.
        
        leafify #(if (nil? %) % (symbol (last (string/split (str %) #"\."))))
        
        members (into [] (if import? (filter #(contains? (:flags %) :public) (:members (clojure.reflect/reflect (eval ns)))) [])) ; eval makes Symbols become a Class object.
        
        ; LEAF variable names:
        
        leaves (if import? (mapv #(:name %) members) (keys (ns-publics-robust (first code))))
        
        ; how you call the variable (this will change do to keywords):
        calls (cond import? (let [static?s (mapv #(contains? % :static) members)
                                  out (mapv #(if %2 %1 (symbol (str "." %1))) leaves static?s) ; non-static vs static.
                                  ] (mapv #(if (= (str %) (str "." ns)) (symbol (str (leafify ns) ".")) %) out)) ; Constructors (which are not static).
                    require? (mapv #(symbol (str ns "/" %)) leaves)
                    refer?   leaves)
        ; Full namespaces. Non-static methods won't work for this.
        full (mapv #(symbol (str ns "/" %)) leaves)
        
        leaf-swap (fn [call newleaf]
                    (let [pieces (string/split (str call) #"/") p0 (first pieces) pr (apply str (rest pieces))]
                      (if (= (count pieces) 1) (symbol newleaf) (symbol (str p0 "/" newleaf)))))
        
        ; (update-calls calls keyword arg) applies the calls array.
        update-calls 
          (fn [c k a] 
            (cond (= k :as) (mapv #(if (nil? %1) %1 (symbol (str a "/" %2))) c leaves) ; replace the root namespace.
                  (= k :rename) (mapv #(if (or (nil? %1) (nil? (get a %2))) %1 (leaf-swap %1 (get a %2))) c leaves) ; rename vars.
                  ; remove vars with :only and :exclude :
                    :else (let [affected-set (apply hash-set a)
                                has? #(contains? affected-set %)
                                keep? #(if (= k :only) (has? %) (not (has? %)))]
                            (mapv #(if (keep? %) % nil) leaves))))
        calu (loop [ix 1 out calls]
               (if (= ix (count code)) out
                 (let [ci (nth code ix)
                       take2 (and (or (= ci :as) (= ci :refer) (= ci :only) (= ci :exclude) (= ci :rename)) (< ix (dec (count code))))]
                   (if take2 (recur (+ ix 2) (update-calls calls ci (nth code (inc ix))))
                     (recur (inc ix) (update-calls calls ci nil))))))
       ; remove nil vars:
       out (into {} (filter #(not (nil? (second %))) (zipmap full calu)))] out))

(defn fullname-ns [fetch]
   "The ns that fetch is referring to"
  (first (second fetch)))

(defn nickname-ns [fetch] 
   "Gets the nickname namespace from a fetch, nil if not using a nickname."
   (let [fetchr (second fetch) n (count fetchr)]
     (loop [ix 0]
       (if (>= ix (dec n)) nil ;nothing found.
           (if (= (nth fetchr ix) :as) (nth fetchr (inc ix)) (recur (inc ix)))))))
 

(defn _hunt [code]
  "Finds fetch groups (i.e. (:require [...] [...] ...) recursively in code.)"
  (cond (not (coll? code)) 
        [] ; leaf entry.
        (or (not (nil? (get {:require 'require :use 'use :refer 'refer :import 'import} (first code)))))
        [code] ; the code is a fetch. Return it as a 1-vector.
        :else 
        (into [] (apply concat (mapv _hunt code))))); look recursivly.
    

(defn extract-full-fetches [code]
  "Extracts and seperates each import/require/use/refer request (a 'fetch').
  Giving it a string will cause a read-string, which takes the FIRST code block it finds."
  (let [code (if (string? code) (read-string code) code)
        code (deep-dequote code)
        fetch-groups (_hunt code); each (:require ...), etc can have multible sub-statements.

        ;_ (println fetch-groups)
 
        ; expand the namespace on each sub-statement. This must be done first!
        fetch-groupsE (mapv #(apply list (concat [(first %)] (apply concat (mapv expand-ns (rest %))))) fetch-groups)

        grfn (fn [c] (let [h (first c) t (rest c)] ; group to vector function.
               (mapv #(list h %) t)))
        fetches (apply concat (mapv grfn fetch-groupsE))
        ]
    fetches))

(defn add-fetch [ns-code fetch]
 "A dumb way to add a fetch."
 (apply list (concat ns-code (list fetch))))


;; Smarter ways to resolve variables into full code:
(defonce somens (create-ns 'namespacers-scratchpad)) ; not sure if this is needed to isolate stray effects of require.
(defn ns-resolve-clean [nsobj code]
  (read-string (subs (str (ns-resolve nsobj code)) 2)))
(defn _recursive-resolve [code nsobj]
  (if (sequential? code)
    (let [codeseq (map _recursive-resolve code (repeat nsobj))]
      (if (vector? code) (into [] codeseq) (apply list codeseq)))
    (let [full0 (try (ns-resolve-clean nsobj code) (catch Exception e code))]
          (if (nil? full0) code full0))))
(defn resolved-code-ns [name-space code] 
  "gets the code for which all symbols are fullly-resolved. It has to wrap code."
  (binding [*ns* somens] ; isolate any effects of using require.
    (let [code (if (string? code) (tracer/read-string-noerr code) code)
          nssym (symbol name-space)]
      (if (= code "error") nil ; nothing meaningful.
        (do 
          (try (require nssym) ; loads the namespace, which allows us ot use nsobj.
            (let [nsobj (find-ns nssym)] ; the object of the namespace.
              (_recursive-resolve code nsobj))
            (catch Exception e code)))))))


; TODO: a function that merges them smartly, but this is only cosmetic.
;(defn merge-fetch [fetch1 fetch2]
;  "Merges two fetches with the same name. New fetches always add stuff, so an :only would have to be replaced.
;   This only works in non-quoted mode (use other fns to convert). Only works on a single fetch."
;  (let [t1 (first fetch1) ; :require :use :refer :import.
;        t2 (first fetch2)
;        _ (if (not (= t1 t2)) (throw (Exception. "Fetches must be both of the same type, i.e. both :require or both :use.")))
;        arg1 (second (fetch1)) ; i.e. [clojure.string :as s :only (split)]
;        arg2 (second (fetch2))
;        _ (if (not (= (first arg1) (first arg2))) (throw (Exception. "ns's/classes must be the same to combine fetches.")))
;        
;
;       ; 
;       ; onlys are only kept if both are only:
;       only1 (> (count (filter #(= % :only) arg1)) 0)
;       only2 (> (count (filter #(= % :only) arg1)) 0)
;       
;       ; loop through the keywords:
;       pieces ; [:as s :only (split)] but not clojure.string.
;         (loop [ix 1])
;       ]
;       
;       ))

;;; Old stuff below. TODO remove/refactor.

(defn ensure-require [text long short]
  "Makes sure that the text has a require in it of the format long :as short, or long (if short is nil)"
  ; TODO: rewrite this code in terms of strmap et al and preserve the structure of the ns string.
  (let [; the special island that starts with ns (nil if we don't find any):
        island (first (cio/extract-outer-islands text))
        i0 (first island)
        i1 (second island)
        island-c (read-string (subs text i0 i1))
        hunt (if (nil? short) (str "[" long "]") (str "[" long " :as " short "]"))
        ; the str (read-string) is for normalization.
        has-require? (.contains (str island-c) (str (read-string hunt)))]
    (if has-require? text
        (str (subs text 0 i0) (str (add-fetch island-c (read-string (str "(:require " hunt ")"))))))))
        
(defn remove-require [text long short]
  (let [hunt (if (nil? short) (str "[" long "]") (str "[" long " :as " short "]"))]
    (string/replace text hunt "")))

(defn use-ns [text]
  "Returns a string that, when ran, has the effect of switching to the namespace defined in text.
  TODO: can we frame this fn in terms of analyze's stuff."
  (let [isl (first (cio/extract-outer-islands text))
        nscode (read-string (subs text (nth isl 0) (nth isl 1)));
        usestr (str "(use '" (nth nscode 1) ")")
        friends (str (rest (rest nscode))) ; remove the outer layer "(ns" and ")".
        ]  
    ;(println "friends: " friends)
    ; Compile a package: 
    (str "(do " usestr "\n" (subs friends 1 (dec (count friends))) ")" )))             
