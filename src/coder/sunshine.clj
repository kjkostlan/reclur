; Unique symbols with no shadows to make code manipulation easier.
; This function depends on *ns*.
; Step 1: Sybol qualification, java symbol thingy, and fn* wrap.
; Step 2: Symbol uniquify by adding a tag (_ix=1234) with varying numbers on the end, for local symbols.
   ; Non-local symbols and java get fully qualifed.
; Step 3: Removal of gensym shenagagins and related stuff.
; Step 4: Custom modification fn on non-resolved symbols, most commonly making lowercse.
; Step 5: Minimalistic anti-shadow with 1,2,3, etc in order of encountering.
; Step 6: Condensing default imports: java.lang and clojure.core.

(ns coder.sunshine
  (:require [clojure.set :as set]
    [clojure.string :as string]
    [clojure.walk :as walk]
    [coder.cnav :as cnav]
    [coder.crosslang.langs :as langs]
    [coder.textparse :as textparse]
    [collections]))

(def ^:dynamic *ns-sym* 'coder.sunshine)

;;;;;;;;;;;;;;;;; Symbol functions ;;;;;;;;;;;;;;;;

; TODO: swallow exception is a problem.
(defn qual [sym] 
  (if-let [r (try (langs/resolved *ns-sym* sym) (catch Exception e false))] r 
    (if-let [r1 (try (langs/resolved 'clojure.core sym) (catch Exception e false))]
      r1 false)))

(defn power-resolve [sym]
  (if-let [sym-qual (qual sym)] sym-qual (langs/clj-resolve-class sym)))

(defn remove-trailing-nums [sym]
  "Useful when the trailing nums should be used to de-shadow."
  (loop [melt (str sym)]
    (if (Character/isDigit (last melt))
      (recur (subs melt 0 (dec (count melt)))) 
      (symbol melt))))

(defn clj-ns-of [sym]
  "The namespace of this symbol, in symbol form.
  Uses the current *ns*."
  (let [sym-qual (subs (str (resolve sym)) 2)
        str-leaf (last (string/split (str sym) #"\/"))]
    (symbol (string/replace (str (second (read-string (str "`" sym-qual)))) (str "/" str-leaf) ""))))

;;;;;;;;;;;;;;;;; Symbol marking ;;;;;;;;;;;;;;;;

(defn split-marked-sym [sym]
  "Returns two strings."
  (let [s (str sym)
        ix-part (if-let [s1 (re-find #"_ix=\d+" s)] s1 "")
        s0 (subs s 0 (- (count s) (count ix-part)))]
    [s0 ix-part]))

(defn mark-sym [sym]
  "Unique, uses gensym. The splitting may not be strictly nesessary,
  due to deepest-first traversal, but it is generally a good idea to have twice-marked syms still get a single mark."
  (gensym (str (first (split-marked-sym sym)) "_ix=")))

(defn marked-ix [sym]
  (Integer/parseInt 
    (str "0" (apply str (filter #(Character/isDigit %) (second (split-marked-sym sym)))))))

(defn unmark-sym [sym]
  (symbol (first (split-marked-sym sym))))

;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;

(defn sym-replace [form rmap]
  "Recursive."
  (walk/postwalk #(if (symbol? %) (get rmap % %) %) form))

(defn binding-head? [x]
  "Does the second element of x define binding pairs?"
  (and (collections/listy? x) (contains? #{`let `let* `loop `loop* `binding} (first x))))

(defn fn-head? [x]
  "Does the second element of x define binding pairs?"
  (and (collections/listy? x) (contains? #{`fn `fn*} (first x))))

(defn binding-unique-tag [binding-vec]
  "Makes symbols defined and used within a binding-vec unique.
   foo -> foo_ix=1234. Uses gensym."
  (let [n (count binding-vec)]
    (loop [ix 0 v [] replace-map {}]
      (if (= ix n) v
        (let [sym (nth binding-vec ix)
              val (nth binding-vec (inc ix))
              replace-map1 (assoc replace-map sym (mark-sym sym))]
          (recur (+ ix 2) (conj v (sym-replace sym replace-map1) 
                            (sym-replace val replace-map)) replace-map1))))))

(defn fn-argpack-unique-tag [pack]
  "Makes ([x y & args] ...) have unique arguments.
   Uses gensym."
  (let [bindings (first pack)
        replace-map (dissoc (zipmap bindings (mapv mark-sym bindings)) '&)]
    (sym-replace pack replace-map)))

;;;;;;;;;;;;;;;;;;;;; Core pipeline functions ;;;;;;;;;;;;;;

(defn qual-all-syms [x]
  "Symbol qualification."
  (let [qualf (fn [x locals] 
                (if (and (symbol? x) (not (get locals x)))        
                  (if-let [sq (qual x)] sq x) x))]
    (cnav/locals-walk qualf x #{})))
 
(defn symbol-unique-tag [x] 
  "Gives local symbols unique tags in the form of foo -> foo_ix=1234.
   Symbols that shadow other symbols are renamed.
   Uses gensym, so there isn't a need to keep track of locals; however other functions will be deterministic."
  (let [walk-f (fn [form] (cond (binding-head? form)
                            (let [v (second form) syms0 (collections/evens v)
                                  v1 (binding-unique-tag v)
                                  rmap (zipmap (collections/evens v) (collections/evens v1))
                                  tail1 (sym-replace (nthrest form 2) rmap)]
                              (apply list (first form) v1 tail1))
                            (langs/unpacked-fn? form) ; (fn [foo] (+ foo bar))
                            (let [fake-argpack (rest form)
                                  fake-argpack1 (fn-argpack-unique-tag fake-argpack)]
                              (apply list (first form) fake-argpack1))
                            (fn-head? form) ; (fn ([foo] (+ foo bar)))
                            (let [arg-packs (rest form)]
                              (apply list (first form) (mapv fn-argpack-unique-tag (rest form))))
                            :else form))]
    (walk/postwalk walk-f x)))

(defn clean-gensym [x]
  "Gensym doesn't prioritize human readabiliy:
   (gensym 'foo) => foo12345.
   #(%) => p1__12345#
   `a# => a__12345__auto__.
   This function cleans it up."
  (let [clean-sym (fn [sym]
                    (if (textparse/qual? sym) sym
                      (let [s0-s1 (split-marked-sym sym) s0 (first s0-s1) s1 (second s0-s1)
                            s0 (string/replace s0 #"__\d\d\d+__auto__" "")
                            s0 (string/replace s0 #"p\d__\d+#" (str "%" (second s0)))
                            s0 (string/replace s0 #"\d\d\d+" "")]
                        (symbol (str s0 s1)))))]
    (walk/postwalk #(if (symbol? %) (clean-sym %) %) x)))

(defn local-symbol-modify [x f]
  "Modifies local symbols and ensues no local symbols 
   will end in numbers when unmarked-sym."
  (let [mod-sym (fn [sym] 
                  (if (textparse/qual? sym) sym
                    (let [s0-s1 (split-marked-sym sym) s0 (first s0-s1) s1 (second s0-s1)
                          s0-mod (f (symbol s0))
                          trail-num? (Character/isDigit (last (str s0-mod)))
                          s0-mod (if trail-num? 
                                   (if (> (count s1) 0) (remove-trailing-nums s0-mod) 
                                     (symbol (str s0-mod "_"))) s0-mod)] ; Don't end in a number.
                      (symbol (str s0-mod s1)))))]
    (walk/postwalk #(if (symbol? %) (mod-sym %) %) x)))

(def core-counts ; Used in clean-mark
  (reduce (fn [acc sym] 
    (let [symu (remove-trailing-nums sym)]
      (if (get acc symu) (update acc symu inc) (assoc acc symu 1))))
        {} (keys (ns-map 'clojure.core))))

(defn clean-mark [x]
  "Cleans up the symbol mark, making sure to not leave anything shadowed.
   Repeated symbols will be marked with 1,2,3,... to avoid shadowing."
  (let [unsorted-locals (set (filterv #(not (textparse/qual? %)) (cnav/all-syms x)))
        n (count unsorted-locals)
        paths (collections/paths x)
        ix2sym (reduce (fn [acc ix]
                         (let [p (nth paths ix) target (collections/cget-in x p)]
                           (if (get unsorted-locals target) 
                            (assoc acc ix target) acc)))
                   {} (range (count paths)))
        locals (filterv identity (distinct (mapv #(get ix2sym %) (range (count paths)))))
        unmarked-locals (mapv unmark-sym locals)
        local-is-marked? (mapv not= locals unmarked-locals)
        core-counts1 (reduce #(if (nth local-is-marked? %2) %1
                                (let [k (nth locals %2)]
                                  (assoc %1 k (inc (get %1 k 0)))))  
                       core-counts (range n))
        replace-map (loop [ix 0 acc {} counts core-counts1]
                      (if (= ix n) acc
                        (if (nth local-is-marked? ix)
                          (let [sym (nth locals ix) symu (nth unmarked-locals ix)
                                n-uses (get counts symu 0)
                                symu1 (if (= n-uses 0) symu (symbol (str symu n-uses)))]
                            (recur (inc ix) (assoc acc sym symu1)
                              (assoc counts symu (inc n-uses))))
                          (recur (inc ix) (assoc acc (nth locals ix) (nth locals ix)) counts))))]
    (sym-replace x replace-map)))

(defn condense-default-imports [x]
  "Condenses down clojure.core and java.lang, both of which are imported by default."
  (let [all-quals (filterv textparse/qual? (cnav/all-syms x))
        java-replace #(symbol (string/replace (str %) "java.lang.Math/" "Math/"))
        clojure-replace #(symbol (string/replace (str %) "clojure.core/" ""))
        replace-map (zipmap all-quals 
                      (mapv #(java-replace (clojure-replace %)) all-quals))]
    (sym-replace x replace-map)))


;;;;;;;;;;;;;;;;;;;;; A processing pipeline ;;;;;;;;;;;;;;

(defn pipeline [ns-sym form mexpand? & local-sym-modify-f]
  "The main code de-shadowing and qualification tool.
   Deterministic and doesn't change the name of any uniquely-named symbols if no local-sym-modify-f is supplied.
   If it changes what the code does there is a BUG somewhere!
   Optional macro-expand-all and symbol modification.
   For the purpose of logging, macroexpanding will run pipeline as a post-processing step as it represents a further standardization of the code."
  (binding [*ns-sym* (if (symbol? ns-sym) ns-sym (symbol (str ns-sym)))]
    (-> (if mexpand? (langs/mexpand *ns-sym* form) form)
      (qual-all-syms)
      symbol-unique-tag
      clean-gensym
      (local-symbol-modify (if (first local-sym-modify-f) (first local-sym-modify-f) identity))
      clean-mark)))

(defn ppipeline [ns-sym form mexpand? & local-sym-modify-f]
  "Pretty print that removes clojure.core and java.lang qualifiers that 99% of the time aren't human-useful."
  (binding [*ns-sym* (if (symbol? ns-sym) ns-sym (symbol (str ns-sym)))]
    (-> (if mexpand? (langs/mexpand *ns-sym* form) form)
      (qual-all-syms) symbol-unique-tag clean-gensym
      (local-symbol-modify (if (first local-sym-modify-f) (first local-sym-modify-f) identity))
      clean-mark condense-default-imports)))

;;;;;;;;;;;;;;;;;;;;;; Tracking where stuff end sup ;;;;;;;;;;;;;;;;;
; There is no best way to do this!

;;;;;;;; Macro fns ;;;;;;;;

(defn hilite-track [ns-sym code code-with-hilite]
  "Finds the path on the expanded code of the macro of the 'hilight'."
  (let [code-ex (pipeline ns-sym code true)
        code-hx (pipeline ns-sym code-with-hilite true)]
    (cnav/tree-diff code-ex code-hx)))

(defn macro-expand-path [ns-sym code path]
  "Where path goes on the macroexpanded code.
   Not gaurenteed to work in all cases (should return nil when fails)."
  (let [sy (gensym "Mark")
        f #(cond (collections/listy? %) (list sy)
             (vector? %) [sy]
             (set? %) #{sy}
             (map? %) {sy sy}
             (number? %) (Math/random)
             (symbol? %) sy
             :else (str sy)) 
        code-marked (collections/cupdate-in code path f)]
    (hilite-track ns-sym code code-marked)))







