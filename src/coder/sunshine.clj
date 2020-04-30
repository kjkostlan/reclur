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
    [coder.cnav :as cnav :refer [third fourth evens odds]]
    [coder.crosslang.langs :as langs]))

(def ^:dynamic *ns-sym* 'coder.sunshine)

;;;;;;;;;;;;;;;;; Symbol functions ;;;;;;;;;;;;;;;;


(defn branch-sym [sym] (if (= sym '/) sym (symbol (first (string/split (str sym) #"/")))))
(defn leaf-sym [sym] (symbol (second (string/split (str sym) #"/"))))
(defn qual? [sym] (> (count (string/split (str sym) #"/")) 1))
; TODO: swallow exception is a problem.
(defn qual [sym] 
  (if-let [r (try (langs/resolved *ns-sym* sym) (catch Exception e false))] r 
    (if-let [r1 (try (langs/resolved 'clojure.core sym) (catch Exception e false))]
      r1 false)))

(defn resolve-class [sym]
  "Namespace-dependent (I think), returns a symbol. Nil if can't be resolved."
  ; TODO: since this depends on namespaces it needs to be incorporated into langs/resolved.
  (if (symbol? sym)
    (try (if (= (type (eval sym)) java.lang.Class)
           (second (string/split (str (eval sym)) #" "))) 
      (catch Exception e nil))))
(defn power-resolve [sym]
  (if-let [sym-qual (qual sym)] sym-qual (resolve-class sym)))

(defn remove-trailing-nums [sym]
  "Useful when the trailing nums should be used to de-shadow."
  (loop [melt (str sym)]
    (if (Character/isDigit (last melt))
      (recur (subs melt 0 (dec (count melt)))) 
      (symbol melt))))

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

(defn djava1 [x]
  "Combines static java calls into one symbol.
   (. Math sin 1.2) => (java.lang.Math/sin 1.2) which can be treated like a qualified symbol."
  (cond (not (collections/listy? x)) x
    (= (first x) 'new) (apply list (symbol (str (second x) ".")) (nthrest x 2))
    (and (= (first x) '.) (resolve-class (second x))) 
    (apply list (symbol (str (resolve-class (second x)) "/" (third x))) (nthrest x 3))
    (resolve-class (branch-sym (first x))) 
    (apply list (symbol (str (resolve-class (branch-sym (first x))) "/" (leaf-sym (first x)))) (rest x)) ; resolve
    :else x))

(defn sym-replace [form rmap]
  "Recursive."
  (walk/postwalk #(if (symbol? %) (get rmap % %) %) form))

(defn binding-head? [x]
  "Does the second element of x define binding pairs?"
  (and (collections/listy? x) (contains? #{`let `let* `loop `loop* `binding} (first x))))

(defn fn-head? [x]
  "Does the second element of x define binding pairs?"
  (and (collections/listy? x) (contains? #{`fn `fn*} (first x))))

(defn unpacked-fn? [form]
  "Functions without packed arguments. Making all arguments packed can make things easier but changes the path."
  (if (and (collections/listy? form) (contains? #{`fn `fn*} (first form)))
    (cond (vector? (second form)) 1 (vector? (third form)) 2 :else false)))

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

(defn qual-all-syms [x java-combine? fn-pack?]
  "Symbol qualification, java combining, and function packing."
  (let [x1 (if java-combine? (walk/postwalk djava1 x) x)
        qualf (fn [x locals] 
                (if (and (symbol? x) (not (get locals x)))        
                  (if-let [sq (qual x)] sq x) x))
        x2 (cnav/locals-walk qualf x #{})
        x3 (if fn-pack?
             (walk/postwalk #(if-let [upk (unpacked-fn? %)] 
                             (if (= upk 1) (list (first %) (rest %)) 
                               (list (first %) (second %) (rest %))) %) x2)
             x2)]
    x3))

(defn symbol-unique-tag [x] 
  "Gives local symbols unique tags in the form of foo -> foo_ix=1234.
   Symbols that shadow other symbols are renamed.
   Uses gensym, so there isn't a need to keep track of locals."
  (let [walk-f (fn [form] (cond (binding-head? form)
                            (let [v (second form) syms0 (evens v)
                                  v1 (binding-unique-tag v)
                                  rmap (zipmap (evens v) (evens v1))
                                  tail1 (sym-replace (nthrest form 2) rmap)]
                              (apply list (first form) v1 tail1))
                            (unpacked-fn? form) ; (fn [foo] (+ foo bar))
                            (let [fake-argpack (rest form)
                                  fake-argpack1 (fn-argpack-unique-tag fake-argpack)]
                              (apply list (first form) fake-argpack1))
                            (fn-head? form) ; (fn ([foo] (+ foo bar)))
                            (let [arg-packs (rest form)]
                              (apply list (first form) (mapv fn-argpack-unique-tag (rest form))))
                            :else form))]
    (walk/postwalk walk-f x)))

(defn clean-gensym [x]
  "Gensym doesn't prioritize human readabiliy.
   (gensym 'foo) => foo12345.
   #(%) => p1__12345#
   `a# => a__12345__auto__"
  (let [clean-sym (fn [sym]
                    (if (qual? sym) sym
                      (let [s0-s1 (split-marked-sym sym) s0 (first s0-s1) s1 (second s0-s1)
                            s0 (string/replace s0 #"__\d\d\d+__auto__" "")
                            s0 (reduce #(string/replace %1 
                                          (re-pattern (str "p" %2 "__\\d\\d\\d+__#")) 
                                          (str "%" %2)) 
                                 s0 (range 1 20))
                            s0 (string/replace s0 #"\d\d\d+" "")]
                        (symbol (str s0 s1)))))]
    (walk/postwalk #(if (symbol? %) (clean-sym %) %) x)))

(defn local-symbol-modify [x f]
  "Modifies local symbols and ensues no local symbols 
   will end in numbers when unmarked-sym."
  (let [mod-sym (fn [sym] 
                  (if (qual? sym) sym
                    (let [s0-s1 (split-marked-sym sym) s0 (first s0-s1) s1 (second s0-s1)
                          s0-mod (remove-trailing-nums (f (symbol s0)))] ; Don't end in a number.
                      (symbol (str s0-mod s1)))))]
    (walk/postwalk #(if (symbol? %) (mod-sym %) %) x)))

(defn clean-mark [x]
  "Cleans up the symbol mark, making sure to not leave anything shadowed.
   Repeated symbols will be marked with 1,2,3,... to avoid shadowing."
  (let [unsorted-locals (set (filterv #(not (qual? %)) (cnav/all-syms x)))
        n (count unsorted-locals)
        paths (collections/paths x)
        ix2sym (reduce (fn [acc ix]
                         (let [p (nth paths ix) target (collections/cget-in x p)]
                           (if (get unsorted-locals target) 
                            (assoc acc ix target) acc)))
                   {} (range (count paths)))
        locals (filterv identity (distinct (mapv #(get ix2sym %) (range (count paths)))))
        core-counts (zipmap (mapv unmark-sym locals) (repeat 0))
        ; Special for clojure.core:
        core-counts (reduce (fn [acc sym] 
                              (let [symu (remove-trailing-nums sym)]
                                (if (get acc symu) (update acc symu inc) acc)))
                      core-counts (keys (ns-map 'clojure.core)))
        replace-map (loop [ix 0 acc {} counts core-counts]
                      (if (= ix n) acc
                        (let [sym (nth locals ix) symu (unmark-sym sym)
                              n-uses (get counts symu)
                              symu1 (if (= n-uses 0) symu (symbol (str symu n-uses)))]
                          (recur (inc ix) (assoc acc sym symu1)
                            (assoc counts symu (inc n-uses))))))]
    (sym-replace x replace-map)))

(defn condense-default-imports [x]
  "Condenses down clojure.core and java.lang, both of which are imported by default."
  (let [all-quals (filterv qual? (cnav/all-syms x))
        java-replace #(symbol (string/replace (str %) "java.lang.Math/" "Math/"))
        clojure-replace #(symbol (string/replace (str %) "clojure.core/" ""))
        replace-map (zipmap all-quals 
                      (mapv #(java-replace (clojure-replace %)) all-quals))]
    (sym-replace x replace-map)))


;;;;;;;;;;;;;;;;;;;;; Putting it all together ;;;;;;;;;;;;;;

(defn pipeline [ns-sym form mexpand? & local-sym-modify-f]
  "The main code de-shadowing and qualification tool.
   Optional macro-expand-all and symbol modification."
  (binding [*ns-sym* ns-sym]
    (-> (if mexpand? (walk/macroexpand-all form) form)
      (qual-all-syms false false) symbol-unique-tag clean-gensym
      (local-symbol-modify (if (first local-sym-modify-f) (first local-sym-modify-f) identity))
      clean-mark)))

(defn fpipeline [ns-sym form mexpand? & local-sym-modify-f]
  "Makes functions and javs easier to manage."
  (binding [*ns-sym* ns-sym]
    (-> (if mexpand? (walk/macroexpand-all form) form)
      (qual-all-syms true true) symbol-unique-tag clean-gensym
      (local-symbol-modify (if (first local-sym-modify-f) (first local-sym-modify-f) identity))
      clean-mark)))

(defn ppipeline [ns-sym form mexpand? & local-sym-modify-f]
  "This one cleans up clojure.core and java.lang stuff as well."
  (binding [*ns-sym* ns-sym]
    (-> (if mexpand? (walk/macroexpand-all form) form)
      (qual-all-syms true false) symbol-unique-tag clean-gensym
      (local-symbol-modify (if (first local-sym-modify-f) (first local-sym-modify-f) identity))
      clean-mark condense-default-imports)))









