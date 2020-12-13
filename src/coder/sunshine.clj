; Qualification, deshadowing, and deterministic-nice-naming tool.
(ns coder.sunshine
  (:require [clojure.set :as set]
    [clojure.string :as string]
    [clojure.walk :as walk]
    [coder.cnav :as cnav]
    [coder.crosslang.langs :as langs]
    [coder.textparse :as textparse]
    [collections]))

(defn let-swap [code]
  "Binding symbols should be bound once they are defined.
   But let statement's arent: (let [a a] a) the second a isn't bound.
   If we switch even/odd, now ODD symbols will be bound-on-declaration: (let [1 a] a).
   Call this function again to restor the original code."
  (let [walkf (fn [form]
                (if (and (collections/listy? form) 
                      (contains? #{'let `let 'let*} (first form)))
                  (apply list (first form)
                    (let [v (second form)] 
                      (into [] (interleave (take-nth 2 (rest v)) (take-nth 2 v))))
                    (rest (rest form))) form))]
    (walk/postwalk walkf code)))

(defn _tovec [form] (cond (not (coll? form)) form (map? form) (into [] (apply concat form)) :else (into [] form)))
(defn _ty [form] (cond (vector? form) 1 (map? form) 2 (set? form) 3 (coll? form) 0 :else -1))
(defn flat-tokenize-mark [code]
  "Returns two vectors: Tokens: The flattened code, with symbols encoding ()[]{}#{} in lieu of the associated collection.
                        Bind?s: Does said token coorespond to a symbol binding, i.e. the [a b c] in (fn [a b c])."
  (let [lo (symbol "(") lc (symbol ")") vo (symbol "[") vc (symbol "]")
        mo (symbol "{") mc (symbol "}") so (symbol "#{") sc (symbol "}")
        let-set #{'let `let 'let*} fn-set #{'fn 'fn* `fn 'defn `defn}
        confuse-set (conj (set/union let-set fn-set) 'loop 'loop* 'recur 'recur* 'binding 'binding*)
        vassoc (fn [v ix k] (if (>= ix (count v)) (conj v k) (assoc v ix k)))]
    (if (coll? code) ; non-collections don't really do much.
      (loop [toks [] ; flat tokens. 
             bind?s [] ; is token 
             lev 0 ; number of levels deep. 
             pathv [] ; current path; path to (nth subvforms lev). It's all numbers.
             subvforms [(_tovec code)] ; List of smaller and smaller subforms of the code, each in vector form.
             subform-types [(_ty code)]] ; 0 = list, 1 = vec, 2 = map, 3 = set.
        (let [subv (get subvforms lev) ; vector unless leaf.
              end-of-coll? (and (> lev 0) (>= (nth pathv (dec lev)) (count (get subvforms (dec lev)))))]
          (cond end-of-coll? (let [ty (get subform-types (dec lev)) ; close with this type, then quit or move on.
                                   toks1 (conj toks (cond (= ty 0) lc (= ty 1) vc (= ty 2) mc :else sc))
                                   bind?s1 (conj bind?s false)]
                               (if (<= lev 1) [toks1 bind?s1]
                                 (let [p2+ (inc (get pathv (- lev 2)))
                                       nuform (get (get subvforms (- lev 2)) p2+)]
                                   (recur toks1 bind?s1 (dec lev) (assoc pathv (- lev 2) p2+)
                                     (assoc subvforms (dec lev) (_tovec nuform))
                                     (assoc subform-types (dec lev) (_ty nuform)))))) 
            (vector? subv) ; dig into sub form.
            (let [ty (get subform-types lev)]
              (recur (conj toks (cond (= ty 0) lo (= ty 1) vo (= ty 2) mo :else so))
                (conj bind?s false) (inc lev) (vassoc pathv lev 0) (vassoc subvforms (inc lev) (_tovec (first subv)))
                (vassoc subform-types (inc lev) (_ty (first subv)))))
            :else ; Non vector forms. The hard part is to determine if a symbol is a binding symbols.
            (let [p1 (get pathv (dec lev)) p2 (get pathv (- lev 2))
                  sym? (and (symbol? subv) (not= subv '&))
                  in-vec? (if sym? (= (get subform-types (dec lev)) 1))
                  boss1 (if (= (get subform-types (- lev 1)) 0) (get subvforms (- lev 1))) ; listy only!
                  boss2 (if (and in-vec? (= (get subform-types (- lev 2)) 0)) (get subvforms (- lev 2)))
                  boss3 (if (and in-vec? (= (get subform-types (- lev 3)) 0)) (get subvforms (- lev 3)))
                  let-bind? (and boss2 (contains? let-set (first boss2))
                              (odd? p1)) ; odd not even because of let-swap.
                  fn-bind? (or (and boss2 (or (= p2 1) (and (= p2 2) (not (coll? (second boss2)))))
                                 (contains? fn-set (first boss2))) ; unpacked
                             (and boss3 (= p2 0)) ; packed
                             (and boss1 (symbol? subv) (contains? fn-set (first boss1))
                               (= p1 1))) ; bound as function name.
                  bind? (if (or let-bind? fn-bind?) subv false)
                  new-leafish (get (get subvforms (dec lev)) (inc p1))]
              (if (and bind? (contains? confuse-set subv))
                (throw (Exception. (str "A symbol is bound to: " subv " which is super-confusing!"))))
              (recur (conj toks subv) (conj bind?s (if bind? bind? false)) lev (assoc pathv (dec lev) (inc p1))
                (assoc subvforms lev (_tovec new-leafish))
                (assoc subform-types lev (_ty new-leafish)))))))
      [[code] [false]])))

(defn unbinds [tokens bind?s]
  "Unbinds are at closing brackets. Nil when nothing is unbound."
  (let [lo (symbol "(") lc (symbol ")") mo (symbol "{") 
        vo (symbol "[") vc (symbol "]") mc (symbol "}")
        so (symbol "#{") sc (symbol "}") n (count tokens) ]
    (loop [acc [] ix 0 lev 0
           binds-at-lev {}] ; vector of sets.
      (if (= ix n) acc
        (let [tok (nth tokens ix)
              open? (or (= tok lo) (= tok vo) (= tok mo) (= tok so))
              close? (or (= tok lc) (= tok vc) (= tok mc) (= tok sc))
              bind? (nth bind?s ix) fn-name? (and bind? (contains? #{'fn 'fn* `fn 'defn `defn} (nth tokens (dec ix))))
              bl (get binds-at-lev lev) 
              unbinds (if close? (get binds-at-lev (inc lev)))] ; unbind when we drop TWO levels, except fn-names
          (recur (conj acc unbinds)
            (inc ix) (cond open? (inc lev) close? (dec lev) :else lev)
            (if bind? (assoc binds-at-lev (if fn-name? (inc lev) lev) 
                        (conj (if bl bl #{}) bind?)) binds-at-lev)))))))

(defn clean-sym [sym]
  "Clojure's singleton nondeterministic symbol generation definitly makes macros easier, but no one says it is easy to read.
   Symbols are guarenteed not to end in a number; this is so that 
   No two clean symbols can map to the same deshadow-sym'ed symbol."
  (let [s (str sym) s0 (first s) s1 (second s) s2 (get s 2) n (count s)
        num? #(string/includes? "0123456789" (str %))
        sclean (cond (re-find #"p\d+__\d+" s) ; inline fns.
              (if (num? s2) (str "%" s1 s2) (str "%" s1))
              (and (> n 4) (num? (get s (dec n))) (num? (get s (- n 2))) (num? (get s (- n 3))) (num? (get s (- n 4)))) ; gensyms.
              (subs s 0 (loop [ix (dec n)] (if (and (> ix -1) (num? (get s ix))) (recur (dec ix)) (inc ix))))
              (string/includes? s "__auto__") ; Hygenic macros.
              (string/replace s #"__\d+__auto__" "")
              :else (str sym))
        sclean (str sclean)
        nc (count sclean)]
    (symbol (if (num? (last sclean)) (str sclean "$") sclean))))
(defn deshadow-sym [sym n-uses]
  "Nice-looking removal of shadows."
  (symbol (cond (= n-uses 0) sym 
            (string/includes? "0123456789" (str (last (str sym))))
            (str sym "_" n-uses) :else (str sym n-uses))))

(def core-clean2count ; A count of sorts for clojure.core symbols 
  ; We also don't allow shadowing core symbols (more an asthetic choice).
  ; In other words, (deshadow-sym (clean x) n) must not equal core symbols over all possible x and n.
  ; To ensure this we need to extract the highest number.
  (let [syms (into [] (keys (ns-map 'clojure.core)))
        syms-clean (mapv clean-sym syms)
        counts (mapv (fn [sym] (Integer/parseInt (str "0" (string/replace sym #"\D" "")))) syms)]
    (reduce (fn [acc ix]
              (let [cl (nth syms-clean ix)
                    c0 (get acc cl 0) c1 (nth counts ix)]
                (assoc acc cl (inc (max c0 c1))))) {} (range (count syms)))))

(defn sym-replace [ns-sym tokens bind?s unbind?s local-sym-modify-f?]
  "Replaces tokens with the cleaned and unshadowed symbols.
   Cleaning is applied after local-sym-modify-f?, not ideal but safer."
  (let [n (count tokens)
        qual-fn #(langs/resolved ns-sym %) ; Qualification for clojure or other namespaces.
        clean-fn (if local-sym-modify-f? #(clean-sym (local-sym-modify-f? %)) #(clean-sym %))
        open-closes (set (mapv symbol ["(" ")" "{" "}" "[" "]" "#{" "}"]))
        pair (loop [ix 0 toks tokens clean2count core-clean2count bounds #{}] ; Add non-local symbols
               (if (= ix n) [clean2count toks]
                 (let [tok (nth tokens ix)
                       add? (nth bind?s ix) remove? (nth unbind?s ix)
                       bounds (if add? (conj bounds add?) bounds) 
                       bounds (if remove? (set/difference bounds remove?) bounds)
                       tok-qual? (if (symbol? tok) (qual-fn tok))
                       unbound-tok-clean? (if (and (symbol? tok) (not tok-qual?) (not (get bounds tok))
                                                (not (contains? open-closes tok)))
                                           (clean-fn tok)) ; unbound but unqualifiable.
                       clean2count1 (if unbound-tok-clean?
                                       (assoc clean2count unbound-tok-clean? 
                                         (inc (get clean2count unbound-tok-clean? 0))) clean2count)]
                   (recur (inc ix) (if tok-qual? (assoc toks ix tok-qual?) toks)
                     clean2count1 bounds))))
        cleansym2count (first pair) ; How many times each cleaned symbol is used up by NON LOCAL syms, including core.
        tokens-qual (second pair)]
    (loop [ix 0 toks tokens-qual bounds #{} replace-map {} clean2count cleansym2count] ; bounds is un NON replaced tokens.
      (if (= ix n) toks
        (let [tok (nth tokens ix)
              add? (if (nth bind?s ix) tok) remove? (nth unbind?s ix)
              bounds (if add? (conj bounds add?) bounds)
              bounds (if remove? (set/difference bounds remove?) bounds)]
          (if add? ; New symbols mean its replace map time.
            (let [tok-clean (clean-fn tok)
                  n-uses (get clean2count tok-clean 0)
                  tok-deshadow (deshadow-sym tok-clean n-uses)
                  replace-map (assoc replace-map tok tok-deshadow)]
              (recur (inc ix) (assoc toks ix tok-deshadow) bounds replace-map (assoc clean2count tok-clean (inc n-uses))))
            (recur (inc ix) 
              (if-let [tok1 (if (and (symbol? tok) (get bounds tok)) ; local tokens get replaced. 
                              (get replace-map tok))] (assoc toks ix tok1) toks)
              bounds replace-map clean2count)))))))

(defn unflat [tokens]
  "Un-flattens the collections."
  (let [n (count tokens) lo (symbol "(") lc (symbol ")") 
        vo (symbol "[") vc (symbol "]") mo (symbol "{")
        so (symbol "#{") sc (symbol "}") mc (symbol "}")]
    (loop [nested-forms {} lev 0 opening-toks [] ix 0]
      (if (= ix n) (first (get nested-forms -1)) ; nested-forms is smaller and smaller as we go from outer to inner forms.
        (let [tok (nth tokens ix)]
          (cond (or (= tok lo) (= tok vo) (= tok mo) (= tok so)) ; opening
            (recur (assoc nested-forms lev []) ; start new vector.
              (inc lev) (assoc opening-toks lev tok) (inc ix))
            (or (= tok lc) (= tok vc) (= tok mc) (= tok sc)) ; closing
            (let [open (nth opening-toks (dec lev))
                  formv (get nested-forms (dec lev))
                  formx (cond (= open lo) (apply list formv)
                          (= open vo) formv
                          (= open mo) (apply hash-map formv)
                          (= open so) (set formv))]
              (recur (assoc (assoc nested-forms (dec lev) []) (- lev 2) 
                       (conj (get nested-forms (- lev 2)) formx))
                (dec lev) opening-toks (inc ix)))
            :else ; leaf forms.
            (recur (assoc nested-forms (dec lev) (conj (get nested-forms (dec lev)) tok))
              lev opening-toks (inc ix))))))))

;;;;;;;;;;;;;;;;;;;;; A processing pipeline ;;;;;;;;;;;;;;

(defn pipeline [ns-sym code mexpand? & local-sym-modify-f]
  "The main code de-shadowing and qualification tool.
   Deterministic even with gensym, etc in the code. Only does local symbol substitution (apart from mexpand?)
   If it changes what the code does there is a BUG somewhere!
   mexpand? must be true for some cases such as more complex let statments."
  (let [code (if mexpand? (langs/mexpand ns-sym code) code)
        form-swap (let-swap code)
        ns-sym (if (symbol? ns-sym) ns-sym (symbol (str ns-sym)))
        tb (flat-tokenize-mark form-swap) tokens (first tb) bind?s (second tb)
        unbind?s (unbinds tokens bind?s)
        tokens1 (sym-replace ns-sym tokens bind?s unbind?s (first local-sym-modify-f))
        form-swap1 (unflat tokens1)]
    (let-swap form-swap1)))

(defn pretty-condense [x]
  "Condenses down clojure.core and java.lang, both of which are imported by default.
   Used for visualization not for metaprogramming."
  (let [all-quals (filterv textparse/qual? (cnav/symbols-in x))
        java-replace #(symbol (string/replace (str %) "java.lang.Math/" "Math/"))
        clojure-replace #(symbol (string/replace (str %) "clojure.core/" ""))
        replace-map (zipmap all-quals 
                      (mapv #(java-replace (clojure-replace %)) all-quals))]
    (walk/postwalk #(if (symbol? %) (get replace-map % %) %) x)))

(defn ppipeline [ns-sym form mexpand? & local-sym-modify-f]
  "This version is more beautiful as it removes clojure.core and java.lang qualifiers that 99% of the time aren't human-useful."
  (pretty-condense (pipeline ns-sym form mexpand? (first local-sym-modify-f))))

;;;;;;;;;;;;;;;;;;;;;; Tracking where stuff end sup ;;;;;;;;;;;;;;;;;
; There is no best way to do this!

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