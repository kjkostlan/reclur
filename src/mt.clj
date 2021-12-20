; MT =  Meta. Functions that work with metadata. Also tools for error throwing and reporting, which is meta in a sense.
; Takes priority above t for where fns go.

(ns mt (:require [clojure.walk :as walk] [t] [globals]))

(defn keep-meta [x f & args]
  "Applies f w/o affecting meta. Throws an error if x had meta and (f x) can't hold meta."
  (if (meta x)
    (with-meta (apply f x args) (meta x))
    (apply f x args)))

(defn dual-get-in [x ph-mph]
  "Simplify your paths.
   ph-mph is a tuple of paths, the first within x the second path within the meta."
  (let [ph (first ph-mph) mph (second ph-mph)]
    (t/cget-in (meta (t/cget-in x ph)) mph)))

(defn dual-assoc-in [x ph-mph v]
  "Simplify your paths.
   ph-mph is a tuple of paths, the first within x the second path within the meta."
  (let [ph (first ph-mph) mph (second ph-mph)]
    (t/cupdate-in x ph
      (fn [xi] (vary-meta xi #(t/cassoc-in % mph v))))))

(defn dual-update-in [x ph-mph f & args]
  "Simplify your paths.
   ph-mph is a tuple of paths, the first within x the second path within the meta."
  (let [ph (first ph-mph) mph (second ph-mph)]
    (t/cupdate-in x ph
      (fn [xi] (vary-meta xi #(apply t/cupdate-in % mph f args))))))

(defn m-postwalk [f form]
  "Preserves metadata when possible, unless f destroys metadata."
  (let [reset-meta #(with-meta % (meta form))
        walk-f #(m-postwalk f %)]
    (cond (map? form) (f (reset-meta (zipmap (mapv walk-f (keys form)) (mapv walk-f (vals form)))))
      (coll? form) (let [form1 (mapv walk-f form)
                         form2 (cond (vector? form) (into [] form1)
                                 (set? form) (set form1) :else (apply list form1))]
                     (f (reset-meta form2)))
      :else (f form))))

(defn _pmwalk [f ph x]
  (f ph
    (#(if (meta x) (with-meta % (meta x)) x)
      (cond (map? x) (zipmap (keys x) (mapv #(t/pwalk1 f (conj ph %1) %2) (keys x) (vals x)))
        (set? x) (set (mapv #(t/pwalk1 f (conj ph %) %) x))
        (vector? x) (mapv #(t/pwalk1 f (conj ph %1) %2) (range) x)
        (coll? x) (apply list (mapv #(t/pwalk1 f (conj ph %1) %2) (range) x))
        :else x))))
(defn pm-postwalk [f x]
  "Pathed post walk, calls (f path subform). Not lazy.
   Preserves metadata, unless f destroys metadata."
  (_pmwalk f [] x))

(defn m-unpack [x]
  "Unpacks metadata with (| meta-data bar), | is a symbol which does not create read-string confusion the way ^ would.
   Only unpacks data which has metadata. This fn is most useful for reporting without needing *print-meta*."
  (walk/prewalk #(if-let [m (meta %)]
                   (let [no-meta (with-meta % nil)]
                     (list (symbol "|") m no-meta)) %) x))

(defn m-pack [x]
  "Packs metadata, undoing the effect of m-unpack."
  (walk/prewalk #(if (and (list? %) (or (= (first %) (symbol "|")) (= (first %) (symbol "^"))))
                   (with-meta (c/third %) (second %)) %) x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Clojures nice syntax quote, but with better meta-preservation ;;;;;;;;;;;;;;;;;;;

(defn _apply-env [x env in-quote?]
  "Stuff not in a quote will be replaced by env.
   All quoted symbols are resolved."
  (let [x (if (meta x) (vary-meta x #(_apply-env % env in-quote?)) x)
        old-meta (fn [x1] (if (or (meta x) (meta x1)) (with-meta x1 (merge (meta x1) (meta x))) x1))
        _apply-env1 (fn [x1] (_apply-env x1 env in-quote?))
        single-arg #(throw (Exception. (str "Two many arguments to " % " because " % " only takes one arg.")))]
    (old-meta
      (cond (map? x) (zipmap (keys x) (mapv _apply-env1 (vals x)))
        (vector? x) (mapv _apply-env1 x)
        (set? x) (set (mapv _apply-env1 x))
        (and (symbol? x) in-quote?) (if-let [x1 (resolve x)] (symbol x1) x)
        (and (symbol? x) (not in-quote?)) (if-let [x1 (get env x)] x1 x)
        (not (coll? x)) x
        (and in-quote? (or (= `unquote (first x)) (= `unquote-splicing (first x))))
         (if (> (count x) 2) (single-arg "unquote") (list (first x) (_apply-env (second x) env false)))
        (and (not in-quote?) (= 'quote (first x))) (list (first x) (_apply-env (second x) env true))
        :else (apply list (mapv _apply-env1 x))))))
(defn _meta-inside-quotes1 [form]
  "Metadata attached to (quote x) will also be attached to x.
   Same with unquote and unquote-splicing."
  (if (and (c/listy? form) (meta form) (second form)
        (contains? #{'quote 'unquote `unquote 'unquote-splicing `unqoute-splicing} (first form)))
    (c/cupdate form 1 #(with-meta % (merge (meta form) (meta %)))) form))
(defn _m-quote-core [x in-quote? resolve-syms?]
  (let [metax (if (meta x) (_m-quote-core (meta x) in-quote? resolve-syms?))
        x (if metax (with-meta x {}) x) ; Removes spurous quote-unquotes.

        single-arg-assert #(throw (Exception. (str "Two many arguments to " % " because " % " only takes one arg.")))

        ; Unquote splicing:
        xv (if (or (sequential? x) (set? x)) (into [] x))
        uqsp? #(and (c/listy? %) (= (first %) `unquote-splicing))
        xv1 (if (and xv (first (filter uqsp? xv)))
              (reduce (fn [acc xi]
                        (if (uqsp? xi)
                          (let [_ (if (> (count xi) 2) (single-arg-assert "unquote-splicing"))
                                body (eval (second xi)) m (merge (meta xi) (meta body))]
                            ; Though meta is less often used in unquote-splicing.
                            (c/vcat acc (mapv (fn [x] (if m (vary-meta x #(merge m %)) x)) body)))
                          (conj acc xi))) [] xv))
        x (if xv1 (cond (set? x) (set xv1) (vector? x) xv1 :else (apply list xv1)) x)

        add-metax (fn [y] (if (or (meta y) metax) (with-meta y (merge metax (meta y))) y))]
    (add-metax
      (cond (map? x) (zipmap (keys x) (mapv #(_m-quote-core % in-quote? resolve-syms?) (vals x)))
        (vector? x) (mapv #(_m-quote-core % in-quote? resolve-syms?) x)
        (set? x) (set (mapv #(_m-quote-core % in-quote? resolve-syms?) x))
        (symbol? x) (if in-quote? (if-let [rx (and resolve-syms? (resolve x))] (symbol rx) x)
                      (throw (Exception. (str x " not in env (all syms outside of quote must be in env)."))))
        (not (coll? x)) x
        (and in-quote? (contains? #{'unquote 'clojure.core/unquote} (first x)))
        (let [body (second x)
              _ (if (> (count x) 2) (single-arg-assert "unquote"))

              obj (eval body)
              obj1 (_m-quote-core obj true false)]
          (if (meta obj) (vary-meta obj1 #(merge % (meta obj))) obj1))
        (and (= 'quote (first x)) (not in-quote?))
        (let [body (_m-quote-core (second x) true true)
              _ (if (> (count x) 2) (single-arg-assert "quote"))] body)
        :else (apply list (mapv #(_m-quote-core % in-quote? resolve-syms?) x))))))
(defn m-quotes [x env ns-or-sym]
  "Similar to syntax quotes with nested quotes and unquotes, etc but with better meta.
   Do NOT use syntax quotes inside the expression, use normal quotes.
   Differences: (^:foo ~bar) WILL put metadata in bar, at a lower priority than what is attached to bar already.
   Symbols will be resolved if possible, else they will remain unqualled rather than qualified in *ns*.
   Best if used through sq+"
  (binding [*ns* (cond (not ns-or-sym) *ns* (symbol? ns-or-sym) (find-ns ns-or-sym) :else ns-or-sym)]
    (let [envq (zipmap (keys env) (mapv #(with-meta (list 'quote %) (meta %)) (vals env)))
          x1 (m-postwalk _meta-inside-quotes1 (_apply-env x envq true))]
      (_m-quote-core x1 true true))))

(defmacro sq+ [x]
  "The macro version of m-quotes which avoids needing to manually build the env.
   It still runs at runtime, not macro-expansion time, and is usually used for functions that generate code."
  ;m-quotes [x env ns-sym]
  (let [env-code (zipmap (mapv #(list 'quote %) (keys &env)) (keys &env))]
    `(m-quotes ~(list 'quote x) ~env-code nil))) ; Use nil, do not pass in the namespace sym as it does not exist during compile.

(defn meta-unshortcut [& args]
  "Returns a map with ^foo is a :tag, ^:bar is :bar true, etc."
  (reduce (fn [acc arg]
            (merge acc
              (cond (map? arg) arg
                (coll? arg) {:tag arg} ; this throws an error in vanilla.
                (keyword? arg) {arg true}
                :else {:tag arg}))) {} args))

(defmacro sq+m [x & meta-args]
  "Equivalant to (with-meta (sq+ x) m and supports meta shorthands.
   The contents of meta-args are NOT passed through an sq+ and thus need to be quoted, etc."
  `(with-meta (sq+ ~x) ~(apply list `meta-unshortcut meta-args)))

;;;;; Error reporting is meta to the standard call stack in in the sense that it can break out of the call stack ;;;;

(defn error [& args] "Throws an error where all args are put into a string like println"
  (throw (Exception. (apply str (interpose " " args)))))

(defn error+ [msg x & log-only]
  "Errors with msg, and logs x which should be too large for a simple message.
   Msg can be a vector or str."
  (let [msg (if (sequential? msg) (apply str msg) (str msg))
        msg (str msg " [M-p opens dump].")]
    (swap! globals/log-atom #(assoc % ::error-plus x))
    (if (first log-only) (println msg " [M-p opens details].")
      (throw (Exception. msg)))))

(defn get-error+ []
  "Returns the thing logged as the last error."
  (::error-plus @globals/log-atom))
