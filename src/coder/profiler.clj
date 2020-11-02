; TODO: this makes heavy use of clojure-only functions. They should be refactored to langs.

(ns coder.profiler
  (:require 
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.main :as main]
    [collections]
    [coder.cnav :as cnav]
    [coder.cbase :as cbase]
    [coder.crosslang.langs :as langs]
    [coder.sunshine :as sunshine]
    [coder.textparse :as textparse]
    [coder.logger :as logger]
    [coder.unerror :as unerror]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Support functions that want to be in collections ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn m-conj [m k v]
  ; Better for collections?
  "Like assoc but conj's v onto (get m k [])."
  (assoc m k (conj (get m k []) v)))

(defn dim-mix [x dim-source & convert-dims-to-vector-form?s]
  "Generic map dimension rearrange and recombine. Stubby paths get removed if stubbier than dim-source's maximum.
   dim-source: dimensions going into each output dimension. Each element is a scalar or vector.
   Multible dimensions into one dimension: later varies faster (as numpy's np).
   convert-dims-to-vector-form?s: convert map entries to vectors (sorts the keys if activated).
   This can only rearrange or combine dimensions, it can't split them.
   (splitting would need a size)."
  ; TODO: does this belong in collecitons?
  (let [paths (set (collections/paths x))
        dim-source (mapv #(if (coll? %) (into [] %) [%]) dim-source)
        ndim (inc (apply max (apply concat dim-source))) ; # of dims we actualy care about.
        cut #(if (> (count %) ndim) (subvec % 0 ndim) %)
        paths-short (set (filterv #(>= (count %) ndim) (mapv cut paths)))
        ; dim-source [1 [2 3] 2]
        old2new (reduce (fn [acc ix] 
                          (reduce #(assoc %1 %2 ix) 
                            acc (nth dim-source ix)))
                  {} (range (count dim-source))) ; Old path index to new path index collapsed.
        map2vec (fn [m] (mapv #(get m %) (sort (keys m))))
        change-ph (fn [ph] ; Change from old to new path.
                    (mapv #(if (= (count %) 1) (first %) %)
                      (map2vec (reduce #(let [jx (get old2new %2)]
                                          (m-conj %1 jx (nth ph %2))) 
                                 {} (range (count ph))))))
        xr (reduce 
             (fn [acc ph]
               (let [add (collections/cget-in x ph)]
                 (assoc-in acc (change-ph ph) add)))
             {} paths-short)
        ; Convert map levels to vec levels:
        convert?s (first convert-dims-to-vector-form?s)]
    (if (map? convert?s) (throw (Exception. "convert-dims-to-vector-form?s, if supplied, must be a vector.")))
    (if (not convert?s) xr
      (collections/dpwalk 
        (fn [path val]
          (let [depth (count path)]
            (if (and (map? val) (get convert?s depth))
              (map2vec val) val))) xr (count convert?s)))))

(defn _dim-split1 [x] 
  (cond (not (map? x)) x
    (= (count x) 0) x
    :else 
    (let [kys (into [] (keys x))
          nk (mapv count kys)
          c0 (apply min nk) c1 (apply max nk)]
      (cond (< c0 c1) (throw (Exception. "Uneven number of key lengths; can't dim split (or it would be more complex)."))
        (= c1 1) x
        :else (reduce-kv #(assoc-in %1 %2 %3) {} x)))))
(defn dim-split [x dims]
  (let [max-dim (apply max 0 dims)
        split?s (reduce #(assoc %1 %2 true) (into [] (repeat max-dim false)) dims)]
    (collections/dpwalk (fn [ph x] (if (get split?s (count ph)) (_dim-split1 x) x)) 
      x max-dim)))

(defn dim-map2vec [x dims]
  "Converts maps to vectors at specified nesting levels. 
   Maps must have integer keys."
  (let [max-dim (apply max 0 dims)
        vec?s (reduce #(assoc %1 %2 true) (into [] (repeat max-dim false)) dims)]
    (collections/dpwalk (fn [ph x] 
                          (if (or (not (map? x)) (not (get vec?s (count ph)))) x
                            (let [kys (into [] (keys x)) max-k (apply max kys)]
                              (if (first (remove integer? kys))
                                (throw (Exception. "Map key not an integer or long for converting to vector.")))
                              (reduce #(assoc %1 %2 (get x %2)) 
                                (into [] (repeat max-k false)) kys)))) 
      x max-dim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Support functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn enclosing-def []
  "Uses the stacktrace. Fully qualified. This function may or may not be useful."
  (let [tr-ob ^"[Ljava.lang.StackTraceElement;" (.getStackTrace ^Thread (Thread/currentThread))
        ^StackTraceElement s3 (aget tr-ob 3)
        ^String nm (.getClassName s3)]
    (symbol (main/demunge nm))))
(defn enclosing-def-slow []
  "Uses the stacktrace. Fully qualified. This function may or may not be useful."
  (let [tr-ob (.getStackTrace (Thread/currentThread))
        s3 (aget tr-ob 3)
        nm (.getClassName s3)]
    (symbol (main/demunge nm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reflection stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-err-str ;tupelo.core
  "Evaluates exprs in a context in which *err* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*err* s#]
       ~@body
       (str s#))))

(defn reflection-check [fn-code]
  (let [err-str (with-err-str
                  (binding [*warn-on-reflection* true]
                    (eval fn-code)))]
    (if (> (count err-str) 3) err-str "no reflection")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Runtime recording;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn forbidden-paths [code-ex]
  "Paths that can't be logged. For example, the [foo bar] in (fn [foo bar])."
  (let [prepends (fn [k phs] (mapv #(collections/vcat [k] %) phs))
        vmapv (fn [f & args] (apply collections/vcat (apply mapv f args)))]
    (into [] (sort
        (cond (map? code-ex)
          (vmapv #(prepends %1 (forbidden-paths %2)) (keys code-ex) (vals code-ex))
          (set? code-ex)
          (vmapv #(prepends % (forbidden-paths %)) code-ex)
          (vector? code-ex)
          (vmapv #(prepends %1 (forbidden-paths %2)) (range) code-ex)
          (coll? code-ex)
          (let [c0 (first code-ex) c1 (second code-ex)
                forbid1 (forbidden-paths (into [] code-ex))
                forbid  (cond (and (contains? #{`fn 'fn*} c0) (vector? c1)) ; (fn [a b] ...)
                          (prepends 1 (mapv vector (range (count c1))))
                          (contains? #{`fn 'fn*} c0) ; (fn ([a] ...) ([a b] ...))
                          (apply collections/vcat
                            (mapv #(conj (prepends %2 (prepends 0 (mapv vector (range (count (first %1))))))
                                     [%2 0])
                              (rest code-ex) (range 1 1e100)))
                          (contains? #{`binding 'binding*} c0)
                          (prepends 1 (mapv vector (range (count c1))))
                          (contains? #{`let 'let* `loop 'loop*} c0)
                          (prepends 1 (mapv vector (range 0 (count c1) 2)))
                          (contains? #{`def 'def*} c0)
                          [[1]]
                          (contains? #{'recur} c0)
                          (into [] (mapv vector (range 1 (count code-ex)))) ; the [0] path is covered by special check.
                          :else [])]
            (collections/vcat forbid1 forbid))
          (contains? cnav/specials code-ex) [[]]
          (and (symbol? code-ex) (string/starts-with? (str code-ex) "java.lang"
                                   )) [[]] ; Doesn't cover all java fns, but Math is the main one.
          :else [])))))

(defn alternative-paths [code-ex forbidden]
  "Map from forbidden paths to equivelent paths. Exists for non-special symbols.
   Relies on sunshine's deobfuscation."
  (let [forbidden-set (set forbidden)
        obs (mapv #(collections/cget-in code-ex %) forbidden)
        paths (collections/paths code-ex)
        substitute (mapv (fn [ph]
                           (let [x (collections/cget-in code-ex ph)
                                 ph0 (butlast ph) ph00 (butlast ph0)
                                 pair-bind? (and (contains? #{`let 'let* `loop 'loop*}
                                                   (collections/cget-in code-ex ph00))
                                              (vector? (collections/cget-in code-ex ph0)))
                                 x+ (collections/cget-in code-ex (butlast ph))]
                             (cond pair-bind?
                               (update ph (dec (count ph)) inc) ; bind to the value, not the definition.
                               (symbol? x) ; Includes the x in (fn [x y z]), etc.
                               (let [phs (cnav/paths-of code-ex x false)
                                     same-sym-allowed-phs (remove #(get forbidden-set %) phs)]
                                 (cond (and (not (first same-sym-allowed-phs))
                                         (vector? x+) (not (first (remove symbol? x+))))
                                   (println "Warning:" (second code-ex) "Unused fn symbol:" x " bindings = " x+)
                                   (and (not (first same-sym-allowed-phs)) (vector? x+))
                                   (println "Warning:" (second code-ex) "Unused let symbol:" x " bindings = " (collections/evens x+)))
                                 (first same-sym-allowed-phs))
                               :else false)))
                     forbidden)
        bad2good (zipmap forbidden substitute)]
    (select-keys bad2good
      (filterv #(get bad2good %) forbidden))))

(defn _log-pathss-with-debug! [log-syms log-pathss log-atom sym-qual runs]
  "Throws and reports various types of errors. Always mexpands.
   With no errors, it will run runs logging to to log-atom.
   Sets log paths (they should later be removed unless mutation is desired)."
  (binding [logger/*log-atom* log-atom logger/*err-print-code?* false logger/*log-stack?* false]
    (println "Setting log paths for these symbols:" log-syms)
    (let [fn-to-run (eval sym-qual)
          logss (mapv #(apply logger/with-log-paths (zipmap log-syms log-pathss) true fn-to-run %) runs)
          results (mapv #(:results (meta %)) logss)
          is-exception? #(instance? java.lang.Exception %)
          err?s (mapv is-exception? results)]
      (mapv (fn [err? lsym lphs] ; Throws an exception if there is an error.
              ; TODO: this only checks runtime exceptions!
              (if err? (println "Warning! we only will detect compile time bad log paths!"))
              (if err? (logger/find-bad-logpaths sym-qual lsym lphs true true nil))) 
        err?s log-syms log-pathss))))

(defn interesting-paths [def-code-ex]
  "Paths that go inside the function of a def AND not the first element of lists."
  (let [phs (collections/paths def-code-ex)]
    (filterv #(let [x0 (collections/cget def-code-ex (first %))
                    inside-outer-fn? (and (coll? x0) (= (first x0) 'fn*))
                    x (collections/cget-in def-code-ex (butlast %))
                    not-first-list? (or (not (collections/listy? x))
                                      (not= (last %) 0))]
                (and inside-outer-fn? not-first-list?)) phs)))

(defn _log-organize [logs good2bad-pathss]
  "Returns map from log path to vector of log :values with said path. good2bad-pathss can be empty.
   If logs is sorted in time it will preserve within-path sorting."
  (reduce (fn [acc log]
                  (let [ph (:path log) val (:value log)
                        logf-sym (first ph) path-in-code (into [] (rest ph))
                        good2bad (get good2bad-pathss logf-sym)
                        acc1 (if-let [code-ph-bad (get good2bad path-in-code)] 
                               (m-conj acc (collections/vcat [logf-sym] code-ph-bad) val) acc)]
                    (m-conj acc1 ph val)))
          {} logs))

(defn deep-profiles [sym-qual runs]
  "Runs f-sym on each of runs. Each run is a vector of arguments to the fn bound to sym-qual.
   Returns map from log-paths to a vector of values. Log-paths are [sym-qual, ~@path-within-code].
   Includes the dependent symbols of f, and gives logs in chronological order.
   Macroexpands the code first!
   Clears all logs in runs.
   Warning: slow! Use on tiny examples!"
  (let [the-fn (eval sym-qual)
        ;syms2code (cbase/varaverse)
        ns-sym (textparse/sym2ns sym-qual)
        log-syms (apply set/union (mapv set (cbase/deep-used-by sym-qual)))
        vanilla (try (mapv #(apply the-fn %) runs)
                  (catch Exception e
                    (str "Couldn't even run unmodified code: " sym-qual "\n"
                      (unerror/pr-error e))))]
    (if (string? vanilla) vanilla
      (let [;_ (println "Deep profile covering:" (mapv textparse/unqual log-syms))
            sym2code-ex #(sunshine/pipeline (textparse/sym2ns %) 
                           (langs/var-source %) true)
            code-exs (mapv sym2code-ex log-syms)
            pathss (mapv set (mapv interesting-paths code-exs))

            bad-pathss (mapv set (mapv forbidden-paths code-exs))
            log-pathss (mapv set (mapv set/difference pathss bad-pathss))            
            log-atom (atom {})
            
            profile (apply collections/vcat (_log-pathss-with-debug! log-syms log-pathss log-atom sym-qual runs))
            good2bad-pathss (zipmap log-syms 
                              (mapv set/map-invert (mapv alternative-paths code-exs bad-pathss)))]
        (_log-organize (:logs @log-atom) good2bad-pathss)))))

(defn food-profiles [sym-qual runs]
  "{fn-sym}[chronological][which-arg-ix] => value of argument.
   Fn sym includes sym-qual itself and any dependencies.
   Somewhat brittle: will break with aliased syms."
  (let [ns-sym (textparse/sym2ns sym-qual)
        log-syms (apply set/union (mapv set (cbase/deep-used-by sym-qual)))
        log-ns-objs (mapv #(find-ns (textparse/sym2ns %)) log-syms)
        codes (mapv langs/var-source log-syms)
       
        fncall-pathss (mapv #(logger/fncall-logpaths %1 (textparse/sym2ns %2)) codes log-syms) ; [sym][whihc path][path]

        log-pathsss (mapv (fn [code fncall-paths] 
                            (mapv (fn [fn-path]
                                    (let [form (collections/cget-in code fn-path)]
                                      (mapv #(conj fn-path %) (range 1 (count form)))))
                              fncall-paths))
                      codes fncall-pathss) ; [which sym][which fncall][which arg-index][path]

        log-pathss (set/rename-keys (dim-mix log-pathsss [[0] [1 2]] [false true])
                     (zipmap (range) (into [] log-syms))); [which sym][which fncall & arg-ix][path]
        the-fn (eval sym-qual)
        sym2code (zipmap log-syms codes)
        fetch (fn [] (mapv #(apply the-fn %) runs))
        logsu (logger/with-log-paths log-pathss false fetch)
        _ (if (instance? java.lang.Exception (:result (meta logsu)))
            (do (println "Running the function encountered an error (which will be thrown)")
              (throw (:result (meta logsu)))))
        logs (into [] (sort-by :start-time logsu)) ; flat vector of logs, [chronological], into [] just a safety.
        logpaths (set (mapv :path logs))
        logpath2calledsym (zipmap logpaths
                            (mapv #(let [lg-sym (first %) code (get sym2code lg-sym)
                                         path-in-code (rest %)
                                         called-sym (collections/cget-in code (conj (into [] (butlast path-in-code)) 0))
                                         called-sym-qual (langs/resolved (textparse/sym2ns lg-sym) called-sym)]
                                     (if (not called-sym-qual)
                                       (throw (Exception. (str "Can't resolve:" called-sym))))
                                     called-sym-qual)
                             logpaths)) ; mapping from log path to whatever fn sym was called in the log.
        logpath2argix (zipmap logpaths (mapv #(dec (last %)) logpaths))
                
        ; Logs that share butlast path will occur together in time as a pack:
        nlog (count logs)
        vassoc (fn [maybe-v ix v] ; fills with "false" up to v. Treats nil maybe-v as [] 
                 (let [v (if maybe-v maybe-v [])
                       v1 (reduce conj v (range (max 0 (inc (- ix (count v))))))]
                   (assoc v1 ix v)))
        ;{fn-sym}{path-as-str}{arg-ix}{chronological} ; different paths may have different wrt-ix range (arity).
        fdprofile (loop [acc {} ix 0]
                    (if (= ix nlog) acc
                      (let [lg (nth logs ix) ph (:path lg) ; Note: logs is chronological.
                            noleaf (apply str (butlast ph))
                            arg-ix (dec (last ph))
                            called-sym (get logpath2calledsym ph)]
                        (recur (update-in acc [called-sym noleaf arg-ix] #(conj (if % % []) lg)) 
                          (inc ix)))))
        fdprofile1 (dim-mix fdprofile [[0] [1 3] [2]])] ;{fn-sym}{path-as-str+chronological}{wrt-ix}
    (zipmap
      (keys fdprofile1)
      (mapv (fn [pack] (let [logss (into [] (sort-by #(:start-time (get % 0)) (vals pack)))
                             logssv (dim-mix logss [[0] [1]] [true true])]
                         (mapv #(mapv :value %) logssv)))
        (vals fdprofile1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Useful for type deducing?;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn inputss-to-fns [profile]
  "Map from sym-qual to vector of vector of args
   {sym-qual}[chronological][which-arg-ix].
   Use deep-profiles to get a profile map.
   The length of the vector determines the arity.
  ; NOTE: this function is kind-of deprecated."
  (let [paths (keys profile)
        ; out logs hit the symbols in the [x y z] fn args.
        ; Since alternative-paths fills in paths we can't log, we should have this in the profile.   
        outer-log? #(let [cp (into [] (rest %))]
                      (and (= (first cp) 2)
                        (> (second cp) 0)
                        (= (collections/third cp) 0)
                        (get cp 3)))
        kys-to-fn (filterv outer-log? (keys profile))
        sym+2argss (reduce (fn [acc ph]
                          (let [sym-qual (first ph) argpak-ix (- (second ph) 2) 
                                arg-ix (last ph)
                                log-vals (get profile ph)]
                            (assoc-in acc [sym-qual argpak-ix arg-ix] log-vals)))
                    {} kys-to-fn)]
        ; sym+2argss is {sym-qual}{which-argpack}{which-arg-ix}[chronological].
        ; We need to make it {sym-qual}[chronological][which-arg-ix].
    (dim-mix sym+2argss [0 [1 3] 2] [false true true])
    #_(println "TODO: debug warning remove this and next statement.")
    #_sym+2argss))