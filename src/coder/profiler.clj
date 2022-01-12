; Higher level logging functions.

(ns coder.profiler
  (:require
    [clojure.set :as set]
    [clojure.string :as string]
    [clojure.main :as main]
    [c] [t]
    [coder.cnav :as cnav]
    [coder.cbase :as cbase]
    [coder.crosslang.langs :as langs]
    [coder.textparse :as textparse]
    [coder.logger :as logger]
    [coder.unerror :as unerror]
    [coder.pathedmexp :as pathedmexp]))

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

(defn _path-remap [code paths-on-code-ex]
  "Paths on non expanded code, where the code is expanded using pathedmexp/pmexpand."
  (let [inv-pm (pathedmexp/inv-path-map code)]
    (set (filterv identity (mapv #(get inv-pm %) paths-on-code-ex)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Reflection stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-err-str ;tupelo.core
  "Evaluates exprs in a context in which *err* is bound to a fresh
  StringWriter. Returns the string created by any nested printing
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Paths in the code;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn interest-paths [defn-code-ex]
  "Not the first element of lists. Code must be expanded with pathedmexp or macroexpand-all."
  (let [phs (t/paths defn-code-ex)
        phs1 (filterv #(let [x0 (c/cget defn-code-ex (first %))
                             ;inside-outer-fn? (and (coll? x0) (= (first x0) 'fn*))
                             x (t/cget-in defn-code-ex (butlast %))
                             not-first-list? (or (not (c/listy? x)) (not= (last %) 0))]
                        (and #_inside-outer-fn? not-first-list?)) phs)] phs1))

(defn forbidden-paths [code-ex]
  "Paths that can't be logged. For example, the [foo bar] in (fn [foo bar]).
   Convention: Assume 'fn isn't overloaded. Code must be expanded with pathedmexp or macroexpand-all."
  (let [prepends (fn [k phs] (mapv #(c/vcat [k] %) phs))
        vmapv (fn [f & args] (apply c/vcat (apply mapv f args)))
        sort1 (fn [x] (sort-by identity t/path-order-compare x))]
    (into [] (sort1
        (cond (map? code-ex)
          (vmapv #(prepends %1 (forbidden-paths %2)) (keys code-ex) (vals code-ex))
          (set? code-ex)
          (vmapv #(prepends % (forbidden-paths %)) code-ex)
          (vector? code-ex)
          (vmapv #(prepends %1 (forbidden-paths %2)) (range) code-ex)
          (coll? code-ex)
          (let [c0 (first code-ex) c1 (second code-ex)
                forbid1 (forbidden-paths (into [] code-ex))
                forbid  (cond
                          (contains? #{'def} c0) [[] [0] [1]] ; (def x ...)
                          (and (contains? #{`fn 'fn 'fn*} c0) (vector? c1)) ; (fn [a b] ...)
                          (conj (prepends 1 (mapv vector (range (count c1)))) [1])
                          (contains? #{`fn 'fn* 'fn} c0) ; (fn ([a] ...) ([a b] ...))
                          (apply c/vcat [[1]]
                            (mapv #(conj (prepends %2 (prepends 0 (mapv vector (range (count (first %1))))))
                                     [%2 0])
                              (rest code-ex) (range 1 1e100)))
                          (contains? #{`binding 'binding*} c0)
                          (prepends 1 (mapv vector (range (count c1))))
                          (contains? #{`let 'let* `loop 'loop*} c0)
                          (conj (prepends 1 (mapv vector (range 0 (count c1) 2))) [1])
                          (contains? #{`def 'def*} c0)
                          [[1]]
                          (contains? #{'recur} c0)
                          (into [] (mapv vector (range 1 (count code-ex)))) ; the [0] path is covered by special check.
                          :else [])]
            (c/vcat forbid1 forbid))
          (or (contains? cnav/specials code-ex) (= code-ex 'fn)) [[]]
          (and (symbol? code-ex) (string/starts-with? (str code-ex) "java.lang"
                                   )) [[]] ; Doesn't cover all java fns, but Math is the main one.
          :else [])))))

(defn alternative-paths [code-ex forbidden]
  "Map from forbidden paths to the first equivelent, allowed paths. Some paths cannot be remapped.
   Code must be expanded with pathedmexp or macroexpand-all."
  (let [forbidden-set (set forbidden)
        obs (mapv #(t/cget-in code-ex %) forbidden)
        paths (t/paths code-ex)
        substitute (mapv (fn [ph]
                           (let [x (t/cget-in code-ex ph)
                                 ph0 (butlast ph) ph00 (butlast ph0)
                                 pair-bind? (and (contains? #{`let 'let* `loop 'loop*}
                                                   (t/cget-in code-ex ph00))
                                              (vector? (t/cget-in code-ex ph0)))
                                 x+ (t/cget-in code-ex (butlast ph))]
                             (cond pair-bind?
                               (update ph (dec (count ph)) inc) ; bind to the value, not the definition.
                               (symbol? x) ; Includes the x in (fn [x y z]), etc.
                               (let [phs (t/find-values-in code-ex x)
                                     same-sym-allowed-phs (remove #(get forbidden-set %) phs)
                                     same-sym-allowed-phs (sort-by identity t/path-order-compare same-sym-allowed-phs)]
                                 (cond (= x '&) "No warning to print"
                                       (and (not (first same-sym-allowed-phs))
                                         (vector? x+) (not (first (remove symbol? x+))))
                                   (println "Warning:" (second code-ex) "Unused fn symbol:" x " bindings = " x+)
                                   (and (not (first same-sym-allowed-phs)) (vector? x+) (not= x (symbol "_")))
                                   (println "Warning:" (second code-ex) "Unused let symbol:" x " bindings = " (c/evens x+)))
                                 (first same-sym-allowed-phs))
                               :else false)))
                     forbidden)
        bad2good (zipmap forbidden substitute)]
    (select-keys bad2good
      (filterv #(get bad2good %) forbidden))))

(defn unexpanded-pathmap [code]
  "Map from path of interest to the path logged. When possible these paths will be the same.
   This is on the unexpanded code."
  (let [code-ex (pathedmexp/pmexpand code)
        ipaths-ex (set (interest-paths code-ex))
        no-paths-ex (set (forbidden-paths code-ex))
        yes-paths-ex (set/difference ipaths-ex no-paths-ex)
        apaths-ex (alternative-paths code-ex no-paths-ex)
        pathmap-ex (merge (zipmap yes-paths-ex yes-paths-ex) apaths-ex)
        expanded2unexpanded (pathedmexp/inv-path-map code)
        e2uf #(get expanded2unexpanded %)
        path-map (zipmap (mapv e2uf (keys pathmap-ex)) (mapv e2uf (vals pathmap-ex)))]
    (select-keys path-map (filter #(and % (get path-map %)) (keys path-map)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Runtime recording;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _logs-with-debug! [log-syms log-pathss sym-qual the-fn runs]
  "Throws and reports various types of errors. No macroexpansion is performed.
   With no errors, it will run runs logging to to log-atom.
   Sets log paths (they should later be removed unless mutation is desired)."
  (binding [logger/*err-print-code?* true logger/*log-stack?* false]
    (let [logs (logger/with-log-paths (zipmap log-syms log-pathss) false the-fn runs)
          results (:results (meta logs))
          is-exception? #(instance? java.lang.Exception %)
          err?s (mapv is-exception? results)]
      (mapv (fn [err? lsym lphs] ; Throws an exception if there is an error.
              ; TODO: this only checks runtime exceptions!
              (if err? (println "Warning! we only will detect compile time bad log paths!"))
              (if err? (logger/find-bad-logpaths sym-qual lsym lphs false true nil)))
        err?s log-syms log-pathss)
      logs)))

(defn _log-organize [logs path-map-map]
  "Returns map from log path to vector of log :values with said path.
   path-map-map is a map of maps from bad to good paths."
  (reduce (fn [acc log]
                  (let [ph (:path log) val (:value log)
                        logf-sym (first ph) path-in-code (into [] (rest ph))
                        path-map (get path-map-map logf-sym) ; All paths => Cooresponding good path.
                        equivalent-paths (filterv #(= (get path-map %) path-in-code) (keys path-map))

                        eq-sympaths (mapv #(c/vcat [logf-sym] %) equivalent-paths)]
                    (reduce #(c/entry-conj %1 %2 val) acc eq-sympaths))) {} logs))

(defn deep-profiles [sym-qual runs]
  "Runs sym-qual on each of runs. Each run is a vector of arguments to the fn bound to sym-qual.
   Returns map from log-paths to a vector of values. Log-paths are [sym-qual, ~path-within-code].
   Includes the dependent symbols of f, and gives logs in chronological order.
   Clears all logs in runs.
   Warning: slow! Use on tiny examples!"
  (let [the-fn (fn [runs] (let [v-ob (resolve sym-qual)] (mapv #(apply v-ob %) runs)))

        ns-sym (textparse/sym2ns sym-qual)
        log-syms (apply set/union (mapv set (cbase/deep-used-by sym-qual)))
        vanilla (try (the-fn runs) (catch Exception e (do (println "Vanilla is not working") (throw e))))]
    (let [codes (mapv #(langs/var-source % true) log-syms)
          path-maps (mapv unexpanded-pathmap codes)
          log-pathss (mapv #(into [] (set (vals %))) path-maps) ; Log the UNexpanded code.

          path-map-map (zipmap log-syms path-maps)
          logs (_logs-with-debug! log-syms log-pathss sym-qual the-fn runs)]
      (_log-organize logs path-map-map))))

(defn food-profiles [sym-qual runs]
  "{fn-sym}[chronological][which-arg-ix] => value of argument.
   Fn sym includes sym-qual itself and any dependencies.
   Somewhat brittle: will break with aliased syms."
  (let [ns-sym (textparse/sym2ns sym-qual)
        log-syms (apply set/union (mapv set (cbase/deep-used-by sym-qual)))
        log-ns-objs (mapv #(find-ns (textparse/sym2ns %)) log-syms)
        codes (mapv langs/var-source log-syms)

        fncall-pathss (mapv #(cnav/fncall-paths %1 (textparse/sym2ns %2)) codes log-syms) ; [sym][which path][path]

        log-pathsss (mapv (fn [code fncall-paths]
                            (mapv (fn [fn-path]
                                    (let [form (t/cget-in code fn-path)]
                                      (mapv #(conj fn-path %) (range 1 (count form)))))
                              fncall-paths))
                      codes fncall-pathss) ; [which sym][which fncall][which arg-index][path]

        log-pathss (set/rename-keys (t/dim-mix log-pathsss [[0] [1 2]] [false true])
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
                                         called-sym (t/cget-in code (conj (into [] (butlast path-in-code)) 0))
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
        fdprofile1 (t/dim-mix fdprofile [[0] [1 3] [2]])] ;{fn-sym}{path-as-str+chronological}{wrt-ix}
    (zipmap
      (keys fdprofile1)
      (mapv (fn [pack] (let [logss (into [] (sort-by #(:start-time (get % 0)) (vals pack)))
                             logssv (t/dim-mix logss [[0] [1]] [true true])]
                         (mapv #(mapv :value %) logssv)))
        (vals fdprofile1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Processing the profile;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pack [profile]
  "Combines like symbols into thier own maps, making a two-level map."
  (let [syms (set (map first (keys profile)))]
    (reduce (fn [acc kv]
              (let [k (first kv) v (second kv)
                    k1 (into [] (rest k)) sym (first k)]
                (update acc sym #(assoc % k1 v))))
      (zipmap syms (repeat {})) profile)))

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
                        (= (c/third cp) 0)
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
    (t/dim-mix sym+2argss [0 [1 3] 2] [false true true])
    #_(println "TODO: debug warning remove this and next statement.")
    #_sym+2argss))