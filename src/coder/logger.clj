; TODO: this makes heavy use of clojure-only functions. They should be refactored to langs.

(ns coder.logger
  (:require [collections]
    [clojure.pprint :as pprint]
    [coder.refactor :as refactor]
    [coder.crosslang.langs :as langs]
    [coder.unerror :as unerror]
    [coder.cnav :as cnav]
    [coder.textparse :as textparse]
    [coder.sunshine :as sunshine]
    [app.rtext :as rtext]
    [clojure.walk :as walk]
    [clojure.test :as test]
    [clojure.set :as set]
    [clojure.string :as string]
    [layout.layoutcore :as layoutcore]
    [layout.blit :as blit]
    [globals]))

(def _junk-ns (create-ns 'logger-ns))

(def ^:dynamic *log-atom* globals/one-atom)
(def ^:dynamic *err-print-code?* true) ; It is expensive.
(def ^:dynamic *log-stack?* true) ; Is it expensive?


;;;;; Error reporting ;;;;

(defn error [& args] "Debug"
  (throw (Exception. (apply str args))))

#_(def ^:dynamic *debug-shorten-code?* false)

(defonce eval-lasterr (atom ""))

(defn eval+ [code]
  "Like eval but also logs any compilation error message, for debugging. Still throws said error.
   @coder.logger/eval-lasterr"
  (try (eval code)
    (catch Exception e
      (do (if *err-print-code?* (unerror/errprint? code))
          (reset! coder.logger/eval-lasterr 
            (merge {:ns (ns-name *ns*) :code code} 
              (langs/convert-exception e "none" :clojure))) (throw e)))))

;;;;;;;;;;;;; Code processing functions ;;;;;;;;;;;;;;

(defn logify-code1 [code log-path path-in-code]
  "Naive logging of the code. Other functions must make sure we don't use a log-path that steps on special forms."
  (let [log-pathq (update log-path 0 #(list 'quote %))]
    (collections/cupdate-in code path-in-code
      #(list `log! % log-pathq))))

(defn symqual2code [sym-qual mexpand?]
  "Fully macroexpanded, in the local namespace.
   Maybe this code should go in cbase instead?"
  (let [code (langs/var-source sym-qual)]
    (if mexpand? (sunshine/pipeline (textparse/sym2ns sym-qual) code true) code)))

(defn log-error-report [bad-code]
  "Should be in the target namespace."
  (println "Logging failed:")
  (unerror/errprint? bad-code))

;;;;;;;;;;; Querying logs and loggers ;;;;;;;;;;;;

(defn get-logs [] (:logs @globals/one-atom)) ; logs are a vector of :path :time :value :Thead.
(defn get-loggers [] (:loggers @globals/one-atom)) ; loggers are {symbol {path ...}} format.
(defn clear-logs! [] (do (let [n-log (count (get-logs))] 
                           (if (> n-log 0) (println "Removing all" n-log "logs")
                             (println "No logs to remove.")) 
                       (swap! globals/one-atom #(assoc % :logs [])))))
(defn logged? [sym-qual path]
  (boolean (get-in (:loggers @globals/one-atom) [sym-qual path])))

(defn last-log-of [sym-qual path-within-code]
  "Useful for debugging. Nil on failure."
  (let [logs (get-logs) lpath (collections/vcat [sym-qual] path-within-code)]
    (last (filter #(= (:path %) lpath) logs))))

(defn logpath2code [log-path]
  "The code that was logged in log-path. It may be macroexpanded code."
  (get-in @globals/one-atom [:log-code-lookup log-path]))

;;;;;;;;;;;;;;; Runtime ;;;;;;;;;;;;;;;;

(defn log! [x path]
  "Called when the logged code runs." 
  (swap! *log-atom*
    #(let [code (get-in @globals/one-atom [:log-code-lookup path])
           log {:path path :time (System/nanoTime) :value x :Thread (format "0x%x" (System/identityHashCode (Thread/currentThread)))
                :TraceOb (if *log-stack?* (.getStackTrace ^Thread (Thread/currentThread)))
                :code code :lang :clojure} ; loggers for other languages will use different lang keywords.
           logs (if (:logs %) (:logs %) [])]
      (assoc % :logs (conj logs log)))) x)

;;;;;;;;;;;;;;; Low-level loggering ;;;;;;;;;;;;;;;;

(defn get-logpaths [sym-qual]
  "Not efficient if there are tonnes of loggers, but so far this hasn't been a problem."
  (let [loggers-for-sym (get (get-loggers) sym-qual {})
        mexpand? (:mexpand? (first (vals loggers-for-sym)))]
    (with-meta (set (keys loggers-for-sym)) {:mexpand? (boolean mexpand?)})))

(defn get-logged-code-and-fn [sym-qual paths mexpand?]
  "[logged code, fn (nil if error), exception (nill if no err)]"
  (let [paths (set paths) ; remove dupes.
        ns-obj (find-ns (textparse/sym2ns sym-qual))
        code (symqual2code sym-qual mexpand?)
        s-fn #(+ (* (double (count %)) 1.0e8)
                (if (number? (last %)) (last %) 0.0))
        paths-sort (into [] (reverse (sort-by s-fn paths))) ; must be added long to short, later to earlier (for things like logging fn args).
        _ (if (and (last paths-sort) (or (< (count (last paths-sort)) 1) ; earliest path.
                                       (not (coll? (collections/cget code (first (last paths-sort))))))) 
            (throw (Exception. "The logpaths must be inside the function part of a defn."))) 
        logged-code (reduce #(logify-code1 %1 (collections/vcat [sym-qual] %2) %2) code paths-sort)
        logged-fn-code (if mexpand? (last logged-code)
                         (apply list 'fn (collections/cget logged-code 2) (subvec (into [] logged-code) 3)))
        err-atm (atom nil)
        f (try (binding [*ns* ns-obj] (eval+ logged-fn-code)) 
                 (catch Exception e (do (reset! err-atm e) nil)))]
    [logged-fn-code f @err-atm]))

(defn set-logpaths! [sym-qual paths mexpand?]
  "Set the log paths of a given code.
   Every call to set-logpaths! will have to rebuild the code."
  (let [code-f (get-logged-code-and-fn sym-qual paths mexpand?)
        logged-f (if-let [x (second code-f)] x (throw (last code-f)))
        code (symqual2code sym-qual mexpand?)
        logged-codes (mapv #(collections/cget-in code %) paths)
        loggers (zipmap paths (repeat (count paths) {:mexpand? mexpand? :source code}))]
    (alter-var-root (find-var sym-qual)
      (fn [old-val]
        (collections/keep-meta old-val (fn [_] logged-f))))
    (swap! globals/one-atom
      (fn [world]
        (let [path+ #(collections/vcat [sym-qual] %)
              world (update world :log-code-lookup
                      #(merge % (zipmap (mapv path+ paths) logged-codes)))
              loggers-old (get-in world [:loggers sym-qual])
              loggers1 (zipmap (keys loggers)
                         (mapv #(merge (get loggers-old % {}) (get loggers %))
                           (keys loggers)))]
          (if (= (count loggers1) 0)
            (update world :loggers #(dissoc % sym-qual))
            (assoc-in world [:loggers sym-qual] loggers1)))))))

(defn with-log-paths [sym-qual paths mexpand? & args]
  "Runs sym-qual with a given set of paths and the supplied & args and returns all logs related to that.
   Makes no permanent changes to paths, even if it ends up throwing an error."
  (let [old-paths (get-logpaths sym-qual)
        tmp-atom (atom {}) good? (atom false)
        maybe-err (try (do (set-logpaths! sym-qual paths mexpand?)
                         (binding [*log-atom* tmp-atom] (apply (eval sym-qual) args))
                         (reset! good? true))
                    (catch Exception e e))
        _ (set-logpaths! sym-qual old-paths (:mexpand? (meta old-paths)))
        _ (if (not @good?) (throw maybe-err))]
    (:logs tmp-atom)))

(defn find-bad-logpaths [run-sym log-sym paths mexpand? throw? runtime-args]
  "Finds a minimum error-generating subset of paths and returns them (throw? = false)
   or throws an error with printing reporting of code (throw? = true).
   Makes no changes to the log-paths.
   If runtime-args is non nil, will try to run the code. Else will only report compile errrors."
  ; TODO: this function was recently modified, needs testing.
  (binding [*err-print-code?* false *log-stack?* false *log-atom* (atom {})]
    (let [paths (into [] (sort paths))
          _ (mapv #(if (symbol? (first %)) (throw (Exception. "Paths must not start with the log symbol for this function."))) paths)
          old-paths (get-logpaths log-sym)
          _ (set-logpaths! log-sym [] false)
          good? (fn [test-paths]
                  (try
                    (let [code-f-ex (get-logged-code-and-fn log-sym test-paths mexpand?)
                          compile-err? (last code-f-ex)]
                      (if (and (not compile-err?) runtime-args) (apply (eval (second code-f-ex)) runtime-args))
                      (if compile-err? false true))
                    (catch Exception e false)))
          _ (if (good? paths)
              (throw (Exception. "The logging doesn't generate an error, no bad set of paths can be found.")))
          bad-path (first (remove #(good? [%]) paths)) ; go for a single bad path.
          minbad-paths (if bad-path [bad-path]
                         (throw (Exception. "TODO: write code to handle bad-on-multible-loggers-only paths.")))
          _ (set-logpaths! log-sym old-paths (:mexpand? (meta old-paths)))]
      (if throw? 
        (let [log-code-fn-err? (get-logged-code-and-fn log-sym minbad-paths mexpand?)
              run-code-fn-err? (get-logged-code-and-fn run-sym [] false)
              run-err? (last run-code-fn-err?)
              log-err? (last log-code-fn-err?)
              runf (second run-code-fn-err?)
              log-code (first log-code-fn-err?)]
          (println "Bad code:")
          (blit/vpu log-code)
          (if log-err? (println (unerror/pr-error log-err?)))
          (if (and runf runtime-args) (apply runf runtime-args))
          (if (and (not log-err?) (not run-err?)) (throw (Exception. "The logged-code errors don't seem repeatable, so the above bad code isnt bad enough.")))
          (throw (Exception. "See console for error report.")))
        (set minbad-paths)))))

(defn reload-tryto-keep-loggers! [ns-sym & quiet?]
  "Reloads the namespace, attempts keeping all loggers we need.
   Even if no code changes it will wipe the loggers so we need to re-add them."
  (let [_ (binding [*ns* _junk-ns]
            (require ns-sym :reload))
        syms (keys (ns-interns ns-sym))
        sym-quals (set (mapv #(langs/resolved ns-sym %) syms))
        loggers (:loggers @globals/one-atom)
        sym-logged (set (keys loggers))
        need-logged (set/intersection sym-quals sym-logged)]
    (mapv
      (fn [sym-qual]
        (let [loggers-for-sym (get loggers sym-qual)
              paths (keys loggers-for-sym)
              mexpand? (:mexpand? (first (vals loggers-for-sym)))
              old-code (:source (get loggers-for-sym (first (keys loggers-for-sym))))
              code (symqual2code sym-qual mexpand?)
              paths1 (set (filterv identity 
                            (mapv #(cnav/drag-path old-code code %) paths)))
              _ (if (not (first quiet?)) 
                  (println "Reloading logged symbol:" ns-sym (count paths1) "of" (count (keys loggers-for-sym)) "kept."))] ; this may get annoying.
           (set-logpaths! sym-qual paths1 mexpand?))) need-logged)))

;;;;;;;;;;;;;;; High-level loggering ;;;;;;;;;;;;;;;;

(defn add-logger! [sym-qual path mexpand? & messages]
  "mexpand? should be false for individual logging requests, but true for deep mass-logging"
  (let [path (into [] path)
        mexpand? (boolean mexpand?)
        ns-obj (find-ns (textparse/sym2ns sym-qual))
        current-loggers (get-in @globals/one-atom [:loggers sym-qual] {}) ; path -> logger
        mexpand?-old (:mexpand? (first (vals current-loggers)))
        untouched-loggers (dissoc current-loggers path)
        _ (cond 
            (and (> (count untouched-loggers) 0) mexpand?-old (not mexpand?))
            (throw (Exception. "Other loggers have macro-expansion, but we don't. We can't mix and max (TODO?)."))
            (and (> (count untouched-loggers) 0) (not mexpand?-old) mexpand?)
            (throw (Exception. "Other loggers don't have macro-expansion, but we do. We can't mix and max (TODO?).")))
        
        new-msg (get messages 0 "")
        already-msg (get messages 1 "")
        new? (not (get current-loggers path))]
    (if (not= (first messages) :quiet)
      (if new?
        (println (str new-msg "Adding logger for:") sym-qual path " expand? " mexpand?)
        (println (str already-msg "Re-Adding logger for:") sym-qual path " expand? " mexpand?)))
    (set-logpaths! sym-qual (set (conj (keys current-loggers) path)) mexpand?)))

(defn remove-logger! [sym-qual path]
  (let [path (into [] path)
        old-loggers (get-in @globals/one-atom [:loggers sym-qual])
        old-paths (set (keys old-loggers))
        new-paths (disj old-paths path)
        hit? (< (count new-paths) (count old-paths))]
    (if hit? (println "Removing logger for" sym-qual path) (println "No logger to remove for" sym-qual path))
    (if hit? (set-logpaths! sym-qual new-paths (:mexpand? (first (vals old-loggers)))))))

(defn remove-loggers! [sym-qual]
  (if (> (count (get-in @globals/one-atom [:loggers sym-qual])) 0)
    (do (println "Removing loggers for:" sym-qual)
      (set-logpaths! sym-qual [] false))
    (println "No loggers to remove for:" sym-qual)))

(defn remove-all-loggers! []
  (let [syms (keys (get @globals/one-atom :loggers {}))]
    (if (> (count syms) 0)
       (do (println "Removing all loggers, " (count syms) "symbols will be cleaned") 
         (mapv remove-loggers! syms))
       (println "No loggers anywhere, no need to remove"))))

(defn toggle-logger! [sym-qual path mexpand?]
  "Returns true for add, false for remove."
  (let [add? (not (logged? sym-qual path))]
    (if add? (add-logger! sym-qual path mexpand?)
      (remove-logger! sym-qual path))
    add?))

;;;;;;;;;;;; Debugger system based on key-symbols instead of logging paths ;;;;;;;;;;;;;;;;;;

(defn w2ps [sym-qual search-key mexpand?]
   (let [code (symqual2code sym-qual mexpand?)
        _ (if (not code) (throw (Exception. (str "Cant find code for:" sym-qual))))
        phs (cnav/paths-of code search-key)] 
     phs))

(defn user-data-logger! [sym-qual path udata]
  "Put user data for the logger here."
  (swap! globals/one-atom
    (fn [world] (update-in world [:loggers sym-qual path]
                  #(merge % udata)))))

(defn add-watchpoint! [sym-qual search-key & adjacents]
  "A debug watch, which is easier to clear. Adjacents is an optional next-to key that helps with disambugation."
  (let [phs (w2ps sym-qual search-key false)
        code (symqual2code sym-qual false)
        _ (if (= (count phs) 0) (throw (Exception. (str "Not found: " search-key))))
        just-before (fn [ph] (if (number? (last ph)) (update ph (dec (count ph)) dec)))
        just-after (fn [ph] (if (number? (last ph)) (update ph (dec (count ph)) inc)))
        ; 1 adj = before or after. 2 = [before, after].
        adj (fn [ph]
              (cond (= (count adjacents) 0) true
                (= (count adjacents) 1)
                (or (= (first adjacents) (collections/cget-in code (just-before ph)))
                  (= (first adjacents) (collections/cget-in code (just-after ph))))
                (>= (count adjacents) 2)
                (and (= (first adjacents) (collections/cget-in code (just-before ph)))
                  (= (second adjacents) (collections/cget-in code (just-after ph))))))
        phs1 (filterv adj phs)]
    (mapv #(add-logger! sym-qual % false "[not-so-Breakpoint]") phs1)
    (mapv #(user-data-logger! sym-qual % {:watchpoint? true}) phs1)))

(defn clear-watchpoints! []
  (let [world @globals/one-atom
        loggers (:loggers world)
        watchers (zipmap (keys loggers)
                   (mapv (fn [pack] (filterv #(:watchpoint? (get pack %))
                           (keys pack))) (vals loggers)))
        watchersu (mapv (fn [k] (mapv #(vector k %) (get watchers k)))
                    (keys watchers))
        watchersu (apply collections/vcat watchersu)]
    (mapv #(apply remove-logger! %) watchersu)
    (println "Cleared:" (count watchersu) "Debug watchers.")))
(def cw! clear-watchpoints!)

(defn get-last-watch-logs [sym-qual search-key]
  "Gets the logs that matches the watch, actually returns a vector of logs
   b/c watches can cover multible paths."
  (let [logs (:logs @globals/one-atom)
        phs (w2ps sym-qual search-key false)
        phs (mapv #(collections/vcat [sym-qual] %) phs)]
    (mapv (fn [ph] (first (filter #(= ph (:path %)) (reverse logs))))
      phs)))

(defn watch! [sym-qual search-key & adjacents]
  "Unifies the add and the observe watching functions.
   Call, run, call again pattern."
  (if (not (get-in @globals/one-atom [:loggers sym-qual search-key]))
    (apply add-watchpoint! sym-qual search-key adjacents))
  (get-last-watch-logs sym-qual search-key))
(def w! watch!)

(defn grab! [sym-qual args search-key & adjacents]
  "Runs the function, returns the debug results."
  (clear-watchpoints!)
  (apply watch! sym-qual search-key adjacents)
  (let [als (:arglists (meta (find-var sym-qual)))
        valid-counts-lo (set (mapv #(if (contains? (set %) '&)
                                     (dec (count %)) (count %)) als))
        valid-counts-hi (set (mapv #(if (contains? (set %) '&)
                                     1e100 (count %)) als))
        n (count args)]
    (if (not (first (filterv #(and (>= n (first %)) (<= n (second %))) 
                      (mapv vector valid-counts-lo valid-counts-hi))))
      (throw (Exception. (str "Wrong number of args " n " passed to " sym-qual)))))
  (try (apply (find-var sym-qual) args) 
    (catch Exception e nil))
  (mapv :value (get-last-watch-logs sym-qual search-key)))

;;;;;;;;;;;; Profiling ;;;;;;;;;;;;;;;;;;

(defonce _gtime-atom (atom 0.0))
(defmacro gtime [code]
  "Global time elapsed, reset once per repl run.
   Different functions share the same timer.
   Greater overhead than time but it is hard to quantify."
  `(let [start# (. System (nanoTime))
         ret# ~code
         milis# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)
         totalmilis# (swap! _gtime-atom (fn [t#] (+ t# milis#)))]
     (do (prn (str "Elapsed time: " milis# " ms, total time this repl call: " totalmilis# " ms")))
     ret#))

(defn gtime-reset! [] (reset! _gtime-atom 0.0))