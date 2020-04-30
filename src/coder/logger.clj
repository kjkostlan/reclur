; TODO: this makes heavy use of clojure-only functions. This should be refactored to langs.

(ns coder.logger
  (:require [collections]
    [clojure.pprint :as pprint]
    [coder.refactor :as refactor]
    [coder.crosslang.langs :as langs]
    [coder.unerror :as unerror]
    [coder.cnav :as cnav]
    [coder.textparse :as textparse]
    [app.rtext :as rtext]
    [clojure.walk :as walk]
    [clojure.test :as test]
    [clojure.set :as set]
    [clojure.string :as string]
    [javac.exception :as jexc]
    [layout.layoutcore :as layoutcore]
    [layout.blit :as blit]
    [globals]))

(def _junk-ns (create-ns 'logger-ns))

;;;;; Error reporting ;;;;

(defn error [& args] "Debug"
  (throw (Exception. (apply str args))))

#_(def ^:dynamic *debug-shorten-code?* false)

(defonce eval-lasterr (atom ""))

(defn clean-error-stack [e-clj stop-stack]
  "No need to go into the guts of clojure or java."
  (let [stack (:StackTrace e-clj) root (fn [elem] (first (string/split (:ClassName elem) #"\.")))
        of-clojure? (fn [elem] (= (root elem) "clojure"))
        of-java? (fn [elem] (= (root elem) "java"))
        stack1 (filterv #(not (or (of-clojure? %) (of-java? %))) stack)
        ;_ (println stop-call)
        ;_ (mapv println stack1)
        stop-stack (if (coll? stop-stack) stop-stack [stop-stack])
        stop? (fn [txt] (boolean (first (filter #(string/starts-with? txt %) stop-stack))))
        stop-ix (first (filter #(stop? (:ClassName (nth stack1 %)))
                         (range (count stack1))))
        stack2 (if stop-ix (subvec stack1 0 (inc stop-ix)) stack1)
        stack3 (if stop-ix (conj stack2 {:ClassName "..." :FileName "" :LineNumber "" :MethodName ""}) stack2)]
    ; {:ClassName clojure.lang.Compiler, :FileName Compiler.java, 
    ;  :LineNumber 6792, :MethodName analyze}
    ;(println (first stack))) 
    (assoc e-clj :StackTrace stack3)))

(defn eval+ [code]
  "Like eval but also logs any compilation error message, for debugging. Still throws said error.
   @coder.logger/eval-lasterr"
  (try (eval code)
    (catch Exception e
      (do (unerror/errprint? code)
          (reset! coder.logger/eval-lasterr 
            (merge {:ns (ns-name *ns*) :code code} 
              (clean-error-stack (jexc/clje e) "none"))) (throw e)))))

;;;;;;;;;;;;; Code processing functions ;;;;;;;;;;;;;;

(defn nest-status [code path-in-code]
  "Special considerations for logging purposes."
  (let [leaf (collections/cget-in code path-in-code)
        twig (collections/cget-in code (butlast path-in-code))
        stik (collections/cget-in code (butlast (butlast path-in-code)))
        fn-call? (and (coll? code) (not (vector? code)) (contains? #{'fn 'fn*} (first twig))) ; (fn* |([x y z] ...)|)
        fn-call1? (and (vector? code) (contains? #{'fn 'fn*} (first twig)) (= (last path-in-code) 1)) ; Unusual (fn* |[x y z]| ...)
        fn-call2? (and (vector? code) (contains? #{'fn 'fn*} (first stik)) (= (last path-in-code) 1)) ; (fn* (|[x y z]| ...))
        binding-vec? (contains? #{'let 'let* 'binding 'loop 'loop*} (first stik))]
    {:fn-call? fn-call? :fn-call1? fn-call1? :fn-call2? fn-call2?
     :binding-vec? binding-vec?}))

(defn logify-code1 [code log-path path-in-code]
  "log-path = ['mynamespace/foo 1 2 3].
   path-in-code = [1 2 3] within code.
   Returns the logged code if successful, false for not successful."
  (let [stik (collections/cget-in code (butlast (butlast path-in-code)))
        twig (collections/cget-in code (butlast path-in-code))
        leaf (collections/cget-in code path-in-code)
        
        log-pathq (update log-path 0 #(list 'quote %))
        lgcore #(list `log! % log-pathq)
        sts (nest-status code path-in-code)
        ch-path (subvec path-in-code 0 ; ch-path = change path.
                  (- (count path-in-code)
                    (cond (:fn-call? sts) 1 (:fn-call1? sts) 1
                     :else 0)))
        ch-path (if (and (:binding-vec? sts) (even? (last path-in-code))) ; binding vectors.
                  (update ch-path (dec (count ch-path)) inc) ch-path)
        target (collections/cget-in code ch-path)
        target-twig (collections/cget-in code (butlast ch-path))
        piece1 (cond (:fn-call? sts)
                 (apply list (conj (into [] (butlast target)) (lgcore (last target))))
                 (:fn-call1? sts)
                 (apply list (first target-twig) leaf (lgcore target) (rest (rest target-twig)))
                 :else
                 (lgcore target))]
    (collections/cassoc-in code ch-path piece1)))

(defn symqual2code [sym-qual mexpand?]
  "Fully macroexpanded, in the local namespace.
   Maybe this code should go in cbase instead?"
  (let [v-info (langs/var-info sym-qual true)
        _ (if (not v-info) (throw (Exception. (str "This (assumbed to be fully qualified symbol) can't be found: " sym-qual))))
        s-code (:source v-info)
        ns-obj (find-ns (textparse/sym2ns sym-qual))
        _ (if (and  mexpand? (not ns-obj)) (throw "Strange where did the ns go?"))
        s-code1 (if mexpand?
                  (into [] (binding [*ns* ns-obj] (clojure.walk/macroexpand-all s-code))))]
    (if mexpand? s-code1 s-code)))

(defn log-error-report [bad-code]
  "Should be in the target namespace."
  (println "Logging failed:")
  (unerror/errprint? bad-code))
 
;;;;;;;;;;; Querying logs and loggers ;;;;;;;;;;;;

(defn get-logs [] (:logs @globals/one-atom)) ; logs are a vector of :path :time :value :Thead.
(defn get-loggers [] (:loggers @globals/one-atom)) ; loggers are {symbol {path ...}} format.
(defn clear-logs! [] (swap! globals/one-atom #(assoc % :logs [])))
(defn logged? [sym-qual path]
  (boolean (get-in (:loggers @globals/one-atom) [sym-qual path])))

(defn last-log-of [sym-qual path-within-code]
  "Useful for debugging. Nil on failure."
  (let [logs (get-logs) lpath (collections/vcat [sym-qual] path-within-code)]
    (last (filter #(= (:path %) lpath) logs))))

;;;;;;;;;;;;;;; Runtime ;;;;;;;;;;;;;;;;

(defn log! [x path]
  "Called when the logged code runs." 
  (swap! globals/one-atom
    #(let [log {:path path :time (System/nanoTime) :value x :Thread (format "0x%x" (System/identityHashCode (Thread/currentThread)))}
           logs (if (:logs %) (:logs %) [])]
      (assoc % :logs (conj logs log)))) x)

;;;;;;;;;;;;;;; Low-level loggering ;;;;;;;;;;;;;;;;

(defn set-logpaths! [sym-qual paths mexpand?]
  "Set the log paths of a given code.
   Every call to set-logpaths! will have to rebuild the code."
  (let [paths (set paths) ; remove dupes.
        ns-obj (find-ns (textparse/sym2ns sym-qual))
        code (symqual2code sym-qual mexpand?)
        s-fn #(+ (* (double (count %)) 1.0e8)
                (if (number? (last %)) (last %) 0.0))
        paths-sort (reverse (sort-by s-fn paths)) ; must be added long to short, later to earlier (for things like logging fn args).
        _ (if (and (last paths-sort) (< (count (last paths-sort)) (if mexpand? 3 1)))
            (throw (Exception. "The logpaths must be inside the function part of a defn."))) 
        logged-code (reduce #(logify-code1 %1 (collections/vcat [sym-qual] %2) %2) code paths-sort)
        ;_ (do (require 'layout.blit) (layout.blit/vp logged-code))
        logged-fn-code (if mexpand? (last logged-code)
                         (apply list 'fn (collections/cget logged-code 2) (subvec (into [] logged-code) 3)))
        loggers (zipmap paths (repeat (count paths) {:mexpand? mexpand? :source code}))]
    (binding [*ns* ns-obj]
      (let [var-f (eval+ logged-fn-code)]
        (alter-var-root (find-var sym-qual)
          (fn [old-val]
            (collections/keep-meta old-val (fn [_] var-f))))
        (swap! globals/one-atom
          (fn [world]
            (let [loggers-old (get-in world [:loggers sym-qual])
                  loggers1 (zipmap (keys loggers)
                             (mapv #(merge (get loggers-old % {}) (get loggers %))
                               (keys loggers)))]
              (if (= (count loggers1) 0)
                (update world :loggers #(dissoc % sym-qual))
                (assoc-in world [:loggers sym-qual] loggers1)))))))))

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
