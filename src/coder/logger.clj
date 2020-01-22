; Different languages will be added eventually.

(ns coder.logger
  (:require [collections]
    [clojure.pprint :as pprint]
    [coder.refactor :as refactor]
    [coder.clojure :as cljparse]
    [coder.cbase :as cbase]
    [clojure.walk :as walk]
    [clojure.test :as test]
    [clojure.set :as set]
    [clojure.string :as string]
    [coder.unerror :as unerror]
    [javac.exception :as jexc]
    [layout.layoutcore :as layoutcore]
    [layout.blit :as blit]
    [globals]))

(defn error [] "Debug"
  (/ 0))

(def ^:dynamic *debug-shorten-code?* false)

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
            (merge {:ns (cbase/ns-ob2sym *ns*) :code code} 
              (clean-error-stack (jexc/clje e) "none"))) (throw e)))))

(defn log! [x path]
  "Called when the logged code runs." 
  (swap! globals/one-atom
    #(let [log {:path path :time (System/nanoTime) :value x :Thread (Thread/currentThread)}
           logs (if (:logs %) (:logs %) [])]
      (assoc % :logs (conj logs log)))) x)

(defn get-logs [] (:logs @globals/one-atom))

(defn get-logger [sym-qual] "nil = no logger."
  (get-in @globals/one-atom [:loggers sym-qual]))

(defn logify-code [path code in-let? in-fn? shallow-only?]
  "Logs every path within the code recursively.
   Path should start with a fully qualified symbol."
  ;(println "CODE TO LOGIFY:") (pprint/pprint code) ; For debugging
  (if (or (not shallow-only?)
        (<= (count path) 4)) ; [symbol fn-body arity location-within-subbody]
    (let [log!-sym (if *debug-shorten-code?* '! `log!)
          iddent (if *debug-shorten-code?* #(apply str (rest %)) identity)
          ;_ (println path code)
          ;_ (println "Path:" path "code head: " (first code))
          pathq (mapv #(if (symbol? %) (list 'quote %) %) path)
          listy? (and (sequential? code) (not (vector? code)))
          in-let1? (and listy? (get #{'let* 'let 'loop 'loop* 'binding 'binding*} (first code)))
          in-fn1? (and listy? (contains? #{'fn 'fn*} (first code)))
          logify (fn [add-path sub-code] (logify-code (conj path add-path) sub-code in-let1? in-fn1? shallow-only?))
          code1 (cond (map? code)
                  (zipmap (keys code) (mapv logify (keys code) (vals code)))
                  (and (vector? code) in-let?)
                  (mapv #(if (even? %1) %2 (logify %1 %2)) (range) code)
                  (vector? code)
                  (mapv logify (range) code)
                  (set? code) code ; can't logify the keys!
                  (and in-fn? (vector? (first code))) ; (fn ***([x y] (+ x y))***) -> (fn ([x y] (log! [x y]) (log! (+ x y))))
                  (let [argv (filterv #(not= % '&) (first code))
                        argvl (list log!-sym argv (iddent path))
                        codel (mapv logify (range) code)]
                    (apply list (first code) argvl (rest codel)))
                  in-fn1? (let [unboxed? (not= (count code) 2)] ; ***(fn ([x y] (+ x y)))***
                            (if unboxed? 
                              (apply list (first code)
                                (second code) (list log!-sym (second code) (iddent (conj path 1)))
                                (mapv logify (range 1 1e100) (rest (rest code))))
                              (apply list (first code)
                                (mapv logify (range 1 1e100) (rest code)))))
                  (symbol? code) (list log!-sym code (iddent path))
                  (not (coll? code)) code 
                  (= (first code) 'new) ; Java interop!
                  (apply list 'new (second code) (mapv logify (range 2 1e100) (rest (rest code))))
                  (= (first code) '.) ; Java interop!
                  (let [codev (into [] code)] 
                    (apply list '. (if (symbol? (second code)) (second code)
                                     (logify 1 (second code)))
                      (nth codev 2) (mapv logify (range 3 1e100) (subvec codev 3))))
                  (= (first code) 'recur) code ; Loop-recur format.
                  (not in-fn1?) (list log!-sym
                                  (apply list (first code) (mapv logify (range 1 1e100) (rest code)))                  
                                  (iddent path))
                  :else (apply list (first code) (mapv logify (range 1 1e100) (rest code))))]
      ;(println "Stuff:" in-fn? in-fn1? (type code) (type code1))
      (if (meta code) (with-meta code1 (meta code)) code1))
    code))

(defn sym2log [sym-qual shallow-only]
  "Symbol -> logged code. Throws errors."
  (if *debug-shorten-code?* (println "WARNING: *debug-shorten-code?* enabled, loggers won't work."))
  (let [v-info (cbase/var-info sym-qual true)
        _ (if (not v-info) (throw (Exception. (str "This (assumbed to be fully qualified symbol) can't be found: " sym-qual))))
        s-code (:source v-info)
        _ (if (nil? s-code) (throw (Exception. (str "Found variable, but no source code for:" sym-qual))))
        ns-obj (find-ns (cbase/ns-of sym-qual))
        _ (if (not ns-obj) (throw "Strange where did the ns go?"))
        s-code1 (into [] (binding [*ns* ns-obj] 
                               (clojure.walk/macroexpand-all s-code)))
        _ (if (not= (count s-code1) 3) (throw (Exception. "Loggers on vars can only be added to functions.")))
        _ (if (not (contains? #{'fn 'fn*} (first (last s-code1)))) (throw (Exception. "Loggers can only be added on def'ed functions.")))

        the-def (first s-code1) ; 'def
        the-name (second s-code1) ; my-fn
        fn-code (nth s-code1 2) ; (fn* ([x] ...) ([x y] ...))
        logged-fn (logify-code [(list 'quote sym-qual) (dec (count s-code1))] fn-code false false shallow-only)]
    (list the-def the-name logged-fn)))

(defn log-error-report [bad-code]
  "Should be in the target namespace."
  (println "Logging failed:")
  (unerror/errprint? bad-code))

(defn add-logger! [sym-qual shallow-only & use-reload-message]
  "Adds a logger at the qualified sym by modifying it's code."
  ;(println "Logging:" sym-qual) ; Debug (extra print).
  (let [use-reload-message (first use-reload-message)
        logged-code (sym2log sym-qual shallow-only)
        logged-fn-code (second (rest logged-code))
        ns-obj (find-ns (cbase/ns-of sym-qual))
        old-logger (get-in @globals/one-atom [:loggers sym-qual]) ; only used for a printout hint
        new-logger {:shallow-only? shallow-only}]
    (binding [*ns* ns-obj]
      ;(clojure.pprint/pprint s-code1)
      ;(clojure.pprint/pprint logged-body) ; debugging in case the logger misbehaves.
      (alter-var-root (find-var sym-qual)
        (fn [old-val] 
          ;(println "target-of eval:" logged-fn)
          ;(try (eval+ logged-body) (catch Exception e (do (println *ns*) (println e))))
          (collections/keep-meta old-val (fn [_] (eval+ logged-fn-code)))))
      (swap! globals/one-atom
         #(assoc-in % [:loggers sym-qual] new-logger))
      (if use-reload-message
        (if (= old-logger new-logger) 
          (println sym-qual " seems to be already logged, but re-added just in case.")
          (println "Added logger for:" sym-qual (if shallow-only "only shallow." "")))))))
 
(defn add-most-loggers! []
  "Attempts to be a comprehensive logger, which will generate a large number of logs that 
   can be analyzed."
  (let [all-vars (keys (cbase/varaverse))
        vars-no-log (filterv #(not (get-logger %)) all-vars) ; No prior logger.
        vars-fn (filterv #(clojure.test/function? %) vars-no-log)
        vars-not-us (filterv #(not (string/starts-with? (str %) "coder.logger")) vars-fn) ; dont log the loggers or else infinite loop!
        vars-not-inner-loop (filterv identity vars-not-us)] ; TODO: performance-based exclude.
    (mapv #(add-logger! % true false) vars-not-inner-loop)))

(def _junk-ns (create-ns 'logger-ns))

(defn reload-but-keep-loggers! [ns-sym]
  "Reloads the namespace, keeping all loggers we need."
  (let [_ (binding [*ns* _junk-ns]
            (require ns-sym :reload))
        syms (keys (ns-interns ns-sym))
        sym-quals (set (mapv #(cbase/qual ns-sym %) syms))
        loggers (:loggers @globals/one-atom)
        sym-logged (set (keys loggers))
        need-logged (set/intersection sym-quals sym-logged)]
    (mapv #(add-logger! % (:shallow-only? (get loggers %)) true) need-logged) nil))

(defn remove-logger! [sym-qual]
  (let [logged? (boolean (get-in @globals/one-atom [:loggers sym-qual])) ; only used for a printout hint
        _ (if logged? (println "Removing the logger of:" sym-qual)
            (println "No logger found for " sym-qual " but removing just in case."))
        ns-sym (cbase/ns-of sym-qual)]
    (swap! globals/one-atom #(assoc % :loggers (dissoc (:loggers %) sym-qual)))
    (reload-but-keep-loggers! ns-sym)))

(defn remove-all-loggers! []
  (let [loggers (:loggers @globals/one-atom)]
    (mapv remove-logger! (keys loggers))))

(defn log-toggle-at-cursor [s fname cursor-ix cache-txt blanck-repl] 
  "Toggles logging code in the form enclosing the cursor, 
   doesn't change the code, instead uses alter-var-root."    
  (let [x (cljparse/forwards cache-txt)
        char-ix (let [ch (get cache-txt (dec cursor-ix))]
                  (if (or (= ch \)) (= ch \]) (= ch \}))
                    cursor-ix (dec cursor-ix)))
        ph (if (= char-ix -1) [] (cljparse/path-at x char-ix false))
        ph (if (coll? (collections/cget-in x ph)) ph (into [] (butlast ph)))
        
        func-name (second (get x (first ph))) ; TODO: work for more complex nested defs, etc.
        ns-sym (cbase/file2ns fname)
        qual-sym (cbase/qual ns-sym func-name)
        logged? (get-in @globals/one-atom [:loggers qual-sym])]
    (if logged? (do (remove-logger! qual-sym) s)
      (let [_ (add-logger! qual-sym)
        
            ; Add a repl that has a basic inspection engine:
            repl-code "(do (require 'globals) \n  (let [logs (:logs @globals/one-atom)\n        loggers (:loggers @globals/one-atom)] \n    ))"
            k-sc (layoutcore/most-on-screen s #(= (:type %) :orepl))
            st=? (fn [st1 st2]
                   (apply = (mapv #(.replaceAll ^String (subs (str %) 0 (min (count (str %)) 70)) "\\s" "")
                              [st1 st2])))
            k (first k-sc) sc (second k-sc) use? (and sc (> sc 0.5))
            need-rcode? (not (st=? repl-code (get-in s [:components k :pieces 0 :text])))
            p0 {:text repl-code} p1 {:text ""} p2 p1
            maybe-mod #(if need-rcode? (assoc (assoc % :pieces [p0 p1 p2]) :cursor-ix 114) %)
            the-repl (maybe-mod (if use? (get (:components s) k) blanck-repl))
            k (if use? k (gensym 'logrepl))
            s1 (if use? (assoc-in s [:components k] the-repl) 
                 ((:add-component (:layout s)) s the-repl k true))] 
       s1))))

(defn clear-logs! [] (swap! globals/one-atom #(assoc % :logs [])))

(defn toggle-core [loggers toggled-keys]
  "Are at least 50% of them currently included?"
  (let [toggled-keys (set toggled-keys)
        included (filterv #(get loggers %) toggled-keys)
        excluded (filterv #(not (get loggers %)) toggled-keys)
        on? (>= (count excluded) (count included))]
    [on? (if on? excluded included)]))

(defn toggle-ui-loggers! []
  (let [syms #{'core/dispatch-listener} oc (toggle-core (get @globals/one-atom :loggers))
        on? (first oc) changes (second oc)]
    (if on? (println "Added global UI logger") (println "Removed global UI logger"))
    ; Adding a loger twice is OK, so the multi-use of atom is OK.
    (if on? (mapv add-logger! changes) (mapv remove-logger! changes))))
 