; Different languages will be added eventually.

(ns coder.logger
  (:require [collections]
    [clojure.pprint :as pprint]
    [coder.refactor :as refactor]
    [coder.clojure :as cljparse]
    [coder.cbase :as cbase]
    [clojure.walk :as walk]
    [clojure.set :as set]
    [layout.layoutcore :as layoutcore]
    [globals]))


(def req-code 
  (let [x (first (cljparse/forwards "(require '[coder.logger :as logger])"))]
    (-> x (vary-meta #(assoc-in % [:PARSE 0 :txt 0 0] "\n"))
      (vary-meta #(assoc-in % [:PARSE 0 :txt 2 2] "\n")))))

(def log-code 
  (let [x (first (cljparse/forwards "coder.logger/logmacro"))]
   (-> x (vary-meta #(assoc-in % [:PARSE 0 :txt 0] "\n"))
      (vary-meta #(assoc-in % [:PARSE 0 :txt 2] "\n")))))

(defn log!! [x path]
  "Called when the logged code runs." 
  (swap! globals/one-atom
    #(let [log {:path path :time (System/nanoTime) :value x}
           logs (if (:logs %) (:logs %) [])]
      (assoc % :logs (conj logs log)))) x)

(defn logify-code [path code in-let? in-fn?]
  "Recursive log of any collections."
  (pprint/pprint code)
  (if (coll? code)
    (let [pathq (mapv #(if (symbol? %) (list 'quote %) %) path)
          listy? (and (sequential? code) (not (vector? code)))
          in-let1? (and listy? (= (first code) 'let*))
          in-fn1? (and listy? (= (first code) 'fn*))
          code1 (if (map? code) 
                  (zipmap (keys code) (mapv #(logify-code (conj path %1) %2 false false) (keys code) (vals code)))
                  (collections/cmap #(logify-code (conj path %2) % in-let1? in-fn1?) code (collections/ckeys code true)))
          code1 (if in-fn? (collections/cassoc code1 0 (first code)) code1)] ; the first is the argument.    
      (if (or (and listy? (= (first code) 'def*)) in-let? in-fn?) code1 
        (list 'coder.logger/log!! code1 pathq)))
    code))

(defmacro logmacro [& args]
  "Applied to a single form, such as a fn. 
   Acts recursively. The first two elements of the path are [file, form]."
  (let [code (apply list args)
        code1 (walk/macroexpand-all code)] 
    (logify-code [*file* (:line (meta &form))] code1 false false)))

(defn add-logger!! [sym-qual & for-reload]
  "Adds a logger at the qualified sym by modifying it's code."
  (let [s-code (:source (cbase/var-info sym-qual true))]
    (if s-code
      (let [ns-obj (find-ns (cbase/ns-of sym-qual))
            s-code1 (binding [*ns* ns-obj] 
                      (walk/macroexpand-all s-code))
            body (last s-code1)
            logged-body (second (logify-code [sym-qual] (last s-code1) false false))]
        (swap! globals/one-atom 
          #(assoc-in % [:loggers sym-qual] true))
        (binding [*ns* ns-obj]
          (do
            (if (first for-reload)
              (println "Re-added logger for:" sym-qual)
              (println "Added logger for:" sym-qual))
            ;(clojure.pprint/pprint logged-body) ; debugging when the logger breaks the code
            (alter-var-root (find-var sym-qual)
              (fn [old-val] 
                (collections/keep-meta old-val (fn [_] (eval logged-body)))))))))))

(def _junk-ns (create-ns 'logger-ns))

(defn reload-but-keep-loggers!! [ns-sym]
  "Reloads the namespace, keeping all loggers we need."
  (let [_ (binding [*ns* _junk-ns]
            (require ns-sym :reload))
        syms (keys (ns-interns ns-sym))
        sym-quals (set (mapv #(cbase/qual ns-sym %) syms))
        sym-logged (set (keys (:loggers @globals/one-atom)))
        need-logged (set/intersection sym-quals sym-logged)]
    (mapv #(add-logger!! % true) need-logged) nil))

(defn remove-logger!! [sym-qual]
  (let [_ (println "Removing the logger of:" sym-qual)
        ns-sym (cbase/ns-of sym-qual)]
    (swap! globals/one-atom #(assoc % :loggers (dissoc (:loggers %) sym-qual)))
    (reload-but-keep-loggers!! ns-sym)))

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
    (if logged? (do (remove-logger!! qual-sym) s)
      (let [_ (add-logger!! qual-sym)
        
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


#_(defn log-toggle [txt cursor-ix]
  "Toggle the logging code for txt given cursor-ix on txt.
   DEPRECATED: we instead desire to do everything in ram rather than modifying a file."
  (let [x (cljparse/forwards txt)
        
        char-ix (let [ch (get txt (dec cursor-ix))]
                  (if (or (= ch \)) (= ch \]) (= ch \}))
                    cursor-ix (dec cursor-ix)))

        ; Path to whatever collection encloses our cursor:
        ph (if (= char-ix -1) [] (cljparse/path-at x char-ix false))

        ph (if (coll? (collections/cget-in x ph)) ph (into [] (butlast ph)))

        xi (collections/cget-in x ph)
        can-toggle? (and (not= ph []) (sequential? xi) (not (vector? xi)))
        set-ends1 (fn [c] (refactor/set-ends-length c [0] 1 1))]
     (if can-toggle? 
       (let [log-import? (= (second x) req-code)
             logged? (= (first xi) log-code)
             x1 (collections/cupdate-in x ph
                  (fn [xi]
                    (if logged?
                      (let [xi1 (set-ends1 xi)]
                        (refactor/lv-update xi1 rest))
                      (collections/keep-meta xi #(conj % log-code)))))
             x2 (if log-import?
                  (if (collections/find-value-in x1 log-code) 
                    x1 ; if we still have log code somewhere don't remove the require.
                    (let [x1 (refactor/set-ends-length x1 [1] 1 1)]
                      (refactor/lv-update x1 #(into [] (concat [(first %)] (subvec % 2))))))
                  (refactor/lv-update x1 #(into [] (concat [(first %)] [req-code] (subvec % 1)))))
             txt2 (cljparse/backwards x2)]
         txt2) (do (println "can't toggle the logger at that cursor location.") txt))))


#_(defn log-toggle-at-cursor [s] 
  "Toggles logging code in the form enclosing the cursor. \n   Does not save, the user will have to ctrl+s."    
  (let [k (first (:selected-comp-keys s))]
    (if (= (:type (get-in s [:components k])) :codebox)
      (let [filename-ix (multicomp/cursor-locate s k)
            fname (first filename-ix) ix (second filename-ix)
        
            txt (multicomp/open-cache s fname)
            txt1 (log-toggle txt ix)
            s1 (multicomp/save-cache s fname txt1)
            ; Add a repl that has a basic inspection engine:
            added-code "(do (require 'globals) \n  (let [logs (:logs @globals/one-atom)] \n  )))"
            k-sc (layoutcore/most-on-screen s #(= (:type %) :orepl))
            k (first k-sc) sc (second k-sc)
            use? (and sc (> sc 0.5))
            p0 {:text added-code} p1 {:text ""} p2 p1
            the-repl (assoc (assoc (if use? (get (:components s1) k) (orepl/new-repl)) :pieces [p0 p1 p2]) :cursor-ix 67)
            k (if use? k (gensym 'logrepl))
            s2 (if use? (assoc-in s1 [:components k] the-repl) 
                 ((:add-component (:layout s1)) s1 the-repl k true))] s2) s)))
 