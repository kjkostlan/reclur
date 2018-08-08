; Different languages will be added eventually.

(ns coder.logger
  (:require [collections] [app.multicomp :as multicomp]
    [clojure.pprint :as pprint]
    [coder.refactor :as refactor]
    [coder.clojure :as cljparse]
    [clojure.walk :as walk]
    [globals]))
(require '[coder.logger :as logger])



(def req-code 
  (let [x (first (cljparse/forwards "(require '[coder.logger :as logger])"))]
    (-> x (vary-meta #(assoc-in % [:PARSE 0 :txt 0 0] "\n"))
      (vary-meta #(assoc-in % [:PARSE 0 :txt 2 2] "\n")))))

(def log-code 
  (let [x (first (cljparse/forwards "coder.logger/logmacro"))]
   (-> x (vary-meta #(assoc-in % [:PARSE 0 :txt 0] "\n"))
      (vary-meta #(assoc-in % [:PARSE 0 :txt 2] "\n")))))



(defn log!! [x path] 
  (swap! globals/one-atom
    #(let [log {:path path :time (System/nanoTime) :value x}
           logs (if (:logs %) (:logs %) [])]
      (assoc % :logs (conj logs log))))
x)

(defn logify-code [path code in-let? in-fn?]
  "Recursive log of any collections."
  (if (coll? code)
    (let [listy? (and (sequential? code) (not (vector? code)))
          in-let1? (and listy? (= (first code) 'let*))
          in-fn1? (and listy? (= (first code) 'fn*))
          code1 (if (map? code) (zipmap (keys code) (mapv #(log-code (conj path %) false false) (keys code)))
                  (collections/cmap #(logify-code (conj path %2) % in-let1? in-fn1?) code (collections/ckeys code)))
          code1 (if in-fn? (collections/cassoc code1 0 (first code)) code1)] ; the first is the argument.
      (if (or (and listy? (= (first code) 'def*)) in-let? in-fn?) code1 
        `(log!! ~code1 ~path)))
    code))

(defmacro logmacro [& args]
  "Applied to a single form, such as a fn. 
   Acts recursively. The first two elements of the path are [file, form]."
(let [code (apply list args)
      code1 (walk/macroexpand-all code)] 
  (logify-code [*file* (:line (meta &form))] code1 false false)))

(defn log-toggle [txt cursor-ix]
  "Toggle the logging code for txt given cursor-ix on txt."
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


(defn log-toggle-at-cursor [s] 
  "Toggles logging code in the form enclosing the cursor. \n   Does not save, the user will have to ctrl+s."    
  (let [k (first (:selected-comp-keys s))]
    (if (= (:type (get-in s [:components k])) :codebox)
      (let [filename-ix (multicomp/cursor-locate s k)
            fname (first filename-ix) ix (second filename-ix)
        
            txt (multicomp/open-cache s fname)
            txt1 (log-toggle txt ix)
            s1 (multicomp/save-cache s fname txt1)] s1) s)))

#_(defn foo [x] (+ x 30))