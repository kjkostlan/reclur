; String-based search tools, the most basic kind of searching.
(ns search.strfind
  (:require [clojure.string :as string] [clojure.set :as set] [clojure.pprint]
            [javac.file :as jfile]
            [layout.layoutcore :as layoutcore]
            [app.rtext :as rtext]
            [app.multicomp :as multicomp]
            [app.fbrowser :as fbrowser]
            [app.orepl :as orepl]
            [app.siconsole :as siconsole])
  (:import [java.util.regex Pattern]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Basic tools ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn wix- [v ix]
  (if (= ix 0) (dec (count v)) (dec ix)))

(defn wix+ [v ix]
  (if (= ix (dec (count v))) 0 (inc ix)))

(defn boring-find [s ky case?]
  "Returns a vector of [start ix, end ix]. 
   ky can be either a string or regexp, as it can be in most cases.
   For non-regex each end ix is (count ky) more than the corresponding start ix."
  (let [case? (or case? (not (string? ky))) ; how to set case on a regexp?
        ky (if case? ky (string/lower-case ky))
        re (if (string? ky) (re-pattern (Pattern/quote ^String ky)) ky)
        s (if case? s (string/lower-case s))
        intervals (mapv count (string/split s re))
        lengths (mapv count (re-seq re s))
        n (count lengths)]
    (loop [acc [] nb4 0 ix 0]
      (if (= ix n) acc
          (let [ni (nth intervals ix) nl (nth lengths ix)]
            (recur (conj acc [(+ nb4 ni) (+ nb4 ni nl)])
                   (+ nb4 ni nl) (inc ix)))))))

(defn find1 [txt ky case? ix dir wrap?]
  "Finds the previous or next instance from ix (depending on the sign of dir).
   ky can be a string or regexp. Returns [start ix, end ix] or nil."
  (let [all-pairs (boring-find txt ky case?)] ; don't know of a better way than just running the whole string.
    (if (< dir 0)
      (let [last-b4 (last (filterv #(<= (last %) ix) all-pairs))]
        (if last-b4 last-b4 (if wrap? (last all-pairs))))
      (let [first-afr (first (filterv #(> (first %) ix) all-pairs))] ; > or >= ?
        (if first-afr first-afr (if wrap? (first all-pairs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Working with the app state ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn search-step [s opts] 
  "Runs a single step."
  (let [boxk (:boxk opts) comps (:components s)
        compk (:target opts) comp (get comps compk)
        go-fn0 (:goto (:layout s)) ;[s k key-is-file? char-ix0 char-ix1]
        go-fn (fn [s k key-is-file? char-ix0 char-ix1] 
                (let [s1 ((:goto (:layout s)) s k key-is-file? char-ix0 char-ix1)
                      cam0 (:camera s) cam1 (:camera s1)
                      max-z (layoutcore/max-z {:components (dissoc (:components s1) boxk)})]
                  (update-in s1 [:components boxk] #(assoc (layoutcore/dont-move % cam0 cam1) :z (inc max-z)))))
        fail (fn [] (siconsole/log s (str "not found: " (pr-str (:key opts)))))
        file2k (fn [filename]
                 (first (filterv #(let [c (get comps %)] 
                                    (and (= (:type c) :codebox) (= (count (:path c)) 1) (= (first (:path c)) filename)))
                   (keys comps))))] 
    (cond (or (:dumb-text? opts) (and (not= (:type comp) :codebox) (not= (:type comp) :fbrowser)))
      (let [txt (rtext/rendered-string comp)
            cur-ix (:cursor-ix comp)
            ix1? (find1 txt (:key opts) (:case? opts) cur-ix (if (:reverse? opts) -1 1) true)]
        (if ix1? (go-fn s compk false (first ix1?) (second ix1?)) (fail)))
      (= (:type comp) :codebox)
      (let [fname (first (:path comp))
            txt (multicomp/open-cache s fname)
            k0 (file2k fname)
            cur-ix (second (multicomp/cursor-locate s k0))
            ix1? (find1 txt (:key opts) (:case? opts) cur-ix (if (:reverse? opts) -1 1) true)]
        (if ix1? (go-fn s fname true (first ix1?) (second ix1?)) (fail)))
      :else ;fbrowser
      (let [files (filterv jfile/texty? (fbrowser/recursive-unwrap (fbrowser/devec-file (:path comp)))) nf (count files)
            comps (:components s)]
        (if (= nf 0)
          (fail)
          (let [file-ix (if (:index opts) (:index opts) 0) 
                fix?1 (loop [ix file-ix nloop 0]
                        (if (<= nloop nf) 
                          (let [fname (nth files ix)
                                txt (jfile/open fname)
                                k0? (file2k fname)
                                cur-ix (if k0? (second (multicomp/cursor-locate s k0?))
                                         (if (:reverse? opts) (count txt) -1))
                                sel-ix? (find1 txt (:key opts) (:case? opts) cur-ix (if (:reverse? opts) -1 1) false)]
                            (if sel-ix? [fname (first sel-ix?) (second sel-ix?)] 
                              (recur ((if (:reverse? opts) wix- wix+) files ix) (inc nloop))))))]
            (if fix?1 (go-fn s (first fix?1) true (second fix?1) (nth fix?1 2)) 
              (fail))))))))

(defn pretty [code]
  (with-out-str (clojure.pprint/pprint code)))

(defn add-search-box [s]
  (let [ckys (:selected-comp-keys s)]
    (if (= (count ckys) 0) (siconsole/log s "Must select a component to search within.")
      (let [boxk (keyword (gensym 'searchbox))
            m0 {:key "" :case? false :reverse? false :dumb-text? false}
            
            blit #(if (symbol? %) (symbol (str "'" %)) %)
            cky (first ckys)
            m (assoc m0 :target (blit cky) :boxk (blit boxk))
            code (list 'do
                       '(require 'search.strfind)
                       (list 'search.strfind/search-step 's m))
            code (pretty code)
            cursor-ix0 (+ (first (first (boring-find code ":key \"\"" false))) 6)
            new-comp (orepl/command-wrapped-repl 's code cursor-ix0)
            s1 (assoc s :selected-comp-keys #{boxk} :typing-mode? true)
            s2 (if (> (count ckys) 1) (siconsole/log s1 "Multible components selected, only selecting the first one.") s1)]
        ((:add-component (:layout s2)) s2 new-comp boxk true)))))
