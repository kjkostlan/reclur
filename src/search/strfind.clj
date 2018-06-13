; String-based search tools, the most basic kind of searching.
(ns search.strfind
  (:require [clojure.string :as string]
            [javac.file :as jfile]
            [layout.layoutcore :as layoutcore]
            [app.rtext :as rtext]
            [app.multicomp :as multicomp]
            [app.fbrowser :as fbrowser]
            [app.orepl :as orepl]
            [app.siconsole :as siconsole]
            [clojure.pprint])
  (:import [java.util.regex Pattern]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Basic searching tools ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn boring-find [s ky case?]
  "Returns a vector of [start ix, end ix]. 
   s-key can be either a string or regexp, as it can be in most cases.
   For non-regex each end ix is (count s-key) more than the corresponding start ix."
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

(defn file-find [files ky case?]
  "Search in any of files and returns a map of filename => start ix, end ix].
   Non-existant files are ignored. Only returns files with matches."
  (let [files (if (sequential? files) (into [] files) [files])
        texts (filterv identity (mapv jfile/open files))
        searches (mapv #(boring-find % ky case?) texts)
        out (zipmap files searches)]
    (select-keys out
      (filterv #(> (count (get out %)) 0) files))))

(defn folder-find [folder ky case?]
  "Search among clj files in said folder, acts recursivly.
   Returns a vector of [filename, start ix, end ix]."
  (let [files (fbrowser/recursive-unwrap folder)
        non-folder (filterv jfile/texty? files)]
    (file-find non-folder ky case?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Working with the app state ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn set-up-search [s opts] 
  (let [comps (:components s)
        sel-keys (:components opts)
        ; ts (get-in s [:tool-state ::strfind])
        ; :searches = vector of [k char-ix0 char-ix1], k = comp key used for :dumb-text?, filename if otherwise.
        ; :search-ix = index on searches that will be selected next.
        simple? (:dumb-text? opts)
        
        ; Texts, in order of key name or filename:
        kys (if simple? (into [] (sort sel-keys))
              (let [file-groups (mapv #(let [c (get comps %) ty (:type c) ph (:path c)]
                                         (cond (= ty :codebox) [(first ph)]
                                            (= ty :fbrowser) (filterv jfile/texty? (fbrowser/recursive-unwrap (fbrowser/devec-file ph)))
                                            :else [])) sel-keys)]
                (into [] (sort (apply hash-set (apply concat file-groups))))))
        texts (if simple? (mapv #(rtext/rendered-string (get kys %)) kys)
                  (mapv #(if (> (count (multicomp/who-has s % 0)) 0) 
                           (multicomp/open-cache s %) 
                           (if-let [x (jfile/open %)] x "")) kys))
        ; Search result groups:
        result-groups (mapv #(boring-find % (:key opts) (:case? opts)) 
                        texts)
        searches (into [] (apply concat 
                            (mapv (fn [k g] (mapv #(vector k (first %) (second %)) g)) 
                                  kys result-groups)))]
    (update-in s [:tool-state ::strfind]
              #(merge % {:searches searches :search-ix 0 :opts opts}))))

(defn search-step [s opts] 
  (let [ts (get-in s [:tool-state ::strfind])
        searches (:searches ts)
        ix (:search-ix ts)
        go-fn (:goto (:layout s)) ;[s k key-is-file? char-ix0 char-ix1]
                                        ; Go to the search
        search (nth searches ix)
        cam0 (:camera s)
        s1 (go-fn s (first search) (not (:dumb-text? opts)) (second search) (nth search 2))
        cam1 (:camera s1)
        ; Keep the repl where it is, and on top:
        box-k (:box-k ts)
        max-z (layoutcore/max-z (dissoc (:components s1) box-k))
        s2 (update-in s1 [:components box-k] #(layoutcore/dont-move % cam0 cam1))
        
        n (count (:searches ts))
        ix1 (if (:reverse? opts)
              (if (= ix 0) (dec n) (dec ix))
              (if (= ix (dec n)) 0 (inc ix)))]
    (assoc-in s2 [:tool-state ::strfind :search-ix] ix1)))

(defn search-loop [s opts]
  (let [ts (get-in s [:tool-state ::strfind])
        stuff? (boolean ts)
        ; recompile a fresh regexp every time => not equal => must convert to string.
        opts-change? (and stuff? (not= (str opts) (str (:opts ts))))

        s1 (if (and stuff? (not opts-change?) (not (:refresh? (:opts ts))))
             s (set-up-search s opts)) ; initial search set up, which sets the tool state.
        ts1 (get-in s1 [:tool-state ::strfind])]
    (if (> (count (:searches ts1)) 0)
      (search-step s1 opts) ; single search step, which involves moving to the component and keeping this box unmoved.
      (siconsole/log s1 (str "\"" (:key opts) "\"" " not found in the choosen comps.")))))

(defn pretty [code]
  (with-out-str (clojure.pprint/pprint code)))

(defn add-search-box [s]
  (let [box-k (keyword (gensym 'searchbox))
        s1 (assoc-in s [:tool-state ::strfind] {:box-k box-k})
        m0 {:key "" :case? false :reverse? false :dumb-text? false :refresh? false}
        ckys (:selected-comp-keys s)
        m (assoc m0 :components
                 (mapv #(if (symbol? %) (symbol (str "'" %)) %) ckys))
        code (list 'do
                   '(require 'search.strfind)
                   (list 'search.strfind/search-loop 's m))
        code (pretty code)
        cursor-ix0 (+ (first (first (boring-find code ":key \"\"" false))) 6)
        new-comp (orepl/command-wrapped-repl 's code cursor-ix0)
        s2 (assoc s1 :selected-comp-keys #{box-k} :typing-mode? true)]
    ((:add-component (:layout s2)) s2 new-comp box-k true)))
