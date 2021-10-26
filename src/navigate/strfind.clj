; String-based search tools, the most basic kind of searching.
(ns navigate.strfind
  (:require [clojure.string :as string] [clojure.set :as set] [clojure.pprint] [clojure.walk :as walk]
            [javac.file :as jfile]
            [app.rtext :as rtext]
            [app.multicomp :as multicomp]
            [app.fbrowser :as fbrowser]
            [app.orepl :as orepl]
            [app.codebox :as codebox]
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
        n (min (count intervals) (count lengths))]
    (loop [acc [] nb4 0 ix 0]
      (if (= ix n) acc
          (let [ni (nth intervals ix) nl (nth lengths ix)]
            (recur (conj acc [(+ nb4 ni) (+ nb4 ni nl)])
                   (+ nb4 ni nl) (inc ix)))))))

(defn find1 [txt opts ix wrap?]
  "Finds the previous or next instance from ix (depending on the sign of dir).
   ky can be a string or regexp. Returns [start ix, end ix] or nil."
  (let [ky (:key opts) case? (:case? opts) dir (if (:reverse? opts) -1 1)
        all-pairs (boring-find txt ky case?)] ; don't know of a better way than just running the whole string.
    (if (< dir 0)
      (let [last-b4 (last (filterv #(<= (last %) ix) all-pairs))]
        (if last-b4 last-b4 (if wrap? (last all-pairs))))
      (let [first-afr (first (filterv #(> (first %) ix) all-pairs))] ; > or >= ?
        (if first-afr first-afr (if wrap? (first all-pairs)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Working with the app state ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn search-step1-naive [component opts wrap?]
  "Just uses the text itself."
  (let [txt (rtext/rendered-string component)
        cur-ix (:cursor-ix component)
        ixs1? (find1 txt opts cur-ix wrap?)]
    (if ixs1? (rtext/scroll-to-see-cursor
               (assoc component :cursor-ix (first ixs1?)
                 :selection-start (first ixs1?)
                 :selection-end (second ixs1?))))))

(defn search-step1-codebox [component opts wrap?]
  "Nil if failed."
  (let [txt (codebox/real-string component)
        cix (if (< (:selection-start component) (:selection-end component))
              (if (:reverse? opts) (min (:cursor-ix component) (:selection-start component))
                (max (:cursor-ix component) (:selection-end component)))
              (:cursor-ix component))
        real-ix (codebox/cursor-to-real-string (assoc component :cursor-ix cix))
        ixs1? (find1 txt opts real-ix wrap?)
        comp1 (if ixs1? (rtext/scroll-to-see-cursor
                          (apply codebox/select-on-real-string component ixs1?)))]
    comp1))

(defn search-step1-fbrowser [s box opts]
  "f-browsers allow searching all files within the fbrowser, and open codeboxes in doing so."
  (let [boxk (:boxk opts)
        fail (fn [] (siconsole/log s (str "not found: " (pr-str (:key opts)))))
        files (into [] (sort (filterv jfile/texty? (fbrowser/recursive-unwrap (fbrowser/devec-file (:path comp))))))
        file-order (zipmap files (range))
        boxes (:components s) nf (count files)
        codeboxks (filterv #(= (:type (get boxes %) :codebox)) (keys boxes))
        cur-box (apply max-key #(get-in boxes [% :z]) codeboxks)
        first-match? (fn [fname] (find1 (jfile/open fname) opts (if (:reverse? opts) 1e100 0) false))
        next-file (fn [fname]
                    (let [maybe-r #(if (:reverse? opts) (reverse %) %)
                          fiix (get file-order fname 0)
                          flist (apply concat (maybe-r [(maybe-r (subvec files (inc fiix))) (maybe-r (subvec files 0 fiix))]))]
                      (first (filter first-match? flist))))
        go-fn0 (:goto (:layout s)) ;[s k key-is-file? char-ix0 char-ix1]
        afloat (fn [s1 fname]
                 (let [max-z (apply max (mapv :z (vals boxes)))]
                   (assoc s1 :components
                     (zipmap (keys (:components s1))
                       (mapv #(if (and (= (:type %) :codebox)
                                    (= (fbrowser/devec-file (:path %)) fname))
                                (assoc % :z (inc max-z)) %)
                         (vals (:components s1)))))))
        go-fn (fn [fname ix0 ix1] (afloat (go-fn0 s fname ix0 ix1) fname))
        local-attempt (if cur-box (search-step1-codebox (get boxes cur-box) opts false))]
        (assoc
          (cond (= nf 0) (fail)
            (not local-attempt)
            (let [current-file (fbrowser/devec-file (:path (get boxes cur-box)))
                  next-file (next-file current-file)]
              (if next-file (apply go-fn next-file (first-match? next-file)) (fail)))
            :else (assoc-in s [:components cur-box] local-attempt))
          :selected-comp-keys #{boxk})))

(defn search-step [s opts]
  "Every time search is called, returns the modified state."
  (let [boxk (:boxk opts) comps (:components s)
        compk (:target opts) comp (get comps compk)
        fail (fn [] (siconsole/log s (str "not found: " (pr-str (:key opts)))))]
    (cond (or (:dumb-text? opts) (and (not= (:type comp) :codebox) (not= (:type comp) :fbrowser)))
      (if-let [comp1 (search-step1-naive comp opts true)] (assoc-in s [:components compk] comp1) (fail))
      (= (:type comp) :codebox)
      (if-let [comp1 (search-step1-codebox comp opts true)] (assoc-in s [:components compk] comp1) (fail))
      :else
      (search-step1-fbrowser s (get comps boxk) opts))))

(defn pretty [code]
  (with-out-str (clojure.pprint/pprint code)))

(defn add-search-box [s]
  (let [ckys (:selected-comp-keys s)]
    (if (= (count ckys) 0) (siconsole/log s "Must select a component to search within.")
      (let [boxk (keyword (gensym 'searchbox))
            m0 {:key "" :case? false :reverse? false :dumb-text? false :boxk 'core/*comp-k*}

            blit #(if (symbol? %) (symbol (str "'" %)) %)
            cky (first ckys)
            m (assoc m0 :target (blit cky))
            code '(fn [s] (navigate.strfind/search-step s m))
            code (walk/postwalk #(if (= % 'm) m %) code)
            code (with-meta code {:global true})

            code-string (binding [*print-meta* true] (layout.blit/vps code))
            cursor-ix0 (+ (first (first (boring-find code-string ":key \"\"" false))) 6)
            new-comp (assoc (orepl/new-repl code-string) :cursor-ix cursor-ix0)
            s1 (assoc s :selected-comp-keys #{boxk} :typing-mode? true)
            s2 (if (> (count ckys) 1) (siconsole/log s1 "Multible components selected, only selecting the first one.") s1)]
        ((:add-component (:layout s2)) s2 new-comp boxk)))))
