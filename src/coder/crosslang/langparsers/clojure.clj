; Clojure-specific parsing and related tools.
; In general finite-state parsers are fast and simple for parsing languages at a rough syntax level.

(ns coder.crosslang.langparsers.clojure
  (:require [coder.javar :as javar] [mt]
    [clojure.string :as string]))

#_(defn vstr [x] "Collection tokens may have a string or a vector in thier head."
  (if (vector? x) (apply str x) x))

(defmacro _add! [] '(do (aset-int st kx ix0) (aset-int en kx ix) (aset-int ty kx (if (>= level ignore-level) 0 mode))))
(defmacro _nx [start-ix] ; next non-space
  (let [code '(loop [jx start-ix comment? false]
                (if (>= jx n) jx
                    (let [cj (aget cs jx)]
                      (if comment? (recur (inc jx) (not= cj \newline))
                          (if (or (= cj \ ) (= cj \tab) (= cj \newline)) (recur (inc jx) false)
                              (if (= cj \;) (recur (inc jx) true) jx))))))]
    (read-string (.replace ^String (pr-str code) "start-ix" (str start-ix))))) ; don't make a habit of this kind of macro.
(defmacro _nnk [start-ix] ; next non-symbol or non-keyword.
  (let [code '(loop [jx start-ix]
                (if (>= jx n) jx
                    (let [cj (aget cs jx)]
                      (if (or (= cj \ ) (= cj \tab) (= cj \newline) (= cj \")
                              (= cj \() (= cj \[) (= cj \{) (= cj \)) (= cj \]) (= cj \})) jx (recur (inc jx))))))]
    (read-string (.replace ^String (pr-str code) "start-ix" (str start-ix))))) ; don't make a habit of this kind of macro.

(defn interstitial-depth-ints [^String s]
  "Ignores reader macros, should be super fast, used for real-time hilighting."
  (let [^chars cs (.toCharArray s)
        n (int (count cs))
        ^ints out (make-array Integer/TYPE (inc n))]
    (loop [ix (int 0) comment? false escape? false quote? false level (int 0)]
      (if (= ix n) "done"
        (let [c (aget cs ix) ix1 (inc ix)
              lev1 (if (or comment? quote? escape?) level
                     (if (or (= c \() (= c \[) (= c \{)) (inc level)
                       (if (or (= c \)) (= c \]) (= c \}))
                         (dec level) level)))]
          (aset-int out (inc ix) lev1)
          (if comment?
            (recur ix1 (not= c \newline) false false lev1)
            (if escape?
              (recur ix1 false false quote? lev1)
              (if (= c \\)
                (recur ix1 false true quote? lev1)
                (if quote?
                  (recur ix1 false false (not= c \") lev1)
                  (recur ix1 (= c \;) false (= c \") lev1))))))))
    out))

(defn tokenize-ints [^String s]
  "Returns three ^ints, the first is the starting ix on s, the second is the ending ix, the third is the token type.
   Even invalid syntax will generate some form of tokens w/o errors, but later on in the pipeline they may cause errors."
  (let [n (count s)
        N (inc (* n 2))
        ^ints st (make-array Integer/TYPE N) ; will be chopped down later as # tokens < # chars.
        ^ints en (make-array Integer/TYPE N)
        ^ints ty (make-array Integer/TYPE N)
        ^chars cs (.toCharArray s)
        n-tok (loop [ix (int 0) ; ix jumps reader macros and (), so our mode is never a brakcet or reader-macro.
                     ix0 (int 0) ; the beginning of the token we are currently on.
                     kx (int 0) ; which token we are currently on.
                     mode (int 0) ; what token code we are just prior to entering the character at ix, and 0 for just left a token even if there is no space.
                     escape? false
                     comment? false
                     quote? false
                     level (int 0) ignore-level (int 12345678)] ; special for #_, the level is the level inside the ().
                (if (>= ix n) (let [ix n] (_add!) (inc kx))
                  (let [c (aget cs ix) c1 (if (< (inc ix) n) (aget cs (inc ix)) \ )
                             sp? (or (= c \ ) (= c \,) (= c \tab) (= c \newline))]
                         (cond
                           escape? (recur (inc ix) ix0 kx mode false false quote? level ignore-level)
                           comment? (recur (inc ix) ix0 kx mode false (not= c \newline) false level ignore-level)
                           quote? (if (= c \") (let [ix (inc ix)] (_add!)
                                                    (recur ix ix (inc kx) 0 false false false level ignore-level))
                                      (recur (inc ix) ix0 kx mode (= c \\) false true level ignore-level))
                           (or (= c \() (= c \[) (= c \{) (and (= c \#) (= c1 \{))) ; Jump the open parens.
                           (let [_ (_add!) j (if (= c \#) 2 1) ix0 ix ix (+ ix j) mode 4 level (inc level) kx (inc kx)] (_add!) ; ix0 to the open paren, ix set to the index after the open paren.
                                (recur ix ix (inc kx) 0 false false false level ignore-level))
                           (or (= c \)) (= c \]) (= c \})) ; Jump the closing parens.
                           (let [_ (_add!) kx (inc kx) ix0 ix ix (inc ix) mode 5] (_add!)
                                (recur ix ix (inc kx) 0 false false false (dec level)
                                       (if (< (dec level) ignore-level) 12345678 ignore-level))) ; can cancel the ignore.
                           (and (= c \#) (= c1 \")) ; Jump into regexp literals.
                           (let [_ (_add!) kx (inc kx)]
                             (recur (+ ix 2) ix kx 3 false false true level ignore-level))
                           (and (= c \#) (not= mode 1) (not= mode 2)) ; Jump hash-tag reader macros.
                           (let [_ (_add!) kx (inc kx)
                                 nx (cond (and (= c1 \?) (< (+ ix 2) n) (= (aget cs (+ ix 2)) \@)) (_nx (+ ix 3))
                                          (or (= c1 \') (= c1 \_) (= c1 \?) (= c1 \=)) (_nx (+ ix 2))
                                          (= c1 \:) (_nnk (inc ix))
                                          :else (inc ix))
                                 mode 7 ix0 ix ix nx
                                 ignore-level0 ignore-level
                                 ignore-level (if (= c1 \_) -1 ignore-level) _ (_add!) ; force an ignore.
                                 ignore-level (if (= c1 \_) (min (inc level) ignore-level0) ignore-level)] ; min operator means nested #_(#_()) don't affect the outside ignore level.
                             (recur ix ix (inc kx) 0 (and (< ix n) (= (aget cs ix) \\)) false false level ignore-level))
                           (or (= c \~) (= c \@) (= c \`) (and (= c \') (not= mode 1) (not= mode 2))) ; Jump other kinds of reader macros.
                           (let [_ (_add!) kx (inc kx)
                                 nx (if (and (= c \~) (= c1 \@)) (_nx (+ ix 2)) (_nx (inc ix)))
                                 ix0 ix ix nx mode 7]
                             (_add!) (recur ix ix (inc kx) 0 (and (< ix n) (= (aget cs ix) \\)) false false level ignore-level))
                           (= c \^) (let [_ (_add!) kx (inc kx) nx (_nx (inc ix)) ix0 ix ix nx mode 8] ; jump over meta tags
                                      (_add!) (recur ix ix (inc kx) 0 (and (< ix n) (= (aget cs ix) \\)) false false level ignore-level))
                           (not= mode 0) (if (or sp? (and (= c \") (not= ix0 (dec ix) (not= (aget cs (dec ix)) \#))) (= c \;) (= c \\)) ; the little switchyard.
                                           (do (_add!) (recur (inc ix) ix (inc kx) (if (= c \\) 1 0) (= c \\) (= c \;) (= c \") level ignore-level)) ; don't enter into quotes just yet.
                                           (recur (inc ix) ix0 kx mode false false quote? level ignore-level))
                           :else (let [mode1 (cond (or sp? (= c \;)) 0 ; entering from a zero mode, the big switchyard.
                                                   (= c \:) 2
                                                   (or (= c \0) (= c \1) (= c \2) (= c \3) (= c \4) (= c \5) (= c \6) (= c \7) (= c \8) (= c \9)
                                                       (= c \") (= c \\)) 3
                                                   (and (or (= c \-) (= c \+))
                                                        (or (= c1 \0) (= c1 \1) (= c1 \2) (= c1 \3) (= c1 \4) (= c1 \5) (= c1 \6) (= c1 \7) (= c1 \8) (= c1 \9))) 3
                                                   :else 1)]
                                   (if (not= mode1 0)
                                     (do (_add!) (recur (inc ix) ix (inc kx) mode1 (= c \\) (= c \;) (= c \") level ignore-level))
                                     (recur (inc ix) ix0 kx 0 false (= c \;) false level ignore-level)))))))]
    [(javar/chop-ints st n-tok) (javar/chop-ints en n-tok) (javar/chop-ints ty n-tok)]))

(defn reads-string [s]
  (let [^String st (.trim ^String s)
        trail? (and (.endsWith st "/")
                 (let [ix (max 0 (.lastIndexOf st "\n"))]
                   (not (.contains ^String (subs st ix) ";"))))]
    (if trail? ; Pull a symbol. This makes hintboxes work for incomplete symbols like "foo/"
      (let [lx #(.lastIndexOf ^String s ^String (str %))
            stops " \t\n()[]{};,'\"~@"
            last-ix (apply max -1 (mapv lx stops))]
        [(symbol (subs s (inc last-ix)))])
      (binding [*default-data-reader-fn* tagged-literal] (read-string (str "[" s "\n]"))))))

(defn interstitial-depth [s]
  "A fast parser used for indent hiliting et al."
  (into [] ^ints (interstitial-depth-ints (str s))))

(defn _map-key-core [contents tok lev are-contents-map?]
  "Subvecs, map-contents does not include the {}."
  (let [l0 (first lev) n (count tok)]
    (loop [acc [] ix 0 parity (if are-contents-map? 0 1)]
      (if (= ix n) acc
        (let [l (nth lev ix) ch (nth contents ix)
              t (nth tok ix)
              parity1 (if (and (= l l0) (= t 0) (> ix 0) (not= (nth tok (dec ix)) 0) are-contents-map?) (inc parity) parity)]
          (if (even? parity)
            (recur (conj acc are-contents-map?) (inc ix) parity1)
            (let [map-open? (and (= ch \{) (or (= ix 0) (not= (nth contents (dec ix)) \#)))
                  map-close (if map-open?
                              (loop [jx (inc ix)]
                                (if (= jx n) (dec jx)
                                  (let [l1 (nth lev jx)]
                                    (if (<= l1 l) (dec jx) (recur (inc jx)))))))
                  map-close (if map-close (max (inc ix) map-close))] ; anti out-of-bounds.
              (if map-open?
                (recur (apply conj acc false
                         (_map-key-core (subs contents (inc ix) map-close)
                           (subvec tok (inc ix) map-close)
                           (subvec lev (inc ix) map-close) true))
                   map-close parity1)
                (recur (conj acc false) (inc ix) parity1)))))))))
(defn map-key-indicators [txt tok-val-each-char inter-levels]
  "Vector that is true for map keys, used for repl hilighting but not much else.
   False when not in a map."
  (_map-key-core txt tok-val-each-char inter-levels false))

(defn clean-sym [sym]
  "Unique to clojure, the reader macro makes the code's value non-deterministic and read-string thus impure.
   Lets fix this!
   Remove all gensym numbers with clean:
   p1__12345# => %1
   foo12345 => foo
   foo__12345__auto__ => foo
   Later we can make a block of code deshadowed by adding numbers starting at 1,2,3,...."
  (let [s (str sym) s0 (first s) s1 (second s) s2 (get s 2) n (count s)
        num? #(string/includes? "0123456789" (str %))
        sclean (cond (re-find #"p\d+__\d+" s) ; inline fns.
                 (if (num? s2) (str "%" s1 s2) (str "%" s1))
                 (and (> n 4) (num? (get s (dec n))) (num? (get s (- n 2))) (num? (get s (- n 3))) (num? (get s (- n 4)))) ; gensyms.
                 (subs s 0 (loop [ix (dec n)] (if (and (> ix -1) (num? (get s ix))) (recur (dec ix)) (inc ix))))
                 (string/includes? s "__auto__") ; Hygenic macros.
                 (string/replace s #"__\d+__auto__" "")
                 :else (str sym))]
    (with-meta (symbol sclean) (meta sym))))

(defn clean-sym-block [x]
  "Cleans all symbols in x, but in a way that appends numbers to (determinstically) avoid shadowing.
   In rare cases, non-generated symbols may also be 'cleaned' but this will not create shadows."
  (let [trail-num-str (fn [x]
                        (if (string/includes? "0123456789" (str (last (str x))))
                          (last (re-seq #"\d+" (str x))) ""))
        trail-num #(let [trail-ns (trail-num-str %)]
                     (if (> (count trail-ns) 0) (Integer/parseInt trail-ns) -1)) ; -1 for no trailing number.
        remove-trail-num #(with-meta (symbol (subs (str %) 0 (- (count (str %)) (count (trail-num-str %))))) (meta %))
        syms (set (t/reduce-walk conj symbol? [] x))
        sym2clean (zipmap syms (mapv clean-sym syms))

        ; Function arity consistancy (without which the % could get confusing):
        %gen-order (fn [sym] (if (re-find #"p\d+__\d+" (str sym))
                               (let [num-str (last (re-seq #"\d+" (str sym)))]
                                 (Integer/parseInt num-str)) false))
        syms% (sort-by %gen-order (filterv %gen-order syms))
        addons (c/vcat [""] (mapv str "abcdefghijklmnopqrstuvwxyz"))
        %sym2clean (loop [acc {} ix 0 last-px 0 symsr (seq syms%)]
                     (if (empty? symsr) acc
                       (let [sym (first symsr)
                             px (Integer/parseInt (first (re-seq #"\d+" (str sym))))
                             increment? (<= px last-px)
                             ix1 (if increment? (inc ix) ix)
                             addon (get addons ix1 (str "0" ix1))
                             sym-clean1 (with-meta (symbol (str (get sym2clean sym) addon)) (meta sym))]
                         (recur (assoc acc sym sym-clean1)
                           ix1 px (rest symsr)))))
        sym2clean (merge sym2clean %sym2clean)

        ; Handling collisions:
        clean2sym1 (reduce (fn [acc sym]
                             (let [sym-clean (get sym2clean sym)
                                   syms-clean (conj (map #(with-meta (symbol (str sym-clean %)) (meta sym)) (range)) sym-clean)
                                   sym-clean1 (first (remove #(contains? acc %) syms-clean))]
                               (assoc acc sym-clean1 sym))) {} (sort syms))
        sym2clean1 (zipmap (vals clean2sym1) (keys clean2sym1))
        replace-f #(if (symbol? %) (with-meta (get sym2clean1 % %) (meta %)) %)] ; Extra careful with that metadata!
    (mt/m-postwalk replace-f x)))
