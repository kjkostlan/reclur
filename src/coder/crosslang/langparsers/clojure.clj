; Clojure-specific parsing and related tools.
; In general finite-state parsers are fast and simple for parsing languages at a rough syntax level.

(ns coder.crosslang.langparsers.clojure
  (:require [coder.javar :as javar]))

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