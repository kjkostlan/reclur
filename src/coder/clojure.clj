
; Simple finite-state machine parsers to get interstitial indent level very fast, for clojure.
; Interstitial means the indent level at each cursor location.
; For the string "(foo)" we would get 011110, i.e. one more element than the (count "(foo)")
; The -ints functions return native arrays, the vanilal forms return vectors.
; Must take linux newlines, the file reader should automatically replace windows newlines with linux newlines.

  ; Token codes (which can generalize to any language):
  ; 0 = space and comments and #_(foo), includes delimiters as found in java, python, etc, even if they are necessary, includes MOST python whitespace.
  ; 1 = symbols (basically the same in any language, + and = are symbols in java even though they are treated differently).
  ; 2 = keywords (reserved words like "class" in java, python, etc).
  ; 3 = literals (boolean, number, string, regexp, etc).
  ; 4 = opening ( [ { #{ almost the same in any language, includes opening <tags>, and includes indent whitespace in python.
  ; 5 = closing ) ] } almost the same in any language, includes </tags>, empty for python as there are no chars to be assigned to a dedent.
  ; 6 = reader macros (does not include metadata, sets, or regexp), includes spaces in reader macros which is possible in poorly formatted code, mostly unique to lisps, does not include C macros.
  ; 7 = meta tag (java annotations and python decorators).
  ; This list seems 95% universal for most languages, so there isn't a need to have a fancier more extensible datatype for now.

; Also contains a reversable parser.

(ns coder.clojure
  (:require [clojure.string :as string] collections
            [clojure.set :as set]))

;;;;;;;;;;;;;;;;;;; Support functions ;;;;;;;;;;;;;;;;;;;;;;;

(defn vstr [x] "Collection tokens may have a string or a vector in thier head."
  (if (vector? x) (apply str x) x))

(defn chop-ints [^ints x n]
  "Chops down an array of ints to size n."
  (let [^ints out (make-array Integer/TYPE n)]
    (loop [ix 0]
      (if (= ix n) out
          (do (aset-int out ix (aget x ix))
              (recur (inc ix)))))))

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

(defn depth-ints [^String s]
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

(defn leaf-tokenize-ints [^String s]
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
                                 mode 6 ix0 ix ix nx
                                 ignore-level0 ignore-level
                                 ignore-level (if (= c1 \_) -1 ignore-level) _ (_add!) ; force an ignore.
                                 ignore-level (if (= c1 \_) (min (inc level) ignore-level0) ignore-level)] ; min operator means nested #_(#_()) don't affect the outside ignore level.
                             (recur ix ix (inc kx) 0 (and (< ix n) (= (aget cs ix) \\)) false false level ignore-level))
                           (or (= c \~) (= c \@) (= c \`) (and (= c \') (not= mode 1) (not= mode 2))) ; Jump other kinds of reader macros.
                           (let [_ (_add!) kx (inc kx)
                                 nx (if (and (= c \~) (= c1 \@)) (_nx (+ ix 2)) (_nx (inc ix)))
                                 ix0 ix ix nx mode 6]
                             (_add!) (recur ix ix (inc kx) 0 (and (< ix n) (= (aget cs ix) \\)) false false level ignore-level))
                           (= c \^) (let [_ (_add!) kx (inc kx) nx (_nx (inc ix)) ix0 ix ix nx mode 7] ; jump over meta tags
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
    [(chop-ints st n-tok) (chop-ints en n-tok) (chop-ints ty n-tok)]))

(defn leaf-tokenize [^String s]
  "Two vectors, [the strings themselves, the type]. Also lumps consecutave tokens as 1."
  (let [x (leaf-tokenize-ints s)
        ^ints st (first x) ^ints en (second x) ^ints ty (nth x 2)
        n0 (count ty)
        x (loop [acc-s [] acc-ty [] ix 0 jx -1]
            (if (= ix n0) [acc-s acc-ty]
                (let [strg (subs s (aget st ix) (aget en ix))]
                  (if (= (count strg) 0) (recur acc-s acc-ty (inc ix) jx) ; skip empty tokens.
                    (let [acc-s (if (< (count acc-s) jx) (conj acc-s []) acc-s)
                          acc-ty (if (< (count acc-ty) jx) (conj acc-ty 0) acc-ty)
                          jx1 (if (and (= (aget ty ix) 0) (= (get acc-ty jx) 0)) jx (inc jx))]
                      (recur (update acc-s jx1 #(if % (conj % strg) [strg]))
                             (assoc acc-ty jx1 (aget ty ix)) (inc ix) jx1))))))]
    [(mapv #(apply str %) (first x)) (second x)]))

(defn _inclusive-indents [tys]
  (let [n (count tys)]
    (loop [acc [] clevel 0 ix 0]
      (if (= ix n) acc
        (let [tyi (nth tys ix) ix1 (inc ix)]
          (cond (= tyi 4) (recur (conj acc (inc clevel)) (inc clevel) ix1) ; open
            (= tyi 5) (recur (conj acc clevel) (dec clevel) ix1) ; close.
            :else (recur (conj acc clevel) clevel ix1))))))) ; neither open nor close.
(defn _bracket-pair [indents ty]
  (let [n (count indents) nobal #(throw (Exception. "Syntax error: Closing bracket with no opening bracket to match it."))
        close-to-open ; backwards map.
        (loop [acc {} ix 0 openings {}]
          (if (= ix n) acc
            (let [tyi (nth ty ix) l (nth indents ix)]
              (cond (= tyi 4) (recur acc (inc ix) (assoc openings l ix)) ; opening (type 4) is always before it's matching close (type 5), except for syntax errors.
                (= tyi 5) (recur (assoc acc ix (if-let [x (get openings l)] x (nobal))) (inc ix) openings)
                :else (recur acc (inc ix) openings)))))
        m-not-sorted (zipmap (vals close-to-open) (keys close-to-open))]
     (into (sorted-map) m-not-sorted)))

(defn bracket-pair [token-tys]
  "Map from opening bracket index to closing bracket index. No special treatment for reader macros or meta."
  (_bracket-pair (_inclusive-indents token-tys) token-tys))

(defn format-tokens [token-strs token-tys]
  "Forms head body tail value type tuples, removing space tokens and putting that space into heads or tails of other tokens.
   Gets the value from read-string, except for reader macros and meta it is nil, and parens it is an empty collection."
  (let [n (count token-strs)
        ; Stuff from the ending empty tokens, which will have to go to the tail of the last token.
        n0 (if-let [x (first (filter #(not= (nth token-tys %) 0) (range (dec n) -1 -1)))] (inc x))]
    (if n0
      (let [add-to-last-tail (apply str (subvec token-strs n0 n))
            out (loop [head-space "" acc [] ix 0]
                  (if (= ix n0) acc
                      (let [s (nth token-strs ix) ty (nth token-tys ix)]
                        (if (= ty 0) ; accumulate head buffer but don't add a token.
                          (recur (str head-space s) acc (inc ix))
                          (let [sp? #(or (= % \ ) (= % \,) (= % \tab) (= % \newline))
                                s-txt (cond (or (= ty 4) (= ty 5)) "" ; no body string for parens.
                                        (or (= ty 6) (= ty 7)) (apply str (filterv #(not (sp? %)) s))
                                        :else s) ; a couple of tokens are jumped over and may have trailing space.
                                s-sp (subs s (count s-txt)) ; s-sp goes after s-txt, except for ty=4 where it goes in the head.
                                val (cond (or (= ty 6) (= ty 7)) nil ; don't give reader macros or meta tags values yet.
                                          (or (= ty 4) (= ty 5)) (cond (or (= s "(") (= s ")")) () (or (= s "[") (= s "]")) [] (or (= s "{") (= s "}")) {} :else #{}) ; paren jumps don't include extra space.
                                          :else (read-string s-txt))
                                new-head (cond (= ty 4) [head-space s-sp ""] :else head-space)
                                new-tail (cond (= ty 4) "" (= ty 5) ["" s-sp ""] :else s-sp)
                                new-tok [new-head s-txt new-tail val ty]]
                            (recur "" (conj acc new-tok) (inc ix)))))))]
        (update-in out [(dec (count out)) 2] #(if (vector? %) (assoc % 2 (str (% 2) add-to-last-tail)) (str % add-to-last-tail))))
      [[(apply str token-strs) "" "" nil 0]]))) ; corner case: we only got space so return a nil token.

(defn _apply-groups [x] ; :tmp-ix is [5] and :tmp-pair is [6] within x. Pairs are inclusive. We could have also cheated with read-string...
  ; removes the stuff from [4] onward.
  (let [n (count x) xix-to-ix (zipmap (mapv #(nth % 5) x) (range))]
    (loop [acc [] ix 0]
      (if (= ix n) acc
        (let [xi (nth x ix) c (get xi 6)]
          (if c ; there IS a closing bracket. 
            (let [ixc (get xix-to-ix c) ; xip is the closing paren index.
                  xc (nth x ixc) mt (xi 3)] ; (xi 3) = (xc 3)
              (recur (conj acc [[(get-in xi [0 0]) (get-in xi [0 1]) (str (get-in xi [0 2]) (xi 1) (xi 2))] ; bieng on the safe side here.
                                (_apply-groups (subvec x (inc ix) ixc))
                                [(str (xc 0) (xc 1) (get-in xc [2 0])) (get-in xc [2 1]) (get-in xc [2 2])] mt]) (inc ixc))) 
            (recur (conj acc [(xi 0) (xi 1) (xi 2) (xi 3)]) (inc ix))))))))

(defn group-tokens [token-strs token-tys]
  "Returns tokens that are four elements long, head body tail value.
   The middle element is a vector of subtokens.
   For non-collections they are strings.
   For collections the the head and tail are three-element vectors, [space head space] and [space tail space].
   Doesn't parse reader macros.
   Body is is a string with no whitespace or vector for nesting, value is the leaf value."
  (let [tokens (format-tokens token-strs token-tys)
        open2close (bracket-pair (mapv #(nth % 4) tokens))
        tokens (mapv #(assoc %1 5 %2) tokens (range))
        tokens (mapv #(if-let [p (get open2close %2)] (assoc %1 6 p) %1) tokens (range))]
    (_apply-groups tokens)))

(defn _maxnum% [tok]
  (max (if (vector? (tok 1)) (apply max 0 (mapv _maxnum% (tok 1)))
           (let [v (last tok)]
             (if (and (symbol? v) (= (str (first (str v))) "%"))
               (let [n (if (= v '%) 1
                           (try (read-string (subs (str v) 1)) (catch Exception e 0)))]
                 (if (= (type n) java.lang.Long) n 0)) 0)))))

(defn _%at? [tok]
  (or (= (tok 3) '%&)
      (and (vector? (tok 1)) (first (filter _%at? (tok 1))))))

(defn _fn-shorthand [tok target]
  (let [n% (_maxnum% target)
        args (mapv #(vector "" "" "" (symbol (str "%" (inc %)))) (range n%))
        args (if (_%at? target) (conj args ["" "" "" '&] ["" "" "" '%&]) args)
        arg-tok ["" args "" []]
        tail (vstr (target 0))]
    [[(tok 0) (str (tok 1) (subs tail 0 1)) (str (tok 2) (subs tail 1))] ; the middle token will be #(
     (into [] (concat [["" "" "" 'fn] arg-tok]
                      (target 1)))
     (target 2) ()]))

(defn _map-ns-apply [tok target]
  (let [body (second tok)
        body1 (subs body 2) ; remove the #:
        mod-k (fn [k] (if (or (symbol? k) (keyword? k))
                           (let [s (str k) pieces (string/split s #"/")
                                 nms (first pieces)
                                 leaf (if (= (count pieces) 1) s (apply str (rest pieces)))
                                 leaf (if (or (symbol? k) (> (count pieces) 1)) leaf (subs leaf 1)) ; remove the leading :
                                 k1 (cond (= (count pieces) 1) (str body1 "/" leaf) ; qualify it.
                                          (or (= nms "_") (= nms ":_")) (str leaf) ; disqualify this.
                                          :else (if (symbol? k) (str k) (subs (str k) 1)))] ; no change.
                             (if (symbol? k) (symbol k1) (keyword k1))) k))
        mid1 (mapv #(if (even? %2) (update %1 3 mod-k) %1) (second target) (range))
        tarhd (target 0)]
    [[(tok 0) (str (tok 1) (tok 2) (tarhd 0) (tarhd 1)) (tarhd 2)] mid1 (target 2) {}]))

(defn _meta-tag [tok mdata target]
  (if (not target) (throw (Exception. "Metadata has no target token.")))
  (let [val (nth mdata 3)
        ; Metadata shortcuts:
        mdata1 (cond (or (symbol? val) (string? val))
                     [(str (mdata 0))
                      [["" "" "" :tag] ["" "" "" (mdata 3)]]
                      (str (mdata 1) (mdata 2)) {}]
                     (keyword? val)
                     [(str (mdata 0))
                      [["" "" "" (mdata 3)] ["" "" "" true]]
                      (str (mdata 1) (mdata 2)) {}]             
               :else mdata)]                  
    [[(tok 0) (tok 1) (tok 2)]
     [["" "" "" (symbol "META-TAG")] mdata1 target] "" ()]))

(def simple-macros {"'" 'quote "#'" 'var "`" 'quote "~" 'clojure.core/unquote "~@" 'clojure.core/unquote-splicing "@" 'clojure.core/deref
                       "#=" (symbol "READ-EVAL") "#?" (symbol "READ-CONDITIONAL") "#?@" (symbol "READ-CONDITIONAL-SPLICING")})
(def macros-simple (zipmap (vals simple-macros) (keys simple-macros)))

(def simple-str-set (set (keys simple-macros)))
(def simple-sym-set (set (vals simple-macros)))

(defn rmacro-1 [toks ix]
  "Applies the effects of the reader macro (if there is one) in the token at index ix."
  (let [tok (nth toks ix) body (second tok)
        rmacro? (and (nil? (last tok)) (not= body "nil") (not= body "")
                     (or (< (count (tok 0)) 2) (not= (subs (tok 0) 0 2) "#_")))]
    (if rmacro?
      (let [_ (if (<= (count toks) (inc ix)) (throw (Exception. "Reader macro has no target.")))
            target (get toks (inc ix))]
        (cond (and (> (count body) 2) (= (subs body 0 2) "#:")) ; ns-macros.
              ; These into [] steps are innefficient for reader macros with a lot of elements in the parent vector, but that should be a rare case.
              (into [] (concat (subvec toks 0 ix)
                               [(_map-ns-apply tok target)]
                               (subvec toks (+ ix 2))))
              (= body "#") ; Fn expansion macros.
              (into [] (concat (subvec toks 0 ix)
                               [(_fn-shorthand tok target)]
                               (subvec toks (+ ix 2))))
              (= body "^") ; metadata slurps tokens 3 at a time.
              (into [] (concat (subvec toks 0 ix)
                               [(_meta-tag tok target (get toks (+ ix 2)))]
                               (subvec toks (+ ix 3))))
              :else ; macros of the form a(x) => (b x).
              (into []
              (concat (subvec toks 0 ix)
                      [[[(tok 0) (tok 1) (tok 2)] [["" "" "" (get simple-macros body)] target] "" ()]]
                      (subvec toks (+ ix 2))))))
      toks)))

(defn inert? [s] ; only space and comments must be closed.
  (or (= s "") (= s " ") (= s ",") (= (-> s (string/replace "," "") (string/replace " " "") (string/replace "\n" "") (string/replace "\t" "")) "")
      (let [lft (leaf-tokenize (str s 1))
            lt (first lft)]
        (and (= (first (second lft)) 0) (= (count lt) 2) (= (second lt) "1")))))
(defn inert-project [s]
  (cond (inert? s) s
        (inert? (str s "\n")) (str s "\n")
        :else " "))

(defn _meta-resugar [tok0 tok]
  (let [mtok (get-in tok [1 1])
        ;stuffb4 (inert-project (mtok 0))
        ipj #(vector (inert-project (% 0)) "" (inert-project (% 2)))
        ; Sugar means no space between the end of the :tag and the beginning of the 'foo, etc.
        wants-sugar? (and (= (count (mtok 1)) 2)
                          (= (get-in mtok [1 0 2]) (get-in mtok [1 1 0]) ""))
        ;sugar-head (if wants-sugar? (get-in tok0 [1 1 0]) "")
        mtok1 (cond (not wants-sugar?) mtok
                    (= (get-in mtok [1 0 3]) :tag) [(ipj (mtok 0)) [["" "" "" :tag] (get-in mtok [1 1])] (ipj (mtok 2)) {}]
                    (boolean? (get-in mtok [1 1 3])) [(ipj (mtok 0)) [(get-in mtok [1 0]) ["" "" "" (get-in mtok [1 1 3])]] (ipj (mtok 2)) {}]
                    :else mtok)
        #_mtok1 #_(assoc-in mtok1 [1 1 0] sugar-head)]
    (assoc-in tok [1 1] mtok1)))

(defn read?s-str [x]
  (try (read-string (str "[" x "\n]"))
       (catch Exception e false)))

(defn _is-%fn? [tok]
  (and (= (get-in tok [1 0 3]) 'fn)
       (= (get-in tok [0 1]) "#(")
       (let [args-tok (get-in tok [1 1])]
         (and (vector? (args-tok 3))
              (let [args (args-tok 1) re #"%\d+"]
                (not (first (filter #(let [arg (str (% 3))]
                                       (not (or (= arg "%") (= arg "&") (= arg "%&")
                                                (re-matches re arg)))) args))))))))
(defn _contains-%fn? [tok] ; checks ourselves and recursivly, only valid cases.
  (or (_is-%fn? tok)
      (if (vector? (tok 1)) (first (filter _contains-%fn? (tok 1)))))) 

(defn _map-ns-repack [tok]
  (let [opening (get-in tok [0 1])
        ;_ (if (< (count opening) 3) (println "opening is: " (pr-str opening)))
        global-ns (subs opening 2 (dec (count opening)))
        project-1 (fn [tok]
                    (let [b (tok 1) ; note that tok has alread been projected.
                          kwd? (keyword? (tok 3))
                          kws (if kwd? ":" "")
                          pieces (string/split b #"/")
                          p0 (first pieces)
                          p0 (if kwd? (subs p0 1) p0)
                          p1 (apply str (rest pieces))
                          b1 (cond (= (count pieces) 1) (str kws "_/" p0)
                                   (= p0 global-ns) (str kws p1)
                                   :else (tok 1))]
                      [(tok 0) b1 (tok 2) (tok 3)]))
        body1 (mapv #(if (and (even? %2) (or (keyword? (%1 3)) (symbol? (%1 3))))
                       (project-1 %1) %1) (tok 1) (range))]
     [(tok 0) body1 (tok 2) (tok 3)]))

(defn _recursive-project-leaves [tok]
  "Project all leaf tokens it finds, non-leafs aren't projected, acts recursivly within tok."
  (if (vector? (tok 1)) (assoc tok 1 (mapv _recursive-project-leaves (tok 1)))
      (let [tok [(str (tok 0)) (tok 1) (str (tok 2)) (tok 3)]]
        [(inert-project (tok 0))
         (cond (= (pr-str (tok 3)) (tok 1)) (tok 1)
               (= (read?s-str (tok 1)) [(tok 3)]) (tok 1)
               :else (pr-str (tok 3)))
         (inert-project (tok 2))
         (tok 3)])))

(defn _recursive-project-colls [tok & not-outer?]
  "Projects the collection-level tokens, including reader macros and metadata. Run after the leaves since rmacros produce invalid leaf projections."
  (if (vector? (tok 1))
    (let [tok0 tok ; the pre-projected form may be useful one day.
          ;_ (println (pr-str "the token is:" tok))
          tok (update tok 1 #(mapv _recursive-project-colls % (repeat true))) ; depths first.
          val (tok 3) rmval (get-in tok [1 0 3])
          v3 #(cond (and (vector? %) (= (count %) 3)) %
                    (inert? (str %)) [(str %) "" ""] :else ["" (str %) ""]) ; consistant format.
          tok (update (update tok 0 v3) 2 v3)
          head (tok 0) tail (tok 2)

          opening (if (vector? head) (head 1) head)
          
          simple-rmacro? (get simple-sym-set rmval)
          is% (= opening "#(")
          reader-candidate? (or simple-rmacro? is%)
          
          sm (if simple-rmacro? opening)]
      (cond (not not-outer?) [(inert-project (apply str (tok 0))) (tok 1) (inert-project (apply str (tok 2))) []] ; the other level has no reader macros.
            (and (get simple-macros sm) (get simple-sym-set rmval)) ; simple reader macros.
            (let [t0 [(inert-project (head 0))
                      (if (and (or (= sm "`") (= sm "'")) (= rmval 'quote))
                        sm (get macros-simple rmval))
                      (inert-project (head 2))]]
              [t0 (assoc (tok 1) 0 ["" "" "" rmval]) ["" "" ""] ()])
            (and (_is-%fn? tok) (not (first (filter _contains-%fn? (tok 1))))) ; function short-hands require checking the deeper levels for nested functions.
            (do ;(println "the tok is:" (pr-str tok))
              [head (-> (tok 1)
                        (assoc 0 ["" "" "" 'fn])
                        (update 1 (fn [arg-tok]
                                    [["" "" ""] (mapv #(-> % (assoc 0 "") (assoc 1 "") (assoc 2 ""))
                                                      (arg-tok 1)) ["" "" ""] []])))
                 (tok 2) tail])
            (= rmval (symbol "META-TAG")) ; metas impose themesleves like it or not, unlike other reader macros which can be prevented from reversing.
            (let [t0 [(inert-project (head 0)) "^" (inert-project (head 2))]
                  tok1 (_meta-resugar tok0 tok)
                  mtt ["" "" "" (symbol "META-TAG")]]
              [t0 (assoc (tok1 1) 0 mtt) [(inert-project (str (first tail))) "" (inert-project (str (last tail)))] ()])
            ; Map namespace literals.
            (and (not= opening "#{") (= (first opening) \#) (= (last opening) \{))
            (_map-ns-repack tok)
            :else ; non-macro cases.
            (let [t0 [(inert-project (head 0))
                      (cond (map? val) "{" (vector? val) "[" (set? val) "#{" :else "(")
                      (inert-project (head 2))]
                  t2 [(inert-project (tail 0))
                      (cond (map? val) "}" (vector? val) "]" (set? val) "}" :else ")")
                      (inert-project (tail 2))]]
              [t0 (tok 1) t2 val])))
    tok))

(defn _head? [tok] ; is there space at the head, only after projection/format normalization is this fn good.
  (if (vector? (tok 0))
    (not= (get-in tok [0 0]) "")
    (not= (tok 0) "")))
(defn _tail? [tok]
  (if (vector? (tok 2))
    (not= (get-in tok [2 2]) "")
    (not= (tok 2) "")))
(defn _sticky? [tok1 tok2] ; Sticky and thus in need of a pad.
  (let [v1 (tok1 3) v2 (tok2 3)
        st1? (or (symbol? v1) (keyword? v1) (number? v1))
        st2? (or (symbol? v2) (keyword? v2) (number? v2))
        o1 (and (coll? v1) (get-in v1 [0 1]))
        o2 (and (coll? v2) (get-in v2 [0 1]))]
    (and (not= (tok1 1) "") (not= (tok2 1) "")
     (or (and st1? st2?)
         (and (instance? java.lang.Character v1) st2?)
         (and (or (symbol? v1) (keyword? v1))
              (or (= (first o2) "#") (= (first o2) "'")))
         (and (= o1 "~") (= o2 "@"))
         (and (= o1 "?#") (= o2 "@"))))))
(defn _pad1 [tok] ; adds a pad, only use if there is no pad.
  (if (vector? (tok 2))
    (assoc-in tok [2 2] " ")
    (assoc tok 2 " ")))
(defn _recursive-project-neighbors [tok]
  "Adds a space if stuff sticks together."
  (if (coll? (tok 1))
    (let [t1 (mapv _recursive-project-neighbors (tok 1))
          h?s (mapv _head? t1)
          t?s (mapv _tail? t1)
          pad-after? (conj (mapv #(and (not (t?s %)) (not (h?s (inc %)))
                                       (_sticky? (t1 %) (t1 (inc %))))
                                 (range (dec (count t1))))
                           false)]
      (assoc tok 1 (mapv #(if %2 (_pad1 %1) %1) t1 pad-after?)))
    tok))

;;;;;;;;;;;;;;;;;;; Non-pipeline parse fns ;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;; The forward pipeline ;;;;;;;;;;;;;;;;;;;;;;;

(defn grouped-tokenize [txt]
  "Creates an array of tokens, each of which is [head body tail value].
   For non-collections:
     The head and tail hold comments/whitespace/etc, the body is a string representation, and the value is the object.
   For collections:
     The head and tail are three-long vectors of strings, with the edge elements holding the whitespace.
       For macros this pattern is sometimes broken for the tail tag.
     The value is an empty collection of the same type of collection represented.
   Does not expand reader macros."
  (let [lt (leaf-tokenize txt)]
    (if (= (second lt) [0])
      [txt [] "" []] ; special empty read.
      ["" (apply group-tokens (leaf-tokenize txt)) "" []])))
  
(defn rmacro-apply [tok]
  "Applys reader macros and meta tags to the token groups.
   Reader macros are tokenized like collections, with the head and tail bieng triplets of strings.
   The meta-tags become [META-TAG foo metadata]."
  (let [body (tok 1) ; a vector to work with.
        body1 (loop [acc body ix (dec (count body))]
                (if (= ix -1) acc
                    (let [acc1 (rmacro-1 acc ix)]
                      (recur acc1 (dec ix))))) ; going backwards applies nested macros in the right order.
        body2 (mapv #(if (vector? (% 1)) (rmacro-apply %) %) body1) ; recursive.
        ] [(tok 0) body2 (tok 2) (tok 3)]))

(defn data-structure [tok]
  "Turns token groups into clojure data structures that should be close to read-string but with metadata storing the idioamancy.
    Slight differences from read-string:
      Read-string only takes the first form, we take all the forms and put them into a vector.
      Reader conditionals and read-eval aren't evaluated, instead we get i.e. (READ-CONDITIONAL foo).
      ` and :: don't qualify symbols.
      Metadata tags don't go into the metadata they become i.e. (META-TAG {:tag foo} bar).
        (META-TAG ... (META-TAG ...)) isn't always nested meta.
  The structural information is stored in mk:
    :txt = [head body tail] of this token, empty body for collections. Unavailable for non-metable stuff, use :children-txts
    :children-txts (for collections): The texts of each child excluding the stuff inside collections (no gran-children).
          Don't use on items that have a :txt already (items that can hold metadata).
        For lists and vectors (our only two sequentials): Map from integer to [head body tail ix].
        For sets: map from value (the clojure object, not the token) to [head body tail ix]
        For maps: map from key to [[headk bodyk tailk ix] [headv bodyv tailv ix]]."
  (let [val (tok 3)
        txt [(tok 0) (if (coll? val) "" (str (tok 1))) (tok 2)]]
    (if (and (= val {}) (odd? (count (tok 1))))
      (throw (Exception. "Map literal must contain an even number of forms.")))
    (if (coll? val) ; collections.
      (let [children (tok 1)
            processed-children (mapv data-structure children)
            ch-txtv (mapv #(vector (% 0) (if (vector? (% 1)) "" (str (% 1))) (% 2)) children)
            n (count ch-txtv) n2 (int (/ n 2))
            clm #(if (meta %) (with-meta % nil) %)
            children-txts (cond (sequential? val)
                                (zipmap (range n) (mapv #(conj %1 %2) ch-txtv (range)))
                                (map? val)
                                (zipmap (mapv #(clm (nth processed-children (* % 2))) (range n2))
                                        (mapv #(vector (conj (nth ch-txtv (* % 2)) %) (conj (nth ch-txtv (inc (* % 2))) %)) (range n2)))
                                :else ; sets.
                                (zipmap (mapv #(clm (nth processed-children %)) (range n))
                                        (mapv #(conj %1 %2) ch-txtv (range))))
            obj (cond (list? val)
                      (apply list processed-children)
                      (map? val)
                      (let [xi (apply hash-map processed-children)]
                        (if (< (count xi) (/ (count processed-children) 2))
                          (throw (java.lang.IllegalArgumentException.
                                   "Duplicate key(s) in map literal.")))
                        xi)
                      (set? val)
                      (let [xi (set processed-children)]
                        (if (< (count xi) (count processed-children))
                          (throw (java.lang.IllegalArgumentException.
                                   "Duplicate key(s) in set literal.")))
                        xi)
                      :else processed-children)]
        (with-meta obj {:txt txt :children-txts children-txts}))
      (cond (or (symbol? val) (coll? val)) (with-meta val {:txt txt}) ; this may shave a small amount of time off.
        (or (nil? val) (boolean? val) (keyword? val) (number? val) (string? val)
              (instance? java.util.regex.Pattern val) (instance? java.lang.Character val)) val
        :else (with-meta val {:txt txt})))))

(defn meta-pack [x]
  "Normally, our metadata goes into :PARSE, which is a one element long.
   However, for metadata we do a three element list -> one element contraction.
      We conj three parses to the :PARSE of the meta target.
        These three are the list itself, and the first two elements thereof (we remove the parse from the meta of these items).
        The first element of the taget :PARSE stores :mkeys, which tell us which key(s) were contracted."
  (let [x1 (if (coll? x) (collections/rmap meta-pack x) x) ; recursive.
        meta-list? (and (list? x1) (= (first x1) (symbol "META-TAG"))) ; a meta-list token.
        ]
    (cond (not (meta x)) (if (map? x) (with-meta x {:PARSE [{}]}) x) ; no meta, so no packing needs to be done.
      meta-list?
      (let [sym (first x1) add-to-meta (second x1) target (last x1)
            
            p0 [(assoc (meta x1) :mkeys (into [] (keys add-to-meta)))] ; x1 didn't yet get the :PARSE treatment.
            p1 (:PARSE (meta sym)) p2 (:PARSE (meta add-to-meta))
            old-meta (meta target)
            new-parse (conj (:PARSE old-meta) [p0 p1 p2])

            add-to-meta1 (with-meta add-to-meta nil)
            dupe-keys (set/intersection (set (keys (dissoc add-to-meta1 :PARSE)))
                                        (set (keys (dissoc old-meta :PARSE))))
            _ (if (not (empty? dupe-keys))
                (throw (IllegalArgumentException.
                        (str "Duplicate meta key(s): " dupe-keys))))
            new-meta (assoc (merge old-meta add-to-meta1) :PARSE new-parse)]
        (with-meta target new-meta))
       ; Make a 1-element :PARSE vector to store our idiomatic meta:
      :else (vary-meta x1 #(hash-map :PARSE [%])))))

(defn forwards [x]
  (-> x grouped-tokenize rmacro-apply data-structure meta-pack))

;;;;;;;;;;;;;;;;;;; The reverse pipeline ;;;;;;;;;;;;;;;;;;;;;;;

   ;:txt = [head body tail ix] of this token, empty body for collections. Unavailable for non-metable stuff, use :children-txts
   ;:children-txts (for collections): The texts of each child excluding the stuff inside collections (no gran-children).

(defn meta-unpack [x]
  "Undoes meta-pack, with defaults when data is missing.
   Clojure doesn't seem to read-string properly with nested metadata."
  (let [mx (meta x) px (:PARSE mx) needs-unpack (dissoc mx :PARSE)
        sym (symbol "META-TAG")
        mtag? (> (count needs-unpack) 0)
        
        x1 (cond (or (not mx) (= mx {})) x ; can't do much here, this catches not px with mtag false.
                 (not px) (list sym mx (with-meta x nil)) ; mtag? must be true if we get here.
                 mtag? ; we have meta, need to make a meta tag.
                 ; Use the :mkeys if possible, otherwise dump everything at once.
                 (let [kwants-unpack (apply hash-set (:mkeys (first (first (last px)))))
                       kneeds-unpack (apply hash-set (keys needs-unpack))
                       subset? (= (count (set/difference kwants-unpack kneeds-unpack)) 0)]
                   (if (and subset? (> (count kwants-unpack) 0)) ; partial pack.
                     (let [pulled-out (zipmap kwants-unpack (mapv #(get mx %) kwants-unpack)) ; individual keys may have their own metadata.
                           pxl (last px)
                           pulled-out (with-meta pulled-out {:PARSE (last pxl)})
                           sym (with-meta sym {:PARSE (second pxl)})
                           
                           px1 (into [] (butlast px)) ; The last element of x1 stores the parse.
                           kremaining (set/difference kneeds-unpack kwants-unpack)
                           remaining (zipmap kremaining (mapv #(get mx %) kremaining))  ; what remains in x's metadata.
                           pxl00 (first (first pxl))]
                       (with-meta
                         (list sym pulled-out (with-meta x (assoc remaining :PARSE px1)))
                         (if (map? pxl00) pxl00 {})))
                      ; Full pack, naive pack.
                      (list sym needs-unpack (with-meta x nil))))
                 ; No meta, just unpack the parse, which should only have one element, and remove any :mkeys that may be there:
                 :else (with-meta x (dissoc (if (map? (first px)) (first px) {}) :mkeys)))]
    (if (coll? x1) (collections/rmap meta-unpack x1) x1)))

(defn naive-unstructure [x & txt-from-parent]
  "Converts the data structure back into token groups, using whatever text it can find or printing txt.
   This may create conflicts that need to be resolved with projections."
  (let [txt-from-parent (first txt-from-parent)
        m (meta x) txt-ch (:children-txts m)
        txt (cond (:txt m) (:txt m)
                  txt-from-parent txt-from-parent
                  (map? x) ["{" "" "}"]
                  (set? x) ["#{" "" "}"]
                  (vector? x) ["[" "" "]"]
                  (coll? x) ["(" "" ")"]
                  :else ["" "" " "])
        clm #(if (meta %) (with-meta % nil) %)
        ch-txts (:children-txts m)
        sort-k #(let [v (get ch-txts %)]
                  (if (vector? (last v)) (get-in v [0 3])
                      (if-let [v3 (get v 3)] v3 0)))
        body (cond (map? x)
                   (let [kys (into [] (sort-by sort-k (keys x))) ; sort by original order assuming we have that data.
                         vls (mapv #(get x %) kys) ; cant use vals as kys are sorted.
                         kys1 (mapv #(naive-unstructure % (first (get ch-txts %))) kys)
                         vls1 (mapv #(naive-unstructure %2 (second (get ch-txts %1))) kys vls)]
                     (into [] (apply concat (mapv vector kys1 vls1))))
                   (set? x)
                   (let [kys (into [] (sort-by sort-k x))
                         kys (mapv #(naive-unstructure % (get ch-txts %)) kys)] kys)
                   (coll? x)
                   (mapv #(naive-unstructure %1 (get ch-txts %2)) x (range))
                   :else (if (txt 1) (txt 1) (pr-str x)))
        obj (cond (map? x) {} (set? x) #{} (vector? x) [] (coll? x) () :else x)]
    [(txt 0) (clm body) (txt 2) (clm obj)]))

(defn projection [tok]
  "The projection step that ensures we update the string values of tok to reflect the new meaning."
  (-> tok (_recursive-project-leaves) (_recursive-project-colls) (_recursive-project-neighbors)))

(defn blit [tok]
  "Turns the tok back into a string."
  (str (if (vector? (tok 0)) (apply str (tok 0)) (tok 0))
       (if (vector? (tok 1)) (apply str (mapv blit (tok 1))) (tok 1))
       (if (vector? (tok 2)) (apply str (tok 2)) (tok 2))))

(defn backwards [x]
  (-> x meta-unpack naive-unstructure projection blit))


;;;;;;;;;;;;;;;;;;; Non-pipeline parse fns ;;;;;;;;;;;;;;;;;;;;;;;

(defn reads-string [s] (read-string (str "[" s "\n]")))

(defn depth [s]
  "A fast parser used for indent hiliting et al."
  (into [] ^ints (depth-ints (str s))))

(defn locate-in [x path x-needs-projection?]
  "Returns the [start end] locations on the original string of the path within x.
   O(n) in the original string, use the break-down function (TODO).
   x-needs-projection? use this whenever x has been modified. False if x is parsed from a .clj file.
   Does not include meta-tags."
  (let [x (if x-needs-projection? (-> x backwards forwards) x)
        unmarked (get-in x path)
        ; Leaf-level change so collections don't get intefered with:
        tmp (loop [ph []]
                 (let [chi (collections/cget-in unmarked ph)]
                     (if (coll? chi)
                       (recur (conj ph (first (collections/ckeys chi))))
                       [(collections/cassoc-in unmarked ph
                                               (with-meta (gensym "LOCATEBEACON") (meta chi)))
                        ph])))
        marked (first tmp)
        submarked-path (second tmp)
        submarked (collections/cget-in marked submarked-path)
        in-set? (and (> (count path) 0) (set? (collections/cget-in x (butlast path))))
        x1 (if in-set? ; in sets you can't reassign a key/index to a new value, must change the value. 
             (collections/cupdate-in x (butlast path)
                                     (fn [st]
                                       (let [v (last path)
                                             v1 (with-meta marked (meta (get st v)))
                                             st1 (conj (disj st v) v1)
                                             ; Also change the :children texts:
                                             ch-txts (get-in (meta st) [:PARSE 0 :children-txts])
                                             ch-txts1 (-> ch-txts (dissoc v) (assoc v1 (get ch-txts v)))]
                                         (vary-meta st1 #(assoc-in % [:PARSE 0 :children-txts] ch-txts1)))))
             (collections/cassoc-in x path (with-meta marked (meta (collections/cget-in x path)))))
        ; naive tokenize and see where the marker goes:
        tok1 (-> x1 meta-unpack naive-unstructure)
        subtpath (into [] (butlast (collections/find-value-in tok1 submarked)))
        ;_ (if (not subtpath) (throw (Exception. "Token not found.")))
        tpath (subvec subtpath 0 (- (count subtpath) (* (count submarked-path) 2)))
        tok-b4 (reduce
                (fn [acc pix]
                  (let [ph (subvec tpath 0 pix)
                        n-keep (nth tpath pix)
                        ph2tok? (even? (count ph)) ; ph takes us to a token, not to a vector of tokens.

                        target0 (get-in tok1 ph)
                        target (subvec target0 0 n-keep)
                        target (if (and ph2tok? (< (count target) 3))
                                 [(target 0) [] ["" "" ""] (target0 3)] ; remove closing half of token.
                                 target)]
                    (collections/cassoc-in acc ph target)))
                tok1 (range (count tpath)))
        ; Don't include leading or trailing spaces in the target tok:
        target (get-in tok1 tpath)
        n-lead (count (if (vector? (target 0)) (get-in target [0 0]) (target 0)))
        n-follow (count (if (vector? (target 2)) (get-in target [2 0]) (target 2)))
        
        nb4 (count (blit tok-b4))
        nin (count (blit (get-in tok1 tpath)))]
    [(+ nb4 n-lead) (+ nb4 nin (- n-follow))]))

(defn path-at [x char-ix x-needs-projection?]
 "The path to the form that the char at char-ix belongs to.
  whitespace lands us in the enclosing form.
  Map literals map both the key and value to the key, and do not dig into collection-valued keys.
  O(n). char-ix is clamped."
  (let [tok (-> x meta-unpack naive-unstructure)
        tok (if x-needs-projection? (projection tok) tok)
        char-ix (max char-ix 0)
        l2f #(into [] (conj (interpose 1 %) 1))
        _two #(subvec % 0 (- (count %) 2))
        stop (atom 0)
        tok2cursor (loop [ph [] n-b4 0 coll-tail? false]
                     (let [t (get-in tok ph)
                           n-head (count (if (vector? (t 0)) (apply str (t 0)) (t 0)))
                           n-headspace (if (vector? (t 0)) (count (first (t 0))) n-head)
                           n-tail (count (if (vector? (t 2)) (apply str (t 2)) (t 2)))
                           v? (and (vector? (t 1)) (> (count (t 1)) 0))]
                       (swap! stop inc)
                       (if (> @stop 10000) (throw (Exception. "Probable infinite loop.")))
                       (cond (and (not coll-tail?) (> (+ n-b4 n-head) char-ix))
                             (if (> (+ n-b4 n-headspace) char-ix) (_two ph) ph) ; hit something.
                             (and v? (not coll-tail?))
                             (recur (conj ph 1 0) (+ n-b4 n-head) false) ; jump in.
                             (and (= ph []) coll-tail?) [] ; overflow.
                             :else
                             (let [last-ix (last ph)
                                   max-last (dec (count (get-in tok (butlast ph))))
                                   n-body (if v? 0 (count (t 1)))
                                   n1 (+ n-b4 n-body (if coll-tail? n-tail (if v? n-head (+ n-head n-tail))))
                                   n-tailspace (if (vector? (t 2)) (count (last (t 2))) n-tail)]
                               (if (> n1 char-ix)
                                 (if (<= (- n1 n-tailspace) char-ix) (_two ph) ph) ; hit stuff
                                 (if (= last-ix max-last)
                                   (recur (subvec ph 0 (- (count ph) 2)) n1 true) ; overflow
                                   (recur (update ph (dec (count ph)) inc) n1 false))))))) ; next element
        vpath (into [] (collections/odds tok2cursor)) ; if everything was sequential this would be the answer.
        tok2x #(if (coll? (% 3)) (-> % data-structure meta-pack) (% 3))
        out (loop [ph [] toki tok ix 0]
              (if (= ix (count vpath)) ph
                  (let [px (nth vpath ix)
                        tok1 (get-in toki [1 px])
                        val (toki 3)
                        px1 (cond (map? val) (tok2x (nth (toki 1) (* (int (/ px 2)) 2))) ; kv pairs.
                                  (set? val) (tok2x (nth (toki 1) px))
                                  :else px)]
                    (if (and (map? val) (even? px)) (conj ph px1) ; don't dig into map keys; compat with collections/cget-in
                        (recur (conj ph px1) tok1 (inc ix))))))]
    out))

;;;;;;;;;;;;;;;;;;;; TESTING ;;;;;;;;;;;;;;;;;;

(require '[javac.file :as jfile])

(defn p0 [s] (grouped-tokenize s))
(defn p1 [t] (rmacro-apply t))
(defn p2 [t] (data-structure t))
(defn p3 [t] (meta-pack t))
(defn p01 [s] (p1 (p0 s)))
(defn p12 [s] (p2 (p1 s)))
(defn p23 [s] (p2 (p2 s)))
(defn p012 [s] (p2 (p1 (p0 s))))
(defn p123 [s] (p3 (p2 (p1 s))))
(defn p0123 [s] (p3 (p2 (p1 (p0 s)))))

(defn u3 [x] (meta-unpack x))
(defn u2 [x] (naive-unstructure x))
(defn u1 [t] (projection t)) ; undoes rmacros.
(defn u0 [t] (blit t))
(defn u10 [s] (u0 (u1 s)))
(defn u21 [s] (u1 (u2 s)))
(defn u32 [s] (u2 (u3 s)))
(defn u210 [s] (u0 (u1 (u2 s))))
(defn u321 [s] (u1 (u2 (u3 s))))
(defn u3210 [s] (u0 (u1 (u2 (u3 s)))))

(defn u1_2 [t] (_recursive-project-leaves t))
(defn u1_1 [t] (_recursive-project-colls t))
(defn u1_0 [t] (_recursive-project-neighbors t))

(defn mpr [x] (binding [*print-meta* true] (pr-str x)))

(require '[clojure.pprint :as pprint]
         '[app.stringdiff :as stringdiff])

(defn ppr [x]
  (str (with-out-str (pprint/pprint x))))

#_(let [s (jfile/open "./src/app/rtext.clj")
        s1 (time ((comp u3210 p0123) s))
        eds (stringdiff/edits-between s s1)
        e0 (last eds)
        k 10
        ix0 (if e0 (:ix0 e0) k)]
    (if e0 [(count eds) (subs s (- ix0 k) (+ ix0 k))
            (subs s1 (- ix0 k) (+ ix0 k))]
      (println "test file reversable: " (= s s1))))

#_(let [s "^foo bar"
        x (forwards s)
        ixs (locate-in x [0] false)]
    [ixs (apply subs s ixs)])

#_(let [x (forwards "a [1,,,],,, b c")
        ph (path-at x 14 false)] ph)
