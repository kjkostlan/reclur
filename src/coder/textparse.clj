; Functions that parse strings and care about the spacing or other particular characters.
; Examples:
 ; Indent level between each character.
 ; What path would bring us to the 47'th character in the string (and visa-versia)?
 ; Is a symbol qualed?

(ns coder.textparse
  (:require
    [c] [t] [np]
    [coder.javar :as javar]
    [clojure.string :as string]))

;;;;;;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn string-indexes-of [txt key]
  "Does not work for regexp. TODO: did we write another version of this fn?"
  (loop [acc [] char-ix 0]
    (let [ix-next (string/index-of txt key char-ix)]
      (if ix-next (recur (conj acc ix-next) (inc ix-next)) acc))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Functions that give us information about where a cursor is ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tokenize-vints [txt tok-ints-fn]
  "Like tokenize ints, [start end type], but a native clojure data structure that is easier to work with."
  (let [x (tok-ints-fn txt)
        ^ints st (nth x 0) ^ints en (nth x 1) ^ints ty (nth x 2)] ; token char ix start. Token char ix end. Token type as the 8 types defined in langs.
    [(into [] st) (into [] en) (into [] ty)]))

(defn _prevnext-ix1 [txt ix dir targetty st en ty return-tok-ix?]
  (let [n (count txt) ix (min n (max 0 ix))
        tok-ix (if (< dir 0) (last (filter #(and (= (nth ty %) targetty) (<= (nth en %) ix)) (range (count st))))
                 (first (filter #(and (= (nth ty %) targetty) (> (nth en %) ix)) (range (count st)))))]
    (if return-tok-ix?
      (if tok-ix tok-ix (if (< dir 0) 0 (dec (count ty))))
      (if tok-ix (nth (if (< dir 0) st en) tok-ix)
        (if (< dir 0) 0 n)))))

(defn _prevnext-ix [txt ix tok-ints-fn dir targetty]
  (let [x (tokenize-vints ix tok-ints-fn)
        st (nth x 0) en (nth x 1) ty (nth x 2)]
    (_prevnext-ix1 txt ix dir targetty st en ty false)))

(defn prev-open-ix [txt ix tok-ints-fn]
  "Just before the open parentheses, or at the beginning of the string."
  (_prevnext-ix txt ix tok-ints-fn -1 4))

(defn prev-open-ix [txt ix tok-ints-fn]
  "Just before the closed parentheses, or at the beginning of the string."
  (_prevnext-ix txt ix tok-ints-fn -1 5))

(defn prev-open-ix [txt ix tok-ints-fn]
  "Just after the open parentheses, or at the end of the string."
  (_prevnext-ix txt ix tok-ints-fn 1 4))

(defn next-close-ix [txt ix tok-ints-fn]
  "Just after the closed parentheses, or at the end of the string."
  (_prevnext-ix txt ix tok-ints-fn 1 5))

(defn enclosing-ixs [txt ix tok-ints-fn]
  "Enclosing () or eq, gives at the end(s) of the string when it fails."
  [(prev-open-ix txt ix tok-ints-fn) (next-close-ix txt ix tok-ints-fn)])

(defn tok12345-at-cursor [txt ix st en ty]
  "The nearest material token at or enveloping the cursor, with flexible definition of what this exactly is.
   Returns: 5 token ixs [very-b4 b4 in afr very-afr], false in the cases where there is no such token."
  (let [ix (min (count txt) (max 0 ix))
        test-set #{1 2 3 4 5}
        tix-lo (apply max (mapv #(_prevnext-ix1 txt ix -1 % st en ty true) test-set))
        tix-hi (apply min (mapv #(_prevnext-ix1 txt ix 1 % st en ty true) test-set))
        ix-lo (nth en tix-lo)
        ix-hi (nth st tix-hi)

        tok-filt (fn [f] (if-let [out (first (filter #(and (f (nth st %) (nth en %))
                                                        (contains? test-set (nth ty %))) (range (count st))))]
                           out false))
        tokix-2 (tok-filt (fn [a b] (= b ix-lo)))
        tokix-1 (tok-filt (fn [a b] (= b ix)))
        tok-ix0 (tok-filt (fn [a b] (and (< a ix) (> b ix))))
        tok-ix1 (tok-filt (fn [a b] (= a ix)))
        tok-ix2 (tok-filt (fn [a b] (= a ix-hi)))]
    [tokix-2 tokix-1 tok-ix0 tok-ix1 tok-ix2]))

(defn choice-convention [tys t-2 t-1 t0 t1 t2]
  "It's somewhat arbitrary where the cursor actually goes."
  (cond t0 t0
    (and t1 (= (nth tys t1) 4)) t1 ; () pull from outside.
    (and t-1 (= (nth tys t-1) 5) ) t-1
    (and t-1 t1 (= (nth tys t1) 5)) t-1 ; () push from inside.
    (and t1 t-1 (= (nth tys t-1) 4)) t1
    t1 t1 t-1 t-1 t2 t2 t-2 t-2))

(defn _jump+ [st en ty tok-ix]
  "Jumps to the last token that concludes the form. Skips over any initial whitespace or other non-material things.
   Use the end of this token. It may return tok-ix"
  (let [n (count st)
        tok-ix0 (loop [ix tok-ix]
                  (if (= ix (dec n)) ix
                    (let [tyi (nth ty ix)]
                      (if (or (= tyi 0) (= tyi 8) (= tyi 6) (= tyi 7)) (recur (inc ix))
                        ix))))
        ty0 (nth ty tok-ix0)]
    (if (= ty0 4)
      (loop [ix (inc tok-ix0) lev 1]
        (if (>= ix (dec n)) (dec n)
          (let [tyi (nth ty ix)]
            (if (and (<= lev 1) (= tyi 5)) ix
              (recur (inc ix)
                (cond (= tyi 4) (inc lev)
                  (= tyi 5) (dec lev)
                  :else lev))))))
      tok-ix0)))

(defn _trimmed-all-with-path-tokixs [st en ty tok-ix]
   "Token ixs, use st and en to get char ixs inclusive."
  (let [tyi (nth ty tok-ix) n (count st)]
    (cond (and (= tyi 8) (< tok-ix (dec n)))
      [st (let [tok-end-of-meta-form (second (_trimmed-all-with-path-tokixs st en ty (inc tok-ix)))]
            (second (_trimmed-all-with-path-tokixs st en ty (inc tok-end-of-meta-form))))]
      (not= tyi 5) [(loop [ix tok-ix]
                      (if (= ix (dec n)) (dec n)
                        (let [tyi (nth ty ix)]
                          (if (or (= tyi 0) (= tyi 6)) (recur (inc ix)) ix))))
                    (_jump+ st en ty tok-ix)]
      :else [(loop [ix (dec tok-ix) lev 1]
               (if (<= ix 0) 0
                 (let [tyi (nth ty ix)]
                   (if (and (= tyi 4) (<= lev 1)) ix
                     (recur (dec ix) (cond (= tyi 4) (dec lev) (= tyi 5) (inc lev) :else lev))))))
             tok-ix])))

(defn trimmed-all-with-path [st en ty tok-ix]
   "All char ixs that go to the path tok-ix goes to, or deeper. () tokens count as inside.
    It is trimmed in that there are no space or punctuation tokens at the ends included."
  (let [tok-ixs (_trimmed-all-with-path-tokixs st en ty tok-ix)]
    [(nth st (first tok-ixs)) (nth en (second tok-ixs))]))

(defn string-to-wpath [txt ix tok-ints-fn reads-string-fn]
  (let [n (count txt) ix (max 0 (min ix n)) x (tokenize-vints txt tok-ints-fn)
        st (first x) en (second x) ty (last x)
        tixs (tok12345-at-cursor txt ix st en ty)
        tok-ix (apply choice-convention ty tixs)
        char-ix01 (trimmed-all-with-path st en ty tok-ix)
        c0 (first char-ix01) c1 (second char-ix01)

        ; Alphanumeric symbols and spaces are pretty universal:
        sym (gensym "IAmUnique")
        txt1 (str (subs txt 0 c0) " " sym " " (subs txt c1))
        x1 (reads-string-fn txt1)
        path (t/find-value-in x1 sym true)
        path (if path path
               (t/find-deepest-value-in
                 x1 #(string/includes? (pr-str %) (str sym)) sym true))] ; Slower but more robust.
    (if (not path) (throw (Exception. (str "String-to-wpath not working: ..." (subs txt1 (max 0 (- ix 20)) (min (+ ix 20) (count txt1))) "..."))))
    path))

(defn line-to-string-ixs [txt linenum search-key]
  "Used for stack tracing. Can be confused once in a while, but will still return the correct linenum."
  (let [lines (string/split txt #"\n")
        line-counts (mapv #(inc (count %)) lines)
        nchars-b4-line (conj (into [] (reductions + 0 line-counts)) (count txt))
        ix-in-line (string/index-of txt (str search-key))
        c0 (get nchars-b4-line (dec linenum))
        jux-of (if c0 (string/index-of (nth lines (dec linenum)) (str search-key)))
        jx-of (if jux-of (+ c0 jux-of))]
    (if (and search-key jx-of) [jx-of (+ jx-of (count (str search-key)))]
      [c0 (nth nchars-b4-line linenum)])))

(defn wpath-to-string-ixs [txt wpath tok-ints-fn reads-string-fn]
  "The string-ixs within text given wrapped path wpath.
   The w stands for wrapped.
   TODO: This function can be expensive O(n^2) if there is a long string and lots of look-alikes."
  (let [wpath (into [] wpath) x (reads-string-fn txt) n (count txt)
        target (t/cget-in x wpath)]
    (if (and (coll? target) (> (count target) 0))
      (let [; Pick the key to the rarest element:
            vs (into [] (c/cvals target))
            counts (mapv count (mapv #(t/find-values-in x %) vs))
            ky (nth (into [] (c/ckeys target)) (np/argmin counts))
            ixs (wpath-to-string-ixs txt (conj wpath ky) tok-ints-fn reads-string-fn)
            ix (int (Math/round (+ (* 0.5 (first ixs)) (* 0.5 (second ixs)))))]
        (enclosing-ixs txt ix tok-ints-fn))
      (let [search-str-key (pr-str target) ; str on a leaf is fairly universal between languages.
            _ (if (= (count search-str-key) 0) (throw (Exception. "Empty search target")))
            str-ixs (loop [acc [] ix 0]
                      (let [ix1 (.indexOf ^String txt ^String search-str-key (inc ix))]
                        (if (and (> ix1 -1) (<= ix1 ix)) (throw (Exception. "Bad str-ixs loop.")))
                        (if (> ix1 -1) (recur (conj acc ix1) (inc ix1)) acc)))
            ; Check the path at each ix:
            wpaths (mapv #(string-to-wpath txt (inc %) tok-ints-fn reads-string-fn) str-ixs)
            kx (first (filter #(= (nth wpaths %) wpath) (range (count wpaths))))
            _ (if (not kx) (throw (Exception. (str "Cant find this path: " wpath))))

            ; From string index to token:
            x (tokenize-vints txt tok-ints-fn) st (first x) en (second x) ty (last x)
            tokixs (tok12345-at-cursor txt (nth str-ixs kx) st en ty)
            tok-ix (apply choice-convention ty tokixs)]
        [(nth st tok-ix) (nth en tok-ix)]))))

(defn x-at-string [txt ix tokenize-ints-fn reads-string-fn]
  "What ever is at the given string.
   The rules at the edges are arbitrary.
   Tries to work even if the syntax of txt is broken."
  (let [x (tokenize-vints txt tokenize-ints-fn)
        st (first x) en (second x) ty (last x)
        tixs (tok12345-at-cursor txt ix st en ty)
        tok-ix (apply choice-convention ty tixs)
        char-ixs (trimmed-all-with-path st en ty tok-ix)
        hot-str (subs txt (first char-ixs) (second char-ixs))]
    (first (reads-string-fn hot-str))))

(defn tokenize-from-ints [txt x]
  "Two vectors, [the strings themselves, the type]. Also lumps consecutave tokens as 1."
  (let [^ints st (first x) ^ints en (second x) ^ints ty (nth x 2)]
    (javar/token-native2vector txt st en ty)))

(defn token-stream-from-ints [^ints st ^ints en ^ints ty]
  "Returns a single clojure vector of ty."
  (let [n (count st)]
    (loop [acc [] ix 0]
      (if (= ix n) acc
        (let [k (- (aget en ix) (aget st ix))
              tyi (aget ty ix)]
          (recur (reduce conj acc (repeat k tyi)) (inc ix)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Parsing symbols as strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn qual [ns-sym code-sym]
  "If it already is quald it will double-qual"
  (symbol (str ns-sym "/" code-sym)))

(defn qual? [sym] (or (= sym 'clojure.core//) (> (count (string/split (str sym) #"/")) 1)))

(defn unqual [sym-qual]
  "The part after the /"
  (cond (not (qual? sym-qual)) sym-qual (= sym-qual 'clojure.core//) '/
   :else (symbol (last (string/split (str sym-qual) #"\/")))))

(defn rm-lang [sym]
  "Removes the ! part at the start of sym (which can be a qualed or unqualled sym), if any is present."
  (let [pieces (string/split (str sym) #"\.")
        p0 (first pieces) lang? (= (first p0) \!)]
    (apply str (interpose "." (if lang? (rest pieces) pieces)))))

(defn sym2ns [qual-sym]
  "Returns the namespace/class (as a symbol) from a qualified symbol"
  (cond (or (= qual-sym '/) (= qual-sym "/")) 'clojure.core
    (.contains ^String (str qual-sym) "/")
    (symbol (first (string/split (str qual-sym) #"\/")))))