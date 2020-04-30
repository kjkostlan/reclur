; TODO: this relies heavily on clojure-based syntax. It should be refactored to use langs.

; Lisps love nesting. Lets make it well-indented!
; This isn't designed to be bulletproof in terms of printing and reading; possible edge-cases; it is mostly for human consumption.
(ns layout.blit
  (:require [coder.crosslang.langs :as langs] [coder.cbase :as cbase]
    [clojure.string :as string]
    [collections]
    [clojure.walk :as walk]
    [clojure.pprint :as pprint]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Basic functions ;;;;;;;;;;;;;;;;;
 
(defn _intsum [^ints a]
  (let [n (count a)]
    (loop [ix (int 0) tot (int 0)]
      (if (= ix n) tot
        (recur (inc ix) (+ tot (aget a ix)))))))

(defn _acopy-int! [^ints fr ^ints to]
  (let [n (count fr)]
    (loop [ix (int 0)]
      (if (< ix n) 
        (do (aset-int to ix (aget fr ix))
          (recur (inc ix)))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Cleaning up clojure's messy gensym code ;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *allow-structural-unmacro* false) ; you need to change the paths in error reporting if you do this!
(def ^:dynamic *print-score?* false)

(defn leaf-unmacro [code]
  "Only partial, more examples to be added soon."
  (cond (symbol? code)
    (let [sym-replacers {'let* 'let 'fn* 'fn 'def* 'def}]
      (get sym-replacers code code))
    (and (collections/listy? code) (= (first code) 'fn*)
      (vector? (second code))
      (re-matches #"p\d*\_+\d+\#+" (str (first (second code))))) ; the annoying % stuff.
    (let [arg-list (second code)
          replace-map (if (= (count arg-list) 1) {(first arg-list) '%}
                        (zipmap arg-list (map #(symbol (str "%" (inc %))) (range))))]
      (walk/postwalk #(if (symbol? %) (get replace-map % %) %) code))
    :else code))

(defn somewhat-unmacro [code]
  "Many macros become very long-winded. Do this before indentation.
   Of course, no need to do this if given a string and don't read it as code."
  (if *allow-structural-unmacro*
    (throw (Exception. "Structural unmacroing not implemented TODO. Note we need a function that changes paths as well!")))
  (walk/prewalk leaf-unmacro code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Code to decide where we should indent and blitting to a nice format ;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lean-space-tok [tok]
  (let [com? (string/includes? tok ";")
        trimmed (string/trim tok)
        not-mt (if (= (count trimmed) 0) " " trimmed)]
    (str not-mt (if com? "\n" ""))))

(defn lean-tokenize [code-or-str]
  "Lean tokenization of the code that removes extra whitespace, etc but keeps comments etc.
   There is a trailing newline for (blanck) tokens that are comments.
   Leaves a single space for space tokens so thingsdontjamtogether."
  (let [s (if (string? code-or-str) code-or-str
            (binding [*print-meta* true]
              (pr-str code-or-str)))
        vt (cbase/tokenize s :clojure)
        strings (first vt)
        types (second vt) ; 0 = empty, 4=opening, 5=closing.
        lean-strings (mapv #(if (= %2 0) (lean-space-tok %1) %1) strings types)
        ] [lean-strings types]))

(defn _binding-scan [tok-strs tok-types ix-opens-binding-vector allowed-newlines] ; no nesting will be considered.
  (let [n (count tok-strs)]
    (loop [ix (inc ix-opens-binding-vector) lev 1 sx 0 allowed allowed-newlines]
      (if (or (= ix n) (= lev 0)) allowed
        (let [ty (nth tok-types ix)
              allowed1 (if (and (= ty 0)
                               (= lev 1)
                               (odd? sx)) 
                         (disj allowed ix) allowed)]
          (recur (inc ix)
            (cond (= ty 4) (inc lev) (= ty 5) (dec lev) :else lev)
            (if (and (not= ty 5) (not= ty 0) (= lev 1)) (inc sx) sx) 
            allowed1))))))
(defn _binding-blocknl [tok-strs tok-types allowed-newlines]
  "Forces newlines before even elements in binding vectors (apart from 0).
   This makes the code easier to read."
  (let [tok-strs (into [] tok-strs) tok-types (into [] tok-types)
        n (count tok-strs) stuff #{"let" "let*" "loop" "loop*" "binding" "binding*"}]
    (loop [ix 0 fresh? false allowed (set allowed-newlines)]
      (if (= ix n) (into [] (sort (into [] allowed)))
        (let [ty (nth tok-types ix)
              txt (nth tok-strs ix)
              match? (boolean (get stuff txt))
              fresh?1 (cond (or (= ty 4) (= ty 5)) 0
                        (get stuff txt) 1
                        :else fresh?)
              allowed1 (if (and fresh? (= ty 4) (string/includes? txt "["))
                         (_binding-scan tok-strs tok-types ix allowed) allowed)]
          (recur (inc ix) fresh?1 allowed1))))))

(defn _get-allowed-newlines! [tok-strs token-types allow-nl?s] "Modifies allow-nl?s"
  (let [n (count token-types)
        allowed-ixs
        (loop [ix (int 0) allowed-ixs []]
            (if (= ix n) allowed-ixs
              (if (or (= ix (dec n)) 
                    (let [ty (aget token-types ix)
                          ty1 (aget token-types (inc ix))]
                      (and (not= ty1 0) (not= ty1 5))))
                (do (aset-int allow-nl?s ix 1)
                  (recur (inc ix) (conj allowed-ixs ix)))
                (recur (inc ix) allowed-ixs))))
        allowed-ixs (_binding-blocknl tok-strs token-types allowed-ixs)
        allowed-ixs (if (= (count allowed-ixs) 0) (range n) allowed-ixs)]
    (into-array Integer/TYPE allowed-ixs)))

(defn setup-arrays [tok-strs types]
  (let [n (count tok-strs) 
        ^ints delta-syntaxs (make-array Integer/TYPE n)
        levmx (loop [ix (int 0) lev (int 0) max-lev (int 0)] 
                (if (< ix n) 
                  (let [ty (nth types ix) max1 (if (> lev max-lev) lev max-lev)]
                    (if (= ty 4) (do (aset-int delta-syntaxs ix 1) 
                                   (recur (inc ix) (inc lev) max1))
                      (if (= ty 5)
                        (do (aset-int delta-syntaxs ix -1)
                          (recur (inc ix) (dec lev) max1))
                        (recur (inc ix) lev max-lev))))
                  max-lev))
        ^ints token-lengths (make-array Integer/TYPE n)
        _ (loop [ix (int 0)]
            (if (< ix n)
              (let [tok (nth tok-strs ix)]
                (aset-int token-lengths ix (count tok)) (recur (inc ix)))))
        ^ints force-newline?s (make-array Integer/TYPE n)
        _ (loop [ix (int 0)]
            (if (< ix n)
              (let [tok (nth tok-strs ix)]
                (aset-int force-newline?s ix 
                  (if (string/includes? tok "\n") 1 0)) 
                (recur (inc ix)))))]    
    [delta-syntaxs token-lengths force-newline?s (make-array Integer/TYPE (+ levmx 2))]))

(def _cost-calculate-blit-code ; take away one type hint and hundreds of times slower!
  '[[^ints add-newline?s
    ^ints force-newline?s ; newlines added at end
    ^ints delta-syntaxs ^ints token-lengths ^ints indent-amounts
    spaces-per-indent tokens]
      (let [n (int (count token-lengths)) k (int (count indent-amounts))
            spaces-per-indent (int spaces-per-indent)]
        (loop [ix (int 0)] ; reset.
          (if (< ix k) (do (aset-int indent-amounts ix (int 0)) (inc ix))))
    
        (loop [ix (int 0) cost (int 0) lev (int 0) x (int 0) ^:blit strs ^:blit []]
          (if (= ix n) [cost ^:blit (apply str strs)]
            (let [add-nl? (aget add-newline?s ix)
                  force-nl? (aget force-newline?s ix)
                  new-line-at-end? (+ add-nl? force-nl?)
                  del (aget delta-syntaxs ix)
                  lev1 (+ lev del)
                  toklen (aget token-lengths ix)
                  _ (if (= del (int 1)) (aset-int indent-amounts lev1
                                          (+ spaces-per-indent x)))
                  x1 (if (> new-line-at-end? 0) (aget indent-amounts lev1) (+ x toklen))
                  ^:blit strs ^:blit (conj strs (nth tokens ix) (if (> add-nl? 0) (str "\n" (apply str (repeat x1 " "))) ""))]
              (recur (inc ix)
                (+ cost
                  (if (or (> new-line-at-end? 0)
                        (= ix (dec n))) (* x x)
                    (int 0)))
                lev1 x1 
                ^:blit strs)))))])

(def _cost-calculate
  (let [cd _cost-calculate-blit-code
        wf (fn [x] (if (sequential? x) 
                     (collections/cfilter #(not (:blit (meta %))) x) x))]
    (eval (list 'fn (first cd) 
            (walk/prewalk wf (second cd))))))

(def _cost-calculate-with-blit
  (let [cd _cost-calculate-blit-code]
    (eval (list 'fn (first cd) (second cd)))))

(def ^:dynamic *width-target* 32.0)
(def ^:dynamic *dudcoverthreshold* 2.0)
(def ^:dynamic *temperature* 0.1)
(def ^:dynamic *spaces-per-indent* 1) ; Doesn't follow as nice a format for >1

(defn _optimize [^ints force-newline?s ; newlines added at end
                 ^ints delta-syntaxs ^ints token-lengths ^ints indent-amounts
                 token-types tokens]
  (let [n (count token-lengths)
        lowest-cost (int 1000000000)
        cost-mult (double (/ 1.0 *width-target* *width-target*))
        temp (double *temperature*)
        dudsteps (int (* *dudcoverthreshold* n))
        spaces-per-indent (int *spaces-per-indent*)
        
        ^ints current-addnl?s (make-array Integer/TYPE n)
        ^ints best-addnl?s (make-array Integer/TYPE n)
        ^ints token-types (into-array Integer/TYPE token-types)
        
        ^ints allow-nl?s (make-array Integer/TYPE n) ; don't allow before a space token.

        ^ints allowed-ixs (_get-allowed-newlines! tokens token-types allow-nl?s)
        k (count allowed-ixs)
        jump0 (fn [fraction] "Insert or delete linebreaks"
                (let [jx (int (Math/floor (* (Math/random) fraction)))
                      change-ix (aget allowed-ixs jx)
                      old-val (aget current-addnl?s change-ix)]
                  (aset-int current-addnl?s change-ix
                           (int (- 1 old-val)))
                  [[change-ix old-val]]))
        jump1-1 (fn [fraction shift-dir] "Shift the location of linebreaks, with no splitting or merging"
                  (let [newline-ixs (filterv #(> (aget current-addnl?s (aget allowed-ixs %)) 0) 
                                      (range k))
                        newline-ixix (int (Math/floor (* fraction (count newline-ixs))))
                        newline-ix (get newline-ixs newline-ixix)
                        newline-ixadj (if (< shift-dir 0) (get newline-ixs (dec newline-ixix) -1)
                                        (get newline-ixs (inc newline-ixix) 1000000000))]
                    (if (and newline-ix
                          (> (Math/abs (- newline-ixadj newline-ix)) 1.5) ; No adjacent allowed index is used.
                          (or (and (> shift-dir 0) (< newline-ix (dec k))) ; Not moving over the edge.
                             (and (< shift-dir 0) (> newline-ix 0))))
                      (let [from-ix (int (aget allowed-ixs newline-ix))
                            to-ix (int (aget allowed-ixs (int (+ newline-ix shift-dir))))]
                        (do (aset-int current-addnl?s from-ix 0)
                          (aset-int current-addnl?s to-ix 1)
                          [[from-ix 1]
                           [to-ix 0]]))
                      [])))
        
        jumper!s [jump0 jump0 jump0 jump0 jump0 #(jump1-1 % -1) #(jump1-1 % 1)]
        j (count jumper!s)
        ] 
    (loop [duds (int 0) current-cost (double 1e100) best-cost (double 1e100) total (int 0)]
      (if (or (= n 0) (>= duds dudsteps)) 
        (do (if *print-score?*
              (println "Best score found:"
              best-cost)) best-addnl?s)
        (let [rx (int (Math/floor (* (Math/random) k)))
              old-vals ((nth jumper!s (* (Math/random) j)) rx) ; change it.
              
              line-cost (_intsum current-addnl?s)
              over-cost (first (_cost-calculate current-addnl?s force-newline?s  
                                 delta-syntaxs token-lengths indent-amounts spaces-per-indent tokens))
              prospective-cost (+ line-cost (* over-cost cost-mult))
              
              improvement (- current-cost prospective-cost)
              keep? (or (> improvement 0) 
                      (< (Math/random) (Math/exp (/ improvement temp))))
              record-set? (< current-cost best-cost)]
          (if (not keep?) (mapv #(aset-int current-addnl?s 
                                   (int (first %)) (int (second %))) old-vals)) ; undo change.
          (if record-set? (_acopy-int! current-addnl?s best-addnl?s))
          (recur (if record-set? (int 0) (inc duds))
            (if keep? prospective-cost current-cost)
            (if record-set? current-cost best-cost) (inc total)))))
    best-addnl?s))

;;;;;;;;;;;;;;;;;;;;;;; API ;;;;;;;;;;;;;;;;;;;;;;

(defn ps [code-or-str]
  "Prettyprint to String, using the standard clojure pprint."
  (let [code (if (string? code-or-str) (read-string code-or-str)
               code-or-str)] 
    (with-out-str (pprint/pprint code))))

(defn vps [code-or-str]
  "Very Prettyprint to String."
  (let [code-or-str (if (string? code-or-str) code-or-str
                      (somewhat-unmacro code-or-str))
        tok-str+types (lean-tokenize code-or-str)
        tokens (first tok-str+types)
        tok-types (second tok-str+types)
        x (apply setup-arrays tok-str+types)
        spaces-per-indent *spaces-per-indent*
        ^ints delta-syntaxs (nth x 0)
        ^ints token-lengths (nth x 1)
        ^ints force-newline?s (nth x 2)
        ^ints indent-amounts (nth x 3)
        ^ints add-newline?s (_optimize force-newline?s delta-syntaxs token-lengths indent-amounts
                              tok-types tokens)]
    (second (_cost-calculate-with-blit add-newline?s
             force-newline?s ; newlines added at end
             delta-syntaxs token-lengths indent-amounts
             spaces-per-indent tokens))))

(defn vp [code-or-str]
  "Very Prettyprint"
  (println (vps code-or-str)))
