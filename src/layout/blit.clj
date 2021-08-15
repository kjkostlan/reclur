; TODO: this relies heavily on clojure-based syntax. It should be refactored to use langs.

; Lisps love nesting. Lets make it well-indented!
; This isn't designed to be bulletproof in terms of printing and reading; possible edge-cases; it is mostly for human consumption.
(ns layout.blit
  (:require [coder.crosslang.langs :as langs] [coder.cbase :as cbase]
    [coder.textparse :as textparse]
    [collections]
    [clojure.string :as string]
    [clojure.walk :as walk]
    [clojure.pprint :as pprint]
    [clojure.set :as set]))

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
    (throw (Exception. "Structural unmacroing not implemented TODO (for core macros only). Note we need a function that changes paths as well!")))
  (walk/prewalk leaf-unmacro code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Other cleanup functions ;;;;;;;;;;;;;;;;;;;;;;;;

(defn sym-nicknames [sym-quals & make-unqual?]
  "Makes a map from symbol to it's nickname, nicknames are unique.
   Qualified symbol can be quite long.
   This function is currently not used here but TODO may be useful."
  (let [sym-quals (set sym-quals)
        stub (fn [sq k]
               (let [pieces (string/split (str sq) #"\/") n (count pieces)
                     pieces (if (> n k) (subvec pieces (- n k) n) pieces)]
                 (apply str (interpose (if (first make-unqual?) "_" "/") pieces))))]
    (loop [acc {} need sym-quals n 1]
      (if (= (count need) 0) acc
        (let [need-stubs (mapv #(stub % n) need)
              need2stub (zipmap need need-stubs)
              freqs (frequencies need-stubs)
              unique-stubs (set (filterv #(= (get freqs %) 1) (keys freqs)))
              stub2need (zipmap need-stubs need)
              uneed2stub (zipmap (mapv #(get stub2need %) unique-stubs) unique-stubs)
              acc1 (merge acc uneed2stub)]
          (recur acc1
            (set/difference need (set (keys uneed2stub))) (inc n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Code to decide where we should indent and blitting to a nice format ;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lean-space-tok [tok]
  (let [com? (string/includes? tok ";")
        trimmed (string/trim tok)
        not-mt (if (= (count trimmed) 0) " " trimmed)]
    (str not-mt (if com? "\n" ""))))

(defn fuse-meta-tag [strings types]
  "Type = 8 is meta tag, to the next tag which is never space under clojure's pr-str."
  (loop [acc-str [] acc-ty [] ix 0]
    (if (= ix (count strings)) [acc-str acc-ty]
      (let [s (nth strings ix) ty (nth types ix)]
        (if (= ty 8)
          (recur (conj acc-str (str s (nth strings (inc ix))))
            (conj acc-ty (nth types (inc ix)))
            (+ ix 2))
          (recur (conj acc-str s)
            (conj acc-ty ty) (inc ix)))))))

(defn lean-tokenize [code-or-str]
  "Lean tokenization of the code that removes extra whitespace, etc but keeps comments etc.
   There is a trailing newline for (blanck) tokens that are comments.
   Leaves a single space for space tokens so thingsdontjamtogether."
  (let [s (if (string? code-or-str) code-or-str
            (pr-str code-or-str))
        vt (cbase/tokenize s :clojure)
        vt (fuse-meta-tag (first vt) (second vt))
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

(defn _get-allowed-newlines! [tok-strs token-types ^ints allow-nl?s] "Modifies allow-nl?s"
  (let [n (count token-types)
        allowed-ixs
        (loop [ix (int 0) allowed-ixs []]
            (if (= ix n) allowed-ixs
              (if (or (= ix (dec n)) 
                    (let [ty (nth token-types ix)
                          ty1 (nth token-types (inc ix))]
                      (and (not= ty1 0) (not= ty1 5))))
                (do (aset-int allow-nl?s ix 1)
                  (recur (inc ix) (conj allowed-ixs ix)))
                (recur (inc ix) allowed-ixs))))
        allowed-ixs1 (_binding-blocknl tok-strs token-types allowed-ixs)
        allowed-ixs2 (if (= (count allowed-ixs1) 0) (range n) allowed-ixs1)]
    (into-array Integer/TYPE allowed-ixs2)))

(def ^:dynamic *width-target* 64.0)
(def ^:dynamic *dudcoverthreshold* 2.0)
(def ^:dynamic *temperature* 0.1)
(def ^:dynamic *spaces-per-indent* 1) ; Doesn't follow as nice a format for >1


; Newlines are AFTER each token.

(defn get-open-ix-ints [tok-types maxlev]
  "Returns ^ints. The opening parenthesis maps to itself." ; Were these functions written before?
  (let [n (int (count tok-types))
        maxlev (int maxlev)
        ^ints lev2open (make-array Integer/TYPE maxlev)
        ^ints open-ixs (make-array Integer/TYPE n)
        tok-types (into [] tok-types)]
    (loop [ix 0 lev -1]
      (if (= ix n) open-ixs
        (let [ty (nth tok-types ix)]
          (cond (= ty 4)
            (do (aset-int open-ixs ix ix)
              (aset-int lev2open (inc lev) ix)
              (recur (inc ix) (inc lev)))
            (= ty 5)
            (do (aset-int open-ixs ix (aget lev2open (max lev 0)))
              (recur (inc ix) (max (dec lev) 0)))
            :else
            (do (aset-int open-ixs ix (aget lev2open (max lev 0)))
              (recur (inc ix) lev))))))))

(defn get-close-ix-ints [tok-types maxlev]
  "Returns ^ints. The closing parenthesis maps to itself."
  (let [tok-typesr (mapv #(cond (= % 4) 5 (= % 5) 4 :else %) (reverse tok-types))
        ^ints openr (get-open-ix-ints tok-typesr maxlev)
        ntok (count tok-types)
        ^ints open (make-array Integer/TYPE ntok)]
    (loop [ix 0]
      (if (= ix ntok) open
        (do (aset-int open (- ntok ix 1) (- ntok (aget openr ix) 1))
          (recur (inc ix)))))))

(defmacro dice [p]
  `(< (Math/random) ~p))

(defn change-loop! [^ints allowed-toggle-ixs ntog ^ints loop-storage nloop last-sucessful-changeix]
  "Randomized change ixs that get back to where we started.
   Modifies loop in place."
  (let [ntog (int ntog) last-sucessful-changeix (int last-sucessful-changeix)
        nloop2 (int (/ nloop 2)) ; Round down. 
        nearby-chance 0.5
        old-boys-chance 0.025] ; kicks in if nearby chance fails.
    (if (= nloop2 0) (throw (Exception. "The loop must be at least two elements long.")))
    (loop [ix 0]
      (if (= ix nloop2) "First half done"
        (let [trial-ix (int (cond (and (> ix 0) (dice nearby-chance)) ; nearby change.
                              (+ (aget loop-storage (dec ix))
                                (if (dice 0.5) -1 1))
                              (dice old-boys-chance)
                              (+ last-sucessful-changeix
                                (cond (dice 0.333333) -1
                                  (dice 0.5) 1 :else 0))
                              :else (int (* (Math/random) ntog))))
              trial-ix (int (cond (< trial-ix 0) 0 (>= trial-ix ntog) (dec ntog) :else trial-ix))
              trial-ix (aget allowed-toggle-ixs trial-ix)]
          (aset loop-storage ix trial-ix)
          (recur (inc ix)))))
    (loop [ix nloop2] ; make the second half a repeat that will undo each toggle.
      (if (= ix nloop) "Done!"
        (do (aset-int loop-storage ix (aget loop-storage (- ix nloop2)))
          (recur (inc ix)))))))

(defn set-up-arrays [tok-strs tok-types]
  "No newlines at all setup (not even forced newlines!)."
  ; Type 4 = open bracket. Type 5 = closing bracket.
  (let [ntok (count tok-strs)
        ^ints newline?s (make-array Integer/TYPE ntok)
        ^ints delta-syntaxs (make-array Integer/TYPE ntok)
        levmx (loop [ix (int 0) lev (int 0) max-lev (int 0)] 
                (if (< ix ntok) 
                  (let [ty (nth tok-types ix) max1 (if (> lev max-lev) lev max-lev)]
                    (if (= ty 4) (do (aset-int delta-syntaxs ix 1) 
                                   (recur (inc ix) (inc lev) (inc max1)))
                      (if (= ty 5)
                        (do (aset-int delta-syntaxs ix -1)
                          (recur (inc ix) (dec lev) max1))
                        (recur (inc ix) lev max-lev))))
                  max-lev))
        
        ;^ints token-lengths (make-array Integer/TYPE ntok)
        ;_ (loop [ix (int 0)]
        ;    (if (< ix ntok)
        ;      (let [tok (nth tok-strs ix)]
        ;        (aset-int token-lengths ix (count tok)) (recur (inc ix)))))
        ^ints force-newline?s (make-array Integer/TYPE ntok)
        _ (loop [ix (int 0)]
            (if (< ix ntok)
              (let [tok (nth tok-strs ix)]
                (aset-int force-newline?s ix  ; Crude to force a newline after a token if it has a newline.
                  (if (string/includes? tok "\n") 1 0)) 
                (recur (inc ix)))))
        x-ends (into-array Integer/TYPE
                 (reductions + (count (first tok-strs)) (mapv count (rest tok-strs))))
        ^ints allowed-nl?s (make-array Integer/TYPE ntok)
        ^ints allowed-ixs (_get-allowed-newlines! tok-strs tok-types allowed-nl?s)]
    {:newline?s newline?s :x-ends x-ends :force-nl?s force-newline?s :allow-nl?s allowed-nl?s
     :allowed-toggle-ixs allowed-ixs
     :open-ix ^ints (get-open-ix-ints tok-types levmx) :close-ix ^ints (get-close-ix-ints tok-types levmx)}))

(defn toggle-newline! [^ints newline?s ^ints x-ends ^ints open-ix ^ints close-ix ntok max-free-x spaces-per-indent ix]
  "Returns the quadratic delta-cost above max-free-x.
   open-ix and close-ix include themselves, and the code is always a collection (non-collections are a trivial special case).
   Modifies newlines and x-ends."
  (let [max-free-x (int max-free-x) ix (int ix) ntok (int ntok)
        shift-if-new (int (if (= (aget open-ix ix) ix) 0 ; newline at opening, which is pointless to have.
                            (if (and (< ix (dec ntok)) (= (aget open-ix (inc ix)) (inc ix))) ; newline before an open ()[]{}, indent.
                              (+ spaces-per-indent (- (aget x-ends ix)) (aget x-ends (aget open-ix ix)))
                              (- (aget x-ends (aget open-ix ix)) (aget x-ends ix))))) ; Vanilla.
        new? (boolean (= (aget newline?s ix) 0))
        shift (int (if new? shift-if-new (- shift-if-new)))
        x-nl (int (aget x-ends ix))
        delta-cost-nl (float (* (if new? 1 -1)
                               (if (> x-nl max-free-x)
                                 (* (- x-nl max-free-x) (- x-nl max-free-x)) 0.0)))]
    (aset-int newline?s ix (if new? 1 0))
    (loop [jx (inc ix) jx1 (int (inc ix)) delta-cost delta-cost-nl] ; loop until we get a token that isn't affected.
      (if (or (= jx ntok) (> jx jx1)) delta-cost
        (let [line-end? (boolean (or (= jx (dec ntok)) (= (aget newline?s jx) 1)))
              xe-old (int (aget x-ends jx))
              xe-new (int (+ xe-old shift))
              cost-old (if (and line-end? (> xe-old max-free-x)) (* (- xe-old max-free-x) (- xe-old max-free-x)) 0)
              cost-new (if (and line-end? (> xe-new max-free-x)) (* (- xe-new max-free-x) (- xe-new max-free-x)) 0)
              jx11 (int (if (= jx1 jx)
                          (if (= (aget open-ix jx) jx) (aget close-ix jx) ; ( jump to )
                            (if line-end? jx (inc jx))) ; newlines stop propagation. 
                          jx1))]
          (aset x-ends jx xe-new)
          (recur (inc jx) jx11 (+ delta-cost (- cost-new cost-old))))))))

(defn optimize! [^ints newline?s ^ints x-ends ^ints force-nl?s ^ints allow-nl?s ^ints allowed-toggle-ixs ^ints open-ix ^ints close-ix max-free-x quad-wt spaces-per-indent]
  "Modifies newlines and x-ends. Use those arrays to make the token order."
  (let [ntok (int (alength x-ends)) n-outer-loops (int (* ntok 16)) loop-size 4 ; TODO: better system for iterations.
        ^ints loop-storage (make-array Integer/TYPE loop-size)
        spaces-per-indent (int *spaces-per-indent*)
        quad-wt (float quad-wt)
        ntog (int (alength allowed-toggle-ixs))]
    (loop [ix (int 0)]
      (if (= ix ntok) "Done force newlines part."
        (do
          (if (or (and (= (aget newline?s ix) 1) (= (aget allow-nl?s ix) 0))
                (and (= (aget newline?s ix) 0) (= (aget force-nl?s ix) 1)))
            (toggle-newline! newline?s x-ends open-ix close-ix max-free-x spaces-per-indent ix ntok)
          (recur (inc ix))))))
    (loop [loopx (int 0) last-sucessful-changeix -1] ; the core optimization step.
      (if (= loopx n-outer-loops) "DONE!"
        (do (change-loop! allowed-toggle-ixs ntog loop-storage loop-size last-sucessful-changeix)
          (let [change-ix (loop [jx 0 total-delta 0.0]
                            (let [toggle-ix (int (aget loop-storage jx))
                                  delta-lin (if (= (aget newline?s toggle-ix) 1) -1 1)
                                  delta-sqr (float
                                              (toggle-newline! newline?s x-ends open-ix close-ix ntok 
                                               max-free-x spaces-per-indent toggle-ix))
                                  delta (float (+ (* quad-wt delta-sqr) delta-lin))
                                  total-delta1 (float (+ delta total-delta))]
                              (if (< total-delta1 0) toggle-ix
                                (if (= jx (dec loop-size)) -1 ; fail to find any improvement.
                                  (recur (inc jx) total-delta1)))))]
            (recur (inc loopx)
              (if (> change-ix -1) change-ix last-sucessful-changeix))))))))

(defn ez-blit [tokens ^ints newline?s ^ints x-ends]
  (let [tokens (into [] tokens)
        ntok (count tokens)
        x-starts (conj (mapv #(- (aget x-ends %) (count (nth tokens %))) (range ntok)) 0)
        strs (loop [acc [] ix 0]
               (if (= ix ntok) acc
                 (recur 
                   (conj acc 
                     (str (nth tokens ix) (if (= (aget newline?s ix) 1) 
                                            (apply str "\n" (repeat (nth x-starts (inc ix)) " ")) "")))
                   (inc ix))))] 
    (apply str strs)))

(defn to-code [code-or-str] (if (string? code-or-str) (read-string code-or-str) code-or-str))

(defn _vps-core [code-or-str]
  (let [code (to-code code-or-str)
        _ (try (pr-str code) (catch Exception e (throw (Exception. "Badly formatted code.")))) ; does this ever happen?
        tok-str+types (lean-tokenize code)
        tok-strs (first tok-str+types)
        tok-types (second tok-str+types)
        x (set-up-arrays tok-strs tok-types)
        max-free-x (int (* *width-target* 0.4)) ; exact scaling here is heuristic.
        quad-wt (/ 1.0 max-free-x max-free-x)
        spaces-per-indent *spaces-per-indent*
        _ (optimize! ^ints (:newline?s x) ^ints (:x-ends x) ^ints (:force-nl?s x) ^ints (:allow-nl?s x) ^ints (:allowed-toggle-ixs x)
            ^ints (:open-ix x) ^ints (:close-ix x) max-free-x quad-wt spaces-per-indent)]
    [tok-strs ^ints (:newline?s x) ^ints (:x-ends x)]))

;;;;;;;;;;;;;;;;;;;;;;; API ;;;;;;;;;;;;;;;;;;;;;;

(defn ps [code-or-str]
  "The standard clojure pprint, but to a string. Contrast with vps."
  (with-out-str (pprint/pprint (to-code code-or-str))))

(defn vps [code-or-str]
  "Very Prettyprint to String."
  (let [code (to-code code-or-str)]
    (if (coll? code) (apply ez-blit (_vps-core code-or-str)) 
      (pr-str code))))

(defn vp [code-or-str]
  "Very Prettyprint"
  (println (vps code-or-str)))

(defn vpsu [code-or-str]
  "Unquals all symbols, which may make it easier to read."
  (vps (walk/postwalk #(if (symbol? %) (textparse/unqual %) %) (to-code code-or-str))))

(defn vpu [code]
  "Unquals all symbols, which may make it easier to read."
  (println (vpsu code)))

;;;;;;;;;;;;;;;;;;; Debugging ;;;;;;;;;;;;;;;;;;;;;;

(defn diff-anal [code-or-str]
  "Unfinished differences could be very useful for optimization."
  (let [^ints nu1 (second (_vps-core code-or-str))
        ^ints nu2 (second (_vps-core code-or-str))
        nuv1 (into [] nu1) nuv2 (into [] nu2) ntok (count nuv1)
        change-ixs (filter #(not= (nth nuv1 %) (nth nuv2 %)) (range ntok))]
    (println "Stuff:" change-ixs (mapv #(nth nuv1 %) change-ixs))))
