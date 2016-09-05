; Automatic code indentation.
; Very similar to pprint EXCEPT it preserves the code's structure, comments, reader-macros, etc.
; The main function is indent/indent.
(ns clooj.coder.indent
  (:require [clooj.coder.grammer :as grammer] [clooj.coder.blitcode :as blitcode]
            [clooj.collections :as collections] [clojure.pprint :as pprint])) ;pprint is just for debugging.

(defn _add-outer-level-newlines [s]
  "Adds a newline between all outer-level forms, which makes the code easier to read."
  (let [p (blitcode/basic-parse s) depth (:inter-depth p) n (count s)]
    (loop [acc [] ix (int 0)]
      (if (= ix n) (apply str acc)
        (let [d (aget ^ints depth ix) d1 (if (< ix (dec n)) (aget ^ints depth (inc ix)) 0)
              c (nth s ix)]
          (recur (if (and (= d 0) (> d1 0)) (conj acc \newline c) (conj acc c)) (inc ix)))))))

(defn _force-21 [x] (mapv #(if (and (= %1 0) (= %2 2)) 1 %1) x (concat [0] x)))
(defn _digest [^chars cs ^ints inter-depth ^ints breakable ^ints token-ty]
  "Creates arrays that shorten the time it takes to optimize the indentation algorythim.
   cs should be the character array after removing extra space.
   Divides up the string into pieces. Breakable spaces AND (forced) newlines are the breakpoints between each piece.
     Note: the indent begins at the beginning of the reader macro.
   Arrays:
   ^ints start-ix = Where said token starts.
   ^ints nums = # of characters in each piece.
     The space \n at the end of the token does not count.
   ^ints genre: 0 = this token ends in a breakable space. 1 = comments, etc make a forced newline. 2 = forced newline and can't indent b/c it's a string.
               -1 = end of array. Note: for simplicity we force a newline after every string that has a linebreak in it.
   ^ints margin = How much to indent the next token (in comparison to our base, which may be ourselved) if we break at this token. 
     (2 for lists, 1 for other datastructures) + any macro chars in accordance of the style guide.
   ^ints depth = The indentation depth at the END of the token.
   ^ints base-ix = Which token encloses the END of the token."
  (let [n (alength ^chars cs) last-close? (and (> n 0) (grammer/cclose? (aget ^chars cs (dec n))))]
    (loop [ix (int 0) l2indt {0 0} l2base {0 0} nums [] genre [] margin [] start-ix [0] depth [] base-ix [] c-this (int 0)]
      ; l2indt[i] = y => a non-string newline at this level i forces indenting y characters beyond the token that opens y.
      (if (= ix n) {:nums (into-array Integer/TYPE (conj nums c-this)) 
                    :genre (into-array Integer/TYPE (_force-21 (conj genre -1))) 
                    :margin (into-array Integer/TYPE (conj margin 0)) 
                    :start-ix (into-array Integer/TYPE start-ix)
                    :depth (into-array Integer/TYPE (conj depth (if (last depth) (- (last depth) (if last-close? 1 0)))))
                    :base-ix (into-array Integer/TYPE (conj base-ix (if (last base-ix) (last base-ix) -1)))}
        (let [b (aget ^ints breakable ix) d (aget ^ints inter-depth (inc ix))
              t (aget ^ints token-ty ix) c (aget ^chars cs ix) 
              ;d1 (if (< ix (dec n)) (aget ^ints inter-depth (inc ix)) d)
              ty-out (cond (= b 1) 0 (= b 2) (if (= t 3) 2 1) :else -1)]
          (recur (inc ix) (if (= t 6) (assoc l2indt d (+ (if (= c \() 2 1) c-this)) l2indt) ; the type of open-char sets the indent amount.
            (if (= t 6) (assoc l2base d (count genre)) l2base) ; the parent index is us.
            (if (> ty-out -1) (conj nums c-this) nums) ; Add to the counts if we hit a new token.
            (if (> ty-out -1) (conj genre ty-out) genre) ; Add to the type if we hit a new token.
            (if (> ty-out -1) (conj margin (get l2indt d)) margin) ; The cached l2indt array keeps track of which levels need how much xtra indent.
            (if (> ty-out -1) (conj start-ix (inc ix)) start-ix)
            (if (> ty-out -1) (conj depth d) depth)
            (if (> ty-out -1) (conj base-ix (get l2base d)) base-ix)
            (if (> ty-out -1) 0 (inc c-this))))))))

(defn _last-affected-index [^ints choices ^ints nums ^ints depth diff-ix]
  "Caculates the last token that (probably) will be shifted in it's x-location due to a point-change in the indent decisions.
   choices = 1 where we choose to put a newline. It is 1:1 with all the other arrays despite some choices bieng forced (to 1).
     Note: Each choice is for AFTER the token.
   num and depth are from _digest. diff-ix = where the change is."
  (let [n (alength ^ints nums)]
    (if (= n 0) (int -1)
      (let [d0 (aget ^ints depth diff-ix)]
            ; The last token that may get it's x-value moved by our change: 
        (loop [ix (int (inc diff-ix)) hold-depth (int (inc d0))] ; hold-depth = if we are at or deeper we ignore newlines.
          (if (>= ix n) (dec n)
            ; Look for the newline that ends the token.
            (let [d (aget ^ints depth ix) c (aget ^ints choices ix)]
              (if (or (>= d hold-depth) (not= c 1)) (recur (inc ix) (int (min hold-depth (inc d)))) ix))))))))

(defn _x-shift [^ints x-vals ^ints nums ^ints margin ^ints base-ix diff-ix post-diff-value]
  "How much the x-value of the affected downstream tokens shifts, + is right and - is left. 
   The first few downstream tokens all shift by this amount, but tokens beyond _last-affected-inxed don't move x-value at all.
   x-vals = where each token's x-value starts.
   num, margin, and base-ix is from _digest.
   diff-ix = where we apply the change. post-diff-value = what we change to (we assume that is opposite to our current value)."
  (if (>= diff-ix (dec (alength ^ints x-vals))) 0
    (let [x-now (aget ^ints x-vals (inc diff-ix))]
      (- (if post-diff-value (+ (aget ^ints x-vals (aget ^ints base-ix diff-ix)) 
                               (aget ^ints margin diff-ix)) ; Indented from a newline.
           (+ (aget ^ints x-vals diff-ix) (aget ^ints nums diff-ix) 1)) x-now)))) ; Not indented.

(defmacro _over-cost [x max-cost-free]
  `(double (if (<= ~x ~max-cost-free) 0.0 (* (- ~x ~max-cost-free) (- ~x ~max-cost-free)))))
(defn _overshoot-cost-diff [^ints x-vals ^ints nums ^ints genre x-shift diff-ix last-affected-ix max-cost-free post-diff-value] 
 "Cost of overshooting the array. diff-ix is where the change is made.
  x-vals is BEFORE the change in array.
  x-shift is how much we shifted (from the _x-shift function).
  last-affected-ix is from _last-affected-index.
  max-cost-free is the maximum # of chars allowed per line; Cost = sum(overshoot^2).
  WARNING: may not work if diff-ix cooresponts to a string's newline due to the fixed indentation amount.
     But that is not allowed to be changed anyway so it shouldn't be used."
  (let [x-token (+ (aget ^ints x-vals diff-ix) (aget ^ints nums diff-ix))
        cost-at-t (_over-cost x-token max-cost-free)] ; creating a newline at the token means the toke also counts.
    (loop [old-cost-acc (double (if post-diff-value 0.0 cost-at-t))
           new-cost-acc (double (if post-diff-value cost-at-t 0.0))
           ix (int (inc diff-ix))]
      (if (> ix last-affected-ix) (- new-cost-acc old-cost-acc)
        (if (= (aget ^ints genre (dec ix)) 2) (recur old-cost-acc new-cost-acc (inc ix)) ; Indent level 2 is immune. 
          (let [x0 (+ (aget ^ints x-vals ix) (aget ^ints nums ix)) x1 (+ x0 x-shift)]
            (recur (+ old-cost-acc (_over-cost x0 max-cost-free)) (+ new-cost-acc (_over-cost x1 max-cost-free)) (inc ix))))))))

(defn _calculate-x-vals [^ints choices ^ints nums ^ints margin ^ints base-ix ^ints genre]
 "Calculates the x-vals array we used above. Use once to initialize the x-vals array."
  (let [n (int (alength ^ints choices))
        ^ints xs (make-array ^ints Integer/TYPE n)]
    (loop [ix (int 0) x (int 0)]
      (if (< ix n)
        (do (aset ^ints xs ix (int x))
          (recur (inc ix) ; Calculate the next x-value: 
            (int (if (> (aget ^ints choices ix) 0) ; Newline.
                   (if (= (aget ^ints genre ix) 2) 0 ; String-based newlines force it to zero.
                     (let [b-ix (aget ^ints base-ix ix)]
                       (+ (aget ^ints xs b-ix) (aget ^ints margin b-ix))))
                     (+ x (aget ^ints nums ix) 1))))))) xs))

(defn _update-xs! [^ints xs ^ints genre x-shift diff-ix last-affected-ix]
  "In-place updating the x-values."
  (loop [ix (int (inc diff-ix))]
    (if (<= ix last-affected-ix) 
      (do (if (not= (aget ^ints genre (dec ix)) 2) ; Don't shift stuff after a string newline.
            (aset ^ints xs ix (int (+ x-shift (aget ^ints xs ix))))) (recur (inc ix))))))

(defn _init-choices [^ints genre ^ints inter-depth]
  "Provides an initialization of the choices array that is valid and performant."
  (let [n (alength ^ints genre)
        out (make-array Integer/TYPE n)]
    (loop [ix (int 0)]
      (if (< ix n) (let [c (if (or (= (mod ix 10) 0) (not= (aget ^ints genre ix) 0)
                                 (= (aget ^ints inter-depth ix) 0)) 1 0)]
                     (aset ^ints out ix (int c)) (recur (inc ix))))) out))

(defn _optimize-choices [^ints choices0 ^ints x-vals0 ^ints nums ^ints margin ^ints depth ^ints base-ix ^ints genre line-length-guess]
  "Minimized the cost using a dumb local greedy algorythim (that actually seems to perform quite well).
   Costs: Overshooting and having too many lines. 
   line-length-guess allows us to adjust how long the lines are but it is not a 1:1 relationship.
   Returns the new {:choices and :x-vals}."
  (let [n (alength ^ints nums)
        nc (loop [acc (int 0) ix (int 0)]
             (if (= ix n) acc (recur (+ acc (aget ^ints nums ix)) (inc ix))))
        _ (if (= (double line-length-guess) 0.0) (throw (Exception. "Zero line-length-guess"))) 
        line-count-guess (double (/ (inc nc) line-length-guess))
        num-lines (loop [acc (int 1) ix (int 0)]
                    (if (= ix n) acc (recur (+ acc (aget ^ints choices0 ix)) (inc ix))))
        ; Error is normalized so that cost totals 0 for awesome and 1.0 for a pretty bad overall situation:
        over-cost-weight (double (/ 1.0 (* line-length-guess line-length-guess line-count-guess)))
        line-cost-weight (double (/ 1.0 line-count-guess))
        x-vals (make-array Integer/TYPE n) choices (make-array Integer/TYPE n)
        bfalse (boolean false) btrue (boolean true)]
    (loop [ix (int 0)] ; copy
      (if (< ix n) (do (aset ^ints choices ix (int (aget ^ints choices0 ix)))
                       (aset ^ints x-vals ix (int (aget ^ints x-vals0 ix))) (recur (inc ix)))))
    ; The optimization proper (mutates choices and x-vals):
    ; This is NOT a rate-limiting step. As of now the performance tweaks we can make are unclear.
    (loop [ix (int 0) improved? (boolean false)]
      (if (and (= ix n) (not improved?)) "Done."
        (if (= ix n)
          (recur 0 bfalse)
          (if (= (aget ^ints genre ix) 0) ; Only genre 0 can be modified.
            (let [c (aget ^ints choices ix) post-diff (if (= c 0) true false)
                  ls (_last-affected-index choices nums depth ix)
                  x-shift (_x-shift x-vals nums margin base-ix ix post-diff)
                  delta-cost (+ (* over-cost-weight (_overshoot-cost-diff x-vals nums genre x-shift ix ls line-length-guess post-diff))
                                (* line-cost-weight (if post-diff 1 -1)))]
              (if (< delta-cost 0.0) 
                (do (_update-xs! x-vals genre x-shift ix ls)
                    (aset ^ints choices ix (int (if post-diff 1 0)))
                    (recur (inc ix) btrue)) (recur (inc ix) improved?))) (recur (inc ix) improved?))))) 
    {:choices choices :x-vals x-vals}))

(defn _apply-choices [^chars cs0 ^ints choices ^ints x-vals ^ints breakable]
  "Returns a string that represents the result of applying the choices."
  (let [n (alength ^chars cs0) ^chars cs (make-array Character/TYPE n)]
    (apply str 
      (loop [acc [] ix (int 0) bix (int 0)] ; bix keeps track of where on the choices array we are.
        (if (= ix n) acc
          (let [bi (aget ^ints breakable ix)] ; breakable marks where the characters are.
            (let [add (cond (= bi 0) (aget ^chars cs0 ix)
                            (and (> bi 0) (= (aget ^ints choices (int bix)) 1)) ; bix is updated at the end of the token.
                            (str \newline (apply str (repeat (aget ^ints x-vals (int (inc bix))) " "))) 
                            :else " ")]
              (recur (conj acc add) (inc ix) (+ bix (if (> bi 0) 1 0))))))))))
    

(defn _remove-extra-space [bcode & dont-project]
  "Removes spaces and newlines in bcode with the exception of those nessessary because of comments, etc.
   TODO: it smashes words in comments into eachother."
  (let [nnl (fn [s] (let [p (blitcode/tokenize (str s)) ^ints ty (:token-type p)
                          n (alength ^ints ty) ^chars cs (:chars p)]
                      (loop [acc [] ix (int 0)]
                        (if (= ix n) (apply str acc)
                          (let [t (aget ^ints ty ix)]
                            (recur (if (= t 0) acc (conj acc (aget ^chars cs ix))) (inc ix)))))))
        bcodenl (fn bcodenl [c] (let [c (if (collections/col? (:obj c))
                                            (update c :obj #(collections/cmap :flatten bcodenl %)) c)]
                                  (update (update c :head nnl) :tail nnl)))]
    ((if (first dont-project) identity blitcode/blit-project) (bcodenl bcode))))

(defn indent [str-or-code & target-len] ;(do (clc) (println (indent/indent s)))
  "Automatically newlines and indents the code (removing old newlines if they aren't needed).
   Use for visualizing code that has undergone substantial transformations, or debugging macro code.
   If a string: It is read into blit-code, preserving the reader-macros, etc.
   Differences from pprint/pprint: Works on strings in a way that preserves reader macros and comments.
                                   More compact since it is better at optimization."
  (let [target-len (if (first target-len) (first target-len) 50) ; scales the line-length but not 1:1 exactly.
        even-enforce? false ; expiremental.
        ; Accept both strings and unblitted code:
        blit (blitcode/reads-string-blit (if (string? str-or-code) str-or-code (grammer/code-to-str str-or-code)))
        blit-sp (blitcode/blit-decrowd (_remove-extra-space blit))
        ^String s (blitcode/blit-to-str blit-sp)
        p (blitcode/newline-parse s even-enforce?)
        digest (_digest (:chars p) (:inter-depth p) (:breakable p) (:token-type p))

        ^ints choices0 (_init-choices (:genre digest) (:inter-depth p))
        ^ints x-vals0 (_calculate-x-vals choices0 (:nums digest) (:margin digest) (:base-ix digest) (:genre digest))
        ; The main optimization step:
        ch-xn (_optimize-choices choices0 x-vals0 (:nums digest) (:margin digest) 
                (:depth digest) (:base-ix digest) (:genre digest) target-len)
        ;out0 (_apply-choices (:chars p) choices0 x-vals0 (:breakable p))
        out (_apply-choices (:chars p) (:choices ch-xn) (:x-vals ch-xn) (:breakable p))] 
    (_add-outer-level-newlines out)))