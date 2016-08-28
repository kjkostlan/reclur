; Automatic code indentation.
; Very similar to pprint EXCEPT it preserves the code's structure, comments, reader-macros, etc.
; The main function is indent/indent.
(ns clooj.coder.indent
  (:require [clooj.coder.grammer :as grammer] [clooj.coder.blitcode :as blitcode]
            [clooj.collections :as collections]))

(defn _remove-extra-space [bcode & dont-project]
  "Removes spaces and newlines in bcode with the exception of those nessessary because of comments, etc.
   TODO: it smashes words in comments into eachother."
  (let [nnl (fn [^String s] (.replace (.replace (.replace (.replace s "\n" "") " " "") "\t" "") "," ""))
        bcodenl (fn bcodenl [c] (let [c (if (collections/col? (:obj c))
                                            (update c :obj #(collections/cmap :flatten bcodenl %)) c)]
                                  (update (update (update c :head nnl) :tail nnl) :body nnl)))]
    ((if (first dont-project) identity blitcode/blit-project) (bcodenl bcode))))

(defn _get-n-lines [^chars cs]
  "Now many lines are in the cs array."
  (let [n (count cs)]
    (loop [acc (int 1) ix 0]
      (if (= ix n) acc
        (recur (+ acc (if (= (aget ^chars cs ix) \newline) 1 0)) (inc ix))))))
(defn _indent-cost [^chars cs line-length-scale]
  "Space-cost estimation: lines that are too short, have too much indent-space, or are too long.
   All the metrics are soft measures of cost.
   cs must already be indented properly."
  (let [; Count the newlines:
        n (count cs)
        n-lines (_get-n-lines cs)
        ^ints counts-per-line (make-array Integer/TYPE n-lines)
        _ (loop [lx (int 0) ix (int 0)]
            (if (= ix n) "Done with counting per line."
              (if (= (aget ^chars cs ix) \newline) (recur (inc lx) (inc ix))
                 (do (aset ^ints counts-per-line lx (inc (aget ^ints counts-per-line lx)))
                     (recur lx (inc ix))))))
        n-non-w (loop [acc (int 0) ix (int 0)] ; non white chars are the only chars that count.
                  (if (= ix n) acc
                    (recur (+ acc (if (grammer/ccom-white? (aget ^chars cs ix)) 0 1)) (inc ix))))

       ; Cost is normalized so that 1 is bad and 0 is good.
        waste-cost (let [capacity (double (max 1 (* n-lines line-length-scale)))] ; 0 = no waste, 1 = complete waste.
                     (max 0.0 (/ (- capacity n-non-w) capacity)))

        overshoot-cost (let [line-length-scale (double line-length-scale) ; 0 = no overshoot. 1 = rms 200% as long as ideal.
                             sum-sqr (loop [acc (double 0.0) ix (int 0)]
                                       (if (= ix n-lines) acc
                                         (let [n+ (double (max 0.0 (- (aget ^ints counts-per-line ix) line-length-scale)))]
                                           (recur (+ acc (* n+ n+)) (inc ix)))))]
                          (/ sum-sqr (* line-length-scale line-length-scale n-lines)))]
    (+ waste-cost overshoot-cost)))
(defn _get-cs-with-indents [^chars cs0 ^ints level ^ints depth ^ints choices ^ints choice-ix]
  (let [n (count cs0)
        nc (count choice-ix)
        ; Set the characters to spaces or newline depending on the target:
        cs-newl (let [^chars cs1 (make-array Character/TYPE n)]
                   (loop [ix (int 0)]
                     (if (= ix n) "Done array copy."
                       (do (aset ^chars cs1 ix (aget ^chars cs0 ix)) (recur (inc ix)))))
                   (loop [ix (int 0)]
                     (if (= ix nc) "Done setting newlines vs spaces."
                       (do (aset ^chars cs1 (aget ^ints choice-ix ix) 
                             (if (= (aget ^ints choices ix) 1) \newline \ )) (recur (inc ix)))))
                   cs1)
        n-lines (_get-n-lines cs-newl)
        ;Moving indent tracker:
        max-indent-level (loop [acc (int 0) ix (int 0)]
                           (if (= ix n) acc
                             (recur (int (max (aget ^ints depth ix) acc)) (inc ix))))
        ^ints i-counts (make-array Integer/TYPE n-lines)
        total-i-count ;also mutates the i-counts array
          (let [^ints d2s (make-array Integer/TYPE (+ max-indent-level 2))]
            ; d2s is how many spaces an indentation at a certain level needs.
            ; The main loop:
            (loop [acc (int 0) ix (int 0) x (int 0) y (int 0)] ; x includes indentation spaces we will add, not just what we have now.
              (if (= ix n) acc ; value of total # spaces added for indentation.
                (let [c (aget ^chars cs-newl ix) d (aget ^ints depth ix)
                      l (aget ^ints level ix) old-l (if (> ix 0) (aget ^ints level (dec ix)) (int 0))
                      sp (aget ^ints d2s d)] ; # spaces we add to the next line if this is a newline.
                  ; Increase in level => an opening "(" => mark x+2 as the location we indent to:
                  (if (> l old-l) (aset ^ints d2s l (+ x 2)))
                  ; decreasing in level changes nothing.
                  (if (= c \newline)
                    (let [ic (aget ^ints d2s d)] ; record the indentation. We will add it later.
                      (aset ^ints i-counts y ic)
                      (recur (+ acc ic) (inc ix) ic (inc y))) ; x set do d = # of spaces in.
                      (recur acc (inc ix) (inc x) y))))))
       ; Apply indents at each line:
       ^chars out (make-array Character/TYPE (+ total-i-count n))]
    (loop [ix (int 0) shift (int 0) y (int 0)]
      (if (= ix n) "Done with applying indents."
        (let [ci (aget ^chars cs-newl ix)]
          (aset ^chars out (+ ix shift) ci) ; set this character.
          (if (= ci \newline) 
            (let [ni (aget ^ints i-counts y)]
              (loop [jx (int 1)] ; set ix + shift + 1 to ix + shift + ni, inclusive.
                (if (> jx ni) "Done with appling this line's indent."
                  (do (aset ^chars out (+ ix shift jx) \ ) (recur (inc jx)))))
              (recur (inc ix) (+ shift ni) (inc y)))
            (recur (inc ix) shift y)))))
 out))

(defn _optimize-indent [^chars cs ^ints level ^ints depth ^ints breakable line-length-scale]
  "Optimizes the indentation by trying to maximize the efficiency.
   Use the results of _indent-parse. Returns a string.
   TODO: uses a slow crappy algorythim. Optimize."
  (let [; Choice-indexes (where we can make a cnoice between space and newline):
        n (count cs)
        _choice-ixs (reduce #(if (= (aget ^ints breakable %2) 1) (conj %1 %2) %1) [] (range n))
        ^ints choice-ixs (into-array Integer/TYPE _choice-ixs) ; where on the chars array we get to choose.

        ; Costs:
        get-new-cs (fn [^ints choices] (_get-cs-with-indents cs level depth choices choice-ixs))
        get-cost (fn [^ints choices] 
                   (let [^chars cs1 (get-new-cs choices)]
                     (_indent-cost cs1 line-length-scale)))
        nc (count choice-ixs)
        ^ints choices (make-array Integer/TYPE nc)]
    ; Many, many ways to improve this algorythim:
    (loop []
      ; Sequentially change each choice and keep any improvements.
      (let [energy0 (get-cost choices)
            energy1 (loop [e energy0 ix 0]
                       (if (= ix nc) e
                          (let [ci (aget ^ints choices ix)
                                _ (aset ^ints choices ix (- 1 ci))
                                e1 (get-cost choices)]
                            (if (> e1 e) (aset ^ints choices ix ci)) ; undo the bad choice.
                            (recur (min e1 e) (inc ix)))))]
        (if (< energy1 energy0) (recur) "Done with optimization.")))
    (get-new-cs choices)))

(defn indent [str-or-code & target-len]
  "Automatically newlines and indents the code (removing old newlines if they aren't needed).
   Use for visualizing code that has undergone substantial transformations, or debugging macro code.
   If a string: It is read into blit-code, preserving the reader-macros, etc.
   TODO: the algorythim to determine the best place to put newline is not good at all."
  (let [target-len (if (first target-len) (first target-len) 50) ; scales the line-length but not 1:1 exactly.
        ; Accept both strings and unblitted code:
        bcode (_remove-extra-space (blitcode/reads-string-blit (if (string? str-or-code) str-or-code (grammer/code-to-str str-or-code))))
        bcode-sp (blitcode/blit-decrowd bcode)
        ^String s (blitcode/blit-to-str bcode-sp)
        p (blitcode/newline-parse s true)
        ^chars indented-cs (_optimize-indent (.toCharArray s) (:level p) (:depth p) (:breakable p) target-len)]
    (apply str indented-cs)))
