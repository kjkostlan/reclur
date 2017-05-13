; Tools to read a string into blitted code that preserves the format, modify
; the code, and go back to a string,
; The whole point is to allow refactoring without wiping the original format.
; DOES NOT SUPPORT the new 1.9 Map namespace syntax (TODO). As of April 8 2017 1.9 is still alpha.

(ns clooj.coder.blitcode
 (:require [clojure.string :as string] [clooj.coder.grammer :as grammer]
           [clooj.collections :as collections] [clojure.pprint :as pprint]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Jumper functions that return the cursor index after the jump is complete.
;;;;; Each function takes (^chars cs ^int ix0 ^int n), where ix0 is the start of the whatever and n is the len of cs.
;;;;; The function returns the index one after the last index of said token.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn space-jump [^chars cs ix0 n]
  ; Returns the index after the end of empty space (if ix0 is not
  ; in empty space it returns ix0).
  (loop [ix (int ix0)]
    (if (>= ix (int n)) ix
      (let [c (aget ^chars cs ix)]
        (if (not (grammer/ccom-white? (aget ^chars cs ix))) ix
          (recur (inc ix)))))))

(defn sym-kwd-jump [^chars cs ix0 n]
  ; Standard fare 101.
  (loop [ix (int ix0)]
    (if (>= ix (int n)) ix
      (let [c (aget ^chars cs ix)]
        (if (= (aget ^ints grammer/sym-kwd-stop (int c)) 1) ix
          (recur (inc ix)))))))

(defn string-jump [^chars cs ix0 n] ; Get the ending quote but watch out for escaped chars!
  (loop [escape (int 0) ix (int (inc ix0))]
    (if (>= ix (int n)) ix
      (let [c (aget ^chars cs ix)]
        (if (and (= escape 0) (= c \")) (inc ix) 
          (recur (if (= c \\) (- 1 escape) 0) (inc ix)))))))

(defn num-jump [^chars cs ix0 n]
  ; Only slightly different than kyds or syms.
  (loop [ix (int ix0)]
    (if (>= ix (int n)) ix
      (let [c (aget ^chars cs ix)]
        (if (or (= (aget ^ints grammer/sym-kwd-stop (int c)) 1)
                (grammer/creader? c)) ix
          (recur (inc ix)))))))

(defn char-jump [^chars cs ix0 n]
  ; Most literals are only 1 char long.
  ; The exceptions = (filterv #(> (count %) 2) (mapv #(pr-str (char %)) (range 65535)))
    ;["\\backspace" "\\tab" "\\newline" "\\formfeed" "\\return" "\\space"]
  (let [^char c1 (if (< ix0 (- n 1)) (aget ^chars cs (+ ix0 1)) \x)
        ^char c2 (if (< ix0 (- n 2)) (aget ^chars cs (+ ix0 2)) \x)]
    (min n 
      (cond (and (= c1 \b) (= c2 \a)) 
                    (+ ix0 10) ;\backspace
          (= c2 \a) (+ ix0  4) ;\tab
          (and (= c1 \n) (= c2 \e)) 
                    (+ ix0  8) ;\newline
          (= c2 \o) (+ ix0  9) ;\formfeed
          (= c2 \e) (+ ix0  7) ;\return
          (= c2 \p) (+ ix0  6) ;\space
          :else     (+ ix0  2) ;\1,\x,...
    ))))
; nil, booleans, keywords: uses the symbol jumper at the top.

(defn open-jump [^chars cs ix0 n] (inc ix0)) ; lists, vectors, and maps use simple brackets.
(defn close-jump [^chars cs ix0 n] (inc ix0))

(defn comment-jump [^chars cs ix0 n]
  (loop [ix (int ix0)]
    (if (>= ix (int n)) ix
      (let [c (aget ^chars cs ix)]
        (if (= c \newline) (inc ix) ; include the newline in the comment token (self-contained). Multi-line comments are seperate tokens.
          (recur (inc ix)))))))

(defn rmacro-jump [^chars cs ix0 n] ; Reader macros.
  ; A few reader macro dispatches (i.e. the use of the #) aren't converted, most notably hash-sets:
       ; (= (pr-str (read-string "#{1 2}")) "#{1 2}") but (not= (read-string "#{1 2}") (read-string "(hash-set 1 2)"))
  ; This function lumps multible reader macros together.
  ; Some reader macros, like quotes, CAN contain comments.
  ; Character literals are NOT considered reader macros.
  ; Includes reader conditionals and read-eval but doesn't actually evaluate them.
  (loop [ix (int ix0)]
    (if (>= ix (int n)) ix
      (let [c (aget ^chars cs ix)]
        (if (and (> ix 0) (= (aget ^chars cs (dec ix)) \#) ;The # dispatches:
              (or (= c \:) (= c \_) (= c \?) (= c \!) (= c \<))) (recur (inc ix))
              ; the start of a collection, keyword, string, token, or number:
          (if (or (grammer/copen? c) (= (aget ^ints grammer/sym-kwd-start (int c)) 1) (grammer/cnumber? c) (= c \") (= c \\)) ix 
            (if (= c \;) (recur (comment-jump cs ix n)) ; Comments splitting between a ' and xyz, very rare case!
              (recur (inc ix)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; The tokenizer which generates arrays from the syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Token id list:
; 0 = empty space, 1 = symbols, 2 = keywords, 3 = strings, 4 = numbers, 5 = char literls, 
; 6 = open ([{, 7 = close }]), 8 = comments, 9 = reader macros.

(defn tokenize [^String s]
  "Tokenizes the string s, giving back arrays 1:1 with s. Does not yet evaluate the token's value.
   ^ints :token-type = what is each token's value.
     Includes regexp and hash-sets as reader macros for now, they get changed later.
   ^ints :token-index = counts upward, incementing at each new token.
   ^chars :chars = (.toCharArray s) convenience field.
   25 ns/char for ~5kb strings."
  (let [^chars cs (.toCharArray s) n (int (count cs))
        ^ints token-type (make-array Integer/TYPE n) ; What tokens we belong to.
        ^ints token-index (make-array Integer/TYPE n) ; 0001112223334567788... as the tokens pass.
        ix0 (space-jump cs 0 n)] ; we start as if we are in empty space.
    (loop [ix (int ix0) tix (int (if (> ix0 0) 1 0))] ; one run of the loop per token.
       (if (>= ix n) {:token-type token-type :token-index token-index :chars cs}
         (let [ci (aget ^chars cs ix)
               ; Switchyard:
               id (int (cond (grammer/ccom-white? ci) 0
                        (= ci \:) 2 (= ci \") 3
                        (grammer/cnumber? ci) 4 (= ci \\) 5 
                        (grammer/copen? ci) 6(grammer/cclose? ci) 7
                        (= ci \;) 8 (grammer/creader? ci) 9
                        :else 1)) ; symbols bieng a catch-all.
               ; Next index:
               nx (cond (= id 0) (space-jump cs ix n)
                        (or (= id 1) (= id 2)) (sym-kwd-jump cs ix n)
                        (= id 3) (string-jump cs ix n) (= id 4) (num-jump cs ix n)
                        (= id 5) (char-jump cs ix n) (= id 6) (open-jump cs ix n)
                        (= id 7) (close-jump cs ix n) (= id 8) (comment-jump cs ix n)
                        (= id 9) (rmacro-jump cs ix n)) nx (int nx)]
           (loop [jx (int ix)] ; Set the token-type and token-index arrays.
             (if (< jx nx) ; nx is the first index after the token.
               (do (aset ^ints token-type jx id)
                   (aset ^ints token-index jx tix)
                   (recur (inc jx)))))
           (recur nx (inc tix)))))))

(defn basic-parse [s]
  "Tokenizes + adds ^ints :inter-depth and ^ints :inter-depth-no-rmacro field.
   inter-depth is interstitial: it has one more element than (count s)
     and the i'th element is the depth of a cursor between the i-1'th and i'th char.
   The depth includes any macro characters in as part of the () they are attached to.
   :inter-depth-no-rmacro like inter-depth but does not include the reader macros
   About 50 ns/char for a 5kb string."
  (let [tk (tokenize s) ^chars cs (:chars tk) n (count cs)
        ^ints ty (:token-type tk) ^ints tix (:token-index tk)
        ^ints inter-depth (make-array Integer/TYPE (inc n))
        ^ints inter-depth-no-rmacro (make-array Integer/TYPE (inc n))]
    ; Depth field: counting paranethesis.
    (loop [ix (int 0) l (int 0)]
      (if (< ix n)
        (let [ti (aget ^ints ty ix)
              ; open and closing syntax is only one level (we haven't yet added macros):
              l1 (int (if (= ti 6) (inc l) (if (= ti 7) (max (dec l) 0) l)))] 
          (aset ^ints inter-depth-no-rmacro (inc ix) l1) ; the decrement is delayed one char.
          (recur (inc ix) l1))))
    (loop [ix (int 0)] ; Array copy step.
      (if (<= ix n) (do (aset ^ints inter-depth ix (aget ^ints inter-depth-no-rmacro ix)) (recur (inc ix)))))
    ; Add 1 for reader-macros before an indent, including hash-sets.
    (if (> n 0)
      (loop [ix (int (dec n)) d (int (aget ^ints inter-depth (int (dec n))))] ; backwards.
        (if (>= ix 0)        ;d is what depth a reader macro here would do.
          (let [ti (aget ^ints ty ix)]
            (if (= ti 9) (aset ^ints inter-depth (inc ix) (int d)))
            (recur (dec ix) (if (= ti 6) (aget ^ints inter-depth (inc ix)) ; depth after the open (
                              (if (not= ti 9) (aget ^ints inter-depth ix) (int d)))))))) ; depth here if not 9
    (assoc tk :inter-depth inter-depth :inter-depth-no-rmacro inter-depth-no-rmacro)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; The vectorizer which takes parse-arrays and makes a recursive, vector form.
;;;;;;;;;;;; Reader-macros are stored as part of the :head and aren't expanded.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn depth-bracket [^ints depth ix0 dir]
  "Finds the locally-first or last ix that matches the depth of ix0. dir = -1 or +1 for left or right."
  (let [n (alength ^ints depth) d0 (aget ^ints depth ix0)] 
    (loop [ix (int ix0)]
      (if (or (>= ix n) (< ix 0) (< (aget ^ints depth ix) d0)) (- ix dir)
        (recur (+ ix dir))))))

(defn token-end [^ints tix ix0 dir]
  "The last index of the token, inclusive, given a token index."
  (let [n (alength ^ints tix) t0 (aget ^ints tix ix0)]
    (loop [ix (int ix0)]
      (if (or (>= ix n) (< ix 0) (not= (aget ^ints tix ix) t0)) (- ix dir) (recur (+ ix dir))))))

(def _comment (keyword (str "clooj.coder.blitcode_comment" "1234321232121")))
(def _err (keyword (str "clooj.coder.blitcode_error" "1234321232121")))

(defn _parse-t-piece [^chars cs ^ints tix ^ints tys ^ints inter-depth n start-ix end-ix] ; indexes are inclusive.
  (let [ty (aget ^ints tys start-ix)
        ; After the reader macro:
        start-ix1 (int (if (= ty 9) (inc (token-end tix start-ix 1)) start-ix))

        ; What type we are starting in, after the reader macro:
        ty1 (aget ^ints tys start-ix1)

        ; The reader macro head, empty if there is no reader macro:
        rmac (collections/asus cs start-ix start-ix1)

        ^String body (collections/asus cs start-ix1
                       (inc (min end-ix (token-end tix start-ix1 1))))]
    ; A big switchyard depending on the type of the token (number, string, etc).
    (cond (and (< ty1 6) (> ty1 0)) ; leaf objects. Read-str would be easier but it was easy enough to write the implementation it all ourselves just for fun.
         {:head rmac :tail body :obj (cond (= ty1 1) (symbol body) (= ty1 2) (keyword (collections/sus body 1)) (= ty1 3) body
                                       (= ty1 4) (let [zp #(str (if (= (first %) \0) "" "0") %)
                                                       body (zp body) ; initial pad to avoid empty exceptions.
                                                       bulast (apply str (butlast body)) lst (if (last body) (last body) \a)
                                                       split-dash (let [x (string/split body (re-pattern "/"))] (mapv zp x))
                                                       nums-only #(.replaceAll ^String % "[^0-9]" "")
                                                       nums-edot (fn [s] (let [s1 (.replaceAll ^String s "[^0-9e\\.]" "") n (count s1)]
                                                                           (loop [acc [] ix (int 0) dot? false e? false]
                                                                             (if (= ix n) (apply str acc)
                                                                               (let [ci (nth s1 ix)]
                                                                                 (cond (= ci \e) (recur (if e? acc (conj acc ci)) (inc ix) dot? true)
                                                                                   (= ci \.) (recur (if dot? acc (conj acc ci)) (inc ix) true e?)
                                                                                   :else (recur (conj acc ci) (inc ix) dot? e?)))))))
                                                       iparse #(let [bi (bigint %)] (if (or (< bi Long/MIN_VALUE) (> bi Long/MAX_VALUE)) bi (long bi)))]
                                                   (cond (= lst \N) (bigint (nums-only bulast))
                                                         (= lst \M) (bigdec (nums-edot bulast))
                                                         (> (count split-dash) 1) (let [nu (iparse (first split-dash)) den (iparse (second split-dash))]
                                                                                    (if (= den 0) _err (/ nu den)))
                                                         (.contains body ".") (Double/parseDouble (nums-edot body)) :else (iparse body)))
                                       (= ty1 5) (cond (= body "\\newline") \newline (or (= body "\\ ") (= body "\\space")) \  (= body "\\backspace") \backspace
                                                       (= body "\\formfeed") \formfeed (= body "\\tab") \tab (= body "\\return") \return 
                                                   :else (if (second body) (second body) (char 0))))}
         (= ty1 0) {:head rmac :tail body} ; empty space => nothing.
         (= ty1 8) {:head rmac :tail body :obj _comment}
         (or (= ty1 7) (= ty1 9)) {:head rmac :tail body :obj _err} ; An error that prevented us from jumping past a reader macro.
         (= ty1 6) ; Open parenthesis tricky recursive code.
         (let [start-ix2 (inc (token-end tix start-ix1 1)) ; the stuff inside the ( starts here. 
               d0 (aget ^ints inter-depth start-ix2)
               ; There is at least one valid token-start b/c of the closing ) unless the string is wrong.
               token-starts (loop [acc [start-ix2] ix (int start-ix2)]
                              (let [d (aget ^ints inter-depth (inc ix)) tyi (aget ^ints tys ix)
                                    ix1 (inc (if (> d d0) (depth-bracket inter-depth (inc ix) 1) ;Collections (maybe with a macro) go one index past the closing ).
                                             ; Go to the start of the next token (or next next token if we have reader macros):
                                             (token-end tix (if (= tyi 9) (min (inc (token-end tix ix 1)) (dec n)) ix) 1)))]
                                (if (> ix1 end-ix) acc (recur (conj acc ix1) ix1))))]
           {:head (str rmac body) :tail (collections/asus cs (last token-starts) (inc end-ix))
            :obj (mapv #(_parse-t-piece cs tix tys inter-depth n %1 (dec %2)) (butlast token-starts) (rest token-starts))}))))
(defn _lump [bcodev] ; Lumps whitespace and comments into the whatever.
  (let [o (:obj bcodev)]
    (if (vector? o) ; Look at stuff inside o.
        (let [o (mapv _lump o) ; recursive.
              sp? (mapv #(let [oi (:obj %)] (or (nil? oi) (= oi _comment) (= oi _err))) o) n (count o)
              ; The first group is non-coding DNA only, unless it's empty it will add to the head:
              groups (reduce (fn [acc ix]
                               (let [oi (nth o ix)]
                                 (if (nth sp? ix) (update acc (dec (count acc)) #(conj % oi)) 
                                     (conj acc [oi])))) [[]] (range n))
              stry (fn [gi] (apply str (:head gi) (mapv #(str (:head %) (:tail %)) gi)))
              ; Exclude the first group in the object:
              o1 (mapv (fn [gi] (let [gi0 (first gi)]
                                  (if (> (count (:head gi0)) 0) 
                                    (assoc gi0 :tail (str (:tail gi0) (stry (rest gi))))
                                    (assoc gi0 :tail (stry gi))))) 
                   (rest groups))
              ; Instead put the first group in the head:
              h1 (str (:head bcodev) (stry (first groups)))]
           (assoc bcodev :obj o1 :head h1)) 
      bcodev)))
(defn parse-to-vcode [p] ; p = a basic parse.
  "Reads string into the vectorized format of blitted code, should be lossless to the original string.
   :head = the opening (, also any macros (including hash-sets, meta-data), etc.
   :tail = the body and the closing ).
     Both :head and :tail can include comments, space, etc.
   :obj = a vector of any children objects."
  (let [n (alength ^chars (:chars p))
        bcodev (_parse-t-piece (:chars p) (:token-index p) (:token-type p) (:inter-depth p) n 0 (dec n))]
    (_lump bcodev)))
(defn reads-string-vcode [s] (parse-to-vcode (basic-parse (str "[" s "\n]"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; The blitter which takes the vectorized code, applis macros, packs meta, and changes the map type.
;;;;;;;;;;;; The reader macros are still stored in the :head but they are  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Some reader macro functions must be stored as symbols, but they cannot be called:
(defn _x [] (throw (Exception. "Reader macros symbols be called directly as functions."))) 
(def syntax-quote _x) (def meta-tag _x) 
(def anon-fn _x) (def read-eval _x) (def condition _x) (def ignore _x) (def map-ns _x) ; anon-fns are later expanded.
(def condition-splicing _x) (def hash-literal _x) (def regex-literal _x) ; literals are later expanded.
(defn rmacro-expand [^String head-plus-tail]
  "Expands a reader-macro into a :strings and :symbols. The string may include whitespace and intra-macro-comments."
  (let [^chars cs (.toCharArray head-plus-tail) n (alength ^chars cs) ix1 (rmacro-jump cs 0 n)]
    (loop [strs [] syms [] ti [] yi nil ix (int 0)] ; ti = the growing string. yi = the symbol.
      (if (>= ix ix1) (let [strs1 (if (nil? yi) strs (conj strs (apply str ti))) syms1 (if (nil? yi) syms (conj syms yi))]
                        {:strings (if (> (count strs1) 0) (assoc (into [] (rest strs1)) 0 (str (first strs1) (second strs1))) []) 
                         :symbols (into [] (rest syms1))}) ; The first string is extra space and symbol is nil.
        (let [c (aget ^chars cs ix)]
          (cond (grammer/ccom-white? c) (recur strs syms (conj ti c) yi (inc ix)) ; bridging mode.
                (= c \;) (let [ixe (comment-jump cs ix n)] 
                           (recur strs syms (apply conj ti (into [] (collections/asus cs ix ixe))) yi ixe))
                :else
                (let [c1 (if (< (inc ix) n) (aget ^chars cs (inc ix)) (char \a)) ; deciding mode.
                      c2 (if (< (+ ix 2) n) (aget ^chars cs (+ ix 2)) (char \a))
                      yix1 (cond (= c \') ['quote (inc ix)] (= c \`) [`syntax-quote (inc ix)]
                                 (= c \^) [`meta-tag (inc ix)] (= c \@) [`deref (inc ix)] ; any unquote-splicing skips over the @.
                                 (= c \~) (if (= c1 \@) [`unquote-splicing (+ ix 2)] [`unquote (inc ix)])
                                 (= c \#)
                                 [(cond (= c1 \_) `ignore (= c1 \() `anon-fn (= c1 \:) `map-ns
                                        (= c1 \') 'var (= c1 \=) `read-eval (= c1 \?) (if (= c2 \@) `condition-splicing `condition) 
                                        (= c1 \{) `hash-literal (= c1 \") `regex-literal)
                                  (+ ix (if (and (= c \#) (= c1 \?) (= c2 \@)) 3 2))])
                      yix1 (if yix1 yix1 [_err (inc ix)]) ; null case oops.
                      ti1 (into [] (collections/asus cs ix (second yix1)))] ; the string we just advanced across.
                  (recur (conj strs (apply str ti)) (conj syms yi) ti1 (first yix1) (second yix1)))))))))
(defn _rmacro-extract [vcode]
  "Adds (recursivly) :rmacro which has :symbols and :strings, iff there are reader macros (including hash-sets and regexp)."
  (let [vec? (vector? (:obj vcode)) rmac (rmacro-expand (if vec? (:head vcode) (str (:head vcode) (:tail vcode))))
        vcode (if vec? (assoc vcode :obj (mapv _rmacro-extract (:obj vcode))) vcode)]
    (if (> (count (:strings rmac)) 0) (assoc vcode :rmacro rmac) vcode)))
(defn _unpack-anon-fns [blit-obj]
  ; Unpacks #(%1 %2 ...) functions, blit is the :obj at the #() level.
  ; There is no need to replace the % symbols they are valid symbols.
  (let [deeper-symbols (filterv symbol? (mapv #(collections/gett-in blit-obj %) (collections/paths-walked blit-obj)))
        index (fn [sym] (cond (= sym '%) 1 (= (first (str sym)) \%) (Integer/parseInt (collections/sus (str sym) 1)) :else 0))
        nargs (apply max 0 (mapv index deeper-symbols))] ; One-based array going on for the fn args.
    ; It goes: fn* [%1 %2 %3...] (body) but tiwh :obj's of course.
    (list {:obj 'fn*} {:obj (mapv #(hash-map :obj (symbol (str "%" %))) (range 1 (inc nargs)))} {:obj blit-obj})))
(defn _meta-pack [vcode-r] ; vcode-r = vcode + reader macros encoded
  "Puts metadata into a :meta category, unpacking the :tag as well."
  (let [o (:obj vcode-r)]
    (if (vector? o)
      (let [o (mapv _meta-pack o) ; recursive. Metadata can be inside of other meta-data.
            r-strings (mapv #(:strings (:rmacro %)) o) r-symbols (mapv #(:symbols (:rmacro %)) o)
            is-meta? (mapv #(= (last %) `meta-tag) r-symbols)] ; Error if metadata is not applied last.
        (assoc vcode-r :obj
          (loop [occ [] ix (int 0)] 
            (if (>= ix (count o)) occ
              (if (nth is-meta? ix) ; Meta-things attach to the next object.
                (let [mstuff (nth o ix) target (get o (inc ix)) 
                      target (if (nil? target) {:obj _err :begin "" :end ""} target) ; _err if nothing to attach to.
                      ; The reader macros attached to the meta become attached to the target:
                      meta-rstrs (:strings (:rmacro mstuff)) meta-syms (:symbols (:rmacro mstuff))
                      target-rmacro {:strings (into [] (concat (butlast meta-rstrs) (:strings (:rmacro target))))
                                     :symbols (into [] (concat (butlast meta-syms) (:symbols (:rmacro target))))}
                      mstuff (dissoc mstuff :rmacro) ; No reader macros will be attached if the metadata is involved.
                      mstuff (update mstuff :obj #(if (coll? %) % {{:obj :tag} {:obj %}}))] ; Expand the :tag shorthand.
                  (recur (conj occ (assoc target :meta mstuff :rmacro target-rmacro)) (+ ix 2))) ; Non-meta must always be after meta.
                (recur (conj occ (nth o ix)) (inc ix))))))) vcode-r)))
(defn _vcode-to-blit [vcode-rm] ; vcode-rm = vcode + reader macros encoded + meta packing.
  (let [vcode-rm (if (coll? (:meta vcode-rm)) (update vcode-rm :meta _vcode-to-blit) vcode-rm)
        o (:obj vcode-rm)
        total-n-r (reduce + (mapv count (:strings (:rmacro vcode-rm))))
        rmacro (:rmacro vcode-rm)
        ; Recursive and sets the type of o.
        o (if (vector? o) ; Use the opening character (after the rmacro part) to determing the type.
            (let [o (mapv _vcode-to-blit o) ;recursive.
                  open-c (get (str (:head vcode-rm) (:tail vcode-rm)) (- total-n-r (if (grammer/copen? (last (last (:strings (:rmacro vcode-rm))))) 1 0)))
                  set? (and (= open-c \{) (= (last (:symbols (:rmacro vcode-rm))) `hash-literal))
                  oo (fn [o] (mapv #(assoc %1 :order %2) o (range)))] ; order the macro.
              (cond set? (apply hash-set (oo o))
                    (or (= open-c \{) (= open-c \^)) (let [o1 (oo o) o1 (if (even? (count o1)) o1 (conj o1 {:obj _err :order (count o1)}))] 
                                                       (zipmap (collections/evens o1) (collections/odds o1)))
                    (= open-c \[) o :else (apply list o))) o) ; List is the default if it isn't recognized (i.e. reader macros).
        ; Apply any macro expansions to o, inside-out. Most convert foo into (symbol foo).
        blito (reduce (fn [occ sym] (cond
                                      (= sym `anon-fn) (_unpack-anon-fns occ)
                                      (= sym `map-ns) (throw (Exception. "TODO for 1.9: Map-namespace syntax."))
                                      (= sym `regex-literal) (re-pattern occ) 
                                      :else (list {:obj sym} {:obj occ})))
                o (reverse (filterv #(not= % `hash-literal) (:symbols rmacro))))]
    (assoc (if (:rmacro vcode-rm) (assoc vcode-rm :rmacro (:strings (:rmacro vcode-rm))) vcode-rm) :obj blito))) ; store macros as strings 
(defn vcode-to-blit [vcode]
  "Lossless conversion from the vcode to the bcode format,
   the blit format is 1:1 with what (read-string (str [ s newline ])) would do.
   :rmacro holds the string(s) that this level's reader macro cooresponds to, if there is any.
     i.e. for :obj = (quote x y) :rmacro would be the string ' (with possible added spaces).
     It is stored in the outer level. :head still contains the macro strings.
   :order, holds the order for objects inside sets and maps."
  (_vcode-to-blit (_meta-pack (_rmacro-extract vcode))))
(defn reads-string-blit [s] 
  "Read-string but wrapping s into a vector format.
   The initial [ and ending newline will have to be removed as the final step."
(vcode-to-blit (parse-to-vcode (basic-parse (str "[" s "\n]")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Conversion of the blitted code back to vectorized code ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn break-up-by-token [s ^ints token-index]
  "Breaks up the cs array into an array of tokens given token-index."
  (let [n (alength ^ints token-index)
        breaks (loop [acc [0] ix (int 0) tx (int (if (= n 0) 0 (aget ^ints token-index (int 0))))]
                 (if (= ix n) (conj acc n)
                   (let [ti (aget ^ints token-index ix)]
                     (recur (if (> ti tx) (conj acc ix) acc) (inc ix) ti))))]
    (if (= n 0) [] (mapv #(subs s %1 %2) breaks (rest breaks)))))

(defn mash-tokens-of-ty [s ty]
  (let [^ints tty (:token-type (tokenize s)) ty (int ty) n (count s)]
    (loop [acc [] ix (int 0)]
      (if (= ix n) (apply str acc)
        (recur (if (= (aget ^ints tty ix) ty) (conj acc (nth s ix)) acc) (inc ix))))))

(defn blit-to-code [blit]
  "Converts blitted code to regular code. Useful to check for map duplications."
  (let [o (:obj blit)] (if (collections/col? o) (collections/cmap :flatten blit-to-code o) o)))

(defn _comment-space-jump [^chars cs ix0 n]
  (if (= (aget ^chars cs ix0) \;)
    (comment-jump cs ix0 n) (space-jump cs ix0 n)))


(def _sym2mstr {`syntax-quote "`" `meta-tag "^" `anon-fn "#" `read-eval "#=" `condition "#?"
                 `ignore "#_" `map-ns "#" `condition-splicing "#?@" `hash-literal "#" `regex-literal "#"
                 'quote "'" `unquote "~" `unquote-splicing "~@" `deref "@"})
(defn _mreader-str-proj [r reader-symbols]
  "Projects r onto the space of reader-macros. Does not include the last ("
  (let [x (rmacro-expand r) st (:strings x) sy (:symbols x)]
    (if (= sy reader-symbols) r ; It agrees with the expansion. No change needed.
      ; Pointwise substitute projection:
      (let [out-pieces (mapv #(if (= (get sy %) (nth reader-symbols %)) (nth st %) 
                                (get _sym2mstr (nth reader-symbols %))) (range (count reader-symbols)))
            ; An unquote and deref is not an unquoute splice:
            out-pieces (mapv #(let [bump? (and (not (grammer/ccom-white? (last %1))) (not (grammer/ccom-white? (first %2)))) ; no space inbetween.
                                    pair? (or (and (= %3 `unquote) (= %4 `deref)) (and (= %3 `condition) (= %4 `deref)))] ; A pairing that can fuse.
                                (if (and bump? pair?) (str %1 " ") %1)) 
                        out-pieces (concat (rest out-pieces) [""] ) reader-symbols (concat (rest reader-symbols) ['xyz]))]
        (apply str out-pieces)))))
(defn leaf-project [s reader-symbols val]
  "Projects s to v. S is a leaf string OR the opening (INCLUDING the reader-macro chars) or closing of the reader-symbols.
   v is a non-collection or a hash-set with a single open/close character.
   reader-symbos is a vector of reader macros that prefix s.
   v can be a hash-set with one element, the character, to look for opening and closing (),[],{}."
  (let [p (tokenize s) ^chars cs (:chars p)
        ^ints ty (:token-type p) ^ints tix (:token-index p) n (alength ^ints ty)
        tokens (break-up-by-token s tix) nt (count tokens)
        token-types (loop [acc (into [] (repeat nt 0)) ix (int 0)]
                      (if (= ix n) acc
                        (recur (assoc acc (aget ^ints tix ix) (aget ^ints ty ix)) (inc ix))))
        ; space/comment, reader, object, space/comment 
        x (loop [ix (int 0) sp0 [] r [] o [] sp1 []] ; vectors of strings.
            (if (= ix nt) {:sp0 (apply str sp0) :r (apply str r) :o (apply str o) :sp1 (apply str sp1)}
              (let [t (nth tokens ix) y (nth token-types ix) ysp? (or (= y 0) (= y 8)) f0 (and (= r []) (= o []))]
                (recur (inc ix) (if (and ysp? f0) (conj sp0 t) sp0)
                  (if (= y 9) (conj r t) r) (if (and (not= y 9) (not ysp?)) (conj o t) o)
                  (if (and ysp? (not f0)) (conj sp1 t) sp1)))))
        ; Always false when it an an open or closing char, but that spurious error is OK:
        val=? (= val (try (first (read-string (str "[ " (:o x) "\n]"))) (catch Exception e e))) ; 1 not= 1.0 (different types), but 1.0 = 1.00 = 1.0e0 even thoug diff strings.
        val-str (cond (= val #{\(}) "(" (= val #{\[}) "[" (= val #{\{}) "{"
                      (= val #{\)}) ")" (= val #{\]}) "]" (= val #{\}}) "}" :else val)]
   (str (:sp0 x) (_mreader-str-proj (:r x) reader-symbols) (if val=? (:o x) val-str) (:sp1 x))))

(defn _macro-extract [blit]
  ":val = what was extracted. :symbols = hirararchy of macro symbols. 
   Does NOT use the string info except to check for use of :body."
  (loop [val blit symbols []]
    (let [o (:obj val)]
      (cond (set? o) {:val val :symbols (conj symbols `hash-literal)} ; macros that don't really ever expand.
            (instance? java.util.regex.Pattern o) {:val val :symbols (conj symbols `regex-literal)}
        (not (list? o)) {:val val :symbols symbols} ; only on lists can macros create more levels.
        :else ; lists, which can be reader-expansions of something.
        (let [o0 (first o) o1e? (= (count (str (:head o0) (:tail o0))) 0)] ; no :head and :tail => reader macro.
          (cond ; Only apply the reader macro if it is valid to do so AND we want to.
            (not o1e?) {:val val :symbols symbols}; stuff in the :head or :tail => we don't want a reader macro.
            (and (= (count o) 2)
              (get #{`map-ns 'var `read-eval 'quote `syntax-quote `meta-tag `deref  `unquote-splicing  `unquote `ignore} (:obj o0)))
            (recur (second o) (conj symbols (:obj (first o)))) ; standard-issue macros.
            (let [fn-maby (:obj (first o)) %vec-maby (:obj (second o)) body-maby (:obj (second (rest o)))
                  %vec-check (if (vector? %vec-maby) (mapv #(and (symbol? (:obj %1)) (= (str (:obj %1)) (str "%" %2))) %vec-maby (range 1 1e100)))]
              (and (= (count o) 3)
                (#(or (= % 'fn) (= % `fn) (= % 'fn*)) fn-maby)
                (vector? %vec-maby) (= (count (filterv not %vec-check)) 0)
                (list? body-maby))) ; anon-functions can be a pain.
             {:val (nth o 2) :symbols (conj symbols `anon-fn)} ; Don't recur, Anon-fns are always the deepest in.
             :else {:val val :symbols symbols})))))) ; failed to ger a macro.

(defn _vec-and-pullmetamacro [blit]
  "Makes the blit code's :obj into a vector. Shallow function, does not act recursivly.
   Projects thier :head's and :tail's of each element of :obj.
   Also pulls out the element's meta and macros.
   The blit itself is assumed to have gone through this process. This allows recursive functions:
     The outer level itself is a vector with no rmacros nesting things, etc, and we apply this fn to it to get it's children that way and so on."
  (let [ordr #(into [] (sort-by :order (into [] %))) o (:obj blit)
        ov (cond (vector? o) o (set? o) (ordr (into [] o)) (map? o) (ordr (concat (keys o) (vals o))) (sequential? o) (into [] o)) ; Vector form.
        ovm (mapv :meta ov)
        n (count ov) ov-extracts (mapv _macro-extract ov)
        thier-rmacros (mapv #(mash-tokens-of-ty (if (coll? (:obj %)) (str (:head %)) (str (:head %) (:tail %))) 9) ov)
        thier-meta-rmacros (mapv #(if (:meta %) (mash-tokens-of-ty (if-let [_x (:head (:meta %))] _x "") 9) "") ov) ; Includes the ending ^
        ; Reader macros attached to the metadata will fall onto whatever the metadata is attached to.
        ; We have to split them back up but only if they actually do agree with the actual object.
        rmacro-symbols (mapv :symbols ov-extracts) ; The ground-truth what reader macros we have.
        thier-rmacro-symbols-meta (mapv #(:symbols (rmacro-expand %)) thier-meta-rmacros) ; Two halfs of what may assemble to ground-truth, or may not.
        thier-rmacro-symbols-not-meta (mapv #(:symbols (rmacro-expand %)) thier-rmacros)
        macro-agree?s (mapv #(= (into [] (concat %1 %2)) (into [] (concat %3 [`meta-tag]))) ;Do the halfs assemble?
                         thier-rmacro-symbols-meta thier-rmacro-symbols-not-meta rmacro-symbols)]
    (assoc blit :obj 
      (loop [acc [] ix (int 0)]
        (if (= ix n) acc
             (let [ovi (nth ov ix) rm-agree? (nth macro-agree?s ix)
                   ovei (:val (nth ov-extracts ix)) mi (nth ovm ix) c? (coll? (:obj ovei)) ; for not c? it doesn't matter head vs tail.
                   xi (if c? (cond (vector? (:obj ovei)) [#{\[} #{\]}] (or (map? (:obj ovei)) (set? (:obj ovei))) [#{\{} #{\}}] :else [#{\(} #{\)}]))
                   rm-syms (nth rmacro-symbols ix) ; Reader macros applying to us, including the metadata.
                   ; rm-agree means (concat thier-rmacro-symbols-meta[ix] thier-rmacro-symbols-not-meta[ix]) = rmacro-symbols[ix]
                   ;   which means we should use the :head's to divide up which reader-macros were attached to the meta vs the main token.
                   ;   If it disagrees we will default and attach all the reader-macros to the main token except the ^.
                   rm-syms (if rm-agree? (nth thier-rmacro-symbols-not-meta ix) (nth rmacro-symbols ix)) ; These don't go on b4 meta symbol.
                   rm-syms-meta (if rm-agree? (nth thier-rmacro-symbols-meta ix) [`meta-tag]) ; These do.
                   h (if c? (leaf-project (:head ovi) rm-syms (first xi)) "") ; head and tail.
                   t (if c? (leaf-project (:tail ovi) [] (second xi)) (leaf-project (str (:head ovi) (:tail ovi)) rm-syms (:obj ovei)))
                   short-hand-meta? (and (map? (:obj mi)) (= (mapv :obj (keys (:obj mi))) [:tag]) 
                                     (= (count (mash-tokens-of-ty (str (:head mi) (:tail mi)) 6)) 0)) ; The shortcut is both requested and legal.
                   mo (if mi (if short-hand-meta? (:obj (get (:obj mi) {:obj :tag})) (:obj mi)))
                   hm (if mi (if short-hand-meta? (leaf-project (str (:head mi) (:tail mi)) rm-syms-meta mo)
                               (leaf-project (str (:head mi)) rm-syms-meta #{\{})))
                   tm (if mi (if short-hand-meta? "" 
                               (leaf-project (str (:tail mi)) [] #{\}})))
                   clean #(if (map? %) (dissoc % :rmacro :meta :order) (throw (Exception. (str "Not a map:" %))))
                   add-this (clean {:head h :tail t :obj (:obj ovei)})]
               (if mi (recur (conj acc (clean {:head hm :tail tm :obj mo}) add-this) (inc ix))
                 (recur (conj acc add-this) (inc ix)))))))))

(defn _begin-power [s]
  "How powerful the beginning is at isolating s.
   -1 (end only) = ends in a comment. 0 = none at all. 1 = no space. 2 (beginning only) = built in newline."
  (let [tok (tokenize s) ^ints ty (:token-type tok) ^chars cs (:chars tok) n (count s)]
    (loop [ix (int 0)]
      (if (= ix n) 0 ; off-the-end never should happen for normal code.
        (let [t (aget ^ints ty ix) c (aget ^chars cs ix) oa? (or (= t 0) (= t 8))]
          (cond (and oa? (= c \newline)) 2 ; newline is awesome.
            (not oa?) (if (or (grammer/copen? c) (grammer/cclose? c) (= c \") (> ix 0)) 1 0) ; Started the token.
            :else (recur (inc ix))))))))
(defn _end-power [s] ; and the end. Isolation depends on the end + the next beginning bieng isolating enough.
  (if (nil? s) (throw (Exception. "Null string given.")))
  (let [tok (tokenize s) ^ints ty (:token-type tok) ^chars cs (:chars tok) n (count s)]
    (loop [ix (int (dec n))]
      (if (= ix -1) 0 ; off-the-end never should happen for normal code.
        (let [t (aget ^ints ty ix) c (aget ^chars cs ix) oa? (or (= t 0) (= t 8))]
          (cond (or (= c \newline) (and oa? (grammer/ccom-white? c))) 1
            (and (= t 8) (not= c \newline)) -1 ; Comment entered b4 newline.
            (not oa?) (if (or (grammer/copen? c) (grammer/cclose? c) (= c \")) 1 0)
            :else (recur (dec ix))))))))
(defn _vcode-prevent-smash [vcode]
  "Ensures that the pieces of vcode do not intefere with eachother. Acts recursivly.
   The two main sources of inteference is not having a space when it is nessessary and TODO what else?"
  (let [v? (vector? (:obj vcode))] 
    (if v?
      (let [o (mapv _vcode-prevent-smash (:obj vcode))
            ; Gets the head and tail pieces:
            gh (fn [oi] (str (:head oi) (if (vector? (:obj oi)) "" (:tail oi))))
            gt (fn [oi] (str (if (vector? (:obj oi)) "" (:head oi)) (:tail oi)))

            ; Any children that are collections: the stuff inside is irrelevent.
            bp (mapv #(_begin-power (gh %)) o) ep (mapv #(_end-power (gt %)) o)

            sp (fn [end-s next-begin-s]  ; calculates the space we need to add, if any, between end-s and next-begin-s.
                 (let [endp (_end-power end-s) next-beginp (_begin-power next-begin-s)]
                   (if (>= (+ endp next-beginp) 1) "" (if (= endp -1) "\n" " "))))

            ; Children-children collision prevention:
            spacers (conj (mapv sp (butlast (mapv gt o)) (rest (mapv gh o))) "") ; Spacers go after the tails.
            o (mapv #(assoc %1 :tail (str (:tail %1) %2)) o spacers)
            ;v?s (mapv #(vector? (:obj %)) o)
            
            v1? (and v? (> (count o) 0)) ; if v1? head-first child and last-child-tail, else head-tail collision prvention.
            head (if v1? (str (:head vcode) (sp (:head vcode) (gh (first o)))) (:head vcode))
            tail (if v1? (str (sp (gt (last o)) (:tail vcode)) (:tail vcode)) (str (sp (:head vcode) (:tail vcode)) (:tail vcode)))]
        (assoc vcode :obj o :head head :tail tail)) vcode)))
(defn _naive-str [vcode]
  (let [o (:obj vcode)]
    (str (apply str (:head vcode) (if (vector? o) (mapv _naive-str o) [])) (:tail vcode))))

(defn blit-to-vcode [blit]
  "Should be the perfect inverse of vcode-to-blit.
   In addition it does a macro projection step."
  (if (coll? (:obj blit))
    (let [vb (_vec-and-pullmetamacro blit)]
      (update vb :obj #(mapv blit-to-vcode %))) blit))

(defn _vcode-to-str [vcode]
  "Converts to a string. Beware: the leaf objects should already have been projected (but it projects away squashed tokens and collisions)."
  (let [vcode1 (_vcode-prevent-smash vcode) out (_naive-str vcode1)
        nh (count (:head vcode1)) nt (count (:tail vcode1)); remove the outer [] wrapper.
        out1 (collections/sus out nh (- (count out) nt))]
    (if (= (last out1) \newline) (collections/sus out1 0 (dec (count out1))) out1))) ; remove the trailing \n if present, as we added that as part of the wrapper.

(defn blit-to-str [blit]
  "Converts the blitcode to a string. Extracts metadata and reader macros.
   Projects the result so that it resembles the string demarcated by :head and :tail but
   also produces code equal to the value of :obj. For any VALID .clj source string,
   (blit-to-str (reads-string-blit s)) = s (unless there are bugs!)."
  (_vcode-to-str (blit-to-vcode blit)))

(defn blit-project [blit]
  "Keeps as much of the original string as possible but keeping the meaning that of the new code."
  (reads-string-blit (blit-to-str blit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; A function maybe superseded by the refactor magic ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn extract-outer-islands [text-or-parsed]
  "Extracts outer level islands from a text OR a text analysis. Using subs pulls the island out, inclusive to the edge ()'s."
  (let [parsed (if (map? text-or-parsed) text-or-parsed (basic-parse text-or-parsed)) ; use already-parsed code to avoid having to parse it here.
        levels (:inter-depth parsed) levels (if (vector? levels) levels (into [] levels)) n (count levels)
        n0 (dec n)
        changes
          (loop [zero1 (if (= (first levels) 1) [0] []) one0 [] ix 0]
            (if (= ix n0) {:zero1 zero1 :one0 one0}
                (let [l0 (nth levels ix) l1 (nth levels (inc ix))
                      ; only outer islands:
                      up?   (and (= l0 0) (= l1 1))
                      down? (and (= l0 1) (= l1 0))]
                  (recur (if up? (conj zero1 (inc ix)) zero1) (if down? (conj one0 (inc ix)) one0) (inc ix)))))
        
        zero1 (mapv dec (:zero1 changes)) one0 (if (= (last levels) 1) (conj (:one0 changes) n) (:one0 changes))]
      (mapv #(vector %1 %2) zero1 one0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Pretty printing functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _fix-evens! [^ints breakable ^ints token-ty ix0 ix-last]
  "Sets the even spaces breakable => 0, in-place modification of breakable.
   Does NOT include the spaces at ix0." 
  (loop [ix (int ix0) parity (int 0) last-b (int 0)]
    (if (<= ix ix-last)
      (let [t (aget ^ints token-ty ix) b (aget ^ints breakable ix)]
        (if (and (= t 0) (= parity 0)) (aset ^ints breakable ix (int 0)))
;(println "Check for swap:" )
        (recur (inc ix) (if (and (not= t 0) (not= t 8) (> last-b 0)) (- 1 parity) parity) b)))))
(defn _next-obj [^ints token-ty ix0 n]
  "Returns the next object's [start end] index.
   If we start on the object we will include that."
  (loop [ix (int ix0)] 
    (if (>= ix0 n) [n n]
      (if (let [t (aget ^ints token-ty ix)] (or (= t 0) (= t 8))) (recur (inc ix)) ; not an object => recur.
        (loop [jx (int ix)]
          (if (or (= jx n) (not= (aget ^ints token-ty jx) 1)) [ix (dec jx)] (recur (inc jx))))))))
(defn _next-bracket [^ints token-ty ix0 n]
  "Returns the next opening bracket's index.
   If we start on the bracket we will include that."
  (loop [ix (int ix0)] 
    (if (>= ix0 n) n
      (if (= (aget ^ints token-ty ix) 6) ix ; found it.
        (recur (inc ix))))))
(def _letty-stuff (apply hash-set (mapv str #{`loop 'loop 'let `let 'let* 'if-let `if-let 'when-let `when-let})))
(defn newline-parse [s & even-force?]
  "Same as basic-parse but adds the :breakable [I:
     0 = illegal OR unnessessary places to replace a space with a newline.
     1 = optional, for example for foo bar it would be the space in between.
     2 = manditory (wherever s has new-lines and we are leaving a comment or in a string).
   The second argument (default false) is for keeping breakable zero between pairs of tokens
     in maps and let-statements (does not check for the rare and not-recommented shadowing the core definitions).
     Note: this only will work properly if there are spaces between the tokens.
     Note: this will not work if the let vector is empty."
  (let [n (count s) p (basic-parse s) 
        ^chars cs (:chars p)
        ^ints breakable (make-array Integer/TYPE n)
        ^ints token-ty (:token-type p)]
    ; Set breakable => 1 whenever it's whitespace without macro bridges or in a string/comment/etc
    (loop [ix 0]
      (if (= ix n) "done with breakable"
        (do (if (and (grammer/ccom-white? (aget ^chars cs ix)) ; whitespace (includes commas).
                     (= (aget ^ints token-ty ix) 0)) ; not in a token (includes strings and comments).
              (aset ^ints breakable ix 1))
          (recur (inc ix)))))
    ; Set breakable to 2 at the \n at the end of each comment :
    (loop [ix 1]
      (if (>= ix n) "Done with comment and string-forcings"
         (do (if (and (= (aget ^chars cs ix) \newline) ; newline.
                   (or (= (aget ^ints token-ty (dec ix)) 8) ; ending a comment.
                       (= (aget ^ints token-ty ix) 3))) ; in a string.
               (aset ^ints breakable ix (int 2))) (recur (inc ix)))))
    (if (and (> n 4) (first even-force?)) ; Option to not allow breakables between even pairs. It does not work for fancy let-statements.
       (throw (Exception. "TODO: get it working for metadata."))
       (let [^ints depths (:inter-depth p) ; either with or without a macro.
             max-level (loop [acc (int 0) ix (int 0)] (if (< ix n) (recur (max acc (aget ^ints depths ix)) (inc ix)) acc))
             ^ints hot-levels (make-array Integer/TYPE (+ max-level 2))]
         (loop [ix (int 0)]
           (if (< ix n)
             (let [c (aget ^chars cs ix) c1 (if (< ix (dec n)) (aget ^chars cs (inc ix)) (char \a))]
               ; Different ways where even stuff is present:
               (if (grammer/copen? c)
                   (cond (and (= c \{) (or (= ix 0) (not= (aget ^chars cs (dec ix)) \#)))
                         (let [nxt-obj (first (_next-obj token-ty (inc ix) n))]
                           (if (< nxt-obj n) (_fix-evens! breakable token-ty nxt-obj (dec (depth-bracket depths nxt-obj 1)))))
                         (and (= c \() (or (= c1 \;) (= c1 \i) (= c1 \l) (= c1 \ ) (= c1 \w) (= c1 \c))) ; Optimization or statement. 
                         (let [next-obj (_next-obj token-ty (inc ix) n)] ; We are at the (, what is the indexes of the next-symbol after the (?
                           (if (contains? _letty-stuff (collections/asus cs (first next-obj) (inc (second next-obj))))
                             (let [first-sym-in-binding (first (_next-obj token-ty (inc (_next-bracket token-ty (second next-obj) n)) n))]
                               (if (< first-sym-in-binding n)
                                 (_fix-evens! breakable token-ty first-sym-in-binding (dec (depth-bracket depths first-sym-in-binding 1)))))))))
               (recur (inc ix)))))))
    (assoc p :breakable breakable)))

(defn _vcode-decrowd [vcode]
  (let [b #(str (:head %) (:tail %)) o (:obj vcode)
        sp-start? (fn [oi] (if (coll? (:obj oi)) (grammer/ccom-white? (first (:head oi))) (grammer/ccom-white? (first (b oi)))))
        sp-end? (fn [oi] (if (coll? (:obj oi)) (grammer/ccom-white? (last (:tail oi))) (grammer/ccom-white? (last (b oi)))))]
    (if (coll? o)
      (let [o (mapv _vcode-decrowd o) ; recursive.
            sp-start?s (mapv sp-start? o) sp-end?s (mapv sp-end? o) n (count sp-start?s)
            add-space?s (conj (mapv #(and (not (nth sp-start?s (inc %))) (not (nth sp-end?s %))) (range (dec n))) false) ; at the end.
            add-end-sp (fn [oi] (if (coll? (:obj oi))
                                    (update oi :tail #(str % " ")) 
                                    (assoc oi :head "" :tail (str (:head oi) (:tail oi) " "))))]
        (assoc vcode :obj (mapv #(if %2 (add-end-sp %1) %1) o add-space?s))) vcode)))
(defn blit-decrowd [blit]
  "Ensures that there is at least one space, comma, etc between adjacent elements in a collection.
   Valid code should stay valid. Only adds spaces, does not remove spaces."
  (vcode-to-blit (_vcode-decrowd (blit-to-vcode blit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;; Testing functions ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn jtest [s ix0 f]
  "Tests jumping. The |'s seperate the string.
   ix0 should be set so that the token starts at the char AFTER the first bar.
   f is the xyz-jump function that matches the token starting at ix0.
   the token should end right BEFORE the second bar. 
   Thus the bars contain the token but nothing more."
  (let [x (f (.toCharArray s) ix0 (count s))]
    (str (collections/sus s 0 ix0) "|" (collections/sus s ix0 x) "|" (collections/sus s x))))

(defn basic-parse-test [s & do-newline-parse?] ; (do (clc) (blitcode/basic-parse-test "foo"))
  "Tests the arrays created by (basic-parse s)."
  (let [parse0 ((if (first do-newline-parse?) newline-parse basic-parse) s)
        parse (dissoc parse0 :chars) ks (keys parse)
        n (count s)
        nis (fn [c] (if (= (count c) (inc n)) "" "  ")) ; Interstitial offset.
        pd (fn [c] (apply str (mapv #(if (number? %) (format " %02d" %) (collections/sus (str %) 0 3)) (into [] c)))) ; always 3 chars long.
        sts (apply str (mapv #(cond (= % \formfeed) "\\F " (= % \return) "\\r " 
                    (= % \newline) "\\n " (= % \tab) "\\t " (= % \ ) "   "
                    :else (str " " % " ")) s))
        n-ky (apply max (mapv #(count (str %)) ks)) _ps (apply str (repeat n-ky " "))
        ks-pad (mapv #(subs (str % _ps) 0 n-ky) ks)]
    (apply str (butlast (apply str _ps "   " sts "\n" (mapv #(str %2 (nis (get parse %1)) (pd (get parse %1)) "\n") ks ks-pad))))))

(defn _ob-last [x] (if (nil? (:obj x)) x (assoc (dissoc x :obj) :obj (if (coll? (:obj x)) (collections/cmap :flatten _ob-last (:obj x)) (:obj x)))))

(defn vcode-test [s & r-extract] ; (do (clc) (blitcode/vcode-test "foo bar"))
  "Shows the tree of (vcode s) in a simple indented form (uses pprint does not use indent.clj circular dependency issue)."
  (let [vcode (reads-string-vcode s)]
    (println (str "[" s "\n]"))
    (pprint/pprint (_ob-last ((if (first r-extract) _rmacro-extract identity) vcode)))))

(defn blit-test [s] ; (do (clc) (blitcode/blit-test "foo bar"))
  "Shows the tree of (vcode s) in a simple indented form (uses pprint does not use indent.clj circular dependency issue)."
  (let [bcode (reads-string-blit s)]
    (println (str "[" s "\n]"))
    (pprint/pprint (_ob-last bcode))))

(defn going-back-test ;(do (clc) (blitcode/going-back-test "(foo \"bar\" baz)"))
  "Can we go from s to vcode to bcode back to vcode. 
   You can add a function that changes the blitted code to test the projection at off-manifold points.
   collections/updayte-in et al are useful to work with short lists.
   About 260 ms for 24000-long string => 10 us/char."
  ([s] (going-back-test s nil))
  ([s mod-f] ; (do (clc) (blitcode/going-back-test "foo bar"))
    (let [vcode (reads-string-vcode s)
          bcode (reads-string-blit s) 
          bcode1 (try (if (nil? mod-f) bcode (mod-f bcode)) 
                   (catch Exception e (throw (Exception. (str "Error with user function " (.getMessage e))))))
          vcode1 (blit-to-vcode bcode1)
          s1 (_vcode-to-str vcode1)]
      (println "STRING:")
      (println s)
      (println "VCODE:")
      (pprint/pprint (_ob-last vcode))
      (println "BCODE:")
      (pprint/pprint (_ob-last bcode))
      (if mod-f (do (println "BCODE (after custom function acts on it):") (pprint/pprint bcode1)))
      (println "VCODE: (after conversion, leaf-level strings maby shuffled head<->tail):")
      (pprint/pprint (_ob-last vcode1))
      (println "STRING (after the round-trip):")
      (println s1))))

(defn round-trip-test [s] (println (blit-to-str (reads-string-blit s))))
(defn decrowd-test [s] ;(do (clc) (blitcode/decrowd-test "xox(foo()bar()[]{}#{})"))
  (println "orginal: " s) (println "Decrowded: " (blit-to-str (blit-decrowd (reads-string-blit s)))))