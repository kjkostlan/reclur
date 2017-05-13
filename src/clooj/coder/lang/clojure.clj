; Clojure parser tools that can be plugged into rcode.
; Eventually (LONG way down the road) other languages will go here.

(ns clooj.coder.lang.clojure
 (:require [clojure.string :as string] [clooj.collections :as collections] [clojure.pprint :as pprint]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Helper functions that support the coding API.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;; Misc support functions ;;;;;;;;;

(def ^ints sym-kwd-start
  "Whether the character, as an int, can start either a symbol or a keyword.
   Add a check for : to differentiate keyword and symbol starts.
   Cast the chars to ints (casting is cheap).
   Includes / but for division but that can only be a symbol that standa alone."
  (let [^ints outs (make-array Integer/TYPE 65536)
        valid "-=qwertyuiopasdfghjklzxcvbnm./!$%&*_+QWERTYUIOP|ASDFGHJKL:ZXCVBNM<>?"
        n (count valid) i1 (int 1)] ; having fun with a tiny performace challange that isn't too important.
    (loop [ix (int 0)]
      (if (= ix n) outs
        (do (aset ^ints outs (int (nth valid ix)) i1) (recur (inc ix)))))
    (loop [ix (int 0)] ; control chars, etc.
      (if (= ix 32) outs
        (do (if (and (not= ix 8) (not= ix 9) (not= ix 10) (not= ix 12) (not= ix 13)) (aset ^ints outs ix i1))
            (recur (inc ix)))))
    (loop [ix (int 127)] ; all the bizarrre chars and unicode fun.
       (if (= ix 65536) outs
         (do (aset ^ints outs ix i1) (recur (inc ix)))))))

(def ^ints sym-kwd-stop
  "Whether the chars-as-ints stops symbols or keywords."
  (let [^ints outs (make-array Integer/TYPE 65536)
        valid "`~@^()\t[{]}\\;\"\n, "
        n (count valid)]
    (loop [ix (int 0)]
      (if (= ix n) outs
        (do (aset ^ints outs (int (nth valid ix)) 1) (recur (inc ix)))))))

(defmacro ccom-white? [c] 
  "white or comma. This is the main function."
  `(or (= ~c \space) (= ~c \newline) (= ~c \tab) (= ~c \return) (= ~c \formfeed) (= ~c \,)))

(defmacro creader? [c] 
  "Rader macro chars. code/syntax quotes, # signs, and the like."
  `(or (= ~c \@) (= ~c \') (= ~c \`) (= ~c \~) (= ~c \#)))

(defmacro copen? [c]
  `(or (= ~c \() (= ~c \{) (= ~c \[)))

(defmacro cclose? [c]
  `(or (= ~c \)) (= ~c \}) (= ~c \])))
  
(defmacro cnumber? [c]
  "0-9, no other symbols (not even decimal, etc)."
  `(or (= ~c \0) (= ~c \1) (= ~c \2) (= ~c \3) (= ~c \4) (= ~c \5) (= ~c \6) (= ~c \7) (= ~c \8) (= ~c \9)))

;;;;;;;;;;;;; Fast jumper functions ;;;;;;;;;

(defn space-jump [^chars cs ix0 n]
  ; Returns the index after the end of empty space (if ix0 is not
  ; in empty space it returns ix0).
  (loop [ix (int ix0)]
    (if (>= ix (int n)) ix
      (let [c (aget ^chars cs ix)]
        (if (not (ccom-white? (aget ^chars cs ix))) ix
          (recur (inc ix)))))))

(defn sym-kwd-jump [^chars cs ix0 n]
  ; Standard fare 101.
  (loop [ix (int ix0)]
    (if (>= ix (int n)) ix
      (let [c (aget ^chars cs ix)]
        (if (= (aget ^ints sym-kwd-stop (int c)) 1) ix
          (recur (inc ix)))))))

(defn string-jump [^chars cs ix0 n] ; Get the ending quote but watch out for escaped chars!
  (loop [escape (int 0) ix (int (inc ix0))]
    (if (>= ix (int n)) ix
      (let [c (aget ^chars cs ix)]
        (if (and (= escape 0) (= c \")) (inc ix) 
          (recur (if (= c \\) (- 1 escape) 0) (inc ix)))))))

(defn regex-jump [^chars cs ix0 n] ; single # followed by string.
  (string-jump cs (inc ix0) n))

(defn num-jump [^chars cs ix0 n]
  ; Only slightly different than kyds or syms.
  (loop [ix (int ix0)]
    (if (>= ix (int n)) ix
      (let [c (aget ^chars cs ix)]
        (if (or (= (aget ^ints sym-kwd-stop (int c)) 1)
                (creader? c)) ix
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
; nil, booleans, keywords: uses the symbol jumper.

(defn open-jump [^chars cs ix0 n] (if (= (aget ^chars cs ix0) \#) (+ ix0 2) (inc ix0))) ; lists, vectors, and maps use simple brackets but sets use #{.
(defn close-jump [^chars cs ix0 n] (inc ix0))

(defn comment-jump [^chars cs ix0 n]
  (loop [ix (int ix0)]
    (if (>= ix (int n)) ix
      (let [c (aget ^chars cs ix)]
        (if (= c \newline) (inc ix) ; include the newline in the comment token (self-contained). Multi-line comments are seperate tokens.
          (recur (inc ix)))))))

;(defn cq [sym] (symbol (str "clojure.core/" sym))); (cq 'foo) = 'clojure.core/foo"
(def rmacros #{"'" "~" "~@" "#?@" "#(" "#:" "#'" "`" "#?" "#_" "#="}) 
;(def max-rmacro-len (apply max (mapv count rmacros)))

(defn rmacro-jump [^chars cs ix0 n] ; Reader macros.
  ; They call it reader macros but it isn't: hash-sets, character literals, metadata.
  ; This function lumps multible reader macros together. TODO: should we?
  TODO ; don't jump over comments.
  TODO ; don't lump multiple reader macro together.
  (loop [ix (int ix0)]
    (if (>= ix (int n)) ix
      (let [c (aget ^chars cs ix)]
        (if (and (> ix 0) (= (aget ^chars cs (dec ix)) \#) ;The # dispatches:
              (or (= c \:) (= c \_) (= c \?) (= c \!) (= c \<))) (recur (inc ix))
              ; the start of a collection, keyword, string, token, or number:
          (if (or (copen? c) (= (aget ^ints sym-kwd-start (int c)) 1) (cnumber? c) (= c \") (= c \\)) ix 
            (if (= c \;) (recur (comment-jump cs ix n)) ; Comments splitting between a ' and xyz, very rare case!
              (recur (inc ix)))))))))

(defn meta-jump [^chars cs ix0 n] ; metadata means go to the next character..
  (inc ix0))


; The namespace that we qualify variables in:
(def ^:dynamic *qual-ns* (find-ns 'clojure.core))

(def read-appliers 
  "Functions that apply reader macros. (anon-fn (+ % 2)) => (fn [%] (+ % 2))
   Some, like quote, don't do anything since the naive expand is sufficient.
   The input is a list (normal code, with idiomic stored in the metadata), the first element is the macro.
   For now only map-namespace, syntax-quote, and anon-fn are here, with metadata processed somewhere else.
   All interior nested reader macros are applied first (the most common case of nesting is quoting in defmacro)."
  {"#:" (fn [c] (throw "TODO"))
   "`" (fn [c] (throw "TODO"))
   "#(" (fn [c] (throw "TODO"))})

(def read-unappliers
  "Functions that unapply reader macros,leaving us back to the naively-expanded state.
   They are only called if there is readmacroK in the metadata.
   Not all reversals are possible (i.e. modifying a fn in #(...) to accept variable args), nothing is done in these cases.
   Interior nested reader macros have not been unapplied yet."
  {"#:" (fn [c] (throw "TODO"))
   "`" (fn [c] (throw "TODO"))
   "#(" (fn [c] (throw "TODO"))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; Functions in the coding API itself.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn token-mark [^String s]
  "Marks positions on s, giving back arrays 1:1 with s. Does not yet evaluate the token's value.
   ^ints :token-type = what is each token's value.
   ^ints :token-index = counts upward, incementing at each new token.
   ^chars :chars = (.toCharArray s) convenience field.
   This function should never crash (even for invalid code).
   25 ns/char for ~5kb strings (slightly out of date benchmark)."
   TODO: new rcode format
  (let [^chars cs (.toCharArray s) n (int (count cs))
        ^ints token-type (make-array Integer/TYPE n) ; What tokens we belong to.
        ^ints token-index (make-array Integer/TYPE n) ; 0001112223334567788... as the tokens pass.
        ix0 (space-jump cs 0 n)] ; we start as if we are in empty space.
    (loop [ix (int ix0) tix (int (if (> ix0 0) 1 0))] ; one run of the loop per token.
       (if (>= ix n) {:token-type token-type :token-index token-index :chars cs}
         (let [ci (aget ^chars cs ix)
               ; Switchyard:
               id (int (cond (ccom-white? ci) 0
                        (= ci \:) 2 (= ci \") 3
                        (cnumber? ci) 4 (= ci \\) 5 
                        (or (copen? ci) (and (= ci \#) (< (inc ix) n) (= (aget ^chars cs (inc ix)) \{))) 6 
                        (cclose? ci) 7
                        (= ci \;) 0 (and (= ci \#) (< (inc ix) n) (= (aget ^chars cs (inc ix)) \")) 8 ; regexp literals. 
                        (creader? ci) 9
                        :else 1)) ; symbols bieng a catch-all.
               ; Next index:
               nx (cond (= id 0) (if (= ci \;) (comment-jump cs ix n) (space-jump cs ix n))
                        (or (= id 1) (= id 2)) (sym-kwd-jump cs ix n)
                        (= id 3) (string-jump cs ix n) (= id 4) (num-jump cs ix n)
                        (= id 5) (char-jump cs ix n) (= id 6) (open-jump cs ix n)
                        (= id 7) (close-jump cs ix n) (= id 8) (regex-jump cs ix n)
                        (= id 9) (rmacro-jump cs ix n)) nx (int nx)]
           (loop [jx (int ix)] ; Set the token-type and token-index arrays.
             (if (< jx nx) ; nx is the first index after the token.
               (do (aset ^ints token-type jx id)
                   (aset ^ints token-index jx tix)
                   (recur (inc jx)))))
           (recur nx (inc tix)))))))

(defn read-apply TODO

)
