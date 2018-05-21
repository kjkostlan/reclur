; Simple finite-state machine parsers to get interstitial indent level very fast.
; Interstitial means the indent level at each cursor location.
; For the string "(foo)" we would get 011110, i.e. one more element than the (count "(foo)")
; the -ints functions return native arrays, the vanilal forms return vectors.
(ns coder.fsmparse)

(defn depth-clojure-ints [^String s]
  "Ignores reader macros."
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

(defn depth-clojure [s] (into [] ^ints (depth-clojure-ints (str s))))



