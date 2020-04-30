; Human readability.

(ns coder.crosslang.langparsers.human
   (:require [coder.javar :as javar]
     [clojure.string :as string]))

(defmacro _add! [] '(do (aset-int st kx ix0) (aset-int en kx ix) (aset-int ty kx mode)))

(defn tokenize-ints [s]
  "Human languages won't be added, so no :forwards, :backwards, etc 
   but we can still tokenize a string in a way that mostly makes sense."
  (let [^chars cs (.toCharArray ^String s)
        n (int (count cs))
        N (inc (* n 2))
        ^ints st (make-array Integer/TYPE N) ; will be chopped down later as # tokens < # chars.
        ^ints en (make-array Integer/TYPE N)
        ^ints ty (make-array Integer/TYPE N)
         n-tok (loop [ix (int 0) ix0 (int 0) kx (int 0) mode (int 0)]
                 (if (>= ix n) (let [ix n] (_add!) (inc kx))
                   (let [c (aget cs ix)
                         c1 (if (< ix (dec n)) (aget cs (inc ix)) \ )
                         open? (or (= c \() (= c \[) (= c \{)) close? (or (= c \)) (= c \]) (= c \}))
                         ty1 (cond (or (= c \ ) (= c \newline) (= c \tab)) 0
                               open? 4 close? 5
                               (or (and (= c \.) (or (= c1 \ ) (= c1 \newline) (= c1 \tab)))
                                 (= c \!) (= c \?) (= c \;) (= c \,)) 6
                               :else 1)]
                     (if (= ty1 mode) (recur (inc ix) ix0 kx mode)
                       (let [ix (inc ix)] (_add!) (recur ix ix (inc kx) ty1))))))]
    [(javar/chop-ints st n-tok) (javar/chop-ints en n-tok) (javar/chop-ints ty n-tok)]))

(defn interstitial-depth [s]
  "How deep we are."
  (let [^chars cs (.toCharArray ^String s)
        n (int (count cs))]
    (loop [acc [0] ix (int 0) dx (int 0)]
      (if (= ix n) acc
        (let [c (aget cs ix)
              open? (or (= c \() (= c \[) (= c \{))
              close? (or (= c \)) (= c \]) (= c \}))
              dx1 (+ dx (if (open? 1) (if close? -1 0)))]
          (recur (conj acc dx1) (inc ix) dx1))))))
          
(defn reads-string [s]
  "Ignores punctuation."
  (let [s1 (string/replace s #"\.[ \t\n]"
             #(string/replace % "." " ")) ; allow numbers and words with periods.
        keeps "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890-."
        alloweds (reduce #(assoc %1 %2 1) (into [] (repeat 256 0)) keeps)
        s2 (apply str (mapv #(if (= (nth alloweds (int %)) 1) % \ ) s1))]
    (read-string (str "[\n" s2 "\n]"))))
