; Different languages will be added eventually.

(ns coder.langs
  (:require [coder.clojure :as clojure]))

(def supported-langs
  {:clojure 
    {:depth clojure/depth ; quick hiliting.
     :reads-string clojure/reads-string ; like read string but better.
     :forwards clojure/forwards ; string -> x.
     :backwards clojure/backwards
     :locate-in clojure/locate-in
     :path-at clojure/path-at
     :tokenize clojure/leaf-tokenize
     }})


(defmacro _add! [] '(do (aset-int st kx ix0) (aset-int en kx ix) (aset-int ty kx mode)))

(defn human-leaf-tokenize [s]
  "Human languages won't be added, so no :forwards, :backwards, etc 
   but we can still tokenize a string in a way that mostly makes sense."
  (let [^chars cs (.toCharArray s)
        n (int (count cs))
        N (inc (* n 2))
        ^ints st (make-array Integer/TYPE N) ; will be chopped down later as # tokens < # chars.
        ^ints en (make-array Integer/TYPE N)
        ^ints ty (make-array Integer/TYPE N)
        ntok (loop [ix (int 0) ix0 (int 0) kx (int 0) mode (int 0)]
               (if (>= ix n) 
                 (let [ix n] (_add!) (inc kx))
                 (let [c (aget cs ix)
                       ty1 (if (or (= c \ ) (= c \newline) (= c \tab)) 0
                             (if (or (= c \() 4 (= c \))) 5
                               (if (or (= c \.) (= c \?) (= c \!) (= c \,) (= c \;)) 2 1)))]
                   (if (= ty1 mode) (recur (inc ix) ix0 kx mode)
                     (let [ix (inc ix)] (_add!) (recur ix ix (inc kx) ty1))))))
        ; Copied code from coder.clojure, don't want to modify the file now but will probably refactor eventually.
        x (loop [acc-s [] acc-ty [] ix 0 jx -1]
            (if (= ix ntok) [acc-s acc-ty]
                (let [strg (subs s (aget st ix) (aget en ix))]
                  (if (= (count strg) 0) (recur acc-s acc-ty (inc ix) jx) ; skip empty tokens.
                    (let [acc-s (if (< (count acc-s) jx) (conj acc-s []) acc-s)
                          acc-ty (if (< (count acc-ty) jx) (conj acc-ty 0) acc-ty)
                          jx1 (if (and (= (aget ty ix) 0) (= (get acc-ty jx) 0)) jx (inc jx))]
                      (recur (update acc-s jx1 #(if % (conj % strg) [strg]))
                             (assoc acc-ty jx1 (aget ty ix)) (inc ix) jx1))))))]
    [(mapv #(apply str %) (first x)) (second x)]))