; Java array handling functions.

(ns coder.javar)

(defn chop-ints [^ints x n]
  "Chops down an array of ints to size n."
  (let [^ints out (make-array Integer/TYPE n)]
    (loop [ix 0]
      (if (= ix n) out
          (do (aset-int out ix (aget x ix))
              (recur (inc ix)))))))

(defn token-native2vector [txt start-array end-array type-array]
  "Converts token java-array-formate to clj format.
   The inputs are java ints arrays (int not long).
   The function has two outputs: [strings, types]."
  (let [^ints st start-array ^ints en end-array ^ints ty type-array
        n0 (count ty)]
    (loop [acc-s [] acc-ty [] ix 0 jx -1]
      (if (= ix n0) [(mapv #(apply str %) acc-s) acc-ty]
        (let [strg (subs txt (aget st ix) (aget en ix))]
          (if (= (count strg) 0) (recur acc-s acc-ty (inc ix) jx) ; skip empty tokens.
            (let [acc-s (if (< (count acc-s) jx) (conj acc-s []) acc-s)
                  acc-ty (if (< (count acc-ty) jx) (conj acc-ty 0) acc-ty)
                  jx1 (if (and (= (aget ty ix) 0) (= (get acc-ty jx) 0)) jx (inc jx))]
              (recur (update acc-s jx1 #(if % (conj % strg) [strg]))
                     (assoc acc-ty jx1 (aget ty ix)) (inc ix) jx1))))))))