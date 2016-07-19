(ns clooj.collections)

; Generic functions to work with clojure's collections.

; TODO: are keys mapped to nil treated the "right" way?
(defn fillin-defaults [c default]
  "When given two maps: adds fields that are in default but not in state to state.
   Note: nil counts as a missing field even if there is an explicit key.
   Example: (collections/fillin-defaults {:a :one :c 3} {:a 1 :b 2}) => {:a :one, :c 3, :b 2}.
     :a and :c weren't changed because we specified non-nil values for them (and there isn't even a default :c).
     :b is the added default we didn't specify.
   This function is useful when dealing with complex input arguments:
     have defaults but let the user override them if they know what they are doing."
  (reduce #(if (nil? (get %1 %2)) (assoc %1 %2 (get default %2)) %1) c (keys default)))

(defn asoc-in [m ks v]
  "Like assoc-in but if ks is [] or nil we will simply replace m with v.
   Note: get-in does not have this problem."
  (if (> (count ks) 0) (assoc-in m ks v) v))

(defn updayte-in [m ks f & args]
  "Like update-in but if ks is [] or nil we simply apply f to m."
  (if (> (count ks) 0) (apply update-in m ks f args) (apply f m args)))

(defn f-bin-search [f n]
  "Binary search with functions.
   Rather than an array, f is a function that takes an integer argument from 0 to n-1.
   If (f i) is monotonicly increasing (from false to true) it finds the first true in O(log(n)).
   If f is always false it returns n."
;Adapted from http://algs4.cs.princeton.edu/11model/BinarySearch.java.html
  (cond (= n 0) 0 
        (f 0) 0
        (not (f (dec n))) n 
        ; always: (f lo) is false but (f hi) is true.
        :else
        (loop [lo 0 hi (dec n)]
          (if (>= (inc lo) hi) hi ; no more searching to do.
          ; hi and lo are at least 2 apart, so mid will always be in between.
            (let [mid (+ lo (long (/ (- hi lo) 2)))] 
              (if (f mid) (recur lo mid) (recur mid hi)))))))