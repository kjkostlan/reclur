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