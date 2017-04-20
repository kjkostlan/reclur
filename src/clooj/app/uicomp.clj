; Composition of listener functions.
(ns clooj.app.uicomp)

(defn uicomp
  "Composition of listener functions that take in s, e, and o and return the modified s.
   nil functions (when no events are specified) are replaced with the identity (simply returns s).
   Like comp the functions are applied right-to-left.
   Useful for having multiple independent or nearly independent .clj files attach their
   own listeners to the top level of some GUI data-structure." 
  ([f] (if f f (fn [s e o] s))) ; default to nil.
  ([f & args] 
    (let [me-first (apply uicomp args)]
      (fn [s e o] (f (me-first s e o) e o)))))

(defn add-top-listeners [lfns s]
  "Adds lfns to the top-level of s (a map). lfns is map of listener keys to functions.
   These added functions go last."
  (reduce #(assoc %1 %2 (uicomp (get lfns %2) (get %1 %2))) s (keys lfns)))