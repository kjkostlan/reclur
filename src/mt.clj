; MT =  Meta. Functions that work with metadata. Also tools for error throwing and reporting, which is meta in a sense.
; Takes priority above t for where fns go.

(ns mt (:require [clojure.walk :as walk] [t] [globals]))

(defn keep-meta [x f & args]
  "Applies f w/o affecting meta. Throws an error if x had meta and (f x) can't hold meta."
  (if (meta x)
    (with-meta (apply f x args) (meta x))
    (apply f x args)))

(defn dual-get-in [x ph-mph]
  "Simplify your paths.
   ph-mph is a tuple of paths, the first within x the second path within the meta."
  (let [ph (first ph-mph) mph (second ph-mph)]
    (t/cget-in (meta (t/cget-in x ph)) mph)))

(defn dual-assoc-in [x ph-mph v]
  "Simplify your paths.
   ph-mph is a tuple of paths, the first within x the second path within the meta."
  (let [ph (first ph-mph) mph (second ph-mph)]
    (t/cupdate-in x ph
      (fn [xi] (vary-meta xi #(t/cassoc-in % mph v))))))

(defn dual-update-in [x ph-mph f & args]
  "Simplify your paths.
   ph-mph is a tuple of paths, the first within x the second path within the meta."
  (let [ph (first ph-mph) mph (second ph-mph)]
    (t/cupdate-in x ph
      (fn [xi] (vary-meta xi #(apply t/cupdate-in % mph f args))))))

(defn m-postwalk [f form]
  "Preserves metadata when possible, unless f destroys metadata."
  (let [reset-meta #(with-meta % (meta form))
        walk-f #(m-postwalk f %)]
    (cond (map? form) (f (reset-meta (zipmap (mapv walk-f (keys form)) (mapv walk-f (vals form)))))
      (coll? form) (let [form1 (mapv walk-f form)
                         form2 (cond (vector? form) (into [] form1)
                                 (set? form) (set form1) :else (apply list form1))]
                     (f (reset-meta form2)))
      :else (f form))))

(defn _pmwalk [f ph x]
  (f ph
    (#(if (meta x) (with-meta % (meta x)) x)
      (cond (map? x) (zipmap (keys x) (mapv #(t/pwalk1 f (conj ph %1) %2) (keys x) (vals x)))
        (set? x) (set (mapv #(t/pwalk1 f (conj ph %) %) x))
        (vector? x) (mapv #(t/pwalk1 f (conj ph %1) %2) (range) x)
        (coll? x) (apply list (mapv #(t/pwalk1 f (conj ph %1) %2) (range) x))
        :else x))))
(defn pm-postwalk [f x]
  "Pathed post walk, calls (f path subform). Not lazy.
   Preserves metadata, unless f destroys metadata."
  (_pmwalk f [] x))

(defn m-unpack [x]
  "Unpacks metadata with (| meta-data bar), | is a symbol which does not create read-string confusion the way ^ would.
   Only unpacks data which has metadata. This fn is most useful for reporting without needing *print-meta*."
  (walk/prewalk #(if-let [m (meta %)]
                   (let [no-meta (with-meta % nil)]
                     (list (symbol "|") m no-meta)) %) x))

(defn m-pack [x]
  "Packs metadata, undoing the effect of m-unpack."
  (walk/prewalk #(if (and (list? %) (= (first %) (symbol "|")))
                   (with-meta (c/third %) (second %)) %) x))


;;;;; Error reporting is meta to the standard call stack in in the sense that it can break out of the call stack ;;;;

(defn error [& args] "Throws an error where all args are put into a string like println"
  (throw (Exception. (apply str (interpose " " args)))))

(defn error+ [msg x]
  "Errors with msg, and logs x which should be too large for a simple message.
   Msg can be a vector or str."
  (let [msg (if (sequential? msg) (apply str msg) (str msg))
        msg (str msg " [M-p opens dump].")]
    (swap! globals/log-atom #(assoc % ::error-plus x))
    (throw (Exception. msg))))

(defn get-error+ []
  "Returns the thing logged as the last error."
  (::error-plus @globals/log-atom))
