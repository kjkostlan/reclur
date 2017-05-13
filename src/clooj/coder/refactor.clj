; Refactoring tools that preserve the format as much as possible 
 ; (with the exception of indentation continuity) <- TODO implement this.

(ns clooj.coder.refactor
 (:require [clojure.string :as string] [clooj.coder.grammer :as grammer]
           [clooj.collections :as collections] [clojure.pprint :as pprint]
           [clooj.coder.blitcode :as blitcode]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; The main engine itself
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def _mwk (keyword (str "Meta-wrap" "58423"))) ; unique ID for getting around that numbers, etc can't be metified. Only used tepoararly, not in the rcode.
(def _bk (keyword (str "Blit" "1423"))) ; Unique id to store the blitcode's positional in the meta-data.
(def _bck (keyword (str "Blit-children" "9867"))) ; Work around for some data-structures bieng non-blittable. 
  ;Only used when children don't have metadata but we do.
  ;Always a hash-map, with the keys = the keys of the object.
  ;  The vals are {:head "...", :tail "..."}, except for maps in which case the vals are [{:head "...", :tail "..."},{:head "...", :tail "..."}].
     ; And the occasional :order and :rmacro for hash-sets and reader-macros.

(defn metable? [x] "Can we add metadata?"
  (instance? clojure.lang.IMeta x))

(defn user-only [m]
  (dissoc m _mwk _bk _bck))
(defn user-meta [x]
  (if-let [m (meta x)] (user-only m) {}))

(defn vary-user-meta [x f] "gets the meta. Removes _mwk, _mk, and _bck entries if present. Applies x. Puts back any entries if present."
  (if-let [m (meta x)] 
    (let [fill-m (fn [mi k] (if (k m) (assoc mi k (k m)) mi)) ; fn to add in the child meta information.
          m0 (dissoc m _mwk _bk _bck)]
      (with-meta x (-> (f m0) (fill-m _mwk) (fill-m _bk) (fill-m _bck)))) x)) ; if no meta, don't change x.

(defn luw [x]
  "Gets the leaf unwrap version of x, useful in refactor-str-lw. Leaves x alone if it isn't wrapped."
  (if (and (map? x) (_mwk x)) (_mwk x) x))

(defn on-uw [x f & args]
  "Applies (f x & args) on the leaf-unwrapped version. Also useful in refactor-str-lw"
  (if (and (map? x) (_mwk x)) (apply f (_mwk x) args) (apply f x args)))

(defn coll0? [x]
  "Like coll? but returns false for meta-wrapped stuff (all collecitons are metable so meta-wrapped stuff is always a leaf)."
  (and (coll? x) (or (not (map? x)) (not (_mwk x)))))

(defn clear-meta [x] 
  "Gets rid of metadata, returns x if x can't have metadata."
  (if (meta x) (with-meta x nil) x))

(defn blitcode-to-blitcodew [blitcode]
  "Work around for the evil non-metability of strings, etc. Any {:obj x} encountered 
   is converted into {:obj {_mwk x}} when x can't support metadata."
  (let [blitcode (if (:meta blitcode) (update blitcode :meta blitcode-to-blitcodew) blitcode) ; recursive on :meta.
        o (:obj blitcode)]
    (cond (coll? o) (assoc blitcode :obj (collections/cmap :flatten blitcode-to-blitcodew o)) ; recursive.
      (metable? o) blitcode ; no need to change.
      :else (update blitcode :obj #(hash-map _mwk %)))))

(defn blitcodew-to-wrcode [blitcodew] 
  "Converts into code for which the :head, :tail, etc is put into the metadata of the :obj.
   The :head, :tail, etc are assigned to the _bk key in the metadata, but the :meta is assigned 1:1 as it represents 
   user metadata." ;{:head "[" :tail "]" :obj [1 2 3]} => {:obj [1 2 3]} where (meta [1 2 3]) is {_bk {:head "[" :tail "]"}}. 
  (let [m (:meta blitcodew) m1 (if m (blitcodew-to-wrcode m)) ; recursive on the meta.
        blitcodew1 (if (coll0? (:obj blitcodew)) (update blitcodew :obj #(collections/cmap :flatten blitcodew-to-wrcode %)) blitcodew)] ; recursive.
    (with-meta (:obj blitcodew1) (assoc (if m1 m1 {}) _bk (dissoc blitcodew1 :meta :obj))))) ; the :meta and :obj don't need to be stored.

(defn wrcode-to-rcode [wrcode]
  "Work around for the evil non-metability of strings, etc.
   The metadata's positional information, i.e. _bk, is stored in _bck of the metadata one level up in the _bck entry.
   This allows {_mwk x} to be converted into x (without losing our _bk).
   _bck is always a map from collections/ckeys (after the unwrapping) to values.
   Maps are stored as tuples, the _bk of the [key value]."
  (let [wrcode1 (vary-user-meta wrcode wrcode-to-rcode)] ; recursive on the meta.
    (if (coll0? wrcode1) 
      (let [wks (collections/ckeys wrcode1) ; keys of what goes into _bck, but wrapped.
            ; the _bck's values are the non-user meta component of the code, maps are packaged as tuples of key and value.
            vs (if (map? wrcode1) (mapv #(vector (_bk (meta %)) (_bk (meta (get wrcode1 %)))) wks) ; luw => use the unwrapped key to access the cooresponding v when going back.
                 (mapv #(_bk (meta (collections/cget wrcode1 %))) wks)) ; any non-map is like a vector with a different hat.
            wrcode2 (collections/cmap :flatten wrcode-to-rcode wrcode1)]; recursive, AFTER we calculated the needed metadata (or else say byby to the metadata).
        (with-meta wrcode2 (assoc (meta wrcode1) _bck (zipmap (mapv luw wks) vs))))
      (luw wrcode1)))) ; Unwrap non-collections (collections are never leaf-wrapped as they have meta, so don't need unwrapping).

(defn rcode-to-wrcode [rcode]
  "Work around for the evil non-metability of strings, etc.
   When going back we must be prepared for a lot of functions."
  (let [rcode1 (vary-user-meta rcode rcode-to-wrcode)] ; recursive on the user's meta.
    (if (coll0? rcode1) ; use coll0? b/c we will leaf-wrap b4 we recursivly operate. Collections never need to be no are leaf-wrapped.
      (let [m (if-let [x (meta rcode1)] x {})
            kvs (_bck m) m1 (dissoc (meta rcode) _bck) ; the keys in kvs are for the leaf-unwrapped values. 
            re (fn [_rcode] (with-meta (collections/cmap :flatten rcode-to-wrcode _rcode) m1)) ; recursive, but don't call it yet.
            ; bck is a child of stuff stored in _bck, x is a child of rcode:
            fill-m (fn [bck x]
                     (cond (meta x) x ; x already has metadata, no need to fill it in.
                       bck (with-meta (if (metable? x) x {_mwk x}) {_bk bck}) ; fill in the meta, which may require an early leaf-wrap.
                       :else x))] ; no metadata whatsoever in either case, leave it alone.
        ; Maps requiring unpacking the tuples.
        (re ; recursive part AFTER since kvs is based on the unwrapped keys and values.
          (cond (map? rcode1) (zipmap (mapv #(fill-m (first (get kvs %)) %) (keys rcode1)) ; first and second to unwrap the tuples.
                              (mapv #(fill-m (second (get kvs %)) (get rcode1 %)) (keys rcode1)))
            (set? rcode1) (apply hash-set (mapv #(fill-m (get kvs %) %) rcode1))
            (vector? rcode1) (mapv #(fill-m (get kvs %) (nth rcode1 %)) (range (count rcode1))) ; not nth, it could be nil.
            :else (apply list (mapv #(fill-m (get kvs %) (collections/cget rcode1 %)) (range (count rcode1))))))) ; list, seq, etc is a ()
      (if (metable? rcode1) rcode1 {_mwk rcode1})))) ; leaf wrap (if it isn't metable).

(defn wrcode-to-blitcodew [wrcode]
  "Unpacking the _bk in the meta and replacing having ^user-defined metadata with :meta."
  (let [bk (if-let [_x (_bk (meta wrcode))] _x {}) ; stuff to put into the blitcode.
        um1 (let [um (user-meta wrcode)] (if (> (count um) 0) (wrcode-to-blitcodew um))) ; recursive on the meta.
        wrcode1 (if (coll0? wrcode) (collections/cmap :flatten wrcode-to-blitcodew wrcode) wrcode) ; recursive.
        blit0 (assoc (clear-meta bk) :obj (clear-meta wrcode1))]  ; strip all actual metadata.
    (if um1 (assoc blit0 :meta um1) blit0))) ; any metadata the user has goes here.

(defn blitcodew-to-blitcode [blitcodew]
  "Unpacking the _bk in the meta and replacing having ^user-defined metadata with :meta."
  (let [blitcodew1 (if (:meta blitcodew) (update blitcodew :meta blitcodew-to-blitcode) blitcodew) ; recursive on the :meta.
        blitcodew2 (if (coll0? (:obj blitcodew1)) (update blitcodew1 :obj #(collections/cmap :flatten blitcodew-to-blitcode %)) blitcodew1)] ; recursive.
    (update blitcodew2 :obj luw))) ; the unwrapping itself.

(defn refactor-str [f s]
  "Standard operating protocol for the refactoring engine.
   string -> blitcode -> rcode -> (your function) -> rocde1 -> blitcode1 -> string.
   f is a function that works with the code.
   s is wrapped in an outer [] as it represents a file."
  (-> s blitcode/reads-string-blit blitcode-to-blitcodew blitcodew-to-wrcode wrcode-to-rcode
    f rcode-to-wrcode wrcode-to-blitcodew blitcodew-to-blitcode blitcode/blit-to-str))

(defn refactor-str-lw [fw s]
  "Stands for leaf-wrapped refactoring of the string. 
   It requires working with leaf-blitted stuff (see luw on-uw) but is better
   is preserving the user's idiosynchronicies."
  (-> s blitcode/reads-string-blit blitcode-to-blitcodew blitcodew-to-wrcode
    fw wrcode-to-blitcodew blitcodew-to-blitcode blitcode/blit-to-str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Testing scribbles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn as-bcodew [s] ; partial pipeline functions.
  (-> s blitcode/reads-string-blit blitcode-to-blitcodew))

(defn as-rwcode [s]
  (-> s blitcode/reads-string-blit blitcode-to-blitcodew blitcodew-to-wrcode))

(defn as-rcode [s]
  (-> s blitcode/reads-string-blit blitcode-to-blitcodew blitcodew-to-wrcode wrcode-to-rcode))

(defn as-rcode1 [f s]
  (-> s blitcode/reads-string-blit blitcode-to-blitcodew blitcodew-to-wrcode wrcode-to-rcode f))

(defn as-wrcode1 [f s]
  (-> s blitcode/reads-string-blit blitcode-to-blitcodew blitcodew-to-wrcode wrcode-to-rcode 
    f rcode-to-wrcode))

(defn as-bcodew1 [f s]
  (-> s blitcode/reads-string-blit blitcode-to-blitcodew blitcodew-to-wrcode wrcode-to-rcode 
    f rcode-to-wrcode wrcode-to-blitcodew))

(defn as-bcode1 [f s]
  (-> s blitcode/reads-string-blit blitcode-to-blitcodew blitcodew-to-wrcode wrcode-to-rcode 
    f rcode-to-wrcode wrcode-to-blitcodew blitcodew-to-blitcode))

(comment

;;;;;;;;;;; Stuff below works.

  (refactor/refactor-str #(conj % 'baz) ":foo ;com\n ,,bar,")
  (refactor/refactor-str #(assoc % 0 'fu) "foo ;com\n ,,bar,")
  (refactor/refactor-str (fn [x] (update x 0 #(conj % :z))) "[:a, ,b,]")

  (refactor/refactor-str (fn [x] (update x 0 #(conj % :zero))) "(a, :b,, :c,,, d,)") ; see *1.

  (refactor/refactor-str (fn [x] (update x 0 #(assoc % :x 'y))) "{:a 1,,,:b 2}")
  (refactor/refactor-str (fn [x] (update x 0 #(conj % :z))) "#{:a, ,:b}")

  (refactor/refactor-str (fn [x] (update x 0 #(conj % 40))) "^intvec [10 20 30]")
  (refactor/refactor-str (fn [x] (collections/updayte-in x [0 2] #(apply list (first %) 1 (rest %)))) "#(+,, 2 3)") ; (require '[clooj.collections :as collections])

;;;;;;;;;;; Stuff below not tested.

;;;;;;;;;;; Stuff below fails.

; NPE in the blitcode:
  (refactor/refactor-str (fn [x] (do (println x)) (update x 0 #(with-meta % {:moo :mar}))) "[10 20 30]")

)

;   *1 The pain of non-metability mainly in lists and vectors:
  ; For (refactor/refactor-str (fn [x] (update x 0 #(conj % :zero))) "(a, :b,, :c,,, d,)")
  ; the ,,, ends up on :b and the ,, is destroyed due to the index shift + the lossy meta, the , on a is duplicated to :zero
  ; If this non-ideal behavior is a problem refactor-str-lw can be used.
  ; For our f in this example it is a drop in replacement.
  ; This is still considered working even though it is not what most would want, because it will 