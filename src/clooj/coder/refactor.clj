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
  (and (coll? x) (or (not (map? x) (_mwk x)))))

(defn blitcode-to-blitcodew [blitcode]
  "Work around for the evil non-metability of strings, etc. Any {:obj x} encountered 
   is converted into {:obj {_mwk x}} when x can't support metadata."
  (let [blitcode (if (:meta blitcode) (update blitcode :meta blitcode-to-blitcodew) blitcode) ; recursive on :meta.
        o (:obj blitcode)]
    (cond (coll? o) (assoc blitcode :obj (collections/cmap :flatten blitcode-to-blitcodew o)) ; recursive.
      (metable? o) blitcode ; no need to change.
      :else (update blitcode :obj #(hash-map _mwk %)))))


(defn blitcodew-to-blitmcode [blitcodew] 
  "Converts into code for which the :head, :tail, etc is put into the metadata of the :obj.
   The :head, :tail, etc are assigned to the _bk key in the metadata, but the :meta is assigned 1:1 as it represents 
   user metadata." ;{:head "[" :tail "]" :obj [1 2 3]} => {:obj [1 2 3]} where (meta [1 2 3]) is {_bk {:head "[" :tail "]"}}. 
  (let [m (:meta blitcodew) 
        blitcodew1 (if m (update blitcodew :meta blitcodew-to-blitmcode) blitcodew) ; recursive on the meta.
        blitcodew2 (if (coll0? (:obj blitcodew1) (update blitcodew1 :obj #(collections/cmap :flatten blitcodew-to-blitmcode %))) blitcodew1)] ; recursive.
    (with-meta (:obj blitcodew2) (assoc (if m m {}) _bk (dissoc blitcodew2 :meta :obj)))))

(defn blitmcode-to-wrcode [blitmcode] 
  "Converts to code that is similar to the unblitted code. We basically just extract the :obj"
  (let [m1 (blitmcode-to-wrcode (meta blitmcode)) ; recursive on the meta.
        o (:obj blitmcode) o1 (if (coll0? o) (collections/cmap :flatten blitmcode-to-wrcode o) o)] ; recursive.
    (with-meta o m1))) ; move the metadata from the code to the other code.

(defn wrcode-to-rcode [wrcode]
  "Work around for the evil non-metability of strings, etc."
  (let [wrcode1 (vary-user-meta wrcode wrcode-to-rcode)] ; recursive on the meta.
    (if (coll? wrcode1) 
      (let [ks (collections/ckeys wrcode1) ; keys of what goes into _bck. 
            ; the _bck's values are the non-user meta component of the code, maps are packaged as tuples of key and value.
            vs (if (map? wrcode1) (mapv #(vector (luw %) (get wrcode1 %)) (vals wrcode1)) ; luw => use the unwrapped key to access the cooresponding v when going back.
                 (mapv #(luw (get wrcode1 %)) ks)) ; any non-map is like a vector with a different hat.
            wrcode2 (collections/cmap :flatten wrcode-to-rcode wrcode1)]; recursive, AFTER we calculated the needed metadata (or else say byby to the metadata).
        (with-meta wrcode2 (assoc (meta wrcode1) _bck (zipmap ks vs))))
      (luw wrcode1)))) ; Unwrap non-collections (collections are never leaf-wrapped as they have meta).

(defn rcode-to-wrcode [rcode]
  "Work around for the evil non-metability of strings, etc.
   When going back we must be prepared for a lot of functions."
  (let [rcode1 (vary-user-meta rcode rcode-to-wrcode)] ; recursive on the user's meta.
    (if (coll0? rcode1) ; use coll0? b/c we will leaf-wrap b4 we recursivly operate.
      (let [m (if-let [x (meta rcode1)] x {})
            kvs (_bck m) m1 (dissoc (meta rcode) _bck) ; no need for the _bck meta key anymore.
            re (fn [_rcode] (with-meta (collections/cmap :flatten rcode-to-wrcode _rcode) m1)) ; recursive, but don't call it yet.
            ; bck is a child of stuff stored in _bck, x is a child of rcode:
            fill-m (fn [bck x] (cond (meta x) x ; x already has metadata, no need to fill it in.
                                 bck (with-meta x bck) ; fill in the meta.
                                 :else x))] ; no metadata whatsoever in either case, leave it alone.
        ; Maps requiring unpacking the tuples.
        (re ; recursive part AFTER since kvs is based on the unwrapped keys and values.
          (cond (map? rcode1) (zipmap (mapv #(fill-m (first (get kvs %)) %) (keys rcode1)) ; first and second to unwrap the tuples.
                              (mapv #(fill-m (second (get kvs %)) (get rcode1 %)) (keys rcode1)))
            (set? rcode1) (apply hash-set (mapv #(fill-m (get kvs %) %) rcode1))
            (vector? rcode1) (mapv #(fill-m (get kvs %) (get rcode1 %)) (range (count rcode1))) ; not nth, it could be nil.
            :else (apply list (mapv #(fill-m (get kvs %) (get rcode1 %)) (range (count rcode1))))))) ; list, seq, etc is a ()
      (if (metable? rcode1) rcode1 {_mwk rcode1})))) ; leaf wrap (if it isn't metable).

(defn wrcode-to-blitcodew [wrcode]
  "Unpacking the _bk in the meta and replacing having ^user-defined metadata with :meta."
  (let [m1 (if (meta wrcode) (vary-user-meta wrcode-to-blitcodew wrcode) {})] ; recursive on the meta.
    (let [bk (if (meta wrcode) (_bk (meta wrcode)) {}) ; stuff to put into the blitcode.
          wrcode1 (if (coll? wrcode) (collections/cmap :flatten wrcode-to-blitcodew wrcode) wrcode) ; recursive.
          blit0 (assoc (with-meta bk nil) :obj (with-meta wrcode1 nil))]  ; strip all actual metadata.
      (if (> (count m1) 0) (assoc blit0 :meta m1) blit0)))) ; any metadata the user has goes here.

(defn blitcodew-to-blitcode [blitcodew]
  "Unpacking the _bk in the meta and replacing having ^user-defined metadata with :meta."
  (let [blitcodew1 (if (:meta blitcodew) (update blitcodew :meta blitcodew-to-blitcode)) ; recursive on the :meta.
        blitcodew2 (if (coll0? (:obj blitcodew1)) (update blitcodew1 :obj #(collections/cmap :flatten blitcodew-to-blitcode %)) blitcodew1)] ; recursive.
    (luw blitcodew2))) ; the unwrapping itself.

(defn refactor-str [f s]
  "Standard operating protocol for the refactoring engine.
   string -> blitcode -> rcode -> (your function) -> rocde1 -> blitcode1 -> string.
   f is a function that works with the code.
   s is wrapped in an outer [] as it represents a file."
  (-> s blitcode/reads-string-blit blitcodew-to-blitmcode blitmcode-to-wrcode wrcode-to-rcode
    f rcode-to-wrcode wrcode-to-blitcodew blitcodew-to-blitcode blitcode/blit-to-str))

(defn refactor-str-lw [fw s]
  "Stands for leaf-wrapped refactoring of the string. 
   It requires working with leaf-blitted stuff (see luw on-uw) but is better
   is preserving the user's idiosynchronicies."
  (-> s blitcode/reads-string-blit blitcodew-to-blitmcode blitmcode-to-wrcode
    fw wrcode-to-blitcodew blitcodew-to-blitcode blitcode/blit-to-str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; Testing scribbles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment

  (refactor/refactor-str #(conj % 'baz) "foo ;com\n ,,bar")
  (refactor/refactor-str #(assoc % 0 'fu) "foo ;com\n ,,bar") 
  (refactor/refactor-str (fn [x] (update x 0 #(conj % :z))) "[:a, ,:b]")
  (refactor/refactor-str (fn [x] (update x 0 #(conj % :z))) "(:a, ,:b)")
  (refactor/refactor-str (fn [x] (update x 0 #(assoc % :x 'y))) "{:a 1,,,:b 2}")
  (refactor/refactor-str (fn [x] (update x 0 #(conj % :z))) "#{:a, ,:b}")
  (refactor/refactor-str (fn [x] (update x 0 #(conj % 400))) "^intvec [10 20 30]")

  (def f0)

)
