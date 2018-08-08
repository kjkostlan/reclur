; Refactoring tools 
; Many more will be added.
(ns coder.refactor
  (:require [collections]))







;;; Low level refactoring tools ;;;


(defn path2vpath [x path]
  "Converts a path within x to a vpath whcih represents the order as appears lexiographically.
   Parts of paths that go into nil-mans land aren't affected."
  (let [n (count path)]
    (loop [ix 0 acc [] xi x]
      (if (= ix n) acc
        (let [phi (nth path ix) xi1 (collections/cget xi phi)]
          (cond (or (sequential? xi) (not (coll? xi))) ; unchanged.
            (recur (inc ix) (conj acc phi) xi1)
            (map? xi)
            (let [cht (get-in (meta xi) [:PARSE 0 :children-txts])
                  next-hop (get-in cht [phi 0 3])] 
              (recur (inc ix) (conj acc (if next-hop next-hop phi)) xi1))
            :else
            (let [cht (get-in (meta xi) [:PARSE 0 :children-txts])
                  next-hop (get-in cht [phi 3])] 
              (recur (inc ix) (conj acc (if next-hop next-hop phi)) xi1))))))))

(defn vpath2path [x path]
  (let [n (count path)]
    (loop [ix 0 acc [] xi x]
      (if (= ix n) acc
        (let [phi (nth path ix) xi1 (collections/cget xi phi)]
          (cond (or (sequential? xi) (not (coll? xi))) ; unchanged.
            (recur (inc ix) (conj acc phi) xi1)
            (map? xi)
            (let [cht (get-in (meta xi) [:PARSE 0 :children-txts])
                  next-hop (first (filter #(= (get-in cht [% 0 3]) phi) (keys cht)))] 
              (recur (inc ix) (conj acc (if next-hop next-hop phi)) xi1))
            :else
            (let [cht (get-in (meta xi) [:PARSE 0 :children-txts])
                  next-hop (first (filter #(= (get-in cht [% 3]) phi) (keys cht)))] 
              (recur (inc ix) (conj acc (if next-hop next-hop phi)) xi1))))))))

(defn end-mpath [x force-parent? ph tail? sub-tail?]
  "The [path within x, path within (meta x)] that leads to the head and tail space assigned to (cget-in x ph).
   Non-metable elements depend on thier parent collection. use collections/dual-get-in to access the meta."
  (let [xi (collections/cget-in x ph)
        parent? (or force-parent? (not (meta xi)))
        ph1 (if parent? (into [] (butlast ph)) ph)
        xi1 (if parent? (get-in x ph1) xi)
        mxi1 (meta xi1)
        ixp (if tail? (dec (count (:PARSE mxi1))) 0)
        mp (conj (if parent? [:PARSE ixp :children-txts (last (path2vpath x ph))] 
                   [:PARSE ixp :txt]) (if tail? 2 0))
        mp1 (if (vector? (get-in mxi1 mp)) (conj mp (if sub-tail? 2 0)) mp)]
    [ph1 mp1]))

(defn prev-patht [x path]
  "Returns the [path tail? sub-tail?] that is immediatly before path in reading order.
   nil if no prev path exists.
   tail? = before or after the element set by the output path.
   sub-tail? = before and after the () within a collection. Has no effect if not within a collection."
  (if (not= path [])
    (let [pathv (path2vpath x path) n (count path)
          wrap? (= (get pathv (dec n)) 0) 
          pathv1 (if wrap?
                   (into [] (butlast pathv))
                   (update pathv (dec n) dec))
          path1 (vpath2path x pathv1)]
    [path1 (not wrap?) wrap?])))

(defn next-patht [x path]
  (if (not= path [])
    (let [pathv (path2vpath x path) n (count path)
          nk (count (collections/cget-in x (butlast path)))
          wrap? (= (get pathv (dec n)) (dec nk)) 
          pathv1 (if wrap?
                   (into [] (butlast pathv))
                   (update pathv (dec n) inc))
          path1 (vpath2path x pathv1)]
    [path1 wrap? false])))


(defn set-ends-length [x path n-head-end n-tail-end]
  "Specifies how many chars to keep in the head and tail of the object bound to path.
   If the number of chars is specified incorrectly.
   Does not change the string.
   Use-cases include removing an object without removing comments,
   and undoing a log insert without any extra stuff leftover or missing stuff."
  (reduce 
    (fn [xo on-parent?]
      (let [prev-pt (prev-patht xo path) next-pt (next-patht xo path)
            mp-prev (if prev-pt (apply end-mpath xo on-parent? prev-pt))
            mp-next (if next-pt (apply end-mpath xo on-parent? next-pt))
            mp-head (end-mpath xo on-parent? path false false)
            mp-tail (end-mpath xo on-parent? path true true)
            set-e (fn [xj mp-us mp-them n-us us-first?] 
                    (let [us (collections/dual-get-in xj mp-us)
                          them (collections/dual-get-in xj mp-them)]
                      (if (or (not us) (not them) (not mp-us) (not mp-them)) 
                        xj
                        (let [combine (if us-first? (str us them) (str them us))
                              nc (count combine)
                              n-us1 (max 0 (min nc n-us))
                              c1 (subs combine 0 (if us-first? n-us1 (- nc n-us1)))
                              c2 (subs combine (count c1))
                              us1 (if us-first? c1 c2) them1 (if us-first? c2 c1)]
                         (-> xj (collections/dual-assoc-in mp-us us1) 
                           (collections/dual-assoc-in mp-them them1))))))]
        (-> xo (set-e mp-head mp-prev n-head-end false) 
          (set-e mp-tail mp-next n-tail-end true))))
     x [false true]))


(defn lv-update [x mod-fn & args]
  "Metadata-preserving sequential collection modification.
   mod-fn must not depend on the contents of x."
  (let [n (count x) x1 (into [] (range n))
        x1 (if (vector? x) x1 (apply list x1))
        x2 (into [] (apply mod-fn x1 args)) n2 (count x2)
        mx (meta x)
        ;
        ;old2new (reduce #(assoc %1 %2) {} (range (count x2)))
        ch (get-in mx [:PARSE 0 :children-txts])
        esn #(if (get %1 %2) (get %1 %2) "")
        ch1 (reduce #(let [from (get x2 %2)
                           ch-from (get ch from)] ; nil unless integer within range. 
                       (if ch-from
                         (assoc %1 %2 
                           (vector (esn ch-from 0) (esn ch-from 1) (esn ch-from 2))) %1))
              (zipmap (range n2) (mapv #(vector "" "" "" %) (range n2))) (range n2))
        mx1 (assoc-in mx [:PARSE 0 :children-txts] ch1)]
   (with-meta (apply mod-fn x args) mx1)))