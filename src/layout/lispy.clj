(ns layout.lispy
  (:require [app.rtext :as rtext] [app.codebox :as codebox]
    [coder.crosslang.langs :as langs])) ; No good editor is complete without lisp-aware tools.

(defn _multy-find [stream-tys ix fs dir]
  "Useful go untill you find a certain type fn."
  (let [n (count stream-tys)
        single-find (fn [ix f]
                      (loop [ix1 ix]
                        (cond (= dir 0) (throw (Exception. "Anti inf loop!"))
                          (<= ix1 0) 0
                          (>= ix1 (dec n)) (dec n)
                          (f (nth stream-tys ix1)) ix1
                          :else (recur (+ ix1 dir)))))]
    (reduce #(single-find %1 %2) ix fs)))

(defn finish-form [stream-tys inter-levels cursor-ix dir tweak-start eat-the-zero?]
  "Returns the final cursor ix after the form."
  (let [lev (get inter-levels cursor-ix 0)
        ix-start (+ cursor-ix dir tweak-start)
        n (count inter-levels)
        zero-eat (fn [ix-a]
                   (let [ix (if (> dir 0) ix-a (dec ix-a))]
                     (loop [ix1 ix]
                       (if (or (<= ix1 0) (>= ix1 n) (> (nth stream-tys ix1) 0))
                         (if (> dir 0) ix1 (inc ix1)) (recur (+ ix1 dir))))))]
    ((if eat-the-zero? zero-eat identity)
      (loop [ix ix-start hit-tok 0]
        (if (or (<= ix 0) (>= ix n)) ix
          (let [lev1 (nth inter-levels ix)
                ty (get stream-tys ix 0)
                ix-a (if (> dir 0) ix (inc ix))]
            (cond (> lev1 lev) (recur (+ ix dir) hit-tok)
              (< lev1 lev) ix-a
              (and (> hit-tok 0) (not= ty hit-tok)) ix-a
              :else (recur (+ ix dir) (if (not= hit-tok 0) hit-tok ty)))))))))

(defn splice-at-cursor [box]
  "Splices at the cursor. (foo | (bar baz)) => foo (bar baz)"
  (let [quote-char-ixs
           (let [txt (rtext/rendered-string box)
                 cur-ix (:cursor-ix box)
                 stream-tys (langs/stream-tokenize txt (get box :langkwd :clojure))
                 ty-cur (get stream-tys cur-ix 0)]
             (if (= ty-cur 3)
               (let [ix0-maybe-quote (inc (_multy-find stream-tys cur-ix [#(= % 0)] -1))
                     ix1-maybe-quote (dec (_multy-find stream-tys cur-ix [#(= % 0)] 1))]
                 (if (and (= (str (nth txt ix0-maybe-quote)) "\"") (= (str (nth txt ix1-maybe-quote)) "\""))
                   [ix0-maybe-quote (inc ix1-maybe-quote)]))))
        [ix0 ix1] (if quote-char-ixs quote-char-ixs (codebox/contain-ixs box))
        box1 (rtext/edit box (dec ix1) ix1 "" [])
        box2 (rtext/edit box1 ix0 (inc ix0) "" [])] box2))

(defn wrap-at-cursor [box txt open close]
  "The opposite of splice. For example, wrapping a (time x) around x."
  (let [stream-tys (langs/stream-tokenize
                     (rtext/rendered-string box)
                     (get box :langkwd :clojure))
        cur-ix (:cursor-ix box) n (count stream-tys)

        ty-cur (get stream-tys cur-ix 0)
        fs (if (= ty-cur 0) [#(not= % 0) #(= % 0)] [#(= % 0)])
        ix0 (inc (_multy-find stream-tys cur-ix fs -1))
        ix1 (_multy-find stream-tys cur-ix fs 1)
        edit? (and ix0 ix1)
        box1 (if edit? (rtext/edit box ix1 ix1 close []) box)
        box2 (if edit? (rtext/edit box1 ix0 ix0 (str open txt) box1))] box2))

(defn slurp-barf [box slurp? dir]
  "No lisp-based editor is complete without this."
  (let [[ix0 ix1] (codebox/contain-ixs box)
        txt (rtext/rendered-string box)
        inter-levels (langs/interstitial-depth txt (get box :langkwd :clojure))

        stream-tys (langs/stream-tokenize txt (get box :langkwd :clojure))

        not-paren #(and (not= % 4) (not= % 5) (not= % 0))
        fs [not-paren #(= % 0)]
        ix-bracket (if (>= dir 0) (dec ix1) ix0)
        bracket (subs txt ix-bracket (inc ix-bracket))
        c-dir (if (or (and (> dir 0) slurp?) (and (< dir 0) (not slurp?))) 1 -1)
        c-ix (if (< c-dir 0) ix-bracket (inc ix-bracket))
        tweak (if (and (< dir 0) (not slurp?)) -1 0)
        eat-the-zero? (not slurp?)
        ix-add (finish-form stream-tys inter-levels c-ix c-dir tweak eat-the-zero?)
        box-debug (rtext/edit box ix-bracket (inc ix-bracket) "" [])]
    (rtext/scroll-to-see-cursor
      (if (< ix-add ix-bracket)
        (-> (rtext/edit box ix-bracket (inc ix-bracket) "" [])
          (rtext/edit ix-add ix-add bracket []))
        (-> (rtext/edit box ix-add ix-add bracket [])
          (rtext/edit ix-bracket (inc ix-bracket) "" []))))))

(defn _set-where [box new-cursor-ix sel?]
  (let [cursor-ix (:cursor-ix box) box (assoc box :cursor-ix new-cursor-ix)]
    (rtext/scroll-to-see-cursor
      (if sel?
        (let [stuff-selected? (> (:selection-end box) (:selection-start box))
              sels (concat [cursor-ix new-cursor-ix]
                     (if stuff-selected? [(:selection-end box) (:selection-start box)] []))]
          (assoc box :selection-start (apply min sels) :selection-end (apply max sels))) box))))

(defn next-thing [box dir sel?]
  "Moves the cursor to the next place, jumping over levels."
  (let [cursor-ix (:cursor-ix box)
        txt (rtext/rendered-string box)
        inter-levels (langs/interstitial-depth txt (get box :langkwd :clojure))

        stream-tys (langs/stream-tokenize txt (get box :langkwd :clojure))

        cursor-ix-tweak (if (and (= dir 1) (= (get stream-tys cursor-ix) 4)) (dec cursor-ix) cursor-ix) ; Why do we need this tweak?
        eat-the-zero? false
        tweak-start 0
        cur-ix1 (finish-form stream-tys inter-levels cursor-ix-tweak dir tweak-start eat-the-zero?)
        cur-ix1 (min (count txt) (max 0 (if (= cur-ix1 cursor-ix) (+ cursor-ix dir) cur-ix1)))]
    (_set-where box cur-ix1 sel?)))

(defn next-tok [box dir sel?]
  "Moves the cursor to the next non-zero token."
  (let [cursor-ix (:cursor-ix box)
        txt (rtext/rendered-string box)
        stream-tys (langs/stream-tokenize txt (get box :langkwd :clojure))

        cix #(if (< dir 0) (dec %) %) n (count txt)
        limit #(cond (and (< dir 0) (<= % 0)) 0
                 (and (> dir 0) (>= % n)) n
                 :else false)
        ix1 (loop [ix cursor-ix] ; Mostly duplicated code with these loops...
              (if (limit ix) (limit ix)
                (let [ty-i (get stream-tys (cix ix))]
                  (if (= ty-i 0) (recur (+ ix dir)) ix))))

        ty0 (get stream-tys (cix ix1) 0)
        ix1 (loop [ix (+ ix1 dir)]
              (if (limit ix) (limit ix)
                (let [ty-i (get stream-tys (cix ix))]
                  (if (= ty-i ty0) (recur (+ ix dir)) ix))))]
    (_set-where box ix1 sel?)))
