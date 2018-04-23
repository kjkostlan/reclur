; Tools for analyzing and implementing string differences.
; Edits have the format of {:ix0 :ix1 :value}, and :type sometimes to inform the overall type but doesn't affect string based edits.
(ns app.stringdiff)


; Far from the best algorithm but covers almost all of the editors needs:
; 1:1 coorespond from the end and the beginning (catches all manual text-editor edits).
; chunked near-1:1 string Levenshtein (catches find-replace and refactoring unless massive changes are made).
; Coorespondances are vectors, 0 for on both s0 and s1, 1 for only on s1 and -1 for only on s0.

(defn length-delta [edit]
  (+ (:ix0 edit) (- (:ix1 edit)) (count (if (string? (:value edit)) (:value edit) (throw (Exception. "Non-string edit"))))))

(defn shift-edit [edit delta] "Convenience fn."
  (update (update edit :ix0 #(+ % delta)) :ix1 #(+ % delta)))

(defn total-length-delta [edits]
  "Change in length when edits are applied to a string. Throws an error for non-string values."
  (apply + (mapv length-delta edits)))

(defn window-edits [edits ix0 ix1 goes-to-end-of-str?]
  "Returns edits that are applied to s0-piece = (subs s0 ix0 ix1).
   Breaking s0 up in some way, applying window-edits to each s0-piece, and recombining
   is the same as applying the edits to the entire s0.
   Thus the s0-piece will be dragged along by edits that change the length of earlier sections."
  (let [; Edits that cover two pieces go the the lower piece.
        n (count edits) k (- ix1 ix0)
        ix1 (if goes-to-end-of-str? (inc ix1) ix1)]
    (loop [acc [] ix 0 shift (- ix0)]
      (if (= ix n) acc
        (let [ed (shift-edit (nth edits ix) shift)
              low? (< (:ix0 ed) 0)
              hi? (>= (:ix0 ed) k)]
          (recur (if (or low? hi?) acc (conj acc ed))
            (inc ix)
            ; shift by number of characters deleted in edits that are too low:
            (if low? (+ shift (- (length-delta ed))) shift)))))))

(defn apply-edit [s edit] "single edit, value taken as string."
  (str (subs s 0 (:ix0 edit)) (:value edit) (subs s (:ix1 edit))))

(defn apply-edits [s edits]
  "Convenience function. It would be possible to avoid the expensive string 
   copy each time though significant effort. StringBuilder class maybe? Array mapping translation?"
  (reduce apply-edit s edits))

(defn levenshtein-corr [^String s0 ^String s1]
   "Adapted from https://stackoverflow.com/questions/15042879/java-characters-alignment-algorithm"
    (let [n0 (int (count s0)) n1 (int (count s1))
          
          stride (int (inc n1)) ; T[i][j] => (aget T (+ j (* i stride)))
          ^ints T (make-array Integer/TYPE (* (inc n0) (inc n1)))
          ^chars cs0 (.toCharArray s0) ^chars cs1 (.toCharArray s1)]
      (loop [i (int 0)]
        (if (<= i n0) (do (aset T (* i stride) i) (recur (inc i)))))
      (loop [j (int 0)]
        (if (<= j n1) (do (aset T j j) (recur (inc j)))))
        
      (loop [i (int 1)]
        (if (<= i n0) 
          (do (loop [j (int 1)]
                (if (<= j n1)
                  (do
                    (if (= (aget cs0 (dec i)) (aget cs1 (dec j)))
                      (aset T (+ j (* i stride)) 
                        (aget T (+ (dec j) (* (dec i) stride))))
                      (let [x (aget T (+ j (* (dec i) stride)))
                            y (aget T (+ (dec j) (* i stride)))]
                        (aset T (+ j (* i stride)) 
                          (if (< x y) (inc x) (inc y)))))
                    (recur (inc j))))) (recur (inc i)))))
      (into [] 
        (rseq
          (loop [acc [] i (int n0) j (int n1)]
             (if (and (<= i 0) (<= j 0)) acc
               (let [Tij (aget T (+ j (* i stride)))]
                 (cond (and (> i 0) (= Tij (+ (aget T (+ j (* (dec i) stride))) 1)))
                   (recur (conj acc -1) (dec i) j)
                   (and (> j 0) (= Tij (+ (aget T (+ (dec j) (* i stride))) 1)))
                   (recur (conj acc 1) i (dec j))
                   :else
                   (recur (conj acc 0) (dec i) (dec j))))))))))

(defn chunk-corr [s0 s1]
  "Chunk coorelation, edits larger than the chunk size will not create efficient corrs."
  (let [n0 (count s0) n1 (count s1) chunk-size 128 corr-size 127]
    (loop [acc [] ix0 0 ix1 0]
      (let [ix0+ (min n0 (+ ix0 chunk-size))
            ix1+ (min n1 (+ ix1 chunk-size))
            si0 (subs s0 ix0 ix0+)
            si1 (subs s1 ix1 ix1+)]
        (if (and (= (count si0) 0) (= (count si1) 0)) (into [] (apply concat acc))
          (let [lc (levenshtein-corr si0 si1)
                corr (if (> (count lc) corr-size) (subvec lc 0 corr-size) lc) ; this shave off the end will help them stay 1:1 for slight shifts.
                shift0 (count (filterv #(not= % 1) corr))
                shift1 (count (filterv #(not= % -1) corr))]
            (recur (conj acc corr) (+ ix0 shift0) (+ ix1 shift1))))))))

(defn edge-match-count [^String s0 ^String s1]
  "the # of chars that match at the [beginning end]. Should be full java speed for long strings."
  (let [n0 (count s0) n1 (count s1) nMin (min n0 n1)]
    (cond (and (= n0 n1) (= s0 s1)) [n0 1] ; n0 = n1 in this case.
      (= (subs s0 0 nMin) (subs s1 0 nMin)) [nMin 0]
      :else
      (let [^chars cs0 (.toCharArray s0) ^chars cs1 (.toCharArray s1)
            beginning (loop [ix (int 0)]
                        (if (= (aget cs0 ix) (aget cs1 ix)) (recur (inc ix)) ix))
            diff (- n1 n0)
            min-ix1 (+ beginning (dec n1) (- (min n0 n1)))
            end (loop [ix1 (int (dec n1))]
                  (if (and (> ix1 min-ix1) (= (aget cs0 (- ix1 diff)) (aget cs1 ix1)))
                    (recur (dec ix1)) (- (dec n1) ix1)))]
        [beginning end]))))

(defn corr-to-edits [s0 s1 corr]
  "Converts to edits that can be applied in order, using our edge + chunked corr."
  (let [corr (conj corr 0) n (count corr) ; pad so that we flush the edit before exiting the loop.
        ledits (loop [acc [] ix 0 ix0 0 ix1 0 cur-ed-count 0 cur-ed-type 0]
                 (if (= ix n) acc 
                   (let [ci (nth corr ix) ix0+ (if (= ci 1) ix0 (inc ix0))
                         ix1+ (if (= ci -1) ix1 (inc ix1))]
                     (if (= ci cur-ed-type) (recur acc (inc ix) ix0+ ix1+ (inc cur-ed-count) cur-ed-type)
                       (recur (if (= cur-ed-type 0) acc 
                                (conj acc (if (= cur-ed-type 1) {:ix0 ix0 :ix1 ix0 :value (subs s1 (- ix1 cur-ed-count) ix1)}
                                            {:ix0 (- ix0 cur-ed-count) :ix1 ix0 :value ""})))
                         (inc ix) ix0+ ix1+ 1 ci)))))]
    (into [] (rseq ledits)))) ; put higher-indexed edits first.

(defn edits-between [s0 s1]
  "Edits (as in what rtext calls an edit) to get from s0 to s1.
   Edits must be applied in the order this fn gives them.
   Tries to be minimalistic, i.e. heuristic toward minimal number of edits and 
   total size of edits.
   TODO: improve combining edits."
  (if (= s0 s1) []
    (let [edge (edge-match-count s0 s1) e0 (first edge) e1 (second edge)
          n0 (count s0) n1 (count s1)
          s0-mid (subs s0 e0 (- n0 e1))
          s1-mid (subs s1 e0 (- n1 e1))
          mid-edits (corr-to-edits s0-mid s1-mid (chunk-corr s0-mid s1-mid))]
      (if (not= (apply-edits s0-mid mid-edits) ; DEBUG TODO remove when trusted.
            (subs s1 e0 (- (count s1) e1))) (throw (Exception. "Edits-between fn failed"))) ; DEBUG TODO: remove when I can trust it.
      (mapv (fn [ed] (-> ed (update :ix0 #(+ % e0)) (update :ix1 #(+ % e0)))) mid-edits))))






#_(let [s0 "foobarbaz"
      s1 "zoozarzaaz"
      edits (edits-between s0 s1)
      wixs [0 2 6] wixs1 (conj (into [] (rest wixs)) (count s0))
      pieces0 (mapv #(subs s0 %1 %2) wixs wixs1) n (count wixs)
      eds (mapv #(window-edits edits %1 %2 (= %3 (dec n))) wixs wixs1 (range n))
      pieces1 (mapv apply-edits pieces0 eds)
] (println eds pieces0 pieces1))