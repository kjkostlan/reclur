; Calculation of indentation level and tools to read strings into code.

(ns clooj.coder.io (:require [clojure.string :as string] [clooj.java.file :as jfile] [clooj.utils :as utils]))


(defn try-to-read [s]
  "Like read-string but nil instead of error and will do nothing for non-strings.
   Good for code-scanners because we don't want an error on bad code."
  (if (string? s) (try (read-string s) (catch Exception e nil)) s))

;; Guess-and-check-esque functions to read parts of the codebase.

(defn status [strng0]
  "0 = valid, 1 = EOF error, -1 = some other error. Detecting EOF errors are useful."
  (let [strng (if (string? strng0) strng0 (apply str strng0))]
    (try 
      (do (read-string strng) 0)  
      (catch Exception e 
        (let [es (str e)] 
          ;(println es)
          (if (.contains es "EOF while reading") 1
            -1))))))

(defn syn? [char]
  "Syntax characters that can alter the code structure or reader. Pass in a one char string or a character."
  (.contains "\"'`()[]{}#;\n^" (if (string? char) char (str char))))

(defn open? [char]
  "Stuff like [ ( and { that makes the nesting level deeper, does not include quotes."
  (.contains "([{" (if (string? char) char (str char))))
(defn jopen? [char]
  "The java version."
  (.contains "([{<" (if (string? char) char (str char))))

(defn close? [char]
  "Stuff like ] ) and }, does not include quotes."
  (.contains "])}" (if (string? char) char (str char))))
(defn jclose? [char]
  " The java version."
  (.contains "])}>" (if (string? char) char (str char))))   

(defn white? [char]  
  "Whitespace."
  (.contains " \t\n" (if (string? char) char (str char))))

(defn macrochar? [char] 
  "code/syntax quotes, # signs, and the like. TODO: include ^, etc?"
  (.contains "@'`~#" (if (string? char) char (str char))))

(defn loosemacrochar? [char]
  "also includes = and ? and ^"
  (or (= (str char) "=") (= (str char) "?") (= (str char) "^") (macrochar? char)))

(defn possible-starts [text0]
  "Gets a superset of locations for which an island can start."
  ; look for open? and backtrack (becuse of reader macros like quoting).
  (let [text (apply vector text0)
        opens (utils/which open? text)
        backtrak (fn [ind0]
                   (loop [valid ind0 ind (dec ind0)] ; valid = starts on an open or macro connected to open.
                     (if (< ind 0) valid; went all the way to beginning.
                       (let [ch (nth text ind) nxt (dec ind)]
                         (if (macrochar? ch) 
                           (recur ind nxt); both valid and the index changes.
                           (if (white? ch)
                             (recur valid nxt); the index changes, but valid is stuck.
                             valid))))))] ; neither whitespace nor macro.
  (into [] (map backtrak opens))))                   
                     
(defn close_ [str issyn]
  ; "(some ) string" => x, such that (subs strng x) => "(some )", or -1 if it fails. ONLY PASS IN VECTORS!
  (let [n (count str)
        isclose (map close? str)
        closers (map #(+ % 1) (utils/which identity isclose)) ; the index + 1 b/c of how subvec works.
        valid (map #(= (status (subvec str 0 %)) 0) closers) ; lazy for performance, dont do everything.
        which-valid (filter #(> % 0) (map #(if %1 %2 0) valid closers))
        ind (into [] (take 1 which-valid))]
    ;(println closers)
    (if (= (count ind) 0) -1 (nth ind 0))))
(defn find-close [str0 openind]
  "Returns the closing bracket index for str, -1 if it malformed, etc, or immediatly if you dont start on a special char. Pass in vectors for best performance."
  ; TODO: make this more performant by being smart at balancing closed stuff vs open stuff, but be careful read-string is slippery.
  (if (>= openind (count str0)) -1 
    (let [text_ (if (string? str0) (apply vector str0) str0) ; everything to vector, which saves on performance IF you pass a vector in.
          ;tmp (println "bounds? " (count text_) openind)
          text (subvec text_ openind) ; O(1) slicing so even a huge vector in is not a large cost.
          issyn (map syn? text)]
      (if (nth issyn 0)
        (if (< (count issyn) 2)
          -1 ; there is at most two syntax character, so it can't be closed.
          (let [cl (close_ text issyn)]
            (if (< cl 0) -1 (+ cl openind))))
        openind)))) ; does not start on a syntax character, so closes right at open.

(defn _close-quote [text quotes qind] 
  ; index on quotes of closing quote, starting on opening quote. -1 = invalid.
  (let [nq (count quotes) st0 (nth quotes qind)
        ; regular expressions:
        st (if (and (> st0 0) (= (nth text (dec st0)) \#)) (dec st0) st0)]
    (loop [ind qind]
      (if (>= (inc ind) nq) -1 ; no closing quote ahead of us.
        ; if we can encapsulate it in quotes we win:
        (if (= (status (str "(" (apply str 
                  (subvec text st (inc (nth quotes (inc ind))))) ")")) 0) 
          (inc ind)
          (recur (inc ind)))))))

(defn _close-comment [text comments cind]
   ; All it takes is a newline to close a comment.
   ; Returns [nextcind nextinc], where nextcind is after the close of our comment.
   (let [nt (count text) nc (count comments)]
    (loop [ind (inc cind) ix (nth comments cind)]
      (if (>= ix nt) [-1 (dec nt)]; all the way to the end.
         (if (= (nth text ix) \newline) 
           [ind ix]; found newline.
           (if (or (= ind -1) (>= ind nc)) 
             (recur -1 (inc ix)) ; no more comments.
             (if (> (inc ix) (nth comments ind))
               (recur (inc ind) (inc ix))
               (recur ind (inc ix)))))))))

(defn quote-comment [text0]
  "Generates vectors by filtering quotes and comments. 0 = normal, 1 = quote, 2 = comment, 1 &2 are inclusive"
  (let [text (into [] text0) textlast (into [] (butlast (cons " " text)))
        quotes (into [] (utils/which #(= % \") text))
        comments (into [] (utils/which #(= % \;) text))
        ;newlines (into [] (utils/which #(= % \newline) text))
        ; \" can NEVER start quotes or comments but CAN end quotes if the \ is escaped.
        canquotestart (into [] (map #(and (not (= (nth textlast %) \\))) quotes)) ;(not (= (nth textlast %) \#))
        cancommentstart (into [] (map #(not (= (nth textlast %) \\)) comments))
        nq (count quotes)
        nc (count comments)
        n (count text)
        big (inc n)
        ]
     (loop [qind 0 cind 0 ix 0 mode (into [] (repeat n 0))]
        ;(println qind cind ix mode)
        (let [openq (loop [ql qind] ; the start of the next quote, index on quote array
                      (if (>= ql nq) big
                        (if (and (>= (nth quotes ql) ix) (nth canquotestart ql))
                          ql (recur (inc ql)))))
              openc (loop [cl cind] ; same for comments instead of quotes.
                      (if (>= cl nc) big
                        (if (and (>= (nth comments cl) ix) (nth cancommentstart cl))
                          cl (recur (inc cl)))))
              ixopenq (if (< openq big) (nth quotes openq) big) ;inxed on main array.
              ixopenc (if (< openc big) (nth comments openc) big)
              ixopen (min ixopenq ixopenc)]
              (if (> ixopen n) mode ; end of string, we are done.
                (if (< ixopenq ixopenc)
                  ; the next quote is b4 the next comment. quind and cind bieng too low is OK.
                  ; closeq should NEVER be -1 unless a bad string.
                  (let [closeq (_close-quote text quotes openq)
                        ixcloseq (if (= closeq -1) big (nth quotes closeq))]
                      (if (= closeq -1) mode ; bail.
                        (recur (inc openq) cind (inc ixcloseq) (utils/set-range mode ixopenq (inc ixcloseq) 1))))
                  ; Comments come before quotes. Comments closing at -1 indicate the last char is a comment.
                  (let [cc (_close-comment text comments openc)
                        closec0 (nth cc 0) ixclosec (nth cc 1)
                        closec (if (= closec0 -1) big closec0)]; closec0 -1 means no more comments.
                      (recur qind closec (inc ixclosec) (utils/set-range mode ixopenc (inc ixclosec) 2)))))))))

(defn _nxt [a ind ix] 
 ; next INDEX above ix, or 1e100 if we don't find anything.
 (loop [iind ind]
   (if (>= iind (count a)) 1e100
     (if (> (nth a iind) ix) iind
     (recur (inc iind))))))
(defn _ixt [a ind ix] 
 ; next VALUE above ix, or 1e100 if we don't find anything.
 (loop [iind ind]
   (if (>= iind (count a)) 1e100
     (if (> (nth a iind) ix) (nth a iind)
     (recur (inc iind))))))
(defn quote-comment-java [text0]
  "Extraction of quotes and comments from java. Not as bulletproof, as java is secondary in priority."
  (let [text (into [] text0) textnext (into [] (rest (conj text " ")))
        quotes (into [] (utils/which #(= % \") text))
        linecomments (into [] (utils/which identity (map #(and (= %1 \/) (= %2 \/)) text textnext)))
        blockstarts (into [] (utils/which identity (map #(and (= %1 \/) (= %2 \*)) text textnext)))
        blockends (into [] (utils/which identity (map #(and (= %1 \*) (= %2 \/)) text textnext)))
        newlines (into [] (utils/which #(= % \newline) text))
        n (count text)]
     (loop [ix 0 qind 0 lcind 0 bsind 0 beind 0 nlind 0 mode (into [] (repeat n 0))]
       (let [qnxt (_ixt quotes qind ix) lcnxt (_ixt linecomments lcind ix)
             bsnxt (_ixt blockstarts bsind ix)
             soonest (min qnxt lcnxt bsnxt)]
         (if (> soonest (count text)) 
           mode ; DONE. dix (dec newix)
           (let [modeix
             (if (= soonest qnxt) ; quotes start things.
               (let [cq (_close-quote text quotes qind)
                     ixc (if (and (>= cq 0) (< cq (count quotes))) (nth quotes cq) 1e100)]
                {:mode  (utils/set-range mode soonest ixc 1) :ix ixc})
               (if (= soonest bsnxt) ; block comments go until the a block end.
                 (let [ixc (_ixt blockends beind soonest)]
                   {:mode  (utils/set-range mode soonest ixc 2) :ix (inc ixc)})
                 ; soonest = lcnxt, which means line comments.
                 (let [ixc (_ixt newlines nlind soonest)]
                   {:mode  (utils/set-range mode soonest ixc 2) :ix ixc})))]
            (let [ix1 (:ix modeix) m (:mode modeix)]
              (recur ix1 (_nxt quotes qind ix1) (_nxt linecomments lcind ix1)
                (_nxt blockstarts bsind ix1) (_nxt blockends beind ix1) (_nxt newlines nlind ix1) m))))))))

(defn _macro-chain-down [text ind] 
  ; the last index of a macro character in text.
  (loop [ix ind]
    (if (= ix 0) 0 ; bottom.
      (if (loosemacrochar? (nth text ix))
        (recur (dec ix))
        (inc ix)))))

(defn parse-summary [text0]
  "Summarizes things once per character 
  :level = indent level, inclusive. :mode = 0 for normal, 1 = quote, 2 = comment.
  SEEMS VERY ROBUST."
  ;(println "hi")
  (let [text (into [] (str " " text0)) ; incase the first one is a space.
        qc (quote-comment text)
        normal (into [] (map #(= % 0) qc))
        n (count text)
        level0 (loop [acc (into [] (repeat n 0)) ix 0 lev 0]
                (if (= ix n) acc ; end of loop.
                  (let [o (open? (nth text ix)) c (close? (nth text ix))
                        eoc (and (> ix 0) (= (nth text (dec ix)) \\ )) ; \ escapes the effect of the level change.
                        eoc (if (< ix 2) eoc (if (= (nth text (- ix 2)) \\ ) false eoc)) ; \\ escapes the escape!
                        nm (nth normal ix) ix1 (inc ix)]
                    (if (or (not nm) (and (not o) (not c)))
                      (recur (assoc acc ix lev) ix1 lev); nothing special.
                      (if o
                        (recur (assoc acc ix (if eoc lev (inc lev))) ix1 (if eoc lev (inc lev)))
                        (recur (assoc acc ix lev) ix1 (if eoc lev (dec lev)))))))); must be c
         ; reader macro chars before a level jump go up.
         level (loop [acc level0 ix (dec n)]
                 (if (<= ix 0) acc ; work backwards all the way to beginning.
                   (let [lev (nth level0 ix) lev0 (nth level0 (dec ix))
                         drop (< lev0 lev)]; level drops may not drop just yet b/c macro chars.
                     (if drop
                       ; drop zone, be careful.
                       (let [lo0 (_macro-chain-down text (dec ix))
                             lo (max lo0 0)]
                         (if (< lo ix) ; did we actually find anything?
                           (recur (utils/set-range acc lo ix lev) (dec lo))
                           (recur acc (dec ix))))
                       (recur acc (dec ix)))))); not drop.
         ]
         {:level (into [] (rest level)) :levelnomacro (into [] (rest level0)) :mode (into [] (rest qc))}))

(defn parse-summary-java [text0] 
  (let [text (into [] (str " " text0))
        qc (quote-comment-java text)
        normal (into [] (map #(= % 0) qc))
        n (count text)
        level (loop [acc (into [] (repeat n 0)) ix 0 lev 0]
                (if (= ix n) acc ; end of loop.
                  (let [o (jopen? (nth text ix)) c (jclose? (nth text ix)) 
                        nm (nth normal ix) ix1 (inc ix)]
                    (if (or (not nm) (and (not o) (not c)))
                      (recur (assoc acc ix lev) ix1 lev); nothing special.
                      (if o
                        (recur (assoc acc ix (inc lev)) ix1 (inc lev))
                        (recur (assoc acc ix lev) ix1 (dec lev)))))))
        ] ; its the same with or without macros as java doesn't have macros.
    {:level (into [] (rest level)) :levelnomacro (into [] (rest level)) :mode (into [] (rest qc))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These functions work at a higher level:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    

(defn extract-outer-islands [text-or-parsed]
  "Extracts outer level islands from a text OR a text analysis. Using subs pulls the island out, inclusive to the edge ()'s."
  (let [parsed (if (map? text-or-parsed) text-or-parsed (parse-summary text-or-parsed)) ; use already-parsed code to avoid having to parse it here.
        levels (:level parsed) levels (if (vector? levels) levels (into [] levels)) n (count levels)
        n0 (dec n)
        changes
          (loop [zero1 (if (= (first levels) 1) [0] []) one0 [] ix 0]
            (if (= ix n0) {:zero1 zero1 :one0 one0}
                (let [l0 (nth levels ix) l1 (nth levels (inc ix))
                      ; only outer islands:
                      up?   (and (= l0 0) (= l1 1))
                      down? (and (= l0 1) (= l1 0))]
                  (recur (if up? (conj zero1 (inc ix)) zero1) (if down? (conj one0 (inc ix)) one0) (inc ix)))))
        zero1 (:zero1 changes) one0 (if (= (last levels) 1) (conj (:one0 changes) n) (:one0 changes))]
      (mapv #(vector %1 %2) zero1 one0)))

(defn local-island [text-or-parsed caret]
   "Gets the local island's start and end, whichever caret is in."
  (let [parsed (if (map? text-or-parsed) text-or-parsed (parse-summary text-or-parsed)) ; use already-parsed code to avoid having to parse it here.
        levels (:level parsed) levels (if (vector? levels) levels (into [] levels)) n (count levels)
        l0 (nth levels caret)
        lo (loop [ix caret]
              (cond (= ix 0) 0 
                    (< (nth levels ix) l0) (inc ix)
                    :else (recur (dec ix))))
        hi (loop [ix caret]
              (cond (= ix n) n 
                    (< (nth levels ix) l0) ix
                    :else (recur (inc ix))))] [lo hi]))

; TODO: is an exaustive parse nessessary for getting the local island?
    