; String and code manipulation that keeps track of position.
; Each data-structure stores :obj (which is the sub-string, etc you want), and :pos.

; String manipulation and query tools, such as regular expressions et al, that are aware of position 
; on the original string.
; Also, you can put in character ranges similar to substring.

 ; stuff is stored as {:obj :pos} key-value pairs, vanilla strings are auto-converted into these with :pos = [0 1 2 ...]
    ; Pos always encodes where on the original string's location, and has one element per character.
    ; Example: #"hi+" matching "1hi hii" is: [{:obj "hi" :pos [1 2]} {:obj "hii" :pos [4 5 6]}]
 ; Functions operate on these :obj :pos objects
 ; No laziness whatsoever, even for normally lazy functions, it is simply too hard to program lazy regexp's, etc. 
 
; TODO:
    ; Applying something twice, such as to string replacements, will propigate where on the original string.
       ; Certain things will make :pos non-integer, such as (replace "our x 100% earthling" "x" "world")
         ;will be [0 1 2 3 . . . . . 5] with the .'s 3.33 3.66 4 4.33 4.66 which coorespond to world.
         ;round these numbers at the end to get cursor locations, etc.

(ns clooj.coder.strmap
 (:require [clojure.string :as string] [clooj.coder.grammer :as grammer] [clooj.coder.io :as cio]))

(defn rangev [& args]
 "Not lazy."
 (into [] (apply range args)))

(defn formatty [strn]
  "Turns strings into {:obj :pos} objects, with :pos [0 1 2 ... n]. {:obj :pos} objects are left alone."
  (cond (map? strn) strn ; already a map, should have :pos.
        (string? strn) {:obj strn :pos (rangev (count strn))}
        :else (throw (Exception. "strmap/ensure-str-pos requires a "))))

; User range limits are the range on the current string, not the initial string that may be several levels back.
(defn _s [p] (if (> (count p) 0) (first p) 0)) ; start
(defn _e [s p] (if (> (count p) 1) (second p) (count s))) ;end

(defn subvector [v st & en]
  "Subvec does not free memory, this is a bit slower but does not have said potentially insidious side-effect, and is good for non-lazy stuff"
  (apply vector (if (nil? en) (subvec v st) (subvec v st (first en)))))

(defn filterv+ [pred coll]
  "Filters a vector of strings OR the :obj part of a {:obj :pos} vector.
   Not lazy."
  (into [] (filter #(if (map? %) (pred (:obj %)) (pred %)) coll)))

(defn subs+ [s st & en]
  "Substring with the :obj :pos format that preserves the concatination."
  (let [s (formatty s) en (if (nil? en) (count (:pos s)) (first en))]
    {:obj (clojure.core/subs (:obj s) st en) :pos (subvector (:pos s) st en)})) ; subvec is too lazy for us.

(defn s+ [strs]
 "String concatination that preserves the positons. :pos arrays are NOT shifted."
 (let [strs (mapv formatty strs)]
   {:obj (apply str (mapv #(:obj %) strs)) :pos (into [] (apply concat (mapv #(:pos %) strs)))}))


(defn re-hits-splits [re str-full & pos]
  "Returns :hits and :pieces (see fns below). Optional range args allowed."
  (let [offset (_s pos)
        strg (subs str-full offset (_e str-full pos)) ; we have to add our offset manually.

        ; re-seq gives vectors of vectors for more complex patterns and a squence of strings for simple patterns.
        ; We just want a vector of strings (taking the first one of each vector).
        hits (mapv #(if (vector? %) (first %) %) (re-seq re strg)) ; a bit inneficient but easier to code.
        pieces (string/split strg re)
        pieces (let [num-missing (- (count hits) (count pieces) -1)] ; missing empty strings at end.
                 (into [] (concat pieces (repeat num-missing ""))))
        
        chit (mapv count hits)
        cpiece (mapv count pieces)
        
        ; The string goes piece-hit-piece-...- piece or hit:
        hit-ends (mapv #(+ % offset) (reductions + (mapv + chit cpiece)))
        hit-starts (mapv - hit-ends chit)
        piece-starts (into [] (concat [0] hit-ends))

        ; return the hits and pieces:
        get-obj (fn [s st ch] {:obj s :pos (rangev st (+ st ch))})
        hits-out   (mapv get-obj hits hit-starts chit)
        pieces-out (mapv get-obj pieces piece-starts cpiece)
        ] {:hits hits-out :pieces pieces-out}))

(defn re-splits [re str-full & pos]
  "Splits a string based on regular expressions. Each :obj is the excised string itself, :pos is an array of char-locations."
  (if (string? re) (throw (Exception. "The string should be second, the re first.")))
  (:pieces (apply re-hits-splits re str-full pos)))

(defn re-hits [re str-full & pos]
  "Returns a vector of matches to a given regular expression to a string. Each match's :obj is the string itself, :pos is an array of char-locations."
  (if (string? re) (throw (Exception. "The string should be second, the re first.")))
  (:hits (apply re-hits-splits re str-full pos)))
 
 
 
 ;;;;;;;;; Code reading and analysis: use our functions to read string into code.
   ; Call our read-string.
   ; Modify the code (this is a bit different than normal b/c the code is a different format, see our format below).
   ; Call the to-string function to get back your string.
     ; Unmodified code will be converted into modified code.
     
; Format: 
;:head and :tail are the string of the ( and ) or {} or #{} '(), etc, respectively.
       ; Head includes any extra spaces, comments, etc.
       ; Head and tail are empty for non-collection codes.
       ; Tail will store any comments/strings up to but not including the next code object.
;:body is the string for the code itself, ONLY for non-collection codes.
  ; Body also stores any extra stuff up to next code object.
  ; Concatatinating all the bodies together will 
;:type is :set :list :vector :map :symbol.
;:pos list where on the original string we are, 1:1 with (str :head :body :tail)
;:obj is the code itself and contains stuff recursively. 
   ; The object code contains recursive code for other objects.
; Reader-macros are expanded, generating code that does not have any :pos :head :body or :tail but DOES have an :obj.

(defn _null-pos-code [code]
   ; Creates code with the same data-structure as _read-string would but without any position info (and no head tail or body either).
   ; Used for code that is generated because of a reader macro expansion but not explicitly written.
  {:obj (if (coll? code) (grammer/cmap :flatten _null-pos-code code) code) :pos [] :head "" :body "" :tail ""
   :type (cond (set? code) :set (list? code) :list (vector? code) :vector (map? code) :map :else :simple)})

(defn _read-string [strg parse offset p2s] ; recursive version.
   ; p2s is the percent2symbol map, used inside of #() litterals. There is no such thing as nested #()'s.
   ; TODO: A LOT of testing for this function!
   ;s is a piece of the original string, parse is a piece of the original parse (pieces of :level and :mode).
     ; for the children levels reduce level by one and then take pieces, etc.
     ; s is a single block of code.
  ; WARNING: If you add your own reader macros (unlikely, esoteric use) you are out of luck!
  ; NOTE: Hash-sets may not preserve the order from read-string. But the code will be equivalent.
  ; NOTE: syntax quotes are handled with (syntax-quote ...), which is not a function but the best we can really do.
    ; This allow you to treat the inside as if it is before qualification so you can make changes to the string, etc.
  ;(println p2s)
  (let [tmp (str (string/trim strg) "      ") ; This helps us detect macros: 
        c1 (subs tmp 0 1) c2 (subs tmp 0 2) c3 (subs tmp 0 3) c4 (subs tmp 0 4)
        
        ; Computes our result given an offset and the expansion, you don't have to worry about spaces.
        ; For '(foo) => (quote (foo))  is an offset of 1 and expansion of 'quote (5 long in string form).
        ; note ' (foo), an offset of 2, is still valid, and sp-add computes # of spaces.
        sp-add (fn [st o] (let [part-after-macro (subs st o)
                             ht (first (re-hits #"\S" part-after-macro))] ; The whitespace chars between the macro.
                         (subs part-after-macro 0 (first (:pos ht))))) ; from 0 up to the hit.
             
        ; gets the macro expansion {:head :body :tail :obj} object.
          ; abridged is the macro character string (i.e ' for quote or # for anomonyous fns).
          ; expanded is the expanded symbol (i.e. quote or (fn [foo bar])).
          ; p2s1 is the map from %1 to p1__4905#, etc.
        mget (fn [abridged expanded p2s1] ; expand-code can be a complex code object. mc is the macro char string.
               (let [o (count abridged) ; the length of the macro itself.
                     white (sp-add strg o)
                     o_ (+ o (count white)) ; the total offset including the spaces. Example: For "' (+ 1 2)" o_ is 2 but o is only 1.
                     parse-ch {:level (subvec (:level parse) o_) :mode (subvec (:mode parse) o_)} ; don't deccrement the level since the macros don't add a level.
                     
                     ; The child code (everything "inside" the reader macro).
                     child (_read-string (subs strg o_) parse-ch (+ offset o_) p2s1)
                     ; just use the range of the macro chars (o not o_) for each one and recursivly.

                     expand+ (_null-pos-code expanded) ; no positional information for macro-generated-code here.
                     
                     ; Return what the macro, including the child, expandes to:
                     ; the :obj is a list made out of the macro followed by the child, the child is kept toghether as a single object.
                     head0 (str abridged white) ; Add this to the beginning of the head of the child.
                     pos0 (mapv #(+ % offset) (range (count head0))) ;add this to the beginning of the pos of the child.

                     obj-expand+ (if (vector? (:obj expand+)) (:obj expand+) [expand+])
                     obj-child (if (vector? (:obj child)) (:obj child) [child])
                     ;_ (println expand+)
                     obj (if (sequential? (:obj expand+)) (conj (into [] (:obj expand+)) child) ; for collection objs just get rid of the outer shell as it has no meaningful info.
                             [expand+ child])

                     out {:obj obj
                          :head head0 :pos pos0
                          :body "" :tail "" :type :list}; reader macros always expand to list objects. The child has a body and tail. We don't.
                     ;(apply str (concat head0 (:head child))) (into [] (concat pos0 (:pos child)))
                     ] out))]
     ; see http://clojure.org/reference/reader for the macros. 
     (cond ; macro, collection, and non-collection cases.
       (= c1 "'") ; quotes.
       (mget "'" 'quote p2s)
       (= c1 "`") ; quotes.
       (mget "'" 'syntax-quote p2s) ; there is no real syntax-quote, this is kindof a hack.   
       ; \ and ; don't need macros.
       (= c1 "@")
       (mget "@" 'deref p2s)
       (= c1 "^") ; ^ "disappears" into the metadata. This doesn't create any metadata.
       (mget "^" [] p2s)
       ; set-litteral macros aren't really macros. (count (read-string %)) is always two for '(...) but unlimited for #{...}.
       ; #"" regexp patterns can be manually made into (re-pattern ...) but that does not happen for the reader.
       (= c2 "#'") ; var quotes.
       (mget "#'" 'var p2s)
       (= c2 "#("); function literals: use read-string to calculate the symbols.
       (let [symbols (second (clojure.core/read-string strg)) n (count symbols)
             percent2sym (zipmap (mapv #(symbol (str "%" %)) (range n)) symbols)] ; maps percentages to thier symbols.
         (mget "#" (list 'fn* symbols) percent2sym))
       (= c2 "#_") ; ignore-next-forms mean that obj is nil but the :body is still there.
       (let [restof (subs strg 2) ; collections vs non-collections on what we ignore only affects the {}.
             n (count strg)
             stob (if (coll? (clojure.core/read-string restof)) ; the stringy stuff only.
                      (let [w (sp-add strg 2)] {:head (str "#_" w) :body (subs strg (+ (count w) 2) (dec n)) :tail (clojure.core/subs (dec n))})
                      {:head "#_" :body restof :tail ""})]
         (assoc stob :pos (rangev offset (inc offset) (count strg)) :obj nil))
       (not (.contains "{}[]()" c1)) ; non-collections are just 1:1, except for the % symbols.
       {:pos (mapv #(+ % offset) (range (count strg))) :head "" :tail "" :body strg :type :simple
       :obj (let [p-lookup (get p2s (string/trim strg))] ; look-up our % symbol.
              (if (nil? p-lookup) (read-string strg) p-lookup))}
       :else ; collections: map the children (with approapiate offsets).
       (let [; Create an array that is one where the children are presetn and zero oterwise:
             whspace?s (mapv #(or (= % \space) (= % \tab) (= % \n)) (into [] strg)) ; whitespace.
             whl (- (count strg) (count (string/triml strg))); whitespaces on the left and right.
             whr (- (count strg) (count (string/trimr strg)))

             outer-stuff (into [] (concat (range (inc whl)) (range (- (count strg) whr 1) (count strg)))); space and brackets.
             zo #(assoc %1 %2 0)
             no-outer-brackets (reduce zo (:level parse) outer-stuff) ; remove the whl whr and outer brackets so they aren't included.
             ; Levels that break the code up into parsed symbols.
             sym-lev (mapv #(if (and (= %2 1) (not %1) (not (= %3 2))) 1 (Math/max (dec %2) 0)) ; Drop unless level = 1 and we are non-comment (comment = mode=2) code and not whspace?s
                                   whspace?s no-outer-brackets (:mode parse))
             isls (cio/extract-outer-islands {:level sym-lev :mode (:mode parse)}) ; the boundaries of individual symbols,etc within the code.
             ni (count isls)

             isl-bracket (first (cio/extract-outer-islands parse)) ; the opening and closing bracket.
             
             ; chilren islands include everything up to the next island.
             isls1 (mapv #(vector %1 %2) (mapv first isls) (mapv first (concat (rest isls) [[(dec (last isl-bracket))]]))) 
        
             ; Compute the children objects. These will end up empty if there are no children:
             str-ch (mapv #(subs strg (first %) (second %)) isls1)
             lev-ch (mapv #(mapv dec (subvector (:level parse) (first %) (second %))) isls1)
             levnomacro-ch (mapv #(mapv dec (subvector (:level parse) (first %) (second %))) isls1)
             mode-ch (mapv #(mapv identity (subvector (:mode parse) (first %) (second %))) isls1)
             parse-ch (mapv #(hash-map :level %1 :levelnomacro %2 :mode %3) lev-ch levnomacro-ch mode-ch)
             offset-ch (mapv #(+ (first %) offset) isls1); the beginning of the island start plus our offset.
             
             children (mapv _read-string str-ch parse-ch offset-ch (into [] (repeat (count offset-ch) p2s)))
             pos (mapv #(+ % offset) outer-stuff)
             ]
          {:pos pos
            :head (subs strg 0 (inc (first isl-bracket)))
            :tail (subs strg (dec (last isl-bracket)))
            :body ""
            :type ; let the first character determine the type we record. TODO: refactor this bit of code to grammer.
            (cond
              (= c2 "#{")
              :set
              (= c1 "{")
              :map
              (= c1 "[")
              :vector
              (= c1 "(")
              :list
              :else
              (throw (Exception. "Problem with strmap/_read-string or the string inputted")))
            :obj children}))))
(defn read-string+ [strg & pos]
  "Like core/read-string but stores the string representation and position along with the code."
    (let [strg (subs strg (_s pos) (_e strg pos))
          parse (cio/parse-summary strg)] 
    (_read-string strg parse (_s pos) [])))
(defn _dec [c] ; decs recursivly all pos stuff.
  (assoc c :pos (mapv dec (:pos c)) :obj (if (vector? (:obj c)) (mapv _dec (:obj c)) (:obj c))))
(defn reads-string+ [strg & pos]
  "Like read-string+ but reads all sub-clauses rather than only one. Useful to avoid manually adding a [] and then not getting off by one, etc.
   The outer level is always a vector."
   (let [c (_dec (read-string+ (str "[" strg "\n]") [(inc (_s pos)) (inc (_e pos))]))]
     (assoc c :head (subs+ (:head c) 1) :tail (subs+ (:tail c) 0 (dec (count (:tail c)))))))
(defn code-to-string [code]
    "Converts code we read with read-string back into string form. (code-to-string (read-string+ s)) SHOULD always be the same as s."
  (let [obj (:obj code)
        ch (if (coll? obj) (mapv code-to-string obj) "")]
     ;(println "stuff: " (:head code) (:body code) (:tail code))
     (str (:head code) (apply str ch) (:body code) (:tail code))))

(defn pos-lookup [code pos]
   "Finds the address of pos in code. An address is a vector of indexes to drill down through the tree
    and to get somewhere in the code. NOTE: addresses include :obj keys as well, but not the final :obj to get the actual symbol etc itself."
  (if (coll? (:obj code))
      (let [x (:pos code)]
        (cond (and (< (first x) pos) (> (last x) pos)) ; code is a collection and the index we want is inside the body.
              (let [o (:obj code)
                    match? (fn [ix] (and (<= (first (:pos (nth o ix))) pos) (>= (last (:pos (nth o ix))) pos)))
                    ix (reduce #(if %1 %1 (match? %2)) nil (range (count o)))] ; which child brackets our interval.
                    (into [] (concat [:obj ix] (pos-lookup (nth o ix)))))))
      [])) ; non-collections do not need to be looked up.

(defn _pprint+ [code+ lev]
    (let [spaces (apply str (repeat (* lev 2) " "))
          keep-going? (vector? (:obj code+))
          qu (fn [s] (if (> (count s) 0) (str "\"" s "\" ") ""))
          p? (> (count (:pos code+)) 0) 
          o (:obj code+)]
      (str spaces (qu (:head code+)) ; includes a space...
        (if keep-going? (if p? "... " "") (qu (:body code+))) ;...so no space before the three dots.
         (qu (:tail code+)) (if p? (str "ix=" (:pos code+)) "") " " (:type code+) " " (if keep-going? (str "(n = " (count o) ")") o)
        (if keep-going? (str (apply str "\n" (interpose "\n" (mapv #(_pprint+ % (inc lev)) o))) "") ""))))
(defn pprint+ [code+]
  "Pretty print so we know what is going on (returns a string, doesn't actually print on it's own).
   May be not efficient because of string concatintion, but not a big deal in most i.e. debugging cases.
   Unlike code-to-string, this shows the code structure itself." ; TODO: room for format improvement.
   (_pprint+ code+ 0))