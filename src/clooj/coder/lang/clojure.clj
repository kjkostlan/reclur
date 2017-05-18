; Clojure parser tools that can be plugged into rcode.
; Eventually (LONG way down the road) other languages will go in our folder.
  ; 0 = empty space, comments, and delimiters such as , in java, python, etc.
  ; 1 = symbols (basically the same in any language). Variables and stuff like = in other languages also.
  ; 2 = keywords (clojure keywords and reserved words like "class" in java, python, etc).
  ; 3 = literals (boolean, number, string, regexp, etc).
  ; 4 = opening ([{#{.
  ; 5 = closing }]).
  ; 6 = reader macros.
   ; Supported token-match entity formats for the rest of the elements:
     ; Strings (forced to be as-is).
     ; head body: fixed head with body being a list of matching characters, arbitrary length.
       ; The longest string and/or head-body takes precedence (if there are no head-function matches).
     ; head function, fixed head and function is (fn [char-array ix-body-start length-of-array]) and returns the ix right after the end.
     ; For non-functions the longest match is used, the only thing that makes sense.
       ; functions with matching heads take priority, the longest matching head that is.

(ns clooj.coder.lang.clojure
 (:require [clojure.string :as string] [clojure.pprint :as pprint]
   [clooj.coder.rcode :as rcode]))

; TODO: Many of these functions are useful for a wider scope than just clojure.
; refactor them out. 
(def all-chars (apply hash-set (mapv char (range 255))))
(def space " ,\n\t\r\b")
(def controls (apply str (mapv char (range 32))))
(defn s2s [x] (apply str x))
(defn escape-str [^chars cs ix-start n] 
  (loop [escape? false ix (int ix-start)]
    (if (>= ix n) n
      (let [c (aget ^chars cs ix)]
        (if (and (= escape? false) (= c \")) (inc ix)
          (recur (if (= c \\) (not escape?) false) (inc ix)))))))

; More spicific to clojure:
(def vanilla (s2s (reduce #(disj %1 %2) all-chars (str space controls "(){}[]\\@'`~\"")))) ; specific to clojure.
(def nst "0123456789ABCDEF./erx")

;(println "Stuff: " rcode/TwoWayParser)

(deftype ClojureLang [] rcode/TwoWayParser

    (token-matchers [this]
      [[0 ";" (s2s (disj all-chars \newline))]
       [0 "" space] [1 "" (s2s (disj (apply hash-set vanilla) \:))] ; : in symbols works but isn't in the spec and never seen in the wild.
       [2 ":" vanilla] [3 "nil"] [3 "false"] [3 "true"] [3 "\\" (fn [^chars cs ix-start n] (min n (inc ix-start)))]
       [3 "\\newline"] [3 "\\tab"] [3 "\\formfeed"] [3 "\\return"] [3 "\\space"] [3 "\\u" "0123456789ABCDEF"]
       [3 "0" nst] [3 "1" nst] [3 "2" nst] [3 "3" nst] [3 "4" nst] [3 "5" nst] [3 "6" nst] [3 "7" nst] [3 "8" nst] [3 "9" nst]
       [3 "\\o" "01234567"] [3 "\"" escape-str] [3 "#\"" escape-str] 
       [3 "^"] ; TODO: what is the most natrual cross-langauge way to categorize metadata?
       [4 "("] [4 "["] [4 "{"] [4 "#{"] [5 "}"] [5 ")"] [5 "]"]
       [6 "#:"] [6 "'"] [6 "@"]  [6 "#"] [6 "#'"] [6 "#_"] [6 "`"] [6 "~"] [6 "~@"] [6 "~?"] [6 "~?@"]]) 

    (non-bracket-group [this x] (throw (Exception. "TODO")))

    (meta-assign [this x] (throw (Exception. "TODO")))

    (type-of-group [this x] (throw (Exception. "TODO")))

    (meta-parse [this x] (throw (Exception. "TODO")))

    (readermacro-apply [this x] (throw (Exception. "TODO")))

    (readermacro-unapply [this x r] (throw (Exception. "TODO")))

    (meta-unparse [this x r] (throw (Exception. "TODO")))

    (coll-project [this x r] (throw (Exception. "TODO")))

    (leaf-project [this x r] (throw (Exception. "TODO")))
)

