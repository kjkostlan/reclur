; Key modifiers. 
; TODO: this isn't really layout related as it provides functionality rather than parameters.
(ns layout.keybind
  (:require [clojure.string :as string]))

(defn b= [a b] (or (and a b) (and (not a) (not b))))

;;;;;;;;;;;;;;;;;; Simple key processings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn c? [kevt] (boolean (or (:MetaDown kevt) (:ControlDown kevt)))) ; had subtle trouble with java's booleans in the past, thus the maybe-unnecessary cast.
(defn a? [kevt] (boolean (:AltDown kevt)))
(defn s? [kevt] (boolean (:ShiftDown kevt)))
(defn normal? [kevt]
  "There may be modifiers on in the background, but this particular keypress would be a key that types some character."
  (or
   (.contains "`1234567890-=\tqwertyuiop[]\\asdfghjkl;'zxcvbnm,./ ~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?" 
     ^String (str (char (:KeyCode kevt))))
    (= (:KeyCode kevt) 192)))

(defn match-letter? [kevt letter] (= (.toLowerCase ^String (str letter)) (.toLowerCase ^String (str (char (:KeyCode kevt))))))

(defn escape? [kevt] (= (:KeyCode kevt) 27))
(defn enter? [kevt] (= (:KeyCode kevt) 10))
(defn space? [kevt] (= (:KeyCode kevt) 49))
(defn backspace? [kevt] (= (:KeyCode kevt) 8))
(defn tab? [kevt] (= (:KeyCode kevt) 9))
(defn tilda? [kevt] (= (:KeyCode kevt) 192))

(defn match-arrow? [kevt twoletter-arrow-code]
  (let [c (:KeyCode kevt) l (str twoletter-arrow-code)]
    (or (and (= c 37) (or (= l "<<") (= l ",,")))
          (and (= c 39) (or (= l ">>") (= l "..")))
          (and (= c 38) (or (= l "^^") (= l "55")))
          (and (= c 40) (or (= l "VV") (= l "vv"))))))

;;;;;;;;;;;;;;;;;; Combinations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn match-letter-or-phrase? [kevt txt]
  (let [txtl (.toLowerCase ^String txt)]
    (or (match-letter? kevt txtl) (match-arrow? kevt txtl) 
      (and (enter? kevt) (= txtl "ret"))
      (and (escape? kevt) (= txtl "esc"))
      (and (tab? kevt) (= txtl "tab"))
      (and (space? kevt) (= txtl "spv"))
      (and (tilda? kevt) (or (= txtl "`") (= txtl "~")))
      (and (backspace? kevt) (= txtl "bsp")))))

(defn emacs-hit? [txt kevt & ix]
  "C-x C-v is the txt, and ix = 0 or 1 points to the x and v; the key events are fed bit by bit."
  (let [ix (if (first ix) (first ix) 0)
        pieces (string/split txt #"\s")
        piece (.toLowerCase ^String (str (get pieces ix)))]
    (and piece
        (let [subpieces (string/split piece #"-")
              mods ^String (apply str (butlast subpieces))  ; i.e. "C" or "CM"
              c=? (b= (c? kevt) (.contains mods "c")) 
              a=? (b= (a? kevt) (.contains mods "m"))
              s=? (b= (s? kevt) (.contains mods "s"))
              l=? (match-letter-or-phrase? kevt (last subpieces))]
          (and c=? a=? s=? l=?)))))

(defn emacs-count [txt] (count (string/split txt #"\s")))
