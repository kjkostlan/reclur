; Simple functions to intepret key events.
(ns clooj.app.claytreeevt)

(defn ctrl+ [key-evt]
  "If the user hits control-a returns the character a, etc. Returns nil otherwise."
  (if (:MetaDown key-evt) (:KeyChar key-evt)))

(defn arrow-key [key-event]
  "Returns the character ^ v < or > for arrow keys. Returns nil otherwise."
  (let [kc (:KeyCode key-event)] 
    (cond (= kc 37) \< (= kc 39) \> (= kc 38) \^ (= kc 40) \v)))

(defn typed-key [key-event]
  "Returns the character typed, including case, tabs, backspaces, and newlines. Returns nil otherwise."
  (let [ch (:KeyChar key-event)
        ^String s "`1234567890-=\b\tqwertyuiop[]\\asdfghjkl;'\nzxcvbnm,./~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>? "]
    (if (.contains s (str ch)) ch)))

(defn esc? [key-event]
  "Did we hit the escape key?"
  (= (int (:KeyChar key-event)) 27))

(defn mouse-button [evt]
  "Returns :left, :middle, :right, :other" ; 1 = left 3 = right
  (let [b (:Button evt)
        bs {1 :left 2 :middle 3 :right}]
    (if (get bs b) (get bs b) :other)))
  