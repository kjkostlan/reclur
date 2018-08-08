; Key modifiers. 
(ns layout.keybind)

;;;;;;;;;;;;;;;;;; Simple keybindings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ctrl+? [kevt letter] 
  (and (or (:ControlDown kevt) (:MetaDown kevt)) (not (:ShiftDown kevt))
    (= (str letter) (str (:KeyChar kevt)))))

(defn ctrl-shift+? [kevt letter] 
  (and (or (:ControlDown kevt) (:MetaDown kevt)) (:ShiftDown kevt)
    (= (str letter) (str (:KeyChar kevt)))))

(defn esc? [kevt] (= (:KeyCode kevt) 27))

(defn shift-enter? [key-evt] (and (:ShiftDown key-evt) (= (:KeyCode key-evt) 10)))
