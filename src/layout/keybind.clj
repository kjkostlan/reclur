; Key modifiers. 
(ns layout.keybind)

;;;;;;;;;;;;;;;;;; Simple key processings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ctrl+? [kevt letter]
  (and (or (:ControlDown kevt) (:MetaDown kevt)) (not (:ShiftDown kevt))
    (= (str letter) (str (:KeyChar kevt)))))

(defn ctrl-shift+? [kevt letter] 
  (and (or (:ControlDown kevt) (:MetaDown kevt)) (:ShiftDown kevt)
    (= (str letter) (str (:KeyChar kevt)))))

(defn ctrlarrow+? [kevt v]
  (let [ctrl? (and (or (:ControlDown kevt) (:MetaDown kevt)) (not (:ShiftDown kevt)))
        c (:KeyCode kevt) v (str v)]
    (and ctrl?   
      (or (and (= c 37) (= v "<"))
        (and (= c 39) (= v ">"))
        (and (= c 38) (= v "^"))
        (and (= c 40) (= v "v"))))))


(defn esc? [kevt] (= (:KeyCode kevt) 27))

(defn shift-enter? [key-evt] (and (:ShiftDown key-evt) (= (:KeyCode key-evt) 10)))
