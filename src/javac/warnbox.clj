; Warning and dialogue boxes.
; Don't use this for complex things, instead use/add the feature to the main windower.

(ns javac.warnbox
  (:import (javax.swing JOptionPane)))

(defn yes-no? [msg]
  "User input, modal dialog box. Closing it returns false."
  (let [value (JOptionPane/showInputDialog nil, msg, "Decide", JOptionPane/INFORMATION_MESSAGE, nil, (into-array ["no", "yes"]), "no")]
    (= value "yes")))