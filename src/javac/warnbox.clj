; Warning and dialogue boxes.
; Don't use this for complex things, instead use/add the feature to the main windower.

(ns javac.warnbox
  (:import (javax.swing JOptionPane)))

(defn yes-no? [^String msg]
  "User input, modal dialog box, boolean return. Closing it returns false."
  (let [btn (JOptionPane/YES_NO_OPTION)
        value (JOptionPane/showConfirmDialog nil msg "Warning" (int btn))]
    (= value (JOptionPane/YES_OPTION))))

(defn yes-no-cancel? [^String msg]
  "User input, modal dialog box, keyword return. Closing it returns :cancel."
  (let [btn (JOptionPane/YES_NO_CANCEL_OPTION)
        value (JOptionPane/showConfirmDialog nil msg "Warning" (int btn))]
    (cond (= value (JOptionPane/YES_OPTION)) :yes
      (= value (JOptionPane/NO_OPTION)) :no
      :else :cancel)))