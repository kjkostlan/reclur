; Warning and dialogue boxes.

(ns javac.warnbox
  (:import (javax.swing JOptionPane SwingUtilities)))

(defmacro swing-wait [body]
  `(let [tmp-atom# (atom nil)]
     (SwingUtilities/invokeAndWait 
       (fn [] (let [x# ~body] 
                (reset! tmp-atom# x#))))
     @tmp-atom#))

(defn warning [^String msg]
  "Warning."
  (SwingUtilities/invokeLater (fn [] (JOptionPane/showMessageDialog nil msg))) true)

(defn yes-no? [^String msg default-yes?]
  "User input, modal dialog box, boolean return. Closing it returns false."
  (swing-wait
    (let [btn (JOptionPane/YES_NO_OPTION)
          m1 (JOptionPane/WARNING_MESSAGE)
          optsv ["Yes" "No"]
          opts (into-array Object optsv)
          op (if default-yes? (nth opts 0) (nth opts 1))
          value (JOptionPane/showOptionDialog nil msg "Warning" (int btn) (int m1) nil opts op)]
      (= value 0))))

(defn choice [^String msg opts default]
  "User input, modal dialog box, boolean return. Closing it returns the default."
  (swing-wait
    (let [btn (JOptionPane/YES_NO_OPTION)
          m1 (JOptionPane/WARNING_MESSAGE)
          opts (into-array Object opts)
          ix (JOptionPane/showOptionDialog nil msg "Warning" (int btn) (int m1) nil opts default)]
      (if (>= ix 0) (nth opts ix) default))))

