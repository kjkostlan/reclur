; Only works on strings for now. But you can always read-string.
(ns clooj.java.clipboard
  (:import (java.awt Toolkit)
           (java.awt.datatransfer Clipboard StringSelection DataFlavor)))

;http://www.avajava.com/tutorials/lessons/how-do-i-copy-a-string-to-the-clipboard.html
(defn copy!!! [x]
  "Converts x to a string and copies it."
  (let [toolkit (Toolkit/getDefaultToolkit)
        clipboard (.getSystemClipboard toolkit)
        strSel (StringSelection. (str x))]
    (.setContents clipboard strSel nil)))

;http://www.avajava.com/tutorials/lessons/how-do-i-get-a-string-from-the-clipboard.html
(defn paste []
  "Returns whatever was in the clipboard as a string."
  (let [toolkit (Toolkit/getDefaultToolkit)
        clipboard (.getSystemClipboard toolkit)]
    (str (.getData clipboard (DataFlavor/stringFlavor)))))