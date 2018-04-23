; The clipboard is for now a very simple global singleton (though other functions may store their own atoms).
; Making it pseudo-functional is planned eventually.
;http://www.javapractices.com/topic/TopicAction.do?Id=82
;http://www.cafeaulait.org/course/week13/14.html

(ns javac.clipboard
  (:import [java.awt.datatransfer Clipboard ClipboardOwner Transferable StringSelection DataFlavor]
    [java.awt Toolkit]))

(defn put-as-string!! [s]
  "Converts non-strings into strings."
  (let [string-sel (StringSelection. (str s))
        cb (.getSystemClipboard (Toolkit/getDefaultToolkit))]
    (.setContents cb string-sel string-sel)))

(defn get-as-string []
  (let [cb (.getSystemClipboard (Toolkit/getDefaultToolkit))
        contents (.getContents cb nil)]
    (try (str (.getTransferData contents DataFlavor/stringFlavor))
      (catch Exception e (str contents)))))
  