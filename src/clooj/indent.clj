(ns clooj.indent
  (:require [clooj.utils :as utils]
            [clooj.brackets :as brackets]
            [clojure.string :as string]
            [clooj.java.textarea_old :as jtext_old])
  (:import  (javax.swing.text DocumentFilter)))


(def special-tokens 
  ["def" "defn" "defmacro" "let" "for" "loop" "doseq" "if" "when"
   "binding" "case" "definline" "defmacro" "condp" "when-let" "if-let" "fn"
   "proxy" "reify" "when-first" "defmethod" "defmulti" "defn-" "defprotocol"
   "defrecord" "defstruct" "deftype" "dotimes" "doto" "extend" "extend-protocol"
   "extend-type" "if-not" "letfn" "ns" "update-proxy" "with-in-str"
   "with-local-vars" "with-out-str"
   "when-let" "when-not" "while" "with-bindings" "with-bindings*"])

(defn first-token [txt]
  (second (re-find #"\((.+?)\s" txt)))
          
(defn second-token-pos [txt]
  (when-let [x (re-find #".+?\s" (string/trimr (first (.split #"\r?\n" txt))))]
    (.length x)))

(defn left-paren-indent-size [txt]
  (let [token1 (first-token txt)]
    (or
      (when (and token1
                 (not (or (some #{token1} special-tokens)
                          (.startsWith (string/triml token1) "["))))
        (second-token-pos txt))
      2)))

(defn compute-indent-size [atomtext offset]
  (let [text-comp (:textarea @atomtext) bracket-pos (first (brackets/find-enclosing-brackets
                             (jtext_old/get-text atomtext) offset))]
    (when (<= 0 bracket-pos)
      (let [bracket (.. text-comp getText (charAt bracket-pos))
            col (:col (jtext_old/get-coords atomtext bracket-pos))]
        (if (= bracket \;)
          (compute-indent-size atomtext bracket-pos)
          (+ col
             (condp = bracket
               \( (left-paren-indent-size (.. text-comp getDocument (getText
                                                    bracket-pos
                                                    (- offset bracket-pos))))
               \\ 0  \[ 1  \{ 1  \" 1
               1))))))) ;"

(defn fix-indent! [atomtext line]
  (let [text-comp (:textarea @atomtext)
        start (.getLineStartOffset text-comp line)
        end (.getLineEndOffset text-comp line)
        document (.getDocument text-comp)
        line-text (.getText document start (- end start))]
    (let [old-indent-size (count (re-find #"\A\ +" line-text))]
      (when-let [new-indent-size (compute-indent-size atomtext start)]       
        (let [delta (- new-indent-size old-indent-size)]
          (if (pos? delta)
            (.insertString document start (apply str (repeat delta " ")) nil)
            (.remove document start (- delta))))))))

(defn fix-indent-selected-lines! [atomtext]
  (utils/awt-event 
    (dorun (map #(fix-indent! atomtext %)
                (utils/get-selected-lines (:textarea @atomtext))))))

(defn auto-indent-str! [atomtext offset]
  (let [indent-size (or (compute-indent-size atomtext offset) 0)]
    (apply str "\n" (repeat indent-size " "))))

(defn setup-autoindent! [atomtext]
  (utils/attach-action-keys! (:textarea @atomtext)
    ["cmd1 BACK_SLASH" #(fix-indent-selected-lines! atomtext)] ; "cmd1 \"
    ["cmd1 CLOSE_BRACKET" #(utils/indent! (:textarea @atomtext))]   ; "cmd1 ]"
    ["cmd1 OPEN_BRACKET" #(utils/unindent! (:textarea @atomtext))]) ; "cmd1 ["
  (.. (:textarea @atomtext) getDocument
    (setDocumentFilter
      (proxy [DocumentFilter] []
        (replace [fb offset len text attrs]
          (.replace
            fb offset len  
            (condp = text
              "\n" (auto-indent-str! atomtext offset)
              text)
            attrs))
        (remove [fb offset len]
          (.remove fb offset len))
        (insertString [fb offset string attr]
          (.insertString fb offset string attr))))))
