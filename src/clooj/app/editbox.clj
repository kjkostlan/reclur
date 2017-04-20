; Handles the editing framework.


(ns clooj.app.editbox
  (:import [java.awt Color Font])
  (:require [clooj.java.file :as jfile]
    [clooj.app.hilighter :as hilighter]))

; TODO: also use frames to store undoing, code collapsing, etc.


(defn line-nos [box] 
  "Graphics commands that will draw the line numbers."
  (let [g0 (.getGraphics (java.awt.image.BufferedImage. 2 2 java.awt.image.BufferedImage/TYPE_INT_RGB))
        t (:Text box)
		metrics (.getFontMetrics g0 (:Font box))
		height (.getHeight metrics)
		width (.charWidth metrics \space)
		^chars text (chars (.toCharArray (str (:Text box) "\n")))
		n (int (count text))
		c-p-l (loop [ix (int 0) lines [] numthis (int 0)] ; chars per line.
				(cond (= ix n) lines
				  (= (aget ^chars text ix) \newline) (recur (inc ix) (conj lines numthis) 0)
				  :else (recur (inc ix) lines (if (= (aget ^chars text ix) \tab) (+ numthis 8) (inc numthis)))))
		g (mapv #(vector :drawString [(str (inc %)) (int (+ 50 (* (nth c-p-l %) width)))
									   (int (* height (+ % 0.8)))]
				   {:Color (Color. (float 0.4) (float 0.6) (float 0.99))}) (range (count c-p-l)))] g))

(defn build []
  {:Type 'JScrollPane
   :file-to-frame {} ; filename -> "frame" (scroll position, undo, etc).
   :index-to-frame {} ; Chronological order -> "frame".
   :Children 
   [{:Type 'JTextArea :Text "Editor" :Font (Font. "monospaced" Font/PLAIN 12)
	 :insertUpdate (fn [s e o] (hilighter/hilight-update! s e o) (assoc s :Graphics (line-nos s)))
	 :removeUpdate (fn [s e o] (hilighter/hilight-update! s e o) (assoc s :Graphics (line-nos s)))
	 :caretUpdate (fn [s e o] (hilighter/hilight-update! s e o) (assoc s :Graphics (line-nos s)))}]})

(defn get-text [s] 
  "Like OO coding with getters and setters."
  (get-in s [:Children 0 :Text]))

(defn set-text [s text] 
  (assoc-in s [:Children 0 :Text] text))
  
(defn get-view [s] (:View s))

(defn set-view [s view] (assoc s :View view))

(defn get-caret [s] (:Caret s))

(defn set-caret [s caret] (if caret (assoc-in s [:Children 0 :Caret] caret) s)) ; nil caret has no effect.