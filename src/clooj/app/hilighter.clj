; Handles the editing framework.


(ns clooj.app.hilighter
  (:import [java.awt Font Color]
           [javax.swing.text DefaultHighlighter DefaultHighlighter$DefaultHighlightPainter])
  (:require [clooj.coder.blitcode :as blitcode]
    [clooj.coder.grammer :as grammer]
    [clooj.app.colorful :as colorful]))


(defn hilite-cursor-region! [text caret obj]
 "Hilights the region around the cursor."
  ; TODO: we can centralize the side effects (bury them in the changes in state) but including hilighter/ctext. 
  (if (> (count text) 0)
	(if (and text caret obj)
          (let [parse (blitcode/basic-parse text)
                ^ints inter-depth (:inter-depth parse)
                n (count text)
                next-clo-chi (fn [clo chi] 
                               (let [cl #(max 0 (min % n))] ; dec and inc.
                                 [(cl (dec (blitcode/depth-bracket inter-depth (cl clo) -1)))
                                  (cl (inc (blitcode/depth-bracket inter-depth (cl chi) 1)))]))
                
                max-levels-to-show 75
                ladder (loop [acc [[caret caret]] ix (int 0) c [caret caret]] ; working outwards.
                         (if (= ix max-levels-to-show) acc
                           (let [c1 (next-clo-chi (first c) (second c))]
                             (if (= c1 c) acc (recur (conj acc c1) (inc ix) c1)))))
                depth-at-cursor (aget ^ints inter-depth (int (max 0 (min n caret))))
                
                numl (count ladder) ; How deep each ladder element is:
                tweak (if (and (grammer/copen? (first text)) (grammer/cclose? (last text))) 1 0)
                ladder-hilite-levels (mapv #(+ (- (dec numl) %) tweak) (range numl)) 
			    hltr (.getHighlighter obj)

			    hpainters (mapv #(DefaultHighlighter$DefaultHighlightPainter. (colorful/level2col %)) ladder-hilite-levels)]
	  (.removeAllHighlights hltr)
	  (.setDrawsLayeredHighlights hltr false)
	  (mapv #(.addHighlight hltr (first %1) (second %1) %2) (reverse ladder) (reverse hpainters))))))
          
(defn hilight-update! [s e o] 
  "Selection hilight superscedes the syntax-level hilight" 
  (let [s0 (:SelectionStart s) s1 (:SelectionEnd s)]
	(if (or (not s0) (not s1) (= s0 s1)) ; selection => don't hilight the level.
	  (try (hilite-cursor-region! (:Text s) (:CaretPosition s) o)
	    (catch Exception e (println "Syntax hilight error: " e)))
	  (let [hltr (.getHighlighter o)
			hpainter (DefaultHighlighter$DefaultHighlightPainter. (Color. (float 0.75) (float 0.75) (float 1.0)))]
		(.removeAllHighlights hltr) (.addHighlight hltr s0 s1 hpainter)))) s)
