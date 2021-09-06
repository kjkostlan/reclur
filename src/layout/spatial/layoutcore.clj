; Tools that are useful for making layouts.
(ns layout.spatial.layoutcore
  (:require [globals]
    [layout.spatial.xform :as xform]
    [layout.spatial.lmodes.stack :as stack]
    [layout.spatial.lmodes.dogpile :as dogpile]
    [layout.spatial.lmodes.split :as split]
    [layout.spatial.lmodes.teleport :as teleport]
    [c] [np]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Library of layouts ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def all-layouts [(stack/layout) (dogpile/layout) (split/layout) (teleport/layout)])
(defn default-lmode [] (stack/layout))

(defn layout-ix [s]
  (let [names (mapv :name all-layouts)
        name (:name (:layout s))
        ix (first (filter #(= (:name (nth all-layouts %)) name)
             (range (count all-layouts))))]
    (if ix ix 0)))

(defn prev-layout [s]
  (let [ix (layout-ix s)]
    (if (> ix 0)
      (let [ly1 (nth all-layouts (dec ix))
            _ (println "Setting layout to:" (:name ly1))]
        (assoc s :layout ly1))
      (do (println (str "We already have the first layout option activated (" (:name (:layout s)) "), can't go any earlier.")) s))))

(defn next-layout [s]
  (let [ix (layout-ix s)]
    (if (< ix (dec (count all-layouts)))
      (let [ly1 (nth all-layouts (inc ix))
            _ (println "Setting layout to:" (:name ly1))]
        (assoc s :layout ly1))
      (do (println (str "We already have the last layout option activated (" (:name (:layout s)) "), can't go any later.")) s))))