; One hit of ctrl-z can undo several events, if they are close together.
(ns layout.undo
  (:require [globals]
    [app.multicomp :as multicomp]
    [app.siconsole :as siconsole]
    [clojure.set :as set]))

(def ^:dynamic *max-undo* 100)

(defn ?0 [x] (if x x 0))
(defn ?vec [x] (if x x []))

(defn summarize-change [s-earlier-in-chain s-later-in-chain]
  "Not perfect, but should be helpful."
 (if (or (not s-earlier-in-chain) (not s-later-in-chain)) ""
  (let [s-old s-earlier-in-chain s s-later-in-chain
        comp-eq? (= (:components s-old) (:components s))
        cam-eq? (= (:camera s-old) (:camera s))]
    (if (and comp-eq? (not cam-eq?)) "Camera."
      (let [files-old (multicomp/who-is-open s-old)
            files-new (multicomp/who-is-open s)
            n-o (set/difference files-new files-old)
            o-n (set/difference files-old files-new)]
        (cond (not= n-o #{}) (str "open files: " n-o)
          (not= o-n #{}) (str "close files: " n-o)
          :else
          (let [files (into [] files-old)
                strs-old (mapv #(multicomp/open-fcache (:components s-old) %) files)
                strs-new (mapv #(multicomp/open-fcache (:components s) %) files)
                changed-ixs (filterv #(not= (nth strs-old %) (nth strs-new %)) (range (count files)))
                changed (mapv #(nth files %) changed-ixs)]
            (if (= changed []) "non-edit component change."
              (str "changed texts of: " changed)))))))))

(defn report! [evt s-old s]
  "Stores an event, but remember that the *max-undo* is used up."
  (swap! globals/undo-atom
    #(let [undo-ix (?0 (:undo-index %)) ; nil when just starting up.
           undos (?vec (:undo-stack %))
           head-length (inc undo-ix)
           undos (if (> (count undos) head-length)
                   (into [] (subvec undos 0 head-length)) undos) ; undone and then a report.
           undos (conj undos {:state s :time (System/nanoTime)})
           n (count undos)
           undos (if (> n *max-undo*) (into [] (subvec undos (- n *max-undo*))) undos)
           old-num-deleted (?0 (:num-undos-deleted %))
           new-num-deleted (if (> n *max-undo*) (+ old-num-deleted (- n *max-undo*)) old-num-deleted)
           undo-ix (dec (count undos))] ; set to the end.
      (assoc % :undo-index undo-ix :undo-stack undos 
       :num-undos-deleted new-num-deleted))))

(defn maybe-report! [evt s-old s]
  "Doesn't report all events, so undos do jumps."
  (cond (= s-old s) nil ; no change.
    (and (= (:camera s-old) (:camera s))
      (= (:components s-old) (:components s))) nil
    (or (= (:type evt) :mouseMoved) (= (:type evt) :mouseDragged)) nil ; too common to record.
    :else
    (report! evt s-old s)))

(defn unredoo! [delta]
  (let [tmp (atom nil)]
    (swap! globals/undo-atom 
      #(let [undos (?vec (:undo-stack %))
             undo-ix0 (?0 (:undo-index %))
             undo-ix (if (< delta 0) 
                       (max 0 (+ undo-ix0 delta))
                       (min (+ undo-ix0 delta) (dec (count undos))))
             s-old (:state (get (:undo-stack %) undo-ix))
             s (:app-state %)]
         (reset! tmp {:main (if s-old s-old s) 
                      :old (:state (get undos (min undo-ix0 undo-ix)))
                      :new (:state (get undos (max undo-ix0 undo-ix)))
                      :old-ix (+ undo-ix0 (?0 (:num-undos-deleted %)))
                      :new-ix (+ undo-ix (?0 (:num-undos-deleted %)))
                      :begin? (= undo-ix 0)
                      :end? (= undo-ix (dec (count undos)))})
         (assoc % :undo-index undo-ix)))
    (let [change (summarize-change (:old @tmp) (:new @tmp)) ; strings that summarize the undo/redo.
          ft (str "(" (:old-ix @tmp) "->" (:new-ix @tmp) ")")
          bgd (cond (:begin? @tmp) " [Beginning]" (:end? @tmp) " [End]" :else "")]
      (siconsole/log (:main @tmp) 
        (str (if (> delta 0) "Redo " "Undo ") change " " ft bgd)))))

(defn undo! []
  "Sets undo. Returns the unmodified app-state except with a log."
  (unredoo! -1))

(defn redo! []
  (unredoo! 1))