(ns clooj.java.layout
  (:require [clooj.collections :as collections])
  (:import (java.awt FlowLayout)))

;(rawdoc/java-awt-layouts)


(defn flow-layout-apply-defaults [layout]
  "Same as java's defaults."
  (let [default {:align :center :hgap 5 :vgap 5}]
    (collections/fillin-defaults layout default)))


(defn flow-layout-make-layout [layout]
  "Makes the java object, all fields of state are optional."
  (let [layout (flow-layout-apply-defaults layout)
        al (cond (= (:align layout) :leading)
                 (FlowLayout/LEADING)
                 (= (:align layout) :trailing)
                 (FlowLayout/TRAILING)
                 (= (:align layout) :center)
                 (FlowLayout/CENTER)
                 :else
                 (throw (Exception. (str "Bad flowLayout type: " (:align layout)))))]
    (FlowLayout. al (:hgap layout) (:vgap layout))))


(defn make-layout [layout]
  "Converts clojure layouts to java layouts. Passing in nil = default layout."
  (let [layout (if (nil? layout) {:Type 'FlowLayout} layout) t (keyword (:Type layout))]
    (cond
      (= t :FlowLayout)
      (flow-layout-make-layout layout)
      :else
      (throw (Exception. (str "TODO: add type to layout-creation: " t))))))