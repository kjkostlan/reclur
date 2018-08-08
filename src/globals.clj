; Only use this file sparingly.
(ns globals
  (:require [javac.file :as jfile]
    [app.chfile :as chfile]))

; Stores the app state and queued events among other things.
(defonce one-atom (atom {}))

(defn read-conf []
  "Reads config.txt in the main folder."
  (let [txt (jfile/open "./config.txt")]
    (read-string txt)))

(defn are-we-child? []
  (:we-are-child? (globals/read-conf)))

(defn can-child? []
  (:enable-child? (globals/read-conf)))

(defn get-working-folder []
  "Gets the folder we save files in."
  (if (or (are-we-child?) (not (can-child?))) chfile/us-folder chfile/child-folder))