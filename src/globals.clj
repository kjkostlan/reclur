; Global singleton atoms.
(ns globals
  (:require [javac.file :as jfile]
    [app.chfile :as chfile]))

; Main atom holding the app state and event queue.
; Changes to this may mutate the disk or be expensive so we can't risk them running more than once.
; We need to use (locking ...) rather than swap! for this atom (maybe an atom isn't the most idiomatic way ...)
; This atom holds only the minimum it needs to hold. Other atoms hold information that doesn't need locking syncs.
(defonce sync-app-atom (atom {}))

; Stores information about the mouse, time, etc.
(defonce external-state-atom (atom {}))

; The logging code needs to mutate some atom from deep within any arbitrary code. 
(defonce log-atom (atom {}))

; The undo atom also lives outside the state:
(defonce undo-atom (atom {}))

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