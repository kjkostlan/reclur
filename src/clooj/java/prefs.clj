; A simplified abstraction interface for working with java's preferences.

(ns clooj.java.prefs
  (:require [clooj.java.file :as jfile])
  (:import (java.util.prefs Preferences)
    (java.io ByteArrayOutputStream ByteArrayInputStream ObjectInputStream ObjectOutputStream)
    (java.awt Window)
    (javax.swing JSplitPane)))

;; define a UUID for clooj preferences
(def _clooj-prefs (.. Preferences userRoot
                     (node "clooj") (node "c6833c87-9631-44af-af83-f417028ea7aa")))

(defn _partition-str [n s]
  (let [l (.length s)]
    (for [i (range 0 l n)]
      (.substring s i (Math/min l (+ (int i) (int n))))))) 

(def pref-max-bytes (* 3/4 Preferences/MAX_VALUE_LENGTH))

(defn write-value-to-prefs!!! ; the triple !!! meand that the disk gets mutated (file io)
  "Writes a pure clojure data structure to Preferences object."
  [key value]
  (let [chunks (_partition-str pref-max-bytes (with-out-str (pr value)))
        node (. _clooj-prefs node key)]
    (.clear node)
    (doseq [i (range (count chunks))]
      (. node put (str i) (nth chunks i)))))

(defn read-value-from-prefs
  "Reads a pure clojure data structure from Preferences object."
  [key]
  (when-not (.endsWith key "/")
    (let [node (. _clooj-prefs node key)]
      (let [s (apply str
                     (for [i (range (count (. node keys)))]
                       (.get node (str i) nil)))]
        (when (and s (pos? (.length s))) (read-string s))))))

(defn write-obj-to-prefs!!!
  "Writes a java object to a Preferences object."
  [key obj]
  (let [bos (ByteArrayOutputStream.)
        os (ObjectOutputStream. bos)
        node (. _clooj-prefs node key)]
    (.writeObject os obj)
    (. node putByteArray "0" (.toByteArray bos))))

(defn read-obj-from-prefs
  "Reads a java object from a Preferences object."
  [key]
  (let [node (. _clooj-prefs node key)
        bis (ByteArrayInputStream. (. node getByteArray "0" nil))
        os (ObjectInputStream. bis)]
    (.readObject os)))

;; saving and restoring window shape in preferences

(defn get-shape [components]
  (for [comp components]
    (condp instance? comp
      Window
        [:window {:x (.getX comp) :y (.getY comp)
                  :w (.getWidth comp) :h (.getHeight comp)}]
      JSplitPane
        [:split-pane {:location (.getDividerLocation comp)}]
      nil)))

(defn watch-shape! [components f]
  (doseq [comp components]
    (condp instance? comp
      Window
        (.addComponentListener comp
          (proxy [java.awt.event.ComponentAdapter] []
            (componentMoved [_] (f))
            (componentResized [_] (f))))
      JSplitPane
        (.addPropertyChangeListener comp JSplitPane/DIVIDER_LOCATION_PROPERTY
          (proxy [java.beans.PropertyChangeListener] []
            (propertyChange [_] (f))))
      nil)))

(defn set-shape! [components shape-data]
  (loop [comps components shapes shape-data]
    (let [comp (first comps)
          shape (first shapes)]
      (try
        (when shape
          (condp = (first shape)
            :window
            (let [{:keys [x y w h]} (second shape)]
              (.setBounds comp x y w h))
            :split-pane
            (.setDividerLocation comp (:location (second shape)))
            nil))
        (catch Exception e nil)))
    (when (next comps)
      (recur (next comps) (next shapes)))))

(defn save-shape!!! [name components]
  (write-value-to-prefs!!! name (get-shape components)))

(defn restore-shape! [name components]
  (try
    (set-shape! components (read-value-from-prefs name))
    (catch Exception e)))

(defn load-settings [default-settings]
  (atom
    (merge default-settings
           (read-value-from-prefs "settings"))))

(defn save-settings!!! [settings]
  (write-value-to-prefs!!!
    "settings"
    settings))