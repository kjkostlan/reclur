; Trinket-specific code, although most trinkets don't deviate a whole lot from canonical behavior.
(ns clooj.guinew.trinket
  (:require [clooj.guinew.cutnpaste :as cutnpaste] [clojure.string :as string]
    [clojure.set :as set])
  (:import [jcode JFrameClient] [java.lang.reflect Method]
    [java.awt Point Dimension Rectangle]))

(defn simple? [cl]
  "Is the class a simple type, for which we have a flat clojure structure for? Nil maps to nil."
  (if (not (nil? cl))
    (let [cl (if (= (type cl) java.lang.Class) cl (type cl)) ; can pass a class or an object.
          simples (hash-set Boolean Byte Integer Long Short Float Double String
                   Point Dimension Rectangle)]
      (contains? simples cl))))

(defn elementary? [t]
  "Is the class t an elementary type or not."
  (or (= t Boolean) (= t Byte) (= t Integer) (= t Long) (= t Short) (= t Float) (= t Double) (= t String)))

(defn simple-java-to-clj [obj]
  "Lossy conversion of some simple java obs to clojure, nil = not in the class we convert.
   Conversion between java and clojure forms is more involved so is done at two levels."
  (if (not (nil? obj)) 
    (let [t (type obj)]
      (cond
        (elementary? t)
        obj ; immutable as-is.
        (= t java.awt.Point)
        [(int (.getX obj)) (int (.getY obj))]
        (= t java.awt.Dimension)
        [(int (.getWidth obj)) (int (.getHeight obj))]
        (= t java.awt.Rectangle)
        [(int (.getX obj)) (int (.getY obj)) (int (.getWidth obj)) (int (.getHeight obj))]
        ; TODO: graphics stroke, etc.
        :else
        nil))))

(defn simple-clj-to-java [cl x]
  "Simple from clojure to java, with a class for cl.
   Returns nil if it fails."
  (cond
    (nil? cl) nil
    (= cl Boolean) (boolean x)
    (= cl Byte) (byte x) (= cl Integer) (int x) (= cl Long) (long x)
    (= cl Short) (short x) (= cl Float) (float x) (= cl Double) (double x)
    (= cl String) (str x)
    (= cl java.awt.Point)
    (Point. (int (first x)) (int (second x)))
    (= cl java.awt.Dimension)
    (Dimension. (int (first x)) (int (second x)))
    (= cl java.awt.Rectangle)
    (Rectangle. (int (first x)) (int (second x)) (int (nth x 3)) (int (nth x 4)))
    :else nil))

(defn java-to-clj-override [java-obj]
  "An often-used override since so much of the conversion has to be hand-written.." 
  (let [^Class cl (.getClass java-obj)]
    (cond)))

(defn clj-to-java-override [^Class cl clj-obj]
  "An often-used override since so much of the conversion has to be hand-written.." 
  (cond))

(defn get-event-target-override [java-e]
  ".getSource is pretty universal but there may be some cases where it doesn't apply."
  (let [^Class cl (type java-e)]
    (cond)))

; Define some convenience functions for the builtin listeners:
(def jc simple-java-to-clj)
(defmacro mm [& code]
  "(mm j Foo Bar) => (hash-set :Foo (jc (.getFoo j)) :Bar (jc (.getBar j)))"
  (apply list `hash-map (apply concat (mapv #(vector (keyword %) `(jc (~(symbol (str ".get" %)) ~(first code)))) (rest code))))
)
(def _builtin-l
  "A map of builtin listener functions, there are a lot of entries here!
   e = java event, c0 = object, old clojure form. j = the java object, after the change occured."
{:JFrame {:componentMoved (fn [e c0 j] (mm j Location)) 
          :componentResized (fn [e c0 j] (mm j Size))}
 :JPanel {}})
(def _e-converts 
  "A map of event conversion :mousePressed et al -> (fn [java-e])."
 {})
(def _e-converts-grid 
  "A map from i.e. [:mousePressed :JPanel] => (fn [java-e java-ob]).
   It's rare to need this, but if we ever do it will override _e-converts"
{})
(defn new-ob-builtin-listen-fns-override [type]
  "Listener functions that keep the clojure state up-to-date when i.e the user moves a window.
   They return the new clj object, but they only have to return the updated keys."
  (get _builtin-l type))

(defn clean-up-listener-map [l-map]
  "Fixes for occasional things that don't work."
 (dissoc l-map javax.swing.event.MouseInputListener))

(defn translate-event-override [java-e java-ob evt-type]
  "Overides the default transalation (which itself can be overrided by overriding the translation).
   evt-type is i.e. :mouseEvent"
  (if-let [x (get _e-converts-grid [evt-type (.getClientProperty java-ob "type")])]
    (x java-e java-ob) ; the grid overrides if it has been specified.
    (if-let [y (get _e-converts evt-type)] (y java-e))))

(defn constructor-override [ob-clj]
  "Overrides the default constructor, if need be. Nil = don't override."
  (let [ty (:Type ob-clj)]
    ; JFrames are an exception, they have no client properties so we needed to extend them and call our class:
    (cond (= ty :JFrame) (let [o (JFrameClient.)] (.pack o) (.setVisible o true) o))))

(defn update-ob-override! [java-ob old-clj-value new-clj-value changes setter-methods]
  "Overrides the default update ob, excluding graphics."
  (let [ty (:Type new-clj-value)]
    (cond)))

(defn update-graphics-override! [java-ob old-clj-value new-clj-value]
  "Overrides the default update graphics."
  (let [ty (:Type new-clj-value)]
    (cond)))

(defn delete-ob-finalizations! [java-ob]
  "Deletes an object. For now there is nothing but eventually we may need to add finalizations"
  (cond))

(defn add-child-override! [java-ob-parent java-ob-child]
  "Adding the child is a bit different for stuff like splitpanes or other things."
  (cond))

(defn remove-child-override! [java-ob-parent java-ob-child]
  "Removing the child."
  (cond))