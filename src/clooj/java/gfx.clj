; Each state's :Graphics is a vector of commands:
; [[:drawOval, [10 10 20 20]], [:fillRect, [10 10 20 20]] .... ] 
; [:drawOval, [10 10 20 20] ; uses defaults for everything, including color, etc.
; [:drawOval, [10 10 20 20], {:Color [0 1 0], :Stroke [...]}] ; overrides the defaults for this one call. 
; The first element is the java argument in keyword form.
; The second is the vector of stuff passed to the java arguments.
; The third is a map of settings like color. Settings only apply to the command at hand, they are
  ; not sticky as they would be in the mutatable land of java.
; TODO: have some defaults if the int vs non-int types are messed up.
(ns clooj.java.gfx
  (:import [java.awt.image BufferedImage])
  (:require [clooj.java.clojurize :as clojurize] [clojure.set :as set]))

; No need for clojurize/on-java-change since we are excluding graphics from that list.

(def gets (clojurize/getters java.awt.Graphics2D))
(def sets (clojurize/getters java.awt.Graphics2D))

; Graphics defaults (all get/set pairs for graphics).
(def g-guina-pig (.createGraphics (BufferedImage. 100 100 BufferedImage/TYPE_INT_ARGB)))
(defn gstate [g] (dissoc (clojurize/get-single-level g true true) :Class))
(def g-class (.getClass g-guina-pig))
(def gdefaults (gstate g-guina-pig)); it is a graphics2D object.
  
;(def gmethods ; map from keyword to method, may have multible methods.
;  (let [methods (.getMethods (.getClass g-guina-pig))
;        names (mapv #(keyword (.getName %)) methods)
;        nargs ()
;        n (count names)]
;    (reduce #(assoc %1 (nth names %2) (conj (get %1 (nth names %2)) (nth methods %2)))
;      (zipmap names (repeat n #{})) (range n))))
;(def confusions (mapv first (filterv #(> (count (second %)) 1) (mapv vector (keys gmethods) (vals gmethods)))))

(defn paint! [g cmds]
  ; This calss conversion stuff is a MESS:
  (let [long2int #(if (= (type %) Long) (int %) %) ; clojure likes longs but java's graphics likes ints. 
        float2double #(if (= (type %) Float) (double %) %) ; graphics2D decides randomally whether to use doubles or floats.
        double2float #(if (= (type %) Double) (float %) %)
        ; We can pass i.e. Integers to functions that accept ints, but we can't reflect-call with java.lang.Integer classes:
        primativify (fn [cl] (cond (= cl Boolean) Boolean/TYPE
                                   (= cl Integer) Integer/TYPE
                                   (= cl Float) Float/TYPE
                                   (= cl Double) Double/TYPE
                                   :else cl))
        args2cl #(into-array java.lang.Class (mapv primativify (mapv type %))) ; need for into-Class b/c of empty arrays.
        arg-m (fn [name args] 
                (let [ai (long2int args)]
                  (first (filter identity
                      (mapv #(let [a (mapv % (mapv long2int args))]
                          (try {:method (.getMethod g-class name (args2cl a)) :args a}
                            (catch Exception e nil))) [identity float2double double2float])))))
        
        draw!! (fn [cname prefix args0]
                 (let [name (str prefix (subs (str cname) 1))
                       args-and-method (arg-m name args0) args (:args args-and-method)
                       ^java.lang.reflect.Method m (:method args-and-method)]
                   (if (nil? m) (throw (Exception. (str "Not recognized command: " cname ", " args0))))
                   (.invoke m g (into-array Object args))))
        ; Sets the graphics state approapiatly so that changes aren't persistant.
        ; gmods (a map) keeps track of any modifications, opts is your options (or empty [] if no options).
        ; returns the new gmods.
        ; It only changes the state of graphics if opts changed, so it is not innefficient.
        set-to!! (fn [opts gmods inits] 
                   (let [optk (apply hash-set (keys opts)) modk (apply hash-set (keys gmods))
                         changes (mapv #(not= (%1 opts) (%1 gmods)) (set/union optk modk))
                         news (set/difference optk modk)
                         reverts (set/difference modk optk)
                         ; command the graphics and update gmods:
                         gmods (reduce #(do (draw!! %1 "set" (%1 opts)) (assoc %2 %1 (%1 opts))) 
                                 gmods (concat changes news))
                         gmods (reduce #(do (draw!! %1 "set" (%1 inits)) (dissoc %2 %1)) 
                                 gmods reverts)] gmods))
        gfx-init-state gdefaults] ; Use our canned defaults for inits to avoid a slow? scan through the reflection.
    (loop [ix 0 gmods {}]
      (if (= ix (count cmds)) "DONE!"
        (let [ci (nth cmds ix) ; this particular draw command.
              opts (if (> (count ci) 2) (nth ci 2) []) ; linewidth, etc (other optional stuff)
              gmods1 (set-to!! opts gmods gfx-init-state)] ; .setColor, etc.
          (draw!! (first ci) "" (second ci));the main thing we draw.
          (recur (inc ix) gmods1))))))
          

;(eval (list (symbol (str "." (subs (str :contains) 1))) "planet" "p"))

