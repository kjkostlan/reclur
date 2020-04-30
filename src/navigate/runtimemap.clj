; Simple Doxygen or Sphynx-like plots only with runtime-aware information.
; It's not fully automatic, the user has to select which fns are logged
; and has control over the queries.
(ns navigate.runtimemap
  (:require [coder.cbase :as cbase]
    [coder.logger :as logger]
    [app.xform :as xform]
    [globals]))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Support functions ;;;;;;;;;;;;;;;;;;;;;

(def this-ns *ns*)

(defn get-inside-size-xy [comp]
  "TODO, used for the tardis mouse move."
  [500 500])

(defn hotfn [f-sym] "Dont store old references to fns" 
  (fn [& args] (apply (ns-resolve this-ns f-sym) args)))

(defn filter-logs [& f]
  "Applies f to. Nil f is true."
  (let [f (first f) logs (:logs @globals/one-atom)]
    (if (or (nil? f) (= f true)) (filterv f logs)) logs))

;;;;;;;;;;;;;;;;;;;;;;;;;;; GUI functions ;;;;;;;;;;;;;;;;;;;;;

(defn mouse-pressed [evt comp]
  #_(println "Usesof test case:" (cbase/uses-of 'app.rtext/ctrl+)
    " Used by test case:" (cbase/used-by 'app.rtext/key-to-edit)) comp)

; gran2
; logger/remove-all-loggers!
; logger/clear-logs!
; logger/remove-logger! [sym-qual]
; logger/reload-but-keep-loggers!
; logger/add-logger! [sym-qual]
; (:logs @globals/one-atom)

(defn mouse-moved [evt comp] #_(println "Mouse move evt:" evt) comp)

(defn every-frame [evt comp] #_(println "Every frame evt:" evt) comp)

(defn mouse-pressed [evt comp] #_(println "Mouse pressed:" evt) comp)

(defn mouse-released [evt comp] #_(println "Mouse released:" evt) comp)

(defn key-pressed [evt comp] #_(println "Key pressed:" evt) comp)

(defn key-released [evt comp] #_(println "Key released:" evt) comp)

(defn mouse-dragged [evt comp] #_(println "Mouse dragged:" evt) comp)

(defn render [box & show-cursor?]
  "TODO" #_(println "render TODO") [])

(defn bind-to-repl [s repl-k]
  "Customized a repl's fns"
  (let [comp (-> (get-in s [:components repl-k])
               (assoc :mouseMoved (hotfn 'mouse-moved))
               (assoc :mouseDragged (hotfn 'mouse-dragged))
               (assoc :everyFrame (hotfn 'every-frame))
               (assoc :mousePressed (hotfn 'mouse-pressed))
               (assoc :mouseReleased (hotfn 'mouse-released))
               (assoc :keyPressed (hotfn 'key-pressed))
               (assoc :keyReleased (hotfn 'key-released))
               (assoc :render (hotfn 'render)))]
    (assoc-in s [:components repl-k] comp)))
