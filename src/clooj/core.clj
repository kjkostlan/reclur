(ns clooj.core
    (:require [clooj.java.gui :as gui]
              [clooj.app.framer :as framer]
              [clooj.coder.repl :as repl]
              [clooj.repl_start :as repl_start])
    (:gen-class
     :methods [^{:static true} [show [] void]]))

; DONT call save-file, we can't run swing stuff from shutdown hooks reliably.
; Instead we do this on the exit-if-close function.
; TODO: get working.
(defn on-shutdown [] ())

;;  startup
(defn -main [& args]
  (gui/setup (framer/window))
  (repl/add-to-repl!! repl_start/code))