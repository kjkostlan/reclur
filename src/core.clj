(ns core
    (:require [app.gaucore :as gaucore])
    (:gen-class
     :methods [^{:static true} [show [] void]]))

; DONT call save-file, we can't run swing stuff from shutdown hooks reliably.
; Instead we do this on the exit-if-close function.
; TODO: get this working so cmd+q doesn't quit.
(defn on-shutdown [] ())

;;  startup
(defn -main [& args] (gaucore/launch-main-app!!))