; User-defined startup-code.
(ns startup)

(defn startup-core []
 "User startup code goes here."
  (do (require 'client.eignertia.core)
    ((eval 'client.eignertia.core/startup))))