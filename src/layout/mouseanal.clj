; Simple functions that analyze mouse events. 
(ns layout.mouseanal
  (:require [crossplatform.cp :as crossp]))

(defn get-scroll-xy [wheel-evt precise?]
  "Option for precise inertial scrolling."
  (let [horiz? (:ShiftDown wheel-evt) 
        delta (if precise? (:PreciseWheelRotation wheel-evt) (:WheelRotation wheel-evt))
        zero (if precise? 0.0 0)]
    (if horiz? [delta zero] [zero delta])))


