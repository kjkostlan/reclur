; Simple functions that analyze mouse events. 
(ns layout.mouseanal
  (:require [crossplatform.cp :as crossp] [globals]))

(defn get-scroll-xy-vanilla [wheel-evt precise?]
  "Option for precise inertial scrolling."
  (let [horiz? (:ShiftDown wheel-evt) 
        delta (if precise? (:PreciseWheelRotation wheel-evt) (:WheelRotation wheel-evt))
        zero (if precise? 0.0 0)]
    (if horiz? [delta zero] [zero delta])))

; Settings for non-inertial scrolling, always room for improvement here:
(def ^:dynamic *boost-timerange-milis* 1000)
(def ^:dynamic *boost-strength* 0.15)

(defn get-scroll-xy [wheel-evt precise?]
  (let [[dx dy] (get-scroll-xy-vanilla wheel-evt precise?)]
    (if (crossp/inertial-scroll-guess?) [dx dy]
      (let [past-wheel-evts (get @globals/external-state-atom :mouse-wheel-history [])
            millis-past (mapv #(- (:Time wheel-evt) (:Time %)) past-wheel-evts)
            dxys (mapv #(get-scroll-xy-vanilla % precise?) past-wheel-evts)
            dxy (get-scroll-xy-vanilla wheel-evt precise?)
            sign-agree? (fn [dxy1 dxy2] 
                          (> (+ (* (first dxy1) (first dxy2))
                               (* (second dxy1) (second dxy2))) 0.5))
            sign-agree?s (mapv #(sign-agree? %1 dxy) dxys)
            ms-range *boost-timerange-milis*
            boost? (fn [ix] (and (nth sign-agree?s ix) (< (nth millis-past ix) ms-range)))
            n (count past-wheel-evts)
            n-boost (- (dec n) (if-let [x (last (remove boost? (range n)))] x 0))
            boost-fac (Math/exp (* n-boost *boost-strength*))
            dxy-out (mapv #(* boost-fac %) dxy)
            to-int #(int (if (> % 0) (+ (Math/round %) 0.5) (- (Math/round %) 0.5)))]
        ;(println "Total #" n "Boost score: " n-boost "Vanilla:" dxy "Output: " dxy-out)
        (if precise? dxy-out (mapv to-int dxy-out))))))