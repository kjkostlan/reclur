; np = numerical tools, in line with Python's numpy. Not high performance.

(ns np (:require [c]))

(defn bool2num [x] "Numpy has false=0, true=1"
  (cond (not x) 0 (= x true) 1 :else x))

(defn linspace
  ;https://crossclj.info/ns/anglican/1.0.0/anglican.ais.html#_linspace
  "returns a equally spaced sequence of points"
  [start end size]
  (let [delta (/ (- end start) (dec size))]
    (map (fn [n] (+ start (* n delta)))
         (range size))))

(defn argmax
  "Different from max-key in that it returns the key not the value."
  ([x]
    (argmax identity x))
  ([f x]
    (let [x (c/vs2m x) kys (into [] (keys x)) n (count kys)]
      (loop [ix 0 record -1e234 best-k nil]
        (if (= ix n) best-k
          (let [ki (nth kys ix)
                fv (bool2num (f (get x ki)))
                gold-medal? (>= fv record)]
            (recur (inc ix) (if gold-medal? fv record)
              (if gold-medal? ki best-k))))))))

(defn argmin
  "Different from max-key in that it returns the key not the value."
  ([x] (argmax #(- %) x))
  ([f x] (argmax #(- (bool2num (f %))) x)))

(defn pnorm-dist [x1 x2 y1 y2 pnorm]
  "1-norm vs 2-norms battle it out."
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (cond (<= pnorm 1e-6)
      (+ (Math/abs dx) (Math/abs dy))
      (= pnorm 2.0)
      (Math/sqrt (+ (* dx dx) (* dy dy)))
      (< pnorm 1e6)
      (Math/pow (+ (Math/pow dx pnorm) (Math/pow dy pnorm)) (/ pnorm))
      :else (max (Math/abs dx) (Math/abs dy)))))