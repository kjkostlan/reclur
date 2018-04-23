; Lisps love indentation. Lets make it beautiful!
(ns app.colorful
  (:import [java.awt Color]))

(defn hsv2rgb [hsv]
 ; 0-1 scale.
  (let [h (* (first hsv) 6) k (int (Math/floor h)) s (second hsv) v (nth hsv 2)
        p0 (- h k) t (- 1 s) n (- 1 (* s p0)) p (- 1 (* s (- 1 p0)))]
    (if (or (= k 0) (= k 6)) [1 p t]
      (if (= k 1)            [n 1 t]
        (if (= k 2)          [t 1 p]
          (if (= k 3)        [t n 1]
            (if (= k 4)      [p t 1]
              (if (= k 5)    [1 t n] [0.5 0.5 0.5]))))))))

(defn hsl2rgb [hsl]
 ; l is on a 0-1 scale, 0 is always black and 1 is always white.
  (let [g 2.2 rgb (hsv2rgb [(first hsl) (second hsl) 1])
       w [0.241 0.691 0.068] ; weight must sum to 1.
       ; actual brightness
       lux (fn [c] (reduce + 0.0 (map #(* (Math/pow %1 %2) %3) c (repeat g) w)))
       ; percieved brightness is inverse gamma to real brightness.
       brt #(Math/pow (lux %) (/ 1.0 g))
       cmax (+ (apply max rgb) 1e-8) ; largest color component.
       b (+ (brt rgb) 1e-8); yes you DO need these numbers to be larger than machine eps.
       bmax (/ b cmax) ; maximum brightness that can be achieved with scaling.
       rgbmax (into [] (map #(* % (/ 1 cmax)) rgb)) ; fully scaled colors.
       bout (nth hsl 2)] 
  (if (< b bout) 
   ; we must make it brighter.
    (if (> b bmax) 
      ; scaling all colors up will not work. Blend with white (lux = brt = 1 for white).
      ; APPROXIMATE brightness as linear in blending.
      ; Blend the max brightness scaled color with white, higher x menas more white.
      ; bmax*(1-x) + x = bout => x - bmax*x = bout - bmax => x = (bout-bmax)/(1-bmax)
      (let [x (/ (- bout b) (- 1.0 b)) xc (- 1 x)]
        (into [] (map #(+ (* % xc) x) rgbmax)))
      ; scaling will work.
      (into [] (map #(* % (/ bout b)) rgb)))
    ; This formula is the same as well:
    (into [] (map #(* % (/ bout b)) rgb)))))

(defn _float01 [x] (float (max 0 (min 1 x)))) ; colors are picky.
(defn make-color [triple] 
  (Color. (_float01 (first triple)) (_float01 (second triple)) (_float01 (nth triple 2))))

(defn level2col [level]
  ; a nice color scheme. Returns a Color object.
  ;(let [sat (min 1 (* 0.125 (Math/ceil (/ (- level 0.00001) 6.0)))) ]
  ;  (hsv2rgb [(mod (/ (+ level 5.0) 6.0) 1.0) sat 1]))
  ; Perceptual distances of hues:
  ; http://jov.arvojournals.org/data/Journals/JOV/932812/i1534-7362-13-7-1-f03.jpeg
  (let [hvals (into [] (map #(/ % 360) [0 37 55 80 115 153 180 220 320]))
        b (if (= level 0) 1 (+ 0.87 (* 0.08 (mod level 2))))
        h (nth hvals (mod (dec level) (count hvals)))
        s (if (<= level (count hvals)) 1 0.5)] (make-color (hsl2rgb [h s b]))))

(defn level2rgb [level]
  ; a nice color scheme. Returns a clojure vector form 0 to 1.
  (let [hvals (into [] (map #(/ % 360) [0 37 55 80 115 153 180 220 320]))
        b (if (= level 0) 1 (+ 0.87 (* 0.08 (mod level 2))))
        h (nth hvals (mod (dec level) (count hvals)))
        s (if (<= level (count hvals)) 1 0.5)] (hsl2rgb [h s b])))