; A simple file to test stuff with (debugging only).
; (require '[clooj.test.bar :as bar])
(ns clooj.test.foo
  ;(:require [clooj.test.bar :as bar])
  )

; a simple error:
;(defn x [] 
; (let [a 1
;       b 1
;       c (graph/plot [1 2 4])]
;   ))

;(defn x3 [x] (bar/*word (* 3 x)))

;(defn uj [] (bar/usesJava 0))

(defn sqr [x] (* x x))

(defn cube [x] (* x x x))