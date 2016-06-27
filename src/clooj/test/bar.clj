; simple tests. For debugging the ability to edit files, etc.
(ns clooj.test.bar
  (:require [clooj.test.baz :as baz])
  (:import [java.awt Graphics]))

(def a (atom {}))

; Test that replaces (* 2 3) with (* times-i)
(defmacro *word [c]
  (let [result 
          (if (and (list? c) (= (count c) 3) (= (first c) '*))
              (let [s1 (second c) s2 (nth c 2)]
                (cond
                  (= s1 1)
                  `(~baz/times-1 ~s2)
                  (= s1 2)
                  `(~baz/times-2 ~s2)
                  (= s1 3)
                  `(~baz/times-3 ~s2)
                  (= s1 4)
                  `(~baz/times-4 ~s2)
                  (= s1 5)
                  `(~baz/times-5 ~s2)
                  :else
                  c)) c)]
    ;(println "c: " c " result: " result)
    (reset! a result)
    result))

(defmacro usesJava [c] ; ok it won't work but whatever.
  `(Graphics/clearRect ~c 0 0 10 10))