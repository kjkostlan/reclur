; Demos that show potential should be turned into full versions.

(ns test.demo
  (:require [coder.cbase :as cbase]
    [clojure.walk :as walk]
    [clojure.pprint :as pprint]
    [layout.blit :as blit]
    [coder.unerror :as unerror]
    [coder.logger :as logger]))

(defn very-pretty []
  "Very pretty print: uses simulated annealing to decide when to indent.
   This feature is around BETA level as Jan 8 2020."
  (let [k 'app.rtext/fit-to-text ;'app.orepl/get-repl-result
        ns-sym (cbase/ns-of k)
        vi (cbase/var-info k true)
        src (:source vi)
        src-ex (binding [*ns* (find-ns ns-sym)] (walk/macroexpand-all src))
        t0 (System/nanoTime)
        vp (blit/vps src-ex)
        t1 (System/nanoTime)]
    (println "(We are looking at macroexpand-all in both cases!)")
    (println "STANDARD PRETTY PRINT:")
    (pprint/pprint src-ex)
    (println "")
    (println "VERY PRETTY PRINT:" (str "simulated annealing is a bit expensive, took: " (/ (- t1 t0) 1e9) "s"))
    (println vp)))


(defn find-compile-error []
  "Show us a compile-time-error.
   This feature is around BETA level as Jan 8 2020."
  (let [bad-code '(loop [ix 0 jx 0]
                    (if (< ix jx) (* ix jx)
                      (if (> ix 10) (recur (inc ix))
                        (recur ix (inc jx)))))]
    (println "ERROR HILIGHT DEMO:")
    (unerror/errprint? bad-code)))

(defn logging []
  "Log the code that kicks in when we move components.
   THIS DOES NOT WORK YET, the logger has bugs need to fix."
  (let [target 'app.selectmovesize/draw-box-handle]
    (logger/add-logger! target false false)))