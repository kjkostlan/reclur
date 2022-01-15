(ns test.summary
  (:require [c]))

; Tests return true if they work, and false otherwise.

(defn report-broken []
  "Returns a vector of broken tests. Also prints out it's results.
   Only runs the automatic tests, the demo tests require human judgement."
  (let [leaf-nms ['smallertests]
        full-nms (mapv #(symbol (str 'test.auto "." %)) leaf-nms)
        _ (mapv #(require %) full-nms)
        nms-objs (mapv #(if-let [ns-obj (find-ns %)] ns-obj (throw (Exception. (str "No namespace found for " %)))) full-nms)

        failures-in-nms (fn [ns-sym ns-obj]
                          (let [sym2var (ns-interns ns-obj)
                                sym-quals (mapv #(symbol (str ns-sym "/" %)) (keys sym2var))
                                var-obs (vals sym2var)
                                failure?s (mapv #(not (%)) var-obs)
                                fails (mapv first (filter #(second %) (mapv vector sym-quals failure?s)))]
                            fails))
        all-fails (apply c/vcat (mapv failures-in-nms full-nms nms-objs))
        nfail (count all-fails)]
    (cond (= nfail 0) (println "All tests succeeded")
      (<= nfail 8) (println "These tests failed:" all-fails)
      :else (println nfail " Tests have failed."))
    all-fails))
