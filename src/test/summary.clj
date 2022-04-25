(ns test.summary
  (:require [c]
    [clojure.test :as test] [coder.profiler :as profiler]))

; Tests return true if they work, and false otherwise.

(def modules-to-run ['smallertests 'testc])

(defmacro with-testout-str
  "Test has a lot more mutation than expected."
  {:added "1.0"}
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [test/*test-out* s#]
       ~@body
       (str s#))))

(defn test-report-vars [ns-sym]
  "The summary makes the test."
  (let [var-map (ns-interns (find-ns ns-sym))
        test-results (mapv #(with-testout-str (test/test-var %)) (vals var-map))]
    (zipmap (keys var-map) test-results)))

(defn report-broken []
  "Returns a vector of broken tests. Also prints out it's results.
   Only runs the automatic tests, the demo tests require human judgement."
  (let [leaf-nms modules-to-run
        full-nms (mapv #(symbol (str 'test.auto "." %)) leaf-nms)
        _ (mapv require full-nms) ; ensure loaded.

        
        test-resultss (mapv test-report-vars full-nms)
        test-result-pairs (apply c/vcat (mapv #(map vector (keys %) (vals %)) test-resultss))
        test-failures (filterv #(not= (second %) "") test-result-pairs)
        n-fail (count test-failures)
        n-total (count test-result-pairs) n-succeed (- n-total n-fail)]
    (mapv #(println "Failed test:" (first %) (second %)) test-failures)
    (print "Total:" n-total "Succeed + not-a-test:" n-succeed "Fail:" n-fail)
    test-failures))
