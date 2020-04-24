; Demos that show potential should be turned into full versions.

(ns test.demo
  (:require [coder.cbase :as cbase]
    [clojure.walk :as walk]
    [clojure.pprint :as pprint]
    [layout.blit :as blit]
    [coder.unerror :as unerror]
    [coder.logger :as logger]
    [coder.cnav :as cnav]))

(defonce futures (atom []))

(defn cancel-all-futures! []
 (swap! futures #(do (mapv future-cancel %) [])))

(defn future-print-loop []
  "Can we see future printouts? Not much of a demo..."
  (future (loop [ix 0]
            (println "Counter:" ix) (flush)
            (Thread/sleep 1000)
            (if (= ix 10) ix (recur (inc ix))))))

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
  "Log the code that kicks in when we move components."
  (cancel-all-futures!)
  (let [target 'app.selectmovesize/draw-box-handle
        mexpand? false
        search-key 'xxyy ; this appears in a let statement.
        code (logger/symqual2code target mexpand?)
        _ (if (not code) (println ""))
        path-of-key (cnav/path-of code search-key)
        _ (if (not path-of-key) (throw (Exception. "Demo misconfigured: cant find key")))
        path (update path-of-key (dec (count path-of-key)) inc) ; the thing after the "box".
        ]
    (logger/add-logger! target path mexpand?)
    (let [f (future
          (loop [ix 0]
            (Thread/sleep 1000)
            (let [logs (logger/get-logs)]
              (println "# logs:" (count logs) "Last log:"
                (last logs)))
            (recur (inc ix))))]
     (swap! futures #(conj % f)))))