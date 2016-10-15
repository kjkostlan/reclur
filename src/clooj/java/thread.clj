(ns clooj.java.thread)
; Threading tools.

; To permently stops everything we started:
(def emergency-stop false) ; use alter-var-root to stop all functions. 
; (require '[clooj.java.thread :as thread]) 
; (alter-var-root (var thread/emergency-stop) (fn [_] true))
; Later (after all the bad functions get at least one invocation):
; (alter-var-root (var thread/emergency-stop) (fn [_] false))

; This try-catch means we only will create an atom once, no matter how many times we reload.
; It prevents us from "losing" pulses. 
(defonce _pulses (atom {}))

(defn get-time-ns [] (. System (nanoTime))) ; as a long.

(defn get-stack-trace []
  "Gets the stack trace, a long string with \n seperating each file and line."
  (apply str (interpose "\n" (mapv str (.getStackTrace (Thread/currentThread))))))

(defn stop!! [lookup-id]
  "Stops a pulse. Iff lookup-id nil nothing happens and we get a nil value.
   This lets the thread finish this iteration, but it won't start another loop."
  (let [pulse (get @_pulses lookup-id)]
    (if pulse
      (do (reset! (:check-our-control? pulse) false)  ; will end when it finished.
          (swap! _pulses dissoc lookup-id) pulse))))

(defn stop-all!! []
  (let [p @_pulses]
    (mapv stop!! (keys p))))

(defn _go!! [pulse]
  (reset! (:check-our-control? pulse) true)
  (let [atom-check? (:atom-check? pulse) check-our-control? (:check-our-control? pulse)
        ms-pausef (if (fn? (:ms-pause pulse)) (:ms-pause pulse) (fn [_] (:ms-pause pulse))) 
        f!! (:f!! pulse) iter-atom (atom 0)
        include-fn-time? (:include-fn-time? pulse)
        f1 (fn [] (while (and @atom-check? (not emergency-stop) @check-our-control?)
           (let [t0 (get-time-ns)
                    _ (f!!)
                    t1 (get-time-ns)
                    timetaken-ms (* (double (- t1 t0)) 0.000001)
                    ms-pause (ms-pausef (swap! iter-atom inc))
                    sleep-ms (if include-fn-time? (Math/max 0.0 (- ms-pause timetaken-ms)) ms-pause)] 
            (try (Thread/sleep (long sleep-ms)) 
                      (catch InterruptedException iex [])))))]
    (.start (Thread. f1))))

;(defn go!! [lookup-id]
;  "Starts a pulse. It's OK to go an already going pulse (nothing happens).
;   Of course, this depends on the pulse's own initiative to keep going."
;  (let [pulse (get @_pulses lookup-id)]
;    (if pulse (_go!! pulse) (throw (Exception. (str "Not found: " lookup-id))))))

; TODO: better fps control if f is not negligable in time, and what to do if interrupted.
(defn pulse!!
  "Runs f!! (which takes no args but should have side effects on other variables) and
   ms-time miliseconds as long as @atom-check? is true (optional argument, defaults as always true).
   Any other threads with lookup-id will be stopped.
   ms-time can be a number or a function of one value (the iteration #, starting at 0).
   include-fn-time? = include the time spent on each function call as time before the next.
     True: If the function takes longer we will slow down as the wait interval is constant. 
   Note: using atom-check? allows for both internal and external stoppage."
  ([f!! ms-time lookup-id] (pulse!! f!! ms-time lookup-id (atom true) true))
  ([f!! ms-time lookup-id atom-check?] (pulse!! f!! ms-time lookup-id atom-check? true))
  ([f!! ms-time lookup-id atom-check? include-fn-time?]
    (let [check-our-control? (atom true)
          pulse {:check-our-control? check-our-control? :f!! f!! :atom-check? atom-check?
                 :ms-pause ms-time :iters 0 :include-fn-time? include-fn-time?}]
      (if (get @_pulses lookup-id) (stop!! lookup-id))
      (stop!! lookup-id)
      (swap! _pulses assoc lookup-id pulse)
      (_go!! pulse))))
      
(defn running? [thread]
  "Is a thread running currently? This is distinct from (not future-done?)."
  (= (str (.getState thread)) "RUNNABLE"))