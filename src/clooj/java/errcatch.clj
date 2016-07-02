; Catches errors in the functional GUI and stores them in atoms here for debugging.
; Each error stores the function, input arguments, thread, and exception generated.
; The goal is not a 100% catch all the errors but it to capture most stuff.
  ; The outer levels of the api should wrap the errors, and the stack can give the inner errors.
  ; This only should be used on errors that may occur on other threads.
; If things stop working, check the errors atom.

(ns clooj.java.errcatch)

; Don't store more than this # (anti memory leak):
(def max-number-stored 100)

; Throw exceptions normally (in addition to storing them). These thrown exceptions get
; complicated when there are InvocationTargetException and different threads.
(def throw-on-error? true)

; All errors that were caught here, in chronological order.
;queues have better O but max-number-stored is not too large and queues don't print well. (def errors (atom (clojure.lang.PersistentQueue/EMPTY)))
(def errors (atom []))

(defn wrap
  "Returns a wrapped function that will try/catch errors.
   Usage: (foo bar baz) => ((errcatch/wrap foo) bar baz).
   Optional default argument when we have an error (only makes sense if throw-on-error? is false)."
  ([f] (wrap f nil))
  ([f default] 
	(fn [& args]
	  (try (apply f args) 
		(catch Exception e 
		  (let [err {:exception e :fn f :args args :thread (Thread/currentThread)} n0 (dec max-number-stored)]
			; the into [] prevents the memory leak subvec has.
			(do (swap! errors #(conj (if (>= (count %) n0) (into [] (subvec % 0 n0)) %) err))
			  (if throw-on-error? (throw e)) default)))))))
