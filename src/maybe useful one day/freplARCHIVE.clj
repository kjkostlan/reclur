; Functional repl that uses futures so that multiple calculations can run non-blocking and
; is also (mostly) functional via the use of funs.
; (require '[clooj.coder.frepl :as frepl])
(ns clooj.coder.frepl
  (:require [clooj.coder.funs :as funs]
            [clooj.coder.indent :as indent]
            [clojure.string :as string]
            [clooj.collections :as collections]
            [clooj.java.file :as jfile]
            [clojure.walk :as walk])
  (:import (java.io ByteArrayOutputStream OutputStreamWriter) (javax.swing SwingUtilities)))

; Global anti-lag parameters, since some loops will be unavoidable:
(def ^{:dynamic true} *check-lag-ms* 500)
(def ^{:dynamic true} *abort-lag-ms* 1000)
(def ^{:dynamic true} num-repl-cmds 0) ; The eventual memory-leak.

; Rebinds the var root to println's not in the terminal. Set to false if you can't
; get the app running in the first place to see the error.
(def rebind-printing? true)

(defn fresh-repl []
  "A brand-new repl with a new namespace, using funs.
   Each repl is independent and has it's own futures."
  {:funs (funs/make-nsbun)
   :max-print 5000
   :printing-calcs []}) ; Each of these has a future and an output string.

(def core-ns (find-ns 'clojure.core))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; String representation functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defn thaw-str [& args]
  "Like str but won't freeze the entire program for infinite datasets.
   Maybe some esoteric .toString functions can cause problems."
   (if (and (= (count args) 1) (string? (first args))) (first args) ; shortcut.
	 (let [max-load 1e6
	       ; Guessing count. Will return <= max-load iff there are <= max-load items in it.
		   dc (fn dc [x]
		         (if (coll? x)
		           (let [x (if (sequential? x) x (into [] x)) ; non-sequential can't be lazy.
		                 x-lite (take (inc max-load) x)
		                 n-low (count x-lite)
		                 n-hi (cond (counted? x) (count x)
		                            (<= n-low max-load) n-low
		                            :else (/ 1.0 0))] ; Infinity.
		             (if (< n-hi max-load) (reduce + (mapv dc x)) n-hi)) 1))
		   ndeep-guess (dc args)] 
	   (if (<= ndeep-guess max-load) (apply str args)
		 (str "<more than " (int max-load) " leaf items>")))))

(defn err-report
  "A string form that also gives stack info (stack info can be disabled by passing in false)."
  ([e] (err-report e false))
  ([e short?]
    (apply str (str (.getMessage e) " (" (last (string/split (str (type e)) #"\.")) ")") "\n" 
      (if short? [] 
        (let [trace (mapv str (.getStackTrace e))]
          (interpose "\n" (take-while #(not (.startsWith % "clooj.coder.repl$eval_BANG__BANG_.invoke")) trace)))))))

(defn _abridged-str [x max-print thaw?]
  (let [sstr (if thaw? thaw-str str)
        limit #(let [s (try (sstr %) (catch Exception e (sstr "STRING REPRESENTATION ERROR: " e)))]
                 (if (> (count s) max-print) (str (subs s 0 max-print) "...<too large to show>") s))]
    (limit x)))
(defn abridged-str [cmd val our-ns max-print] 
  "An abridged string representation that is formatted repl-style."
   (str (_abridged-str our-ns max-print false) "=> " (_abridged-str cmd max-print false) "\n" (_abridged-str val max-print true)))

(defn get-result [calc max-print]
  "Dependent on whether the future is done."
  (if (not (future-done? (:future calc))) "<In progress>"; still running.
    (let [x @(:future calc)]
      (if (:error x) (err-report (:error x))
        (abridged-str (:cmd calc) (:result x) "REPL" max-print)))))

(defn get-printed-str [f-repl]
  "Returns the string printed by the repl.
   Subsequent calls to the f-repl may yield a different string b/c of futures finishing."
  (apply str
    (interpose "\n" (mapv #(get-result % (:max-print f-repl)) (:printing-calcs (update f-repl :funs funs/up-to-date))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Repl commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn up-to-date [f-repl]
  "Tells the inner functional-ns to get itslf up-to-date with it's futures."
  (update f-repl :funs funs/up-to-date))

(defn eval [f-repl cmd]
  "Evals the string cmd in repl-ob, returning the modified repl-ob"
  (let [codes (try (read-string (str "[" cmd "\n]")) (catch Exception e e)) ; put it into a [] to keep the symbol working.
        funs (funs/future-eval-in (funs/up-to-date (:funs f-repl)) codes)
        fut (last (:futures funs)) ; this future will contain {:result :bindings :error}, we need the :result and :error;
        mcf #(if (and (vector? codes) (> (count codes) 1)) 
               (interpose "\n" (into [] (concat [%1] %2))) (first %2))
        fut1 (future ; a slight modification.
               {:result (mcf "Multiple results:" (get @fut :result)) :error (:error @fut) :cmd cmd})
        calc {:future fut1 :cmd cmd}]
    (assoc f-repl :funs funs :printing-calcs (conj (:printing-calcs f-repl) calc))))

(defn clear-done-cmds [f-repl]
  "Clears all calculations whose future is done.
   Use this upon user requesting to clear the repl."
  (update (up-to-date f-repl) :printing-calcs (fn [prc] (filterv #(-> % :future future-done? not) prc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Code representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn brevity [code nms] 
  "Represents the code in a more consice function. Uses a namespace to prevent collisions.
   Use (pr-str (brevity code *ns*)) to get a more-readable function. Use pr-str DONT use str."
  (let [d (fn [s re-as-s] (.replaceAll s re-as-s ""))
        nsresolve #(try (ns-resolve %1 %2) (catch Exception e))
        ;refers (mapv str (keys (ns-refers nms))) ; includes the clojure.core and any :use stuff.
        ; The functions that convert to an easier symbol:
        qualed? #(second (.split % "/")) ; are we qualified with a / ?
        no-__foo__ #(d % "__.*__") no-12345 #(d % "\\d\\d\\d+")
        no-qual #(d % ".+\\/")
        cap1 #(str (.toUpperCase (str (first %))) (apply str (rest %)))
        ; mainly composiiton of replacements, order matters a little.
        sr-rep #(let [str1 (no-qual (no-12345 (no-__foo__ %))) ; takes a string.
                      v0 (nsresolve nms (symbol %)) v1 (nsresolve nms (symbol str1))
                      broke? (and v0 (not= v0 v1)) made? (and (nil? v0) (not (nil? v1)))
                      str1 (if broke? % str1)] ; prevent renaming if we broke a resolution.
                   (if made? ; capitolize made symbols so that they are almost certanly not used. made? is still valid code but harder to read.
                     (cap1 str1) str1))
        simplify-f #(symbol (sr-rep (str %)))
        
        ; Build a map from the bulky vars to the non-bulky ones.
        f1 (collections/reduce-to-map (fn [acc c] (if (symbol? c) (assoc acc c (simplify-f c)) acc)) {})
        bulk-to-slim (do (walk/postwalk f1 code) (f1))
        ; Break duplicate keys:
        bulk-to-slim1 (collections/resolve-collisions (fn [ks v] (mapv #(symbol (str v (if (= % 0) "" %))) (range (count ks)))) bulk-to-slim)
        ; The actuator step. We can't do postwalk because (quote ...) shouldn't do anything to the ...
        _brevity (fn _brevity [code] 
                   (cond (symbol? code) (get bulk-to-slim1 code)
                         ; Special exclusion for quotes: quote is a special form so when it is the first arguent of a list it's ALWAYS.
                         (and (coll? code) (or (not= (first code) 'quote) (not (collections/listoid? code)))) 
                         (collections/cmap :flatten _brevity code) 
                         :else code))]
    (_brevity code)))

(defn previty [code & hint-dont-print]
  "EZ function generating printouts."
  (let [out (indent/indent (brevity (try (read-string code) (catch Exception e code)) *ns*))]
    (if (not (second hint-dont-print)) (println (str (first hint-dont-print)) out) out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; This !@#$ breaks functionality ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reload-file!! [file]
  "Orthogonal to the other functions.
   Reloads a file's namespace. ANY namespace that uses said file will now use updated variables.
   HOWEVER, deleted variables (or the old name for renamed ones) are still accessable untill program restarts. TODO: fix this.
   Finally, this can print an error to the console despite working properly at times. TODO: thier bug.
   Does not catch errors."
  (let [dotpath (jfile/file2dotpath file) namespace (create-ns '_debugger.namespace)]
    (binding [*ns* namespace] (require (symbol dotpath) :reload))))