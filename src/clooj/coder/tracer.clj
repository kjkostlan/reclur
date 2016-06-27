; Stack trace and error analysis.
(ns clooj.coder.tracer (:require [clooj.app.state_old :as app_old] [clojure.string :as string]
                                 [clooj.java.file :as jfile]
                                 [clooj.coder.grammer :as grammer]
                                 [clooj.coder.strmap :as strmap]
                                 [clooj.coder.io :as cio]))
 
(defn read-string-noerr [strng def]
  "read-string but with returns some default def instead of throwing an error."
  (try (read-string strng) (catch Exception e def))) 
                                 
(defn get-location [err] 
  "Gets the :file :line and :char location for which an error occurs.
   Unfortanatly this is not very good because i.e. let statements always 
   error on the first line for compile errors."
   (let [msg (.getMessage err)
         get-tok #(first (re-seq %1 %2))
         erode #(.substring % 1 (dec (count %)))
         loc (get-tok #"\(.+\)" msg)
         file (erode (get-tok #"\(.+\D:" loc))
         line (Integer. (erode (get-tok #":\d+:" loc)))
         char (Integer. (erode (get-tok #":\d+\)" loc)))]
         {:file file :line line :char char}))

(defn singleclean [s]
  ; Difference between Java's and our strings.
  ; This is only really relevant for cleaning up stack traces.
  (let [s (string/replace s "_BANG_" "!")
        s (string/replace s "_QMARK_" "?")
        s (string/replace s #"\$fn__\d\d\d\d" "")
        s (string/replace s #"\.invoke" "")
        s (string/replace s "$" "/")
        s (string/replace s "(" " (")] s))

; print the call-stack:
(defn print-stack [] 
  (try (throw (Exception. "")) (catch Exception e (.printStackTrace e))))
(defn print-lasterr []
(let [le (:lasterr @app_old/repl-state)]
    (if (nil? le) (println "no errors yet to print")
      (.printStackTrace le)))) 
    
(defn exception2str [e] (str (string/replace (type e) "class " "") ": " (.getMessage e)))

(defn exception2stack [e cleanup]
  "Converts an exception to a call-stack (sequence of strings)."
  (let [messy (map str (.getStackTrace e))
        notcj (fn [x] (let [pcs (string/split x #"\.")
                            p0 (if (> (count pcs) 0) (first pcs) "")] 
                        (and (not (= p0 "clojure")) (not (= p0 "java")))))
        out (if cleanup (filter notcj messy) messy)
        out (if cleanup (map singleclean out) out)
        out (concat [(exception2str e)] (into [] out))
        dbggs #(or (.contains % "non_debug_repl!!!") (.contains % "on_text_to_repl!!!"))
        out (if cleanup (filter #(not (dbggs %)) out) out)]
    (into [] out)))

