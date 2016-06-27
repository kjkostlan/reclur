(ns clooj.repl.main
  (:import (java.io
             BufferedReader BufferedWriter
             InputStreamReader
             File PipedReader PipedWriter PrintWriter Writer
                    StringReader PushbackReader)
           (clojure.lang LineNumberingPushbackReader)
           (java.awt Rectangle)
           (java.net URL URLClassLoader URLDecoder)
           (java.util.concurrent LinkedBlockingQueue))
  (:require [clj-inspector.jars :as jars]
            [clojure.string :as string]
            [clojure.tools.nrepl :as nrepl]
            [clojure.java.io :as io]
            [clooj.coder.io :as cio]
            [clooj.coder.namespacer :as namespacer]
            [clooj.brackets :as brackets]
            [clooj.protocols :as protocols]
            [clooj.utils :as utils]
            [clooj.repl.debugger :as debugger]
            [clooj.app.state_old :as app_old]
            [clooj.coder.cbase :as cbase]
            [clooj.java.file :as jfile]
            [clooj.brackets :as brackets]
            [clooj.search :as search]
            [clooj.java.textarea_old :as jtext_old]))

(use 'clojure.java.javadoc)

(defn is-eof-ex? [throwable]
  (and (instance? clojure.lang.LispReader$ReaderException throwable)
       (or
         (.startsWith (.getMessage throwable) "java.lang.Exception: EOF while reading")
         (.startsWith (.getMessage throwable) "java.io.IOException: Write end dead"))))

(defn read-string-at [source-text start-line]
  `(let [sr# (java.io.StringReader. (str (apply str (repeat ~start-line "\n"))
                                         ~source-text))
         rdr# (clojure.lang.LineNumberingPushbackReader. sr#)]
     (take-while #(not= % :EOF_REACHED)
                 (repeatedly #(try (read rdr#)
                                   (catch Exception e# :EOF_REACHED))))))

; Stuff printed as println will still go to the command window, TODO: fix this.
(defn send-to-repl!!!
    ([cmd] (debugger/on-text-to-repl!!! cmd)))

(defn selected-region [ta]
  (if-let [text (.getSelectedText ta)]
    {:text text
     :start (.getSelectionStart ta)
     :end   (.getSelectionEnd ta)}
    (let [[a b] (brackets/find-line-group ta)]
      (when (and a b (< a b))
        {:text (.. ta getDocument (getText a (- b a)))
         :start a
         :end b}))))

; TODO: namespace of box for which we selected this in.
(defn send-selected-to-repl!!! []
  (let [ta (:textarea @app_old/src-text-area)
        region (selected-region ta)
        txt (:text region)]
    (if-not txt
      (jtext_old/append-text! app_old/repl-out-text-area "Malformed expression\n")
      (let [line (.getLineOfOffset ta (:start region))]
        (send-to-repl!!! txt)))))

(defn send-alltext-to-repl!!! [hint]
  (let [text (jtext_old/get-text app_old/src-text-area)]
    (jtext_old/append-text! app_old/repl-out-text-area hint)
    (send-to-repl!!! text)))

(defn view-history-item!! [ind dir]
    "ind: where we are. dir: which way we are going"
    (let [history @app_old/repl-history
          txt (jtext_old/get-text app_old/repl-in-text-area)
          caret (jtext_old/get-caret-position app_old/repl-in-text-area)
          newind (search/find-history-ix history ind dir (subs txt 0 caret))
          newtxt (if (= newind -1) txt (nth history newind))]
     (jtext_old/set-text! app_old/repl-in-text-area newtxt caret)
     (if (> newind -1) (reset! app_old/repl-history-ind newind))))

(defn show-previous-repl-entry!! []
  (let [ind (max 0 (dec @app_old/repl-history-ind))] 
     (view-history-item!! ind -1)))

(defn show-next-repl-entry!! []
  (let [n (count @app_old/repl-history)
        ind (min (- n 1) (inc @app_old/repl-history-ind))] 
     (view-history-item!! ind 1)))

(defn add-repl-input-handler!! []
  (let [atom-in app_old/repl-in-text-area
        atom-out app_old/repl-out-text-area
        ta-in (:textarea @atom-in)
        ta-out (:textarea @atom-out)
        get-caret-pos #(.getCaretPosition ta-in)
        ready #(let [caret-pos (get-caret-pos)
                     txt (jtext_old/get-text atom-in)
                     trim-txt (string/trimr txt)]
                 (and
                   (pos? (.length trim-txt))
                   (<= (.length trim-txt)
                       caret-pos)
                   (= -1 (first (brackets/find-enclosing-brackets
                                  txt
                                  caret-pos)))))
        submit #(when-let [txt (jtext_old/get-text atom-in)]
                  (do (send-to-repl!!! txt)
                      (jtext_old/set-text! app_old/repl-in-text-area "" 0)))
        at-top #(zero? (.getLineOfOffset ta-in (get-caret-pos)))
        at-bottom #(= (.getLineOfOffset ta-in (get-caret-pos))
                      (.getLineOfOffset ta-in (count (jtext_old/get-text atom-in))))
        prev-hist #(show-previous-repl-entry!!)
        next-hist #(show-next-repl-entry!!)]
    (utils/attach-child-action-keys! ta-in ["UP" at-top prev-hist]
                              ["DOWN" at-bottom next-hist]
                              ["ENTER" ready submit])
    (utils/attach-action-keys! ta-in ["cmd1 UP" prev-hist]
                        ["cmd1 DOWN" next-hist]
                        ["cmd1 ENTER" submit])))

; reloads a file:
(defn reload-file [file]
  "Reloads a file's namespace. ANY namespace that uses said file will now use updated variables.
   HOWEVER, deleted variables (or the old name for renamed ones) are still accessable untill program restarts. TODO: fix this.
   Finally, this can print an error to the console despite working properly at times. TODO: thier bug."
  (let [dotpath (jfile/file2namespace file) namespace (create-ns '_debugger.namespace)]
    (binding [*ns* namespace] (require (symbol dotpath) :reload))))

         
(defn find-file [str]
  "searches for the path-local filename given a 'nickname', using a heuristic
   based on usage patterns in the code."
   (let [found-ns (cbase/guess-ns str)]
     (if (nil? found-ns) nil (jfile/namespace2file found-ns))))

(defn idiomatic-require [hint repl?]
  "gets the idiomatic require form of the namespace gotten by hint. Returns the code in string form."
  (let [nsfull (cbase/guess-ns hint)]
    (if (nil? nsfull) nil ; nothing.
      (let [non-repl (cbase/guess-fetch nsfull)]
        (if repl? (rest (namespacer/ns-quote (list 'ns non-repl))) non-repl))))); no need for the repl.

(defn user-decide-file [] 
  "have the user decide on a file"
  (let [uinput (utils/user-input "type a filename, path, or even an nickname used in :as may work")]
    (if (nil? uinput) -1 (find-file uinput))))

; WARNING: this and related code is very sensitive to runtime-breaking upon refactoring!

(defn toggle-breakpoint [text caret]
    "toggles a breakpoint in a string of text, returns {:text modified text :caret  new caret location}"
  (let [isl (cio/local-island text caret)
        lo (nth isl 0)
        hi (nth isl 1)
        bp "(debugger/breakpoint!!! "
        nbp (count bp)
        ensure #(namespacer/ensure-require % "clooj.repl.debugger" "debugger")
        remove #(namespacer/remove-require % "clooj.repl.debugger" "debugger")]
    (if (= lo -1)
      {:text text :caret caret}; no modification as it can't find a breakpoint.
      (let [isls (subs text lo hi) lo1 (+ lo 1)]
        (if (= (subs isls 0 (max nbp (inc (- hi lo)))) bp)
          ; remove breakpoint (and remove the require if nessessary), place cursor at beginning of removed breakpoint:
          (let [out (str (subs text 0 lo) "(" (subs isls (+ nbp 1) (- (count isls) 1)) (subs text hi))]
            (if (.contains out "debugger/breakpoint!!!") {:text out :caret lo1} 
              (let [outsmall (remove out)] {:text outsmall :caret (+ lo1 (- (count outsmall) (count out)))})))
          ; make breakpoint:
          (let [out (str (subs text 0 lo) "(debugger/breakpoint!!! " isls ")" (subs text hi))
                outlarge (ensure out)]
                {:text outlarge :caret (+ lo1 (- (count outlarge) (count out)))}))))))