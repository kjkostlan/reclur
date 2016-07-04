; Automatically fixes errors. 

(ns clooj.coder.autofix
  (require [clooj.coder.io :as cio] [clojure.string :as string]
           [clooj.coder.tracer :as tracer]
           [clooj.coder.grammer :as grammer]
           [clooj.coder.strmap :as strmap]
           [clooj.coder.cbase :as cbase]
           [clooj.coder.namespacer :as namespacer]
           [clooj.java.file :as jfile]))


(defn fix [text err]
  "Attempts to fix an error err in some source text.
   Only very simple, obvious stuff can be fixed.
   But it is useful in that it can auto-import stuff, etc.
   :str = the output, fixed, string. nil if no fix can be found.
   :map = approx where the locations in the old characters go to on the new string. nil if no fix.
      TODO: use map. Map is for knowing where to re-scroll the text-areas when updating the string."
   (let [cause (.getCause err)
         causes (mapv string/trim (string/split (str cause) #":"))
         loc (tracer/get-location err)
         bad-char (grammer/line-char2char text (:line loc) (:char loc)) ; the character that caused the error in the first place.
         ;rest-of (.substring text char) ; the beginning of the error string.
         
         ; error id shorthands TODO: this is not the most robust thing to version changes, maby some other way.
         no-ns? (= (second causes) "No such namespace")
         no-var? (= (second causes) "No such var")
         no-sym? (= (second causes) "Unable to resolve symbol")

         parsed (cio/parse-summary text)
         ; outer level but don't use cio/outer-level-islands b/c it's not robust.
         islands (cio/extract-outer-islands parsed)
         ]
     ; We are using a variable that is not defined yet. We need to import something:
     (cond (or no-ns? no-var? no-sym?)
       (let [sym_ (.trim (.replace (nth causes 2) "in this context" "")); The symbol from the error msg.
             
             ; 99% of the time: should be the symbol that is the culprit.
             sym (first (strmap/filterv+ #(.contains % sym_) (strmap/re-hits grammer/token-match text bad-char)))
             
             code (strmap/reads-string+ text)
             address (strmap/pos-lookup code (:pos sym)) ; where to find the symbol.

             ; use databases to get the fully-qualified name and fetch:
             x (cbase/guess-full-fetch (first sym))
             sym-short (:short x)
             sym-fetch (:fetch x)
             
             ; Add the namespace:
             code (assoc-in code [:obj 0 :obj] (conj (get-in code [:obj 0 :obj]) {:str (str sym-fetch)}))
             
             ; Replace the symbol with the correct one:
             code (assoc-in code address {:str (str sym-short)})
             s (strmap/code-to-string code)
             ] {:str s :map (into [] (range (count s)))})
      :else nil)))
    