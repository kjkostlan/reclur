; Jump to definition and related functions.

(ns app.funcjump
  (:require [coder.clojure :as cljparse]
    [app.hintbox :as hintbox]
    [app.multicomp :as multicomp]
    [coder.cbase :as cbase]
    [javac.file :as jfile]
    [collections]))

(defn find-def [s cbox]
  "Finds [filename, cursor-ix0, cursor-ix1] of whatever def/defn/etc was found by the symbol near the cursor ix in symbox."
  (let [at-cursor (hintbox/whats-at-cursor cbox)] ; attempts to fully qualify the symbol.
    (if (:resolved? (meta at-cursor))
      (let [ns-target (cbase/ns-of at-cursor)
            unqual-sym (cbase/unqual at-cursor)
            fname-target (cbase/ns2file ns-target)
            txt (jfile/open fname-target)]
        (if txt
          (let [code+ (cljparse/forwards txt) 
                ix (first (filter #(= (second (nth code+ %)) unqual-sym) 
                            (range (count code+))))]
            (if ix
              (let [path-in-code [ix 1]
                    ix01 (cljparse/locate-in code+ path-in-code false)]
                [fname-target (first ix01) (second ix01)]))))))))
 
(defn find-users [s cbox]
  "Opposite of find-def."
  (let [at-cursor (hintbox/whats-at-cursor cbox)] ; attempts to fully qualify the symbol.
    (if (:resolved? (meta at-cursor))
      (let [user-defs (cbase/uses-of at-cursor)
            user-files (mapv #(cbase/ns2file (cbase/ns-of %)) user-defs)
            s-user-files (set user-files)
            file2code+ (zipmap s-user-files (mapv #(cljparse/forwards (jfile/open %))
                                              s-user-files))
            ix01s (mapv (fn [fname sym] 
                          (let [usym (cbase/unqual sym) 
                                code+ (get file2code+ fname)
                                ix (first (filter #(= (second (nth code+ %)) usym) 
                                            (range (count code+))))]
                            (cljparse/locate-in code+ [ix 1] false))) 
                    user-files user-defs)]
        [user-files (mapv first ix01s) (mapv second ix01s)]))))