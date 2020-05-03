; Jump between functions.
(ns navigate.funcjump
  (:require [clojure.string :as string]
    [app.codebox :as codebox]
    [coder.cnav :as cnav]
    [collections]
    [coder.sunshine :as sunshine]
    [coder.textparse :as textparse]
    [coder.cbase :as cbase] [coder.crosslang.langs :as langs]))

; TODO: these functions have a lot of duplicate code with each other and others outside of func-jump.
; Some sort of refactor...
(defn try-to-go-ups [s shallow?]
  "Tries to go to all places one level above (all places = opens multiple codeboxes). 
   Returns unmodified s if it can't do so."
  (if-let [fc (get (:components s) (first (:selected-comp-keys s)))]
     (if (contains? #{:codebox :orepl :graphbox} (:type fc))
       (let [x (codebox/x-qual-at-cursor fc)]
         (if (and (symbol? x) (string/includes? (str x) "/")) 
           (let [lys (:gotos (:layout s))
                 syms-qual (cbase/uses-of x)
                 f01s (if shallow? (mapv cbase/defpath-fstr-ixs syms-qual)
                        (apply collections/vcat
                          (mapv (fn [sq] (let [source (:source (langs/var-info sq true))
                                               ns-sym (textparse/sym2ns sq)
                                               source-qual (sunshine/pipeline ns-sym source false)
                                               subdefpaths (cnav/paths-of source-qual x false)]
                                           (mapv #(cbase/subdefpath-fstr-ixs sq %) subdefpaths)))
                            syms-qual)))
                 fnames (mapv first f01s) ix0s (mapv second f01s) ix1s (mapv last f01s)]
             (if (> (count fnames) 0) (lys s fnames ix0s ix1s)
               (do (println "No uses of this symbol found.") s))) 
            (do (println "A non-local symbol needs to be aimed at.") s))) 
        (do (println "A codebox, repl, or graphbox needs to be selected.") s)) 
    (do (println "No components selected.") s)))

(defn try-to-go-into [s]
  "Tries to go into the symbol that is currently bieng spotlighted by whatever is selected in the state s.
   Returns unmodified s if it can't do so."
  (if-let [fc (get (:components s) (first (:selected-comp-keys s)))]
     (if (contains? #{:codebox :orepl :graphbox} (:type fc))
       (let [x (codebox/x-qual-at-cursor fc)]
         (if (and (symbol? x) (string/includes? (str x) "/")) 
           (let [f01 (cbase/defpath-fstr-ixs x) ly (:goto (:layout s))
                 fname (first f01) ix0 (second f01) ix1 (last f01)]
             (ly s fname ix0 ix1)) 
           (do (println "A nonlocal symbol needs to be aimed at.") s))) 
       (do (println "A codebox, repl, or graphbox needs to be selected.") s)) 
    (do (println "No components selected.") s)))