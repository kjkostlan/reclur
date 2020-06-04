; Jump between functions.
(ns navigate.funcjump
  (:require [clojure.string :as string]
    [app.codebox :as codebox]
    [app.rtext :as rtext]
    [coder.cnav :as cnav]
    [collections]
    [coder.logger :as logger]
    [coder.sunshine :as sunshine]
    [coder.textparse :as textparse]
    [coder.cbase :as cbase] [coder.crosslang.langs :as langs]
    [javac.file :as jfile]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Support functions ;;;;;;;;;;;;;;;;;

(defn stackelem-to-fixsym [elem]
  "Tries to convert a stack element to [fname ix0 ix1 sym-qual].
   nil fail."
  (let [elem (if-let [x (re-find #"\(.*\)" elem)] x elem)
        elem (-> elem (string/replace ":" " ")
               (string/replace "(" "")
               (string/replace ")" ""))
        pieces (string/split elem #" +")]
    (if (>= (count pieces) 2)
      (let [p0 (first pieces) fname? (.endsWith ^String p0 ".clj") ; TODO: not just clojure.
            fname (if fname? (if (.startsWith ^String p0 "./src") p0 (str "./src/" p0))
                    (langs/ns2file (textparse/sym2ns p0)))
            line-num (try (Integer/parseInt (.trim ^String (second pieces)))
                       (catch Exception e false))
            txt (if line-num (jfile/open fname))]
        (if (and txt line-num)
          (let [ixs (textparse/line-to-string-ixs txt line-num false)]
            (if (first ixs) [fname (first ixs) (second ixs) (if fname? false p0)])))))))

(defn stack-click [box]
  "Tries to find the file and character ixs for a stack."
  (let [txt (rtext/rendered-string box)
        ixjx (rtext/cursor-ix-to-ugrid box) linenum-in-box (inc (second ixjx))
        str-with-stacktrace (apply subs txt (textparse/line-to-string-ixs txt linenum-in-box false))
        x (stackelem-to-fixsym str-with-stacktrace)]
    (if x [(first x) (second x) (nth x 2)])))

(defn log-up-trace-fixs [sym-qual]
  "Attempts to find the location that called sym-qual in the most recent log within sym-qual.
   [file ix0 ix1 sym]"
  (let [logs (coder.logger/get-logs)
        stack-match (fn [log]
                      (let [tr (langs/convert-stack-trace (:TraceOb log))
                            deepest (first tr)
                            pieces (string/split deepest #" ")]
                        (if (= (symbol (first pieces) sym-qual)) tr)))
        trace (first (filter stack-match (reverse logs)))
        _ (if (not trace) (throw (Exception. ("No log called inside the defbody for symbol: " sym-qual))))
        parent-elem (nth trace 
                      (dec (last (filter 
                                   (fn [ix]
                                       (let [elem (nth trace ix)
                                             tr-sym (symbol (first (string/split elem #" ")))]
                                         (= tr-sym sym-qual)))
                                   (range (count trace))))))]
    (stackelem-to-fixsym parent-elem)))

;;;;;;;;;;;;;;;;;;;; Main navigation functions ;;;;;;;;;;;;;;;;;;;;;;
; TODO: these functions have a lot of duplicate code with each other and others outside of func-jump.
; Some sort of refactor...

(defn try-to-go-ups [s shallow? log?]
  "Tries to go to all places one level above (all places = opens multiple codeboxes). 
   Returns unmodified s if it can't do so."
  (if-let [fc (get (:components s) (first (:selected-comp-keys s)))]
     (if (contains? #{:codebox :orepl :graphbox} (:type fc))
       (let [x (codebox/x-qual-at-cursor fc)]
         (if (and (symbol? x) (string/includes? (str x) "/")) 
           (let [lys (:gotos (:layout s))
                 syms-qual (if (not log?) (cbase/uses-of x))
                 goto-me-log (if log? (try (log-up-trace-fixs x) (catch Exception e (println "Logged-based traceback not working."))))
                 f01s (cond (and shallow? (not log?)) 
                        (mapv cbase/defpath-fstr-ixs syms-qual)
                        (not log?)
                        (apply collections/vcat
                          (mapv (fn [sq] (let [source (:source (langs/var-info sq true))
                                               ns-sym (textparse/sym2ns sq)
                                               source-qual (sunshine/pipeline ns-sym source false)
                                               subdefpaths (cnav/paths-of source-qual x false)]
                                           (mapv #(cbase/subdefpath-fstr-ixs sq %) subdefpaths)))
                            syms-qual))
                        (not goto-me-log) [] ; log goto fail.
                        (not shallow?) [(subvec goto-me-log 0 3)]
                        :else (let [dest-sym-qual (last (goto-me-log))]
                                (cbase/defpath-fstr-ixs dest-sym-qual)))
                 fnames (mapv first f01s) ix0s (mapv second f01s) ix1s (mapv last f01s)]
             (if (> (count fnames) 0) (lys s fnames ix0s ix1s)
               (do (if (not log?) (println "No uses of this symbol found.")) s))) 
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