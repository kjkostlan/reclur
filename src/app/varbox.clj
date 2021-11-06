; A special codebox that allows temporary modifications of variables,
; In particular variables with auto-generated code for debugging.
(ns app.varbox
  (:require [app.codebox :as codebox] [c] [np]
    [app.fbrowser :as fbrowser]
    [app.rtext :as rtext]
    [coder.textparse :as textparse]
    [layout.blit :as blit] [layout.keyanal :as ka]
    [coder.crosslang.langs :as langs]
    [app.stringdiff :as stringdiff]))

(declare interact-fns) ; Possible dependency cycle with the new function being used by some interact fns.

;;;; New or deleted colorization ;;;;

(defn get-edits-ascending [box]
  "Edit diffs compared to original string."
  (stringdiff/last2first-first2last
    (stringdiff/edits-between (:source-txt box) (codebox/real-string box))))

(defn get-vis-edits-ascending [box]
  "Visual edits, ascending on the real string."
  (let [real-string-edits (get-edits-ascending box)
        real-strs (conj (mapv codebox/piece-real-string (:pieces box)) "")
        ren-strs (conj (mapv :text (:pieces box)) "")
        dark-matter (mapv #(- (count %1) (count %2)) real-strs ren-strs)
        fold-shifts (loop [acc [] ix 0 sh 0]
                      (if (= ix (count ren-strs)) (conj (apply c/vcat acc) sh)
                        (let [n (count (nth real-strs ix)) d (nth dark-matter ix)]
                          (recur (conj acc (repeat n sh)) (inc ix) (+ sh d)))))
        re-ix #(- % (get fold-shifts % (last fold-shifts)))
        edits-render (mapv (fn [edit] (-> (update edit :ix0 re-ix) (update :ix1 re-ix))) real-string-edits)]
    edits-render))

(defn char-status [box]
  "Inserted (1), unchanged (0) and deleted (-1) regions on the rendered string. Uses stringdiff.
   Deleted regions cover the two characters before and after the edit."
  (let [edits-render (get-vis-edits-ascending box)
        
        blanck (into [] (repeat (count (rtext/rendered-string box)) 0))
        slice-to (fn [x ix0 ix1 v] (reduce #(assoc %1 %2 v) x (range (max 0 ix0) (min (count x) ix1)))); Belongs in np?
        inserts (reduce (fn [statuss edit] (slice-to statuss (:ix0 edit) (+ (:ix0 edit) (count (:value edit))) 1))
                  blanck edits-render)
        inserts+deletes (reduce (fn [statuss edit] 
                                  (if (< (:ix0 edit) (:ix1 edit))
                                    (slice-to statuss (dec (:ix0 edit))
                                      (inc (:ix0 edit)) -1) statuss))
                          inserts edits-render)]
    inserts+deletes))

(defn colorize [box txt piece-ix char-ix0 char-ix1]
  (let [rgbas (codebox/colorize box txt piece-ix char-ix0 char-ix1)
        stats (subvec (char-status box) char-ix0 char-ix1)
        mix (fn [a b fr] (mapv #(+ (* %1 (- 1.0 fr)) (* %2 fr)) a b))
        get-col (fn [rgba c-stat]
                  (mix rgba (cond (= c-stat -1) [1 0 0 1] (= c-stat 0) [0.6 0.6 1.0 1] :else [0 1 0 1]) 
                    (if (= c-stat -1) 1.0 0.6)))]
    (mapv get-col rgbas stats)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn undo-change-at-cursor [box]
  "The editing is temporary, afterall, so reversion should be easy."
  (let [edits (get-edits-ascending box)]
    (if (= (count edits) 0) box
      (let [edits-vis (get-vis-edits-ascending box)
            vis-ixs (mapv #(+ (:ix0 %) (* 0.5 (count (:value %)))) edits-vis)
            abs-deltas (mapv #(Math/abs (- % (:cursor-ix box))) vis-ixs)
            ix (np/argmin abs-deltas)
            undoing-edits (stringdiff/remove-edit-edits (:source-txt box) edits ix)]
        (codebox/apply-edits-to-real-string box undoing-edits)))))

(defn new-varbox [] ; interact-fns
  (codebox/update-precompute
    (merge (assoc (codebox/new-codebox) :outline-color [0.7 0.7 0.7 1.0]
      :type :varbox
      :colorize-fn (fn [& args] (apply colorize args))) (interact-fns))))

(defn load-var [sym-qual]
  "Creates a new box set to the value of the var."
  (let [box (new-varbox) sym-qual (symbol sym-qual)
        box (assoc box :sym-qual sym-qual)
        src (langs/var-source sym-qual)
        txt (binding [*print-meta* true] (blit/vps src))
        box (assoc box :source src :source-txt txt :pieces [{:text txt}])]
    box))

(defn key-press [kevt box]
  (let [c-back? (and (ka/backspace? kevt) (ka/c? kevt))]
    (if c-back? (undo-change-at-cursor box)
      (codebox/key-press kevt box))))

(def dispatch
  {:mousePressed codebox/mouse-press
   :keyPressed (fn [kevt box] (key-press kevt box))
   :keyReleased rtext/key-release
   :mouseDragged rtext/mouse-drag
   :mouseWheelMoved rtext/mouse-wheel})

(defn interact-fns []
  {:dispatch dispatch
   :render (fn [box & show-cursor?]
             (let [box (codebox/update-precompute box) ;If is up-to-date already will not do anything.
                   head (get box :head "") foot (get box :foot "")
                   title (str "Temp edit: " (:sym-qual box))]
               (apply rtext/render (assoc box :path title) show-cursor?)))
   :expandable? codebox/expandable?
   :expand-child codebox/expand-child :contract-child codebox/contract-child
   :is-child? (fn [box] false)})

;;;;;;;;;;;; Extra fns for var saving.

(defn compile-err-report [^Exception e]
  "Don't print the stack trace, just the cause."
  (let [c (.getCause e)]
    (.getMessage c)))

(defn get-ns-obj [box]
  (let [ns-sym (textparse/sym2ns (:sym-qual box)) ns-obj (find-ns ns-sym)]
    (if (not ns-obj) (throw (Exception. (str "Can't find namespace to modify code in: " ns-sym)))) ns-obj))

(defn eval-box [box code] "What the code inside the box evals to."
  (let [ns-obj (get-ns-obj box)]
    (binding [*ns* ns-obj] (eval code))))

(defn save-to-var! [box]
  "Saves what is in the box to the var, but does not modify the :source.
   Prints an exception if the var cannot be evaled."
  (let [sym-qual (:sym-qual box)
        code (try (read-string (codebox/real-string box))
               (catch Exception e (println "Syntax err save-var" sym-qual (.getMessage e))))]
    (if (not (nil? code))
      (try (do (eval-box box code) (println "Var-tmp-save:" sym-qual))
        (catch Exception e (println "Compile error for" (str sym-qual ":\n ")
                             (compile-err-report e)))))))

(defn revert-var! [box]
  "When a box is closed, these changes which are temporary debugs are reverted"
  ; app/multisync when closed?
  (let [code0 (:source box)]
    (try (eval-box box code0)
      (catch Exception e
        (println "Cannot revert var" (:sym-qual box) "original var has error in it."))) box))