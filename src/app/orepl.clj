; One repl: it shows the output of commands.
(ns app.orepl
  (:require [clojure.string :as string]
    [clojure.set :as set]
    [app.codebox :as codebox]
    [app.rtext :as rtext]
    [javac.clipboard :as clipboard] 
    [javac.file :as jfile]
    [javac.exception :as jexc]
    [coder.plurality :as plurality]))

; The rtext has three pieces: 
; The first is the text entered. The second is a newline. The last is the repl's output.

(declare interact-fns) ; Possible dependency cycle with the new function being used by some interact fns.

;;;;;;;;;;;;;;;;;;;;;;;;; Look and feel ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pr-error [e] ; shows the stack trace.
  (let [e-clj (jexc/clje e)]
    (apply str (:Message e-clj) " (" (subs (str (type e)) 6) ")" "\n"
      (let [st (:StackTrace e-clj)]
        (interpose "\n" (mapv #(str  "  " (:ClassName %) " " (:LineNumber %)) st))))))

(defn colorize [box s piece-ix char-ix0 char-ix1]
  "levels and sp?s are one element per character. 
    non-white chars in sp?s are rendered the same as comments." ; 380 ms.
  (let [box1 (codebox/set-precompute (update box :pieces #(vector (first %))))
        t1 (:text (first (:pieces box1))) n1 (+ (count (:text (second (:pieces box)))) (count (:text (nth (:pieces box) 2))))]
    (subvec (into [] (concat (codebox/colorize box1 t1 (into [] (repeat (count t1) 0)) 0 (count t1))
                     (repeat n1 [1 1 1 1])))
      char-ix0 char-ix1))) ; lazy way.

(defn limit-length [s]
  (let [max-len 10000 tmp "...<too long to show>"]
    (if (> (count s) max-len) (str (subs s 0 (- max-len (count tmp))) tmp) s)))

;;;;;;;;;;;;;;;;;;;;;;;;; Setting up a repl ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-repl []
  (assoc rtext/empty-text :type :orepl :lang :clojure :pieces [{:text "(+ 1 2)"} {:text "\n"} {:text "3"}]
   :outline-color [0.2 0.2 1 1]
   :interact-fns (interact-fns) :path "repl" :show-line-nums? false :colorize-fn colorize))

(defn command-wrapped-repl [app-state-symbol code cursor-ix0]
  "Generates a fresh repl that is set up to run command code applied to the overall state, encoded as app-state-symbol.
   Code should be a pprinted string since we care about cursor index."
  (let [b4 (str "(let [" app-state-symbol " @_state-atom\n"
                       "result\n")
        afr "] \n (reset! _state-atom result) [])"
        
        cursor-ix (+ (count b4) cursor-ix0)
        code (str b4 code afr)]
    (assoc (new-repl) :cursor-ix cursor-ix
      :pieces [{:text code} {:text "\n"} {:text ""}])))

;;;;;;;;;;;;;;;;;;;;;;;;; Running the repl ;;;;;;;;;;;;;;;;;;;;;;;;;;

(def r-ns *ns*)

(def _state-atom (atom {})) ; only used locally for the mutation-free run-standard-repl function.
(defn set-state [s-new] "No effect outside of repl." (reset! _state-atom s-new))
(defn swap-state [f] "No effect outside of repl." (swap! _state-atom f))

(defn shift-enter? [key-evt] (and (:ShiftDown key-evt) (= (:KeyCode key-evt) 10)))

(defn recognized-cmd? [sym] (boolean (ns-resolve r-ns sym)))

(defn cmd?-parse [s] 
  "Sugar. Returns [cmd-sym, [arg symbols]] if a valid cmd shorthand, otherwise returns nil."
  (let [tokens (string/split s #"[, ;]+")
        sym (if (> (count tokens) 0) (symbol (first tokens)))]
    (if (and sym (recognized-cmd? sym))
      [sym (mapv read-string (rest tokens))])))

(defn run-standard-repl [s repl-k txt]
    (reset! _state-atom s)
    (let [result (try (let [code (read-string txt)] ; internal mutation of _state-atom possible.
                        (try (let [y (str (binding [*ns* r-ns] (eval code)))]
                               (limit-length y))
                             (catch Exception e (limit-length (str "Runtime error:\n" (pr-error e))))))
                     (catch Exception e (limit-length (str "Syntax error: " e))))]
      (assoc-in @_state-atom [:components repl-k :pieces 2 :text] result)))

(defn _run-cmd [s cmd-sym args]
  "Returns the modified state."
  ; TODO: add a sort of spell-check.
  (if (recognized-cmd? cmd-sym) 
    (let [tool-fn (binding [*ns* r-ns] (eval cmd-sym))] 
      (apply tool-fn s args))
    (throw (Exception. (str "Unrecognized command: " cmd-sym)))))
(defn run-cmd [s cmd-sym arg-symbols repl-k]
  "Special cmd sequence that is more like shell scripting."
  (let [args (binding [*ns* r-ns] (mapv #(if (symbol? %) (eval %) %) arg-symbols))]
    (try (_run-cmd s cmd-sym args)
      (catch Exception e
        (assoc-in s [:components repl-k :pieces 2 :text] (limit-length (str "Cmd err: " e)))))))

(defn run-repl [s repl-k] 
  ; TODO: put repls on another process with it's own threads.
  ; The other process has no windows (headless).
  ; Sync our namespace variables with that process.
  ; [org.clojure/tools.nrepl "0.2.12"]
  ; https://github.com/clojure/tools.nrepl seems most promising.
  ; Infinite loops may starve (memory) said process.
    ; But if it hangs just use stop-server and restart.
    ; Have two servers open at once for insta-switch.
  ; If there is a *gui-atom* in the input we run it locally on a future.
    ; Crashes can leak into here but at least this gives us a way to modify the gui.
  (let [box (get-in s [:components repl-k])
        txt (:text (first (:pieces box)))]
    (if-let [x (cmd?-parse txt)] 
      (run-cmd s (first x) (second x) repl-k)
      (run-standard-repl s repl-k txt))))
 
(defn ensure-three-pieces [box]
  (let [ps (mapv #(if (:text %) % {:text ""}) (:pieces box)) n (count ps)]
    (assoc box :pieces
      (cond (= n 0) [{:text ""} {:text "\n"} {:text ""}]
        (= n 1) [(first ps) {:text "\n"} {:text ""}]
        (= n 2) [(first ps) {:text "\n"} {:text ""}]
        :else [{:text (apply str (:text (first ps)) (drop-last (:text (second ps))))}
               {:text "\n"} (nth ps 2)]))))
  
(defn key-press [key-evt box]
  "Modified to: only edit the first piece. The first piece must stay non-empty (given a space).
   No run-repl here. Run-repl modifies the state so we call it from multicomp."
  (let [ed (rtext/key-to-edit box key-evt) arrow? (= (:type ed) :arrow)
        n0 (count (:text (first (:pieces box))))
        ed (if (and (not arrow?) (:ix0 ed)) (update ed :ix0 #(min % n0)) ed)
        ed (if (and (not arrow?) (:ix1 ed)) (update ed :ix1 #(min % n0)) ed)
        box (if (or (= (:type ed) :type) (= (:type ed) :cut))
               (update box :cursor-ix #(min % n0)) box)] ; move cursor back for typing.
    (ensure-three-pieces 
      (if (= (:type ed) :paste) ; paste the string in instead of the pieces.
        (let [txt (clipboard/get-as-string)]
          (update-in box [:pieces 0 :text] #(str (subs % 0 (:ix0 ed)) txt (subs % (:ix1 ed)))))
          (rtext/dispatch-edit-event box ed)))))

;;;;;;;;;;;;;;;;;;;;;;;;; Namespace reloading et al ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _resolve [sym]
  (try (resolve sym) (catch Exception e)))

(defonce var-atom
   ;Fully-qualified symbols -> var for the old namespace vars. Thus we don't lose the old references.
   (atom {}))

(defn _update-core!! [ns-symbol removing?] ; returns (not throws) an exception if the code doesn't compile.
  (let [tmp-sym (gensym 'tmp) ns-tmp (create-ns tmp-sym)]
    (let [maybe-er (binding [*ns* ns-tmp] ; So the require we call doesn't affect the orepl ns.
                     (let [ns-object (find-ns ns-symbol)
                           vars-dayold (if ns-object (ns-interns ns-object) {}) ; symbol -> var.
                           _ (mapv #(ns-unmap ns-object %) (keys vars-dayold)) 
                           maybe-e (if removing? false ; the actual reloading part.
                                     (try (do (require ns-symbol :reload) false)
                                       (catch Exception e e)))
                           ns-object (find-ns ns-symbol) ; the :reload needs this.
                           var-at @var-atom
                           vars-new (if removing? {} (if maybe-e vars-dayold (ns-interns ns-object))) ; compile error = don't change the namespace.
                           deleted-stuff (set/difference (apply hash-set (keys vars-dayold)) (apply hash-set (keys vars-new)))]
                       ;(if (> (count deleted-stuff) 0) (println "Removed from the code: " ns-symbol deleted-stuff))
                       ; Keep the old var objects around, just set their value to the new stuff.
                       (mapv (fn [sym] 
                               (let [sym-full-qual (symbol (str ns-symbol "/" sym))
                                     var-old (if-let [v (get var-at sym-full-qual)] v (get vars-dayold sym))
                                     old-val @var-old deleted? (boolean (get deleted-stuff sym))
                                     err (Exception. (str sym-full-qual (if removing? " belongs to a .clj file that was deleted." " has been removed from it's .clj file.")))
                                     new-val (if deleted?
                                               (if (fn? old-val) (fn [& args] (throw err)) err)
                                               @(get vars-new sym))]
                                 ; Don't know which of these (or both) is necessary. They are very similar:
                                 (alter-var-root var-old (fn [_] new-val))
                                 (swap! var-atom #(assoc % sym-full-qual var-old))
                                 (intern ns-object sym new-val)))
                         (keys vars-dayold)) maybe-e))]
      (remove-ns tmp-sym) maybe-er)))

(defn reload-file!! [cljfile]
  "Reloads a given clj file, removing the ns if the file no longer exists."
  (let [ns-symbol (symbol (jfile/file2dotpath cljfile))
        file-exists? (jfile/exists? cljfile)]
    (cond 
      (and (not (find-ns ns-symbol)) (not file-exists?)) {:error false :message "Non-existant namespace with non-existant file, no reloading needed."}
      file-exists?
      (let [maybe-e (_update-core!! ns-symbol false)
            report (if maybe-e {:error (pr-error maybe-e) :message "Compile error"}
                      {:error false :message (str cljfile " saved and updated without error.")})] report)
      :else (do (_update-core!! ns-symbol true) {:error false :message "File no longer exists, deleted ns."}))))

(defn save-and-update!!! [cljfile text]
  "Saves a clj file and attempts to reload the namespace, returning the error info if there is a compile error."
  (jfile/save!!! cljfile text)
  (reload-file!! cljfile))

(defn delete-and-update!!! [cljfile]
  (jfile/delete!!! cljfile) (reload-file!! cljfile) {:error false :message (str "Deleted" cljfile)})

;;;;;;;;;;;;;;;;;;;;;;;;; Component interface ;;;;;;;;;;;;;;;;;;;;;;;;;;

; No child UI is planned in the near future.
(defn expandable? [mouse-evt box] false)
(defn expand-child [mouse-evt box] (throw (Exception. "No plans to implement orepl child-UI.")))
(defn contract-child [box child] (throw (Exception. "No plans to implement orepl child-UI.")))

(defn dispatch-heavy [evt s k]
  "Running the repl may affect s, depending on the command. This is agnostic to which repl is focused, i.e the k value."
  (if (and (= (:type evt) :keyPressed) (shift-enter? evt))
    (reduce #(if (= (:type (get (:components %1) %2)) :orepl)
               (run-repl %1 %2) %1) s (:selected-comp-keys s)) s))

(def dispatch 
  (plurality/->simple-multi-fn
    {:mousePressed rtext/mouse-press
     :mouseDragged rtext/mouse-drag
     :mouseWheelMoved rtext/mouse-wheel
     :keyPressed key-press
     :keyReleased rtext/key-release}
     (fn [e-clj comp] comp)
     (fn [e-clj comp] (:type e-clj))))

(defmacro updaty-fns [code] 
  (let [a1 (gensym 'args)] 
    (zipmap (keys code) (mapv #(list `fn ['& a1] (list `apply % a1)) (vals code)))))
(defn interact-fns []
  (updaty-fns
  {:dispatch dispatch 
   :dispatch-heavy dispatch-heavy
   :render rtext/render
   :mouseMoved (fn [_ box] box)
   :expandable? expandable?
   :is-child? (fn [box] false)
   :expand-child expand-child :contract-child contract-child}))
