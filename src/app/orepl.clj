; One repl: it shows the output of commands.
(ns app.orepl
  (:require [clojure.string :as string]
    [clojure.set :as set]
    [app.codebox :as codebox]
    [app.rtext :as rtext]
    [javac.clipboard :as clipboard] 
    [javac.file :as jfile]
    [javac.exception :as jexc]
    [coder.plurality :as plurality]
    [layout.colorful :as colorful]
    [layout.keybind :as kb]))

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
        t1 (:text (first (:pieces box1))) n1 (+ (count (:text (second (:pieces box)))) (count (:text (nth (:pieces box) 2))))
        col-output (conj (colorful/cmdix2rgb (:num-run-times box)) 1)]
    (subvec (into [] (concat (codebox/colorize box1 t1 (into [] (repeat (count t1) 0)) 0 (count t1))
                     (repeat n1 col-output)))
      char-ix0 char-ix1))) ; lazy way.

(defn limit-length [s]
  (let [max-len 10000 tmp "...<too long to show>"]
    (if (> (count s) max-len) (str (subs s 0 (- max-len (count tmp))) tmp) s)))

;;;;;;;;;;;;;;;;;;;;;;;;; Setting up a repl ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-repl []
  (assoc rtext/empty-text :type :orepl :lang :clojure :pieces [{:text "(+ 1 2)"} {:text "\n"} {:text "3"}]
   :num-run-times 0
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
        txt (:text (first (:pieces box)))
        s1 (update-in s [:components repl-k :num-run-times] inc)]
    (if-let [x (cmd?-parse txt)] 
      (run-cmd s1 (first x) (second x) repl-k)
      (run-standard-repl s1 repl-k txt))))

(defn ensure-three-pieces [box]
  (let [ps (mapv #(if (:text %) % {:text ""}) (:pieces box)) n (count ps)]
    (assoc box :pieces
      (cond (= n 0) [{:text ""} {:text "\n"} {:text ""}]
        (= n 1) [(first ps) {:text "\n"} {:text ""}]
        (= n 2) [(first ps) {:text "\n"} {:text ""}]
        :else [{:text (apply str (:text (first ps)) (drop-last (:text (second ps))))}
               {:text "\n"} (nth ps 2)]))))

(defn key-press [key-evt box]
  (if (kb/shift-enter? key-evt) box ; this was handled in the heavy dispatch.
    (let [ed (rtext/key-to-edit box key-evt)
          box1 (if (= (:type ed) :paste)
                 (rtext/dispatch-edit-event box
                   (let [v (:value ed) ty (:comp-type (meta v))
                         v1 (cond (string? v) v
                              (= ty :codebox) (apply str (codebox/real-strings {:pieces v}))
                              :else (apply str (mapv :text v)))]
                     (assoc ed :value v1)))
                 (rtext/key-press key-evt box))]
      (ensure-three-pieces box1))))

;;;;;;;;;;;;;;;;;;;;;;;;; Namespace reloading et al ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _resolve [sym]
  (try (resolve sym) (catch Exception e)))

(defonce var-atom
   ;Fully-qualified symbols -> var for the old namespace vars. Thus we don't lose the old references.
   (atom {}))

(defn _clear-vars!! [ns-symbol] ; sets vars to throw or be an error message.
  (let [nms (find-ns ns-symbol)]
    (if nms
      (let [interns (ns-interns nms)]
        (zipmap (keys interns) 
          (mapv (fn [k vr] (alter-var-root vr (fn [_] (fn [& args] (throw (Exception. (str "Variable:" k "/" ns-symbol " has been removed."))))))) 
            (keys interns) (vals interns)))))))

(defn _update-simple!! [ns-symbol removing?]
  (let [tmp-sym (gensym 'tmp) ns-tmp (create-ns tmp-sym)]
    (_clear-vars!! ns-symbol)
    (binding [*ns* ns-tmp]
      (let [maybe-e (if removing? false ; the actual reloading part.
                      (try (do (require ns-symbol :reload) false)
                              (catch Exception e e)))]
        maybe-e))))

(defn reload-file!! [cljfile]
  "Reloads a given clj file, removing the ns if the file no longer exists."
  (let [ns-symbol (symbol (jfile/file2dotpath cljfile))
        file-exists? (jfile/exists? cljfile)]
    (cond 
      (= ns-symbol 'ect) {:error false :message "The project.clj file has no namespace."} ; special for saving changes to the project.clj
      (and (not (find-ns ns-symbol)) (not file-exists?)) {:error false :message "Non-existant namespace with non-existant file, no reloading needed."}
      file-exists?
      (let [maybe-e (_update-simple!! ns-symbol false) ;(_update-core!! ns-symbol false)
            report (if maybe-e {:error (pr-error maybe-e) :message "Compile error"}
                      {:error false :message (str cljfile " saved and updated without error.")})] report)
      :else (do (_update-simple!! ns-symbol true) {:error false :message "File no longer exists, deleted ns."}))))

(defn save-and-update!!! [cljfile text]
  "Saves a clj file and attempts to reload the namespace, returning the error info if there is a compile error."
  (jfile/save!!! cljfile text)
  (reload-file!! cljfile))

(defn delete-and-update!!! [cljfile]
  (jfile/delete!!! cljfile) (reload-file!! cljfile) {:error false :message (str "Deleted" cljfile)})

(defn mouse-press [m-evt comp]
  (let [nc (:ClickCount m-evt) ctxt (:text (first (:pieces comp)))]
    (if (and (or (= nc 2) (= nc 4)) (<= (:cursor-ix comp) (count ctxt)))
      (let [cbox (codebox/from-text ctxt (:lang comp))
            ckys [:selection-start :selection-end :cursor-ix]
            cbox (merge cbox (select-keys comp ckys))
            cbox1 (codebox/select-twofour-click cbox (= nc 4))]
        (merge comp (select-keys cbox1 ckys)))
      (rtext/mouse-press m-evt comp))))

;;;;;;;;;;;;;;;;;;;;;;;;; Component interface ;;;;;;;;;;;;;;;;;;;;;;;;;;

; No child UI is planned in the near future.
(defn expandable? [mouse-evt box] false)
(defn expand-child [mouse-evt box] (throw (Exception. "No plans to implement orepl child-UI.")))
(defn contract-child [box child] (throw (Exception. "No plans to implement orepl child-UI.")))

(defn dispatch-heavy [evt s k]
  "Running the repl may affect s, depending on the command. This is agnostic to which repl is focused, i.e the k value."
  (if (and (= (:type evt) :keyPressed) (kb/shift-enter? evt))
    (reduce #(if (= (:type (get (:components %1) %2)) :orepl)
               (run-repl %1 %2) %1) s (:selected-comp-keys s)) s))

(def dispatch 
  (plurality/->simple-multi-fn
    {:mousePressed mouse-press
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