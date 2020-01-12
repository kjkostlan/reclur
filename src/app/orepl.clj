; One repl: it shows the output of commands.
(ns app.orepl
  (:require [clojure.string :as string]
    [clojure.set :as set]
    [app.codebox :as codebox]
    [app.rtext :as rtext]
    [coder.logger :as logger]
    [coder.cbase :as cbase]
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

(defn pr-error [e & hide-stack?]
  (let [e-clj (logger/clean-error-stack (jexc/clje e) ["app.orepl$eval" "app.orepl$get_repl_result"])
        msg0 (if (:Message e-clj) (:Message e-clj) "<no Message>")
        msg (apply str (interpose "\ncompiling:" (string/split msg0 #", compiling:")))]
    (apply str msg " (" (subs (str (type e)) 6) ")" "\n"
      (let [st (if (first hide-stack?) [] (:StackTrace e-clj))]
        (interpose "\n" (mapv #(str  "  " (:ClassName %) " " (:LineNumber %)) st))))))

(def colorize-top codebox/colorize)

(defn colorize-out [box s piece-ix char-ix0 char-ix1]
  (let [rgba (conj (colorful/cmdix2rgb (:num-run-times box)) 1)]
    (into [] (repeat (inc (- char-ix1 char-ix0)) rgba))))

(defn colorize [box s piece-ix char-ix0 char-ix1]
  (let [box (codebox/set-precompute box)
        cols-if-top (colorize-top box s piece-ix char-ix0 char-ix1)
        cols-if-out (colorize-out box s piece-ix char-ix0 char-ix1)]
    (mapv #(if (= %1 0) (nth cols-if-top %2)
             (nth cols-if-out %2)) piece-ix (range (count cols-if-top)))))

(defn limit-length [s]
  (let [max-len 10000 tmp "...<too long to show>"]
    (if (> (count s) max-len) (str (subs s 0 (- max-len (count tmp))) tmp) s)))


;;;;;;;;;;;;;;;;;;;;;;;;; Setting up a repl ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-repl []
  (assoc rtext/empty-text :type :orepl :lang :clojure :pieces [{:text "(+ 1 2)"}] :result "3"
   :num-run-times 0
   :outline-color [0.2 0.2 1 1]
   :cmd-history [] :cmd-history-viewix 1e100
   :interact-fns (interact-fns) :path "repl" :show-line-nums? false :colorize-fn colorize))

(defn filled-repl [app-state-symbol code cursor-ix0]
  "Generates a fresh repl that is set up to run command code applied to the overall state, encoded as app-state-symbol.
   Code should be a pprinted string since we care about cursor index."
  (let [b4 (str "(let [" app-state-symbol " @_state-atom\n"
                       "result\n")
        afr "] \n (reset! _state-atom result) [])"
        cursor-ix (+ (count b4) cursor-ix0)
        code (str b4 code afr)]
    (assoc (new-repl) :cursor-ix cursor-ix
      :pieces [{:text code}])))

;;;;;;;;;;;;;;;;;;;;;;;;; Running the repl ;;;;;;;;;;;;;;;;;;;;;;;;;;


(def r-ns *ns*) ; default ns for the repls.

(defonce _state-atom (atom {})) ; only used locally for the mutation-free run-standard-repl function.
(defn set-world! [s-new] "No effect outside of repl." (reset! _state-atom s-new))
(defn swap-world! [f] "No effect outside of repl." (swap! _state-atom f))
(def ^:dynamic *this-k* nil) ; so we know who we are.
(def ^:dynamic *world* nil) ; access to the (almost?) entire application's state.

(defn auto-require! [e]
  "Attempts to require a namespace given a java.lang.ClassNotFoundException e.
   Returns false and does nothing if the exception isnt that.
   Requires the needed class and returns true if the exception is that (TODO: import as well).
   Will throw an exception if the require does so."
  (let [msg (.getMessage e)
        pieces (string/split msg #"[ :,]+")
        ix0 (first (filter #(= (nth pieces %) "java.lang.ClassNotFoundException") (range (count pieces))))
        ns-str (get pieces (inc ix0))]
    (if ns-str
      (do (println "Loading:" ns-str)
        (require (symbol ns-str)) true) false)))

(defn wrapped-auto-require! [e]
  (try (auto-require! e)
    (catch Exception e false)))

(defn repl-err-msg-tweak [msg]
  (string/replace msg #"compiling:\(javac\/cpanel.clj:\d+:\d+\)" ""))

(defn get-repl-result [s repl-k txt tmp-namespace-atom]
  "The result will contain error messages if there is a problem."
  (let [_err? (atom true)
        code-or-e (try
               (let [codei (read-string txt)]
                 (reset! _err? false) codei)
               (catch Exception e e))
        lim-len (fn [x] (try (limit-length x)
                          (catch Exception e "LIMIT LEN NOT WORKING -(:")))]
    (if @_err?
      (lim-len (str "Syntax error:" (.getMessage code-or-e) " " (type code-or-e)))
      (try
        (let [current-ns-sym (get-in s [:components repl-k :*ns*])
              r-ns1 (if current-ns-sym (cbase/ns-sym2ob current-ns-sym) r-ns)
              y (str (binding [*ns* r-ns1 *this-k* repl-k *world* s] 
                  ;(clojure.stacktrace/print-stack-trace (Exception. "foo"))
                  ;(clojure.stacktrace/print-stack-trace (try (eval code) (catch Exception e e)))
                  (let [out (eval code-or-e)] (reset! tmp-namespace-atom *ns*) out)))]
          (limit-length y))
        (catch Exception e
                (if (wrapped-auto-require! e) (get-repl-result s repl-k txt tmp-namespace-atom)
                  (lim-len (str "Runtime error:\n" (repl-err-msg-tweak (pr-error e))))))))))

#_(defn get-repl-result [s repl-k txt tmp-namespace-atom]
  "The result will contain error messages if there is a problem."
  (try (let [code (read-string txt)] ; internal mutation of _state-atom possible.
         (try (let [current-ns-sym (get-in s [:components repl-k :*ns*])
                    r-ns1 (if current-ns-sym (cbase/ns-sym2ob current-ns-sym) r-ns)
                    y (str (binding [*ns* r-ns1 *this-k* repl-k *world* s] 
                             ;(clojure.stacktrace/print-stack-trace (Exception. "foo"))
                             ;(clojure.stacktrace/print-stack-trace (try (eval code) (catch Exception e e)))
                             (let [out (eval code)] (reset! tmp-namespace-atom *ns*) out)))]
                (limit-length y))
              (catch Exception e
                (if (wrapped-auto-require! e) (get-repl-result s repl-k txt tmp-namespace-atom)
                  (limit-length (str "Runtime error:\n" (repl-err-msg-tweak (pr-error e))))))))
    (catch Exception e (limit-length (str "Syntax error: " (.getMessage e) " " (type e))))))

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
    (reset! _state-atom s) ; allow user modifications.
    (let [txt (rtext/rendered-string (get-in s [:components repl-k]))
          new-ns-at (atom r-ns)
          result (get-repl-result s repl-k txt new-ns-at)
          ;_ (println "result:" result)
          state1 @_state-atom
          repl1 (-> (get-in state1 [:components repl-k]) (assoc :result result)
                  (assoc :*ns* (cbase/ns-ob2sym @new-ns-at))
                  (update :num-run-times inc)
                  (update :cmd-history #(conj % txt)))]
      (assoc-in @_state-atom [:components repl-k] repl1)))

(defn old-cmd-search [box delta]
   "Select a region of text to narrow-down old cmds."
  (let [txt-sel (rtext/selected-rendered-string box)
        hit? #(.contains ^String % txt-sel)
        n-cmd (count (:cmd-history box))
        cx (max (min (+ (:cmd-history-viewix box) delta) (dec n-cmd)) 0)]
    (loop [cx cx n 0]
      (if (>= n n-cmd) (do (println (if (= n 0) "No prior commands in repl." "No box cmd history match to selection found.")) box)
        (let [cx (if (< cx 0) (dec n-cmd) cx) cx (if (>= cx n-cmd) 0 cx) ;wrap
              cmd (nth (:cmd-history box) cx)]
          (if (.contains ^String cmd txt-sel)
             (let [box1 (assoc box :cmd-history-viewix cx :result "" :pieces [{:text cmd}])]
               (if (> (count txt-sel) 0)
                  (let [sel0 (string/index-of cmd txt-sel)
                        sel1 (+ sel0 (count txt-sel))]
                    (assoc box1 :cursor-ix sel0 :selection-start sel0 :selection-end sel1))
                 box1))
             (recur (inc cx) (inc n))))))))

(defn key-press [key-evt box]
  (cond (kb/emacs-hit? "S-ret" key-evt) box ; this was handled in the heavy dispatch.
    (kb/emacs-hit? "S-^^" key-evt) (old-cmd-search box -1)
    (kb/emacs-hit? "S-vv" key-evt) (old-cmd-search box 1)
    :else
    (rtext/key-press key-evt box)))

;;;;;;;;;;;;;;;;;;;;;;;;; Namespace reloading et al ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn _resolve [sym]
  (try (resolve sym) (catch Exception e)))

(defonce var-atom
   ;Fully-qualified symbols -> var for the old namespace vars. Thus we don't lose the old references.
   (atom {}))

(defn _clear-var! [ns-sym var-sym var-ob]
  "sets vars to throw or be an error message."
  (alter-var-root var-ob
   (fn [_] (fn [& args] (throw (Exception. (str "Variable:" ns-sym "/" var-sym " has been removed."))))))) 

(defn _mark-vars! [ns-symbol]
  "Makes variables with an ::old flag."
  (let [nms (find-ns ns-symbol)
        alter (fn [vr] (if (instance? clojure.lang.IObj vr)
                         (vary-meta vr #(assoc % ::old true)) vr))]
    (if nms
      (let [interns (ns-interns nms)]
        (mapv (fn [v] (alter-var-root v alter))
          (vals interns))))))

(defn _get-marked-vars [ns-symbol]
  "Returns a map from symbols to var obs with the marked symbol."
  (let [nms (find-ns ns-symbol)]
    (if nms
      (let [interns (ns-interns nms)
            var?obs (mapv #(if (::old (meta (var-get %))) % false) (vals interns))
            intern?s (zipmap (keys interns) var?obs)
            kys (filterv #(get intern?s %) (keys interns))]
        (zipmap kys (mapv #(get interns %) kys))) {})))

(defn _update-simple! [ns-symbol removing?]
  (let [tmp-sym (gensym 'tmp) ns-tmp (create-ns tmp-sym)]
    (_mark-vars! ns-symbol)
    (let [maybe-e (if removing? false ; the actual reloading part.
                    (try (do (logger/reload-but-keep-loggers! ns-symbol) false)
                            (catch Exception e e)))
          rm-vars (_get-marked-vars ns-symbol)]
      (if (not maybe-e) (mapv #(_clear-var! ns-symbol %1 %2) (keys rm-vars) (vals rm-vars)))
      maybe-e)))

(defn reload-file! [cljfile]
  "Reloads a given clj file, removing the ns if the file no longer exists."
  (let [ns-symbol (symbol (jfile/file2dotpath cljfile))
        file-exists? (jfile/exists? cljfile)]
    (cond 
      (= ns-symbol 'ect) {:error false :message "The project.clj file has no namespace."} ; special for saving changes to the project.clj
      (and (not (find-ns ns-symbol)) (not file-exists?)) {:error false :message "Non-existant namespace with non-existant file, no reloading needed."}
      file-exists?
      (let [maybe-e (_update-simple! ns-symbol false) ;(_update-core! ns-symbol false)
            report (if maybe-e {:error (pr-error maybe-e true) :message "Compile error"}
                      {:error false :message (str cljfile " saved and updated without error.")})] report)
      :else (do (_update-simple! ns-symbol true) {:error false :message "File no longer exists, deleted ns."}))))

(defn save-and-update!! [cljfile text]
  "Saves a clj file and attempts to reload the namespace, returning the error info if there is a compile error."
  (jfile/save!! cljfile text)
  (reload-file! cljfile))

(defn delete-and-update!! [cljfile]
  (jfile/delete!! cljfile) (reload-file! cljfile) {:error false :message (str "Deleted" cljfile)})

(defn mouse-press [m-evt comp]
  (let [nc (:ClickCount m-evt) ctxt (:text (first (:pieces comp)))]
    (if (and (or (= nc 2) (= nc 4)) (<= (:cursor-ix comp) (count ctxt)))
      (let [cbox (codebox/from-text ctxt (:lang comp))
            ckys [:selection-start :selection-end :cursor-ix]
            cbox (merge cbox (select-keys comp ckys))
            cbox1 (codebox/select-twofour-click cbox (= nc 4))]
        (merge comp (select-keys cbox1 ckys)))
      (rtext/mouse-press m-evt comp))))

;;;;;;;;;;;;;;;;;;;;;;;;; Event switchyard with defaults ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn allow-override-fns [fn-map bubble?]
  "Lets the user override what happens. With bubble? we always apply the true function;
   minimize the risk of getting locked out."
  (zipmap (keys fn-map)
    (mapv (fn [k f]
            (fn [evt box]
              (let [the-f (get box k)
                    box1 (if (and bubble? (not (:unselected? evt))) (f evt box) box)]
                (if-let [the-f (get box k)] (the-f evt box1) box1))))
      (keys fn-map) (vals fn-map))))

(defn allow-append [f k]
  "For functions that return a vector (i.e. rendering), lets the user append onto it"
  (fn [box & args]
    (into [] (concat (apply f box args) (if-let [g (get box k)] (apply g box args) [])))))

;;;;;;;;;;;;;;;;;;;;;;;;; Component interface ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn render+ [box & show-cursor?]
  (let [box-with-out (assoc box :pieces [(first (:pieces box)) {:text (str " \n" (:result box))}])]
    (apply rtext/render box-with-out show-cursor?)))

; No child UI is planned in the near future.
(defn expandable? [mouse-evt box] false)
(defn expand-child [mouse-evt box] (throw (Exception. "No plans to implement orepl child-UI.")))
(defn contract-child [box child] (throw (Exception. "No plans to implement orepl child-UI.")))

(defn dispatch-heavy [evt s k]
  "Running the repl may affect s, depending on the command. This is agnostic to which repl is focused, i.e the k value."
  (if (and (= (:type evt) :keyPressed) (kb/emacs-hit? "S-ret" evt))
    (reduce #(if (= (:type (get (:components %1) %2)) :orepl)
               (run-repl %1 %2) %1) s (:selected-comp-keys s)) s))

(def dispatch 
  (plurality/->simple-multi-fn
    (allow-override-fns
      {:mousePressed mouse-press
       :mouseReleased (fn [_ box] box)
       :mouseDragged rtext/mouse-drag
       :mouseWheelMoved rtext/mouse-wheel
       :keyPressed key-press
       :keyReleased rtext/key-release
       :mouseMoved (fn [_ box] box) ; These are normally ignored for idle CPU, orepls are an exception.
       :everyFrame (fn [_ box] box)} true)
    (fn [e-clj comp] comp)
    (fn [e-clj comp] (:type e-clj))))

(defmacro updaty-fns [code] 
  (let [a1 (gensym 'args)] 
    (zipmap (keys code) (mapv #(list `fn ['& a1] (list `apply % a1)) (vals code)))))
(defn interact-fns []
  (updaty-fns
  {:dispatch dispatch 
   :dispatch-heavy dispatch-heavy
   :render (allow-append render+ :render)
   :expandable? expandable?
   :is-child? (fn [box] false)
   :expand-child expand-child :contract-child contract-child}))