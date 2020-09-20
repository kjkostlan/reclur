; One repl: it shows the output of commands.
; It assumes the language is in clojure. A repl for another language is a big endeavor.
(ns app.orepl
  (:require [clojure.string :as string]
    [clojure.set :as set]
    [app.codebox :as codebox]
    [app.rtext :as rtext]
    [globals]
    [navigate.funcjump :as funcjump]
    [coder.logger :as logger]
    [coder.textparse :as textparse]
    [coder.plurality :as plurality]
    [coder.crosslang.langs :as langs]
    [coder.sunshine :as sunshine]
    [coder.unerror :as unerror]
    [javac.clipboard :as clipboard] 
    [javac.file :as jfile]
    [layout.colorful :as colorful]
    [layout.keyanal :as ka]
    [layout.blit :as blit]))

; The rtext has three pieces: 
; The first is the text entered. The second is a newline. The last is the repl's output.

(declare interact-fns) ; Possible dependency cycle with the new function being used by some interact fns.

;;;;;;;;;;;;;;;;;;;;;;;;; Look and feel ;;;;;;;;;;;;;;;;;;;;;;;;;;

(def colorize-top codebox/colorize)

(defn colorize-out [box s piece-ix char-ix0 char-ix1]
  (let [rgba (conj (colorful/cmdix2rgb (:num-run-times box)) 1)]
    (into [] (repeat (inc (- char-ix1 char-ix0)) rgba))))

(defn colorize [box s piece-ix char-ix0 char-ix1]
  (let [box (codebox/set-precompute box)
        cols-if-top (colorize-top box s piece-ix char-ix0 char-ix1)
        cols-if-out (colorize-out box s piece-ix char-ix0 char-ix1)
        n0 (dec (count (:pieces box)))]
    (mapv #(if (< %1 n0) (nth cols-if-top %2)
             (nth cols-if-out %2)) piece-ix (range (count cols-if-top)))))

(defn limit-length [s]
  (let [max-len 10000 tmp "...<too long to show>"]
    (if (> (count s) max-len) (str (subs s 0 (- max-len (count tmp))) tmp) s)))

;;;;;;;;;;;;;;;;;;;;;;;;; Creating a repl box ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-repl [& code]
  (assoc (merge rtext/empty-text (interact-fns))
    :type :orepl :langkwd :clojure :pieces [{:text (if (first code) (first code) "(+ 1 2)")}
                                                                  {:text "\n3"}] 
   :num-run-times 0
   :outline-color [0.2 0.2 1 1]
   :cmd-history [] :cmd-history-viewix 1e100
   :path "repl" :show-line-nums? false :colorize-fn colorize))

;;;;;;;;;;;;;;;;;;;;;;;;; Low repl running ;;;;;;;;;;;;;;;;;;;;;;;;;;


(def r-ns *ns*) ; default ns for the repls.

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
              r-ns1 (if current-ns-sym (find-ns current-ns-sym) r-ns)
              y (str (binding [*ns* r-ns1 *this-k* repl-k *world* s] 
                  ;(clojure.stacktrace/print-stack-trace (Exception. "foo"))
                  ;(clojure.stacktrace/print-stack-trace (try (eval code) (catch Exception e e)))
                  (let [out (eval code-or-e)] (reset! tmp-namespace-atom *ns*) out)))]
          (limit-length y))
        (catch Exception e
                (if (wrapped-auto-require! e) (get-repl-result s repl-k txt tmp-namespace-atom)
                  (lim-len (str "Runtime error:\n" (repl-err-msg-tweak (unerror/pr-error e))))))))))

(defn set-result [box result]
  (let [pieces (:pieces box)
        resultn (str "\n" result)
        pieces1 (cond (= (count pieces) 0) [{:text " "} {:text resultn}]
                  (= (count pieces) 1) (conj pieces {:text resultn})
                  :else (assoc pieces (dec (count pieces)) {:text resultn}))]
    (assoc box :pieces pieces1)))

(defn run-repl [s repl-k]
  "This returns the modified s, which will include any alteration of s in @globals/one-atom"
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
    (logger/gtime-reset!)
    (let [_ (swap! globals/one-atom #(assoc % :app-state s))
          txt (rtext/rendered-string (get-in s [:components repl-k]))
          new-ns-at (atom r-ns)
          result (get-repl-result s repl-k txt new-ns-at) ; DONT put the repl into a swap. Swap may call more than once.
          s1 (:app-state @globals/one-atom) ; s1 not= s for more complex repl-as-application cases.
          repl1 (get-in s1 [:components repl-k])]
      (if (map? repl1) ; Our repl is still there.
        (let [repl2 (-> repl1 (set-result result)
                      (assoc :*ns* (textparse/sym2ns @new-ns-at))
                      (update :num-run-times inc)
                      (update :cmd-history #(conj % txt)))]
          (assoc-in s1 [:components repl-k] repl2))
        s1)))

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
             (let [box1 (assoc box :cmd-history-viewix cx :pieces [{:text cmd} {:text ""}])]
               (if (> (count txt-sel) 0)
                  (let [sel0 (string/index-of cmd txt-sel)
                        sel1 (+ sel0 (count txt-sel))]
                    (assoc box1 :cursor-ix sel0 :selection-start sel0 :selection-end sel1))
                 box1))
             (recur (inc cx) (inc n))))))))

(defn ensure-two-pieces [box]
  "Everything goes into the first peice except the end."
  (let [pieces (into [] (:pieces box)) n (count pieces)
        pieces0 (if (> n 1) (butlast pieces) pieces)
        piece1 (if (> n 1) (last pieces) {:text ""})
        two-pieces [{:text (apply str (mapv :text pieces0))} piece1]]
    (assoc box :pieces two-pieces)))

(defn key-press [key-evt box]
  (ensure-two-pieces
    (cond (ka/emacs-hit? "S-ret" key-evt) box ; this was handled in the heavy dispatch.
      (ka/emacs-hit? "S-^^" key-evt) (old-cmd-search box -1)
      (ka/emacs-hit? "S-vv" key-evt) (old-cmd-search box 1)
      :else
      (codebox/key-press key-evt (codebox/set-precompute box)))))

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
                    (try (do (logger/reload-tryto-keep-loggers! ns-symbol) false)
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
            report (if maybe-e {:error (unerror/pr-error maybe-e true) :message "Compile error"}
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
      (let [cbox (codebox/from-text ctxt (:langkwd comp))
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

(defn dispatch-heavy-doubleclick [s s1 k]
  (let [file-ixs (funcjump/stack-click (get-in s [:components k]))
        rm-sel #(assoc-in (assoc-in % [:components k :selection-start] 0) [:components k :selection-end] 0)]
    (if file-ixs
      (rm-sel (apply (:goto (:layout s)) s file-ixs)) s1)))

; No child UI is planned in the near future.
(defn expandable? [mouse-evt box] false)
(defn expand-child [mouse-evt box] (throw (Exception. "No plans to implement orepl child-UI.")))
(defn contract-child [box child] (throw (Exception. "No plans to implement orepl child-UI.")))

(defn dispatch-heavy [evt s s1 k]
  "Running the repl may affect s, depending on the command. Running is agnostic to which repl is focused, i.e the k value."
  (cond (and (= (:type evt) :keyPressed) (ka/emacs-hit? "S-ret" evt))
    (reduce #(if (= (:type (get (:components %1) %2)) :orepl)
               (run-repl %1 %2) %1) s1 (:selected-comp-keys s1)) 
    (and (= (:type evt) :mousePressed)
      (= (:ClickCount evt) 2))
    (dispatch-heavy-doubleclick s s1 k)
    :else s1))

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
   :render rtext/render
   :expandable? expandable?
   :is-child? (fn [box] false)
   :expand-child expand-child :contract-child contract-child}))

;;;;;;;;;;;;;;;;;;;;;;;;; High level repl dispatching ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn dispatch-hot-repls [s evt-c selected+- single-comp-dispatch-fn]
  "Mouse move evts and everyFrame evts are ignored, as they cost CPU battery life.
   But what if you are using the repl as a video game? They should let you do anything. We store the hot repl list."
  (let [hots (:hot-repls s)
        sel (set (:selected-comp-keys s))
        target (cond (= selected+- 0) hots (> selected+- 0) (set/intersection hots sel)
                 (< selected+- 0) (set/difference hots sel))]
    (reduce (fn [s k] (single-comp-dispatch-fn evt-c s k)) 
        s target)))

(defn dispatch-warm-repls [s evt-c selected+- single-comp-dispatch-fn]
  "Warm repls have events, but not mose moved or every frame events.
   Events are added by adding a key, such as :MousePressed to the repl."
  (let [warms (set (:warm-repls s))
        sel (set (:selected-comp-keys s))
        target (cond (= selected+- 0) warms (> selected+- 0) (set/intersection warms sel)
                 (< selected+- 0) (set/difference warms sel))]
    (reduce (fn [s k] (single-comp-dispatch-fn (assoc evt-c :unselected? (not (contains? sel k))) s k)) 
        s target)))

(defn update-warmhot-repls [s]
  "Which repls have user-customized events?
   :warm-repls = have events besides :mouseMoved and :everyFrame. :hot-repls = have :mouseMoved or :everyFrame events."
  (let [warm-fns #{:mousePressed :mouseReleased :keyPressed :keyReleased :mouseDragged}
        hot-fns #{:mouseMoved :everyFrame}
        comps (:components s)
        repl-kys (filterv #(= (:type (get comps %)) :orepl) (keys comps))
        warm-kys (filterv #(> (count (set/intersection (set (keys (get comps %))) warm-fns)) 0) repl-kys)
        hot-kys (filterv #(> (count (set/intersection (set (keys (get comps %))) hot-fns)) 0) repl-kys)]
   (assoc s :warm-repls warm-kys :hot-repls hot-kys)))

;;;;;;;;;;;;;;;;;;;;;;;;; Interacting with the logger ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-lrepl [sym-qual path-within-code]
  "Start seeing your log immediately (ok with a ctrl+enter)."
  (let [path-within-code (into [] path-within-code)
        src-piece-str (str (collections/cget-in (langs/var-source sym-qual) path-within-code))
        hint-str (if (< (count src-piece-str) 32) src-piece-str
                   (str (subs src-piece-str 0 14) "..." (subs src-piece-str (- (count src-piece-str) 14))))
        code-str (str "^:active-logview\n"
                   "(get (logger/last-log-of '" sym-qual " " path-within-code ") :value \"<No logs yet>\")\n;" hint-str)]
    (assoc (new-repl) :pieces [{:text code-str} {:text ""}])))

(defn is-repl-loggy? [box]
  "Change the log-view repl immediately."
  (let [code-str (:text (first (:pieces box)))]
    (or (.contains ^String code-str "^:active-logview")
      (= code-str "(+ 1 2)"))))

(defn log-and-toggle-viewlogbox [s]
   "There is one repl we want to keep changing as we toggle logs in different places.
    If no place that can be logged is selected the unmodified s is returned."
    (if-let [fc (get (:components s) (first (:selected-comp-keys s)))]
      (if (= (:type fc) :codebox)
        (let [comps (:components s)
              cpath (codebox/cursor2cpath fc)
              added? (logger/toggle-logger! (first cpath) (rest cpath) false)]
          (if added?
            (let [replks (filterv #(= (:type (get comps %)) :orepl) (keys comps))
                  logviewk (first (filter #(is-repl-loggy? (get comps %)) replks))
                  lrepl (make-lrepl (first cpath) (rest cpath))
                  lrepl1 (if logviewk (assoc (get comps logviewk) :pieces (:pieces lrepl)) lrepl)]
              (if logviewk (assoc-in s [:components logviewk] lrepl1)
                ((:add-component (:layout s)) s lrepl1 (keyword (gensym "logviewrepl")))))
              s))
          (do (println "Must select something in a codebox to use this function") s)) 
        (do (println "Must select something (in a codebox) to use this function") s)))

;;;;;;;;;;;;;;;;;;;;;;;;; Useful user-to-type-in fns ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cmdunzip [& opts]
  "Converts concise command-line like options into a flat map.
   Keywords are keys; if no value it becomes true."
  (let [opts (if (coll? (first opts)) (into [] (apply concat opts)) opts)
        opts (into [] opts) n (count opts)]
    (loop [acc {} ix 0]
      (if (>= ix n) acc
        (let [k (nth opts ix) k-or-v (get opts (inc ix))
              v (if (or (= ix (dec n)) (keyword? k-or-v)) true k-or-v)]
          (recur (assoc acc k v)
            (if (keyword? k-or-v) (inc ix) (+ ix 2))))))))

(defn cview [code & opts]
  "Code viewing tool with optional options to adjust the view."
  (let [opts (apply cmdunzip opts)
        defaults {:pipeline? true :expand? true :console? false
                  :unqual? true}
        ns-sym (symbol (str *ns*))
        opts (merge defaults opts)
        code (cond (or (:pipeline? opts) (:sunshine? opts)) (sunshine/pipeline ns-sym code (:expand? opts))
               (or (:expand? opts) (:mexpand? opts) (:macroexpand? opts) (:macro-expand? opts)) (langs/mexpand ns-sym code) 
               :else code)
        report (if (:unqual? code) (blit/vps code) (blit/vpsu code))]
    (if (:console? opts) (println report) report)))