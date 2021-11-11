; One repl: it shows the output of commands.
; It assumes the language is in clojure. A repl for another language is a big endeavor.
(ns app.orepl
  (:require [clojure.string :as string]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [app.codebox :as codebox]
    [app.rtext :as rtext]
    [globals] [t]
    [navigate.funcjump :as funcjump]
    [coder.logger :as logger]
    [coder.profiler :as profiler]
    [coder.textparse :as textparse]
    [coder.crosslang.langs :as langs]
    [coder.crosslang.langparsers.clojure :as parse-clojure]
    [coder.sunshine :as sunshine]
    [coder.unerror :as unerror]
    [javac.clipboard :as clipboard]
    [javac.file :as jfile]
    [layout.colorful :as colorful]
    [layout.keyanal :as ka]
    [layout.blit :as blit]
    [layout.browseedn :as browseedn]))

(declare interact-fns) ; Possible dependency cycle with the new function being used by some interact fns.

;;;;;;;;;;;;;;;;;;;;;;;;; Look and feel ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn colorize-output [txt num-run-times]
  (let [txt (if txt txt "")
        [tokens token-tys] (langs/tokenize txt :clojure) ; the repl always uses :clojure to represent data.
        char-token (apply c/vcat
                     (mapv #(repeat (count %1) %2) tokens token-tys))
        inter-levels (langs/interstitial-depth txt :clojure)
        levels-inclusive (mapv max (butlast inter-levels) (rest inter-levels))
        mapk-indics (parse-clojure/map-key-indicators txt char-token inter-levels)]
    (mapv #(conj (colorful/repl-out-multicolor %1 %2 %3 num-run-times) 1)
      levels-inclusive char-token mapk-indics)))

(defn colorize [box s piece-ix char-ix0 char-ix1]
  (let [txt0 (:text (first (:pieces box))) txt1 (:text (second (:pieces box)))
        cols-piece0 (codebox/colorize box s piece-ix 0 (count txt0))
        cols-piece1 (colorize-output txt1 (:num-run-times box))]
    (subvec (c/vcat cols-piece0 cols-piece1) char-ix0 char-ix1)))

(defn limit-length [s]
  (let [max-len 10000 tmp "...<too long to show>"]
    (if (> (count s) max-len) (str (subs s 0 (- max-len (count tmp))) tmp) s)))

(def ^:dynamic *keep-loggers-on-reload* false) ; Loggers tend to have a short lifecycle.

;;;;;;;;;;;;;;;;;;;;;;;;; Creating a repl box ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn new-repl [& code]
  (assoc (merge rtext/empty-text (interact-fns))
   :type :orepl :langkwd :clojure :pieces [{:text (if (first code) (first code) "(+ 1 2)")} {:text "\n3"}]
   :num-run-times 0
   :outline-color [0.2 0.2 1 1]
   :cmd-history [] :cmd-history-viewix 1e100
   :data-browse-path []
   :path "repl" :show-line-nums? false :colorize-fn colorize))

;;;;;;;;;;;;;;;;;;;;;;;;; Representing repl results ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn repl-err-msg-tweak [msg]
  (string/replace msg #"compiling:\(javac\/cpanel.clj:\d+:\d+\)" ""))

(defn ensure-two-pieces [box]
  "Everything goes into the first peice except the end."
  (let [pieces (into [] (:pieces box)) n (count pieces)
        pieces1 (cond (< n 2) (c/vcat [{:text " "}] pieces)
                  (= n 2) pieces
                  :else (conj [{:text (apply str (mapv :text (butlast pieces)))}] (last pieces)))]
   (assoc box :pieces pieces1)))

(defn constrain-selection-piece0 [box]
  "The first piece is the only editable part."
  (let [n0 (count (get-in box [:pieces 0 :text]))]
    (-> (update box :selection-start #(min % n0)) (update :selection-end #(min % n0)))))

(defn get-summary [box]
  "Returns [summary-object, summary string], depends on the :view-path of the box.
   Only use this for repl successes."
  (let [view-path (get box :view-path [])
        x-piece (t/cget-in (:value (:result box)) view-path)
        h-pixels (max 20 (get-in box [:size 1] 210))
        xp-summary (binding [browseedn/*target-fillchars* (int (* h-pixels 2))
                             browseedn/*min-child-fraction* (/ 20.0 h-pixels)]
                     (browseedn/summarize x-piece))
        txt (str "\n" (if (= view-path []) "" (str ";" view-path "\n")) ; Prepends must not change the string's value.
              (try (limit-length (binding [*print-namespace-maps* false] (blit/vps xp-summary)))
                (catch Exception e (str "LIMIT LEN NOT WORKING BUG IN OUR CODE " e))))]
    [xp-summary txt]))

(defn show-result [box]
  "Depending on the nature of result, the repl result may need to be summarized."
  (let [pieces (:pieces box)
        result (:result box)
        no-err? (= (:type result) :success)
        [sumy sumy-txt] (if no-err? (get-summary box) [[] ""])
        result-str (if no-err? sumy-txt (:value result))
        pieces1 (cond (= (count pieces) 0) [{:text " "} {:text result-str}]
                  (= (count pieces) 1) (conj pieces {:text result-str})
                  :else (assoc pieces (dec (count pieces)) {:text result-str}))
        result1 (assoc result :summary sumy :summary-txt sumy-txt)
        box (assoc box :result result1)]
    (codebox/update-precompute (assoc box :pieces pieces1))))

;;;;;;;;;;;;;;;;;;;;;;;;; Interacting with the logger ;;;;;;;;;;;;;;;;;;;;;;;;;;

(def repl-log-tag ::repl-logger?)

(defn sym+path-convert [sym+path]
  "The sym+path may point to an unloggable path such as a fn argument, this tries to fix it."
  (let [sym-qual (first sym+path)
        code (if (symbol? sym-qual) (try (langs/var-source sym-qual) (catch Exception e false)))
        path2logpath (profiler/unexpanded-pathmap code)]
    (c/vcat [sym-qual] (get path2logpath (rest sym+path)))))

(defn codebox2sym+path [box & throw-err?]
  "Allows alternative log paths."
  (let [f #(sym+path-convert (codebox/cursor2cpath box))]
    (if (first throw-err?) (f) (try (f) (catch Exception e false)))))

(defn sym+path-at-cursor [s]
  "The cursor's position in the current codebox, if it exists."
    (if-let [sel-box (get (:components s) (first (:selected-comp-keys s)))]
      (if (= (:type sel-box) :codebox)
        (codebox2sym+path sel-box))))

(defn sym+path-at-cursor-lastcodebox [s]
  "The last cursor into a codebox."
    (if-let [box (get s ::last-codebox-used)]
      (codebox2sym+path box true)))

(defn make-lrepl [sym-qual path-within-code]
  "Start seeing your log immediately (ok with a ctrl+enter)."
  (let [path-within-code (into [] path-within-code)
        src-piece-str (str (t/cget-in (langs/var-source sym-qual) path-within-code))
        hint-str (if (< (count src-piece-str) 32) src-piece-str
                   (str (subs src-piece-str 0 14) "..." (subs src-piece-str (- (count src-piece-str) 14))))
        code-str (str "^:active-logview\n"
                   "(get (logger/last-log-of '" sym-qual " " path-within-code ") :value \"<No logs yet>\")\n;" hint-str)]
    (assoc (new-repl) :pieces [{:text code-str} {:text ""}])))

(defn get-lrepl-sym+path [box]
  "Is the box an active, logging repl? What symbol and path within code is it logging?"
  (if (= (:type box) :orepl)
    (let [piece0 (try (read-string (:text (first (:pieces box))))
                   (catch Exception e false))]
      (if-let [ph (t/find-value-in piece0 'logger/last-log-of)]
        (let [ph1 (into [] (butlast ph)) ensure-c? #(if (sequential? %) % [%])]
          (c/vcat [(t/cget-in piece0 (conj ph1 1 1))]
             (ensure-c? (t/cget-in piece0 (conj ph1 2)))))))))

(defn repl-log-sym+paths [s] "Does not include the at-cursor box."
  (set (filter identity (map get-lrepl-sym+path (vals (:components s))))))

(defn make-cursor-lrepl [replk]
  "Like l-repl but it logs what is at the cursor."
  (let [code '(fn [s] (log-and-see-what-is-at-cursor! s replk false))
        code (walk/postwalk #(if (= % 'replk) replk %) code)
        code (with-meta code {:global true})
        code-string (binding [*print-meta* true] (layout.blit/vps code))
        box (codebox/update-precompute (new-repl code-string))] box))

(defn add-cursor-lrepl [s]
  "Adds a repl that will log the cursor."
  (let [replk (keyword (gensym "clicklog"))
        box (make-cursor-lrepl replk)]
    ((:add-component (:layout s)) s box replk)))

(defn cursor-lrepl? [box]
  (let [lsym 'log-and-see-what-is-at-cursor!]
    (if (= (:type box) :orepl)
      (let [sp0 (str (get-in box [:pieces 0 :text]))]
        (and (string/includes? sp0 (str lsym))
          (string/includes? sp0 (str :global)))))))

(defn any-cursor-lrepl? [s]
  "Anything is a cursor-lrepl."
  (first (filter cursor-lrepl? (vals (:components s)))))

(defn add-marked-logger! [sym-qual logpath toggle?]
  "Adds a logger then adds repl-log-tag to it. This allows us to remove only the repl-added loggers.
   Returns whether or not we added the logger."
  (let [added? (if toggle?
                 (logger/toggle-logger! sym-qual logpath false ["" :quiet])
                 (do (logger/add-logger! sym-qual logpath false "" :quiet) true))
        path-to-new-logger [:loggers sym-qual logpath]]
    (if added?
      (do (if (not (get-in @globals/log-atom path-to-new-logger))
            (throw (Exception. "No logger seemed to be added, or at least we can't find it.")))
        (swap! globals/log-atom
                      #(assoc-in % (conj path-to-new-logger repl-log-tag) repl-log-tag))))
    added?))

(defn log-and-toggle-viewlogbox! [s]
   "This function toggles the logger and the cooresponding repl."
    (let [sym+path (sym+path-at-cursor s)
          _ (if (not sym+path) (println "Cannot find valid logpath at current cursor location (or it's not a codebox)"))
          logpath (if sym+path (into [] (rest sym+path)))]
      (if (empty? logpath) s
        (let [comps (:components s)
              sym-qual (first sym+path)
              added? (add-marked-logger! sym-qual logpath true)
              sym+path1 (c/vcat [sym-qual] logpath)
              logviewk (first (filter #(= (get-lrepl-sym+path (get comps %)) sym+path1) (keys comps)))]
          (if added?
            (let [lrepl (make-lrepl sym-qual logpath)
                  lrepl1 (if logviewk (assoc (get comps logviewk) :pieces (:pieces lrepl)) lrepl)
                  sym+path1 (c/vcat [sym-qual] logpath)
                  path-to-new-logger [:loggers sym-qual logpath]
                  _ (if (not (get-in @globals/log-atom path-to-new-logger))
                      (throw (Exception. "No logger seemed to be added, or at least we can't find it.")))
                  _ (swap! globals/log-atom
                      #(assoc-in % (conj path-to-new-logger repl-log-tag) repl-log-tag))]
              (if logviewk (assoc-in s [:components logviewk] lrepl1)
                ((:add-component (:layout s)) s lrepl1 (keyword (gensym "logviewrepl")))))
            (if logviewk (update s :components #(dissoc % logviewk)) s))))))

(defn remove-closed-repl-logpaths! [s]
  "Don't let zombie loggers slow everything down. Still does not remove the logs themselves, however."
  (let [sym+paths (repl-log-sym+paths s)
        currently-logged-sym2paths (logger/get-loggers)
        last-loggable-sym+path (if (any-cursor-lrepl? s)
                                 (if-let [x (::precompute-lastcodebox-sym+path s)]
                                   x (sym+path-at-cursor-lastcodebox s)))]
    (mapv (fn [sym-qual]
            (let [lpack (get currently-logged-sym2paths sym-qual)
                  lphs (keys lpack)
                  lphs-repl (filterv #(get-in lpack [% repl-log-tag]) lphs)
                  lphs-keep (filterv #(get sym+paths (c/vcat [sym-qual] %)) lphs-repl)
                  lphs-keep (if (= (first last-loggable-sym+path) sym-qual)
                              (conj lphs-keep (into [] (rest last-loggable-sym+path)))
                              lphs-keep)
                  lphs-remove (set/difference (set lphs-repl) (set lphs-keep))]
              ;(if (> (count lphs-remove) 0) (println "Removing:" lphs-remove " for " sym-qual))
              (mapv #(logger/toggle-logger! sym-qual % false) lphs-remove)))
      (keys currently-logged-sym2paths))) s)

(defn log-and-see-what-is-at-cursor! [s repl-k & all-logs]
  "Sets the log to the cursor, if any, and reports the output (most recent log is the default).
   Uses ::last-codebox-used."
  (let [box (get s ::last-codebox-used)

        sym+ph (if box (if-let [ph (::precompute-lastcodebox-sym+path s)]
                         ph (codebox2sym+path box true)))
        logpath (into [] (rest sym+ph))
        _ (if (not (empty? logpath)) (add-marked-logger! (first sym+ph) logpath false))
        log-val (if (not (empty? logpath))
                  (let [sym-qual (first sym+ph)
                        all? (first all-logs)]
                    (if all? (logger/all-logs-of sym-qual logpath)
                      (:value (logger/last-log-of sym-qual logpath)))))
        log-val (if log-val log-val "<No logs in cursor-clicked places yet>")
        result {:type :success :value log-val :full-state-run? false}]
    (remove-closed-repl-logpaths! s)
    (update-in s [:components repl-k] #(show-result (assoc (update % :num-run-times inc) :result result)))))

(defn orepl-based-updates! [s0 s]
  "Updates to s needed to keep more global functions working."
  (if (= s0 s) s
    (let [boxes0 (:components s0) boxes (:components s)
          selk0 (first (:selected-comp-keys s0)) selk (first (:selected-comp-keys s))
          clicked-on-codebox (if (or (not= selk0 selk) (not= (get boxes0 selk0) (get boxes selk)))
                               (get boxes selk))
          clicked-on-codebox (if (= (:type clicked-on-codebox) :codebox) clicked-on-codebox)
          _ (if (and (not= boxes0 boxes)
                  (or (and (any-cursor-lrepl? s0) (not (any-cursor-lrepl? s)))
                    (not= (repl-log-sym+paths s0) (repl-log-sym+paths s))))
              (remove-closed-repl-logpaths! s))]
      (if clicked-on-codebox
        (assoc s ::last-codebox-used clicked-on-codebox) s))))

;;;;;;;;;;;;;;;;;;;;;;;;; Repl running ;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn get-repl-result [s repl-k txt tmp-namespace-atom]
  "Result's :type can be :syntax-err, :runtime-err, :success.
   :value has not yet been made into a string and could be infinitely big."
  (let [_syntax-err? (atom true)
        code-or-e (try (let [codei (read-string txt)]
                        (reset! _syntax-err? false) codei)
                        (catch Exception e e))
        full-state-run? (:global (meta code-or-e))]
    (if @_syntax-err?
      {:type :syntax-err :value (str "\nSyntax error:" (.getMessage code-or-e) " " (type code-or-e))}
      (try
        (let [current-ns-sym (get-in s [:components repl-k :*ns*])
              r-ns1 (if current-ns-sym (find-ns current-ns-sym) r-ns)
              y (binding [*ns* r-ns1 *this-k* repl-k *world* s]
                  (let [out (if full-state-run? ((eval code-or-e) s) (eval code-or-e))] (reset! tmp-namespace-atom *ns*) out))]
          {:type :success :value y :full-state-run? full-state-run?})
        (catch Exception e
                (if (wrapped-auto-require! e) (get-repl-result s repl-k txt tmp-namespace-atom)
                  {:type :runtime-err :value (str "\nRuntime error:\n" (repl-err-msg-tweak (unerror/pr-error e)))}))))))

(defn run-repl-core [s repl-k]
  "This returns the modified app state s. Most functions don't work with s and we just add in the report.
   However, ^:global on a function will cause us to run it on s as a whole and we skip the report step."
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
    (let [txt (rtext/rendered-string (get-in s [:components repl-k]))
          new-ns-at (atom r-ns)
          result (get-repl-result s repl-k txt new-ns-at)]
      (if (string/includes? txt "*print-meta*")
        (print "*print-meta* does not work from the repl, use mt/m-unpack instead."))
      (if (:full-state-run? result) (:value result) ;The repl code was a fn that ran on s and returned a modified s.
        (let [repl (get-in s [:components repl-k])
              repl1 (-> (assoc repl :result result :view-path []) (show-result)
                      (assoc :*ns* (textparse/sym2ns @new-ns-at))
                      (update :num-run-times inc)
                      (update :cmd-history #(conj % txt)))]
          (assoc-in s [:components repl-k] (codebox/update-precompute repl1))))))

(defn run-repl [s repl-k]
  "Calls run-repl-core on repl-k, but first also runs all cursor-lrepls if repl-k isn't a cursor repl."
  (logger/gtime-reset!)
  (let [boxes (:components s)
        cursor-lrepl-kys (set (filterv #(cursor-lrepl? (get boxes %)) (keys boxes)))
        is-cursor-lrepl? (get cursor-lrepl-kys repl-k)
        has-cursor-repl? (any-cursor-lrepl? s)
        cursor-sym+path (if has-cursor-repl? (sym+path-at-cursor-lastcodebox s))
        _ (if cursor-sym+path (add-marked-logger! (first cursor-sym+path) (rest cursor-sym+path) false))
        s (assoc s ::precompute-lastcodebox-sym+path cursor-sym+path)
        s1 (if is-cursor-lrepl? (run-repl-core s repl-k)
             (reduce run-repl-core s (concat cursor-lrepl-kys [repl-k])))]
    (dissoc s1 ::precompute-lastcodebox-sym+path)))

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
                    (try (do (if *keep-loggers-on-reload*
                               (logger/reload-try-to-keep-loggers! ns-symbol)
                               (logger/reload-lose-loggers! ns-symbol)) false)
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

;;;;;;;;;;;;;;;;;;;;;;;;; Component interface ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mouse-press [m-evt box]
  (let [box1 (codebox/mouse-press m-evt box) ; vanilla.
        nclick (:ClickCount m-evt) code-txt (:text (first (:pieces box)))
        cursor-on-piece1 (- (:cursor-ix box1) (count code-txt))
        run-result (:result box1)]
    (cond
      (and (= nclick 1) (= (get run-result :type) :success) (>= cursor-on-piece1 0) (= (:Button m-evt) 1)) ;Navigate inwards.
      (let [sumy (:summary run-result) sumy-txt (:summary-txt run-result)
            ph-on-x (browseedn/path-at-cursor sumy sumy-txt cursor-on-piece1)]
        (if (and ph-on-x (not (empty? ph-on-x)))
          (let [new-viewpath (conj (get box :view-path []) (first ph-on-x))
                box2 (assoc box1 :view-path new-viewpath)]
            (show-result box2))
           box1))
      (and (= nclick 1) (= (get run-result :type) :success) (>= cursor-on-piece1 0) (= (:Button m-evt) 3)) ;Navigate outwards.
      (let [old-viewpath (get box :view-path [])
            new-viewpath (if (empty? old-viewpath) [] (into [] (butlast old-viewpath)))
            box2 (assoc box1 :view-path new-viewpath)]
        (show-result box2))
      :else box1)))

(defn key-press [key-evt box]
  (ensure-two-pieces
    (cond (ka/emacs-hit? "S-ret" key-evt) box ; this was handled in the heavy dispatch.
      (ka/emacs-hit? "S-^^" key-evt) (old-cmd-search box -1)
      (ka/emacs-hit? "S-vv" key-evt) (old-cmd-search box 1)
      :else
      (let [tyk (str (ka/typed-key key-evt))
            need-constraint? (or (not (ka/c? key-evt))
                               (= tyk "v") (= tyk "x"))]
        (codebox/key-press key-evt (if need-constraint? (constrain-selection-piece0 box) box))))))

(defn on-resize [evt box]
  (show-result box))

(defn dispatch-heavy-doubleclick [s s1 k]
  (let [file-ixs (try (funcjump/stack-click (get-in s [:components k]))
                   (catch Exception e (do (println "No stack-click detected") false)))
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
  {:mousePressed (fn [evt box] (mouse-press evt box))
   :mouseReleased (fn [_ box] box)
   :mouseDragged rtext/mouse-drag
   :mouseWheelMoved rtext/mouse-wheel
   :keyPressed key-press
   :resized (fn [evt box] (on-resize evt box))
   :keyReleased rtext/key-release})

(defn interact-fns []
  {:dispatch dispatch
   :dispatch-heavy dispatch-heavy
   :render rtext/render
   :expandable? expandable?
   :is-child? (fn [box] false)
   :expand-child expand-child :contract-child contract-child})

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
        defaults {:pipeline? true :expand? true :console? false :unqual? true}
        ns-sym (symbol (str *ns*))
        opts (merge defaults opts)
        code (if (or (:expand? opts) (:mexpand? opts) (:macroexpand? opts) (:macro-expand? opts)) (langs/mexpand ns-sym code) code)
        code (if (or (:pipeline? opts) (:sunshine? opts)) (sunshine/deshadow-qual ns-sym code) code)
        report (if (:unqual? opts) (blit/vps code) (blit/vpsu code))]
    (if (:console? opts) (println report) report)))

(defn lines [x & k]
  "Long vector? Make one (or more) element each line."
  (let [k (if (first k) (first k) 1)
        s (map #(if (= (mod (inc %) k) 0) "\n" " ") (range))]
    (apply str (interleave x s))))

;;;;;;;;;;;;;;;;;;;;;;;;; Most interactive boxes are repls ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn add-repl [s boxk code cursor-place-f run-it-now?]
  "Adds a repl with very-pretty-pritned code and selects it. cursor-place-f can be a number."
  (let [code-string (binding [*print-meta* true] (layout.blit/vps code))
        cursor-ix0 (if (number? cursor-place-f) cursor-place-f (cursor-place-f code-string))
        new-box (assoc (new-repl code-string) :cursor-ix cursor-ix0)
        s1 (assoc s :selected-comp-keys #{boxk} :typing-mode? true)
        s2 ((:add-component (:layout s1)) s1 new-box boxk)]
      (if run-it-now? (run-repl s2 boxk) s2)))
