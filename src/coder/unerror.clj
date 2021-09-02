; TODO: this relies heavily on clojure-based syntax. It should be refactored to use langs.
; Helps you remove bugs by catching *compile* time errors.

(ns coder.unerror
  (:require
    [clojure.set :as set]
    [clojure.string :as string]
    [layout.blit :as blit]
    [c] [t]
    [coder.crosslang.langs :as langs]))

;;;;;;;;;;;; Simple helper functions that are fairly generic ;;;;;;;;;;;;

(defn plog [x] (do (println x) x)) ; TMP

(defn sym-to-class [sym]
  (let [maybe-cl (try (eval (symbol sym)) (catch Exception e))]
    (if (instance? java.lang.Class maybe-cl) maybe-cl)))

(defn cool-head [code n]
  (let [codev (into [] code)]
    (apply list (concat (repeat n "Filler") (subvec codev n)))))

(def specials #{'if 'fn 'fn* 'let 'let* 'try 'catch 'throw 'monitor-enter 'monitor-exit 'loop 'recur 'def 'binding '. 'new 'var})

(defn _abridged-error [err path where]
  {:msg (str (:msg err)
          (let [txt (str (first (:path err)))]
            (if (> (count txt) 64)
              (str "\n  This error occured " where (subs txt 0 60) "...")
              (str "\n  This error occured " where txt)))) :path path})

;;;;;;;;;;;; Visually nice errors to make it less annoying ;;;;;;;;;;;;

(defn pr-error [e & hide-stack?]
  (let [e-clj (langs/convert-exception e ["app.orepl/eval" "app.orepl/get_repl_result"] :clojure)
        ; Complicated get message because of "the syntax error that wasn't".
        syntax-err-kys #{"EOF while reading" "arguments to if"
                         "Unmatched delimiter"
                         "Mismatched argument count"}
        emsg (:Message e-clj)
        cmsg (try (str (.getCause e)) (catch Exception eception false))
        ;_ (println "EMSG:" emsg "CMSG:" cmsg)
        msg0 (cond (and (not emsg) (not cmsg)) "<no message>"
               (not emsg) cmsg (not cmsg) emsg
               (and (string/includes? emsg "Syntax error")
                 (not (first (filter #(string/includes? cmsg %) syntax-err-kys))))
               ;(string/includes? cmsg "Unable to resolve symbol") ; more niche cases may be added.
               (str (-> emsg (string/replace #"Syntax error compiling [a-zA-z ]*at " "")
                      (string/replace ")." ")")
                      (string/trim)) " " cmsg)
               (string/includes? emsg "Syntax error")
               (str emsg " " cmsg)
               :else emsg)
        msg0 (-> msg0 (string/replace "java.lang." "")
               (string/trim))
        msg (apply str (interpose "\ncompiling:" (string/split msg0 #", compiling:")))
        msg1 (apply str msg " (" (subs (str (type e)) 6) ")" "\n"
               (let [st (if (first hide-stack?) [] (:StackTrace e-clj))]
                 (interpose "\n" st)))]
    (-> msg1
      (string/replace "(clojure.lang.Compiler$CompilerException)" "")
      (string/replace "RuntimeException: " "")
      (string/replace "reading source at" "at"))))

;;;;;;;;;;;; Macro expansion-based checking ;;;;;;;;;;;;;

(defn var2sym [v]
  "Fully qualified."
  (let [st (str v)]
    (symbol (subs st 2))))

(defn _msg-merrxpand [code ^java.lang.Exception e]
  (let [macr (first code)
        macr (if-let [macrfull (resolve macr)] (var2sym macrfull) macr)
        msg-full (.getMessage e)
        head "[On macroexpand] "]
    (if (string/includes? msg-full "did not conform to spec") ; long-winded message.
      (str head macr " used with bad spec") (str head msg-full))))

(defn _on-macroexpand-err-catch [ns-sym code path]
  "What if the macro gets cranky? This code will show you the minimal part that is bad."
  (if (coll? code)
    (try (do (pr-str (langs/mexpand (symbol (str ns-sym)) code)) nil)
      (catch Exception e
        (if-let [deeper-error
                  (cond
                    (map? code) (c/juice-1 #(_on-macroexpand-err-catch ns-sym (get code %) (conj path %)) (keys code))
                    (set? code) (c/juice-1 #(_on-macroexpand-err-catch ns-sym % (conj path %)) (keys code))
                    :else (let [codev (into [] code)]
                            (c/juice-1 #(_on-macroexpand-err-catch ns-sym (nth codev %) (conj path %)) (range (count codev)))))]
          deeper-error {:msg (_msg-merrxpand code e) :path path :b4expand? true})))))


;;;;;;;;;;;; Spec checks for specific clojure special forms ;;;;;;;;;;;;

(defn _symbol-not-found-check [sym path locals state]
  ; Non-java classes only!
  (if (or (and (:listy? state) (= (last path) 0) (get specials sym)) ; (specials can't be rebound, exceptions to this rule?)
          (get locals sym) (resolve sym)
          #_(and (= sym 'catch) (:try? state)))
       nil
  {:path path :msg (str "unable to resolve symbol: " sym
                     (cond (= (last (str sym)) \.) " (java constructors must be first)"
                       (= (first (str sym)) \.) " (possible java call wrong syntax)" :else ""))}))

(defn _symlist-check [body-vec path-to-vec]
  (if (vector? body-vec)
    (if-let [bad-ix (first (filter #(not (symbol? (nth body-vec %))) (range (count body-vec))))]
     {:msg (str "the " bad-ix "'th arg isn't a symbol") :path (conj path-to-vec bad-ix)})
    {:msg "[args] is not a vector" :path path-to-vec}))

(defn _binding-spec-check [code-vec path-to-vec]
  (if (vector? code-vec)
    (let [n (count code-vec)]
      (loop [ix 0]
        (cond (= ix (dec n)) {:msg (str "The " (/ ix 2) "-th symbol in [body] has no value assigned")
                              :path (conj path-to-vec ix)}
          (= ix n) nil
          (not (symbol? (nth code-vec ix)))
          {:msg (str "The " (/ ix 2) "-th symbol in [body] isnt a symbol")
           :path (conj path-to-vec ix)}
          :else (recur (+ ix 2)))))
    {:msg "[body] is not a vector." :path path-to-vec}))


(defn _fn-check-default-format [code-list state]
  (let [packets (into [] (rest code-list))
        bad-packet0 (fn [packet-ix]
                      (let [p (nth packets packet-ix)]
                        (if (and (coll? p) (vector? (first p))
                              (not (vector? p)) (not (set? p)) (not (map? p))) false
                          {:msg "Bad function argument form."
                           :path (conj (:path state) (inc packet-ix))})))
        bad-packet1 (fn [packet-ix] (_symlist-check (first (nth packets packet-ix))
                                      (conj (:path state) (inc packet-ix) 0)))]
    (if-let [bad-ix0 (first (filter bad-packet0 (range (count packets))))]
      (bad-packet0 bad-ix0)
      (if-let [bad-ix1 (first (filter bad-packet1 (range (count packets))))]
        (bad-packet1 bad-ix1) false))))
(defn _fn-check [code-list state]
  (if (vector? (second code-list))
    (if-let [report (_fn-check-default-format (list 'fn* (apply list (rest code-list))) state)]
      (let [np0 (count (:path state))]
        (update report :path
          #(into [] (concat (subvec % 0 (inc np0)) (subvec % (+ np0 2)))))))
    (_fn-check-default-format code-list state)))


(defn _loop-recur-check1 [code path n-binding tail?]
  (let [add-path (fn [added] (apply conj path added))]
    (cond
      (map? code)
      (if-let [err0 (_loop-recur-check1 (keys code))]
        {:path path :msg (assoc err0 :msg (str (:msg err0) " (error in a KEY to this map)."))}
        (c/juice-1 #(_loop-recur-check1 (get code %) (add-path [%]) n-binding false) (keys code)))
      (set? code) (c/juice-1 #(_loop-recur-check1 % (add-path [%] false) n-binding false) code)
      (vector? code) (c/juice-1 #(_loop-recur-check1 (nth code %) (add-path [%]) n-binding false) (range (count code)))
      (= code 'recur) {:msg "recur not called as function." :path path}
      (not (coll? code)) false
      (and (= (first code) 'recur) (not tail?)) {:msg "recur not in tail position" :path path}
      (and (= (first code) 'recur) (< (dec (count code)) n-binding)) {:msg "Too few args to recur" :path path}
      (and (= (first code) 'recur) (> (dec (count code)) n-binding)) {:msg "Too many args to recur" :path path}
      :else (let [codev (into [] (cool-head code 1)) if? (= (first code) 'if) n (count codev)]
              (c/juice-1 #(_loop-recur-check1 (nth codev %) (add-path [%]) n-binding (or if? (= % (dec n))))
                (range (count code)))))))
(defn _loop-recur-check [code-list state]
  (let [n-binding (/ (count (second code-list)) 2)]
    (_loop-recur-check1 (cool-head code-list 2) (:path state) n-binding true)))

;;;;;;;;;;;; Spec checks for java interaction special forms ;;;;;;;;;;;;


(defn _java-constructor-check [code-list path]
  (cond (< (count code-list) 2) {:msg "Java constructer with no class" :path path}
    (not (symbol? (second code-list))) {:msg "Java constructor arg not a symbol" :path (conj path 1)}
    (not (sym-to-class (second code-list))) {:msg (str "Can't find java class " (second code-list)) :path (conj path 1)}
    (try (eval (list 'fn [] (apply list 'new (second code-list) (range (- (count code-list) 2)))))) ; Only arity is checked @compile. They use eval, so can we!
     {:msg (str "Wrong constructor arity") :path path}))

(defn _java-call-check [code-list path locals]
  (let [x1 (second code-list)]
    (cond (<= (count code-list) 2)
      {:msg "Too few arguments to java fn call." :path path}
      (not (symbol? (second (rest code-list)))) {:msg "The member-call is not a symbol" :path (conj path 2)}
      (sym-to-class x1)
      (try (do (eval (list 'fn [] (apply list '. x1 (second (rest code-list))
                                    (range (- (count code-list) 3))))) false) ; Only arity checked. Again they use eval, so can we.
        (catch Exception e
          {:msg (str (sym-to-class (second code-list)) " has no method " (second (rest code-list)) " with arity " (- (count code-list) 3)) :path path}))
      :else false)))

(defn _javasym-err-msg [sym arity]
  "At runtime it reflect checks everything, at compile it only checks the arity for static fields."
  ; TODO: is this used at all as a fn?
  (let [txt (str sym)
        pieces (string/split txt #"/")
        dot-start? (= (str (first (str sym))) ".")
        dot-end? (= (str (last (str sym))) ".")
        cl-sym (symbol (string/replace (first pieces) "." ""))
        class-found? (sym-to-class cl-sym)]
    (cond
      (and dot-end? (= arity -1)) (str sym " must be at the beginning like a function call.")
      dot-end?
      (if class-found? false (str "Can't find this java class: " cl-sym))
      dot-start?
      (if (= arity -1) (str sym " must be at the beginning like a function call.") false)
      class-found? false
      :else (str sym " is a java class but it isn't found")))) ; this last conditon is probably not reachable.


;;;;;;;;;;;; Generic spec check ;;;;;;;;;;;;


(defn _spec-assert [code-list state]
  ; Notes: binding doesn't allow you to set your own vars so it is neutral.
  (let [state (update state :path #(if % % []))
        sym (first code-list) let? (or (= sym 'let) (= sym 'let*))
        loop? (or (= sym 'loop) (= sym 'loop*)) bind? (= sym 'binding)
        path (:path state []) x2 (second code-list)]
    (cond
      (or let? loop? bind?)
      (let [msg-tail (cond let? " of let" loop? " of loop" bind? " of binding")]
        (if-let [body (second code-list)]
          (if-let [err (_binding-spec-check body (conj path 1))]
            (assoc err :msg (str (:msg err) msg-tail))
            (if loop? (_loop-recur-check code-list state) false))
          {:msg (str "No [body] " msg-tail) :path path}))
      (or (= sym 'fn) (= sym 'fn*))
      (_fn-check code-list state)
      (= sym 'def)
      (cond (< (count code-list) 2) {:msg "Too few arguments to def." :path path}
        (> (count code-list) 3) {:msg "Too many arguments to def." :path path}
        (not (symbol? (second code-list))) {:msg "The first argument to def isn't a symbol." :path (conj path 1)})
      (= sym 'var)
      (cond (< (count code-list) 2) {:msg "Too few arguments to var." :path path}
        ;This is allowed! (> (count code-list) 3) {:msg "Too many arguments to var." :path path}
        (not (symbol? (second code-list))) {:msg "Var must take a symbol as it's argument." :path path})
      (= sym 'if) (cond (< (count code-list) 3) {:msg "Too few arguments to if." :path path}
                    (> (count code-list) 4) {:msg "Too many arguments to if." :path path})
      (= sym 'throw) (cond (< (count code-list) 2) {:msg "Too few arguments to throw." :path path}
                    (> (count code-list) 3) {:msg "Too many arguments to throw." :path path})
      (and (= sym 'catch) (not (get (:locals state) sym)))
       (cond (not (:try? state)) {:msg "Catch not enclosed by try." :path path}
         (< (count code-list) 3) {:msg  "Too few arguments to catch." :path path}
         (not (symbol? (second code-list))) {:msg "The exception class was badly specified." :path (conj path 1)}
         (not (symbol? (nth code-list 2))) {:msg "The exception binding symbol isn't a symbol." :path (conj path 2)}
         (not (sym-to-class x2)) {:msg (str "The exception's class '" x2 "' can't be found") :path (conj path 1)})
      (and (= sym 'new) (not (get (:locals state) sym)))
      (_java-constructor-check code-list path)
      (= sym '.)
      (_java-call-check code-list path (:locals state))
      :else false))) ;try, moniter-enter, moniter-exit seem to have no restrictions on compile.


;;;;;;;;;;;; Recursive compilation error functions ;;;;;;;;;;;;


(defn _recursive-on-bindings [bindings state walk-fn]
  "Runs the code with continusly updates states. Returns [new-state, err-report].
   Must do a spec-check first."
  (let [n (count bindings) bindings (into [] bindings)
        udp #(assoc %1 :path (conj (:path %1) %2))]
    (loop [ix 0 statei state]
      (if (>= ix n) [statei false]
        (let [sym (nth bindings ix) val (nth bindings (inc ix))]
          (if-let [err (walk-fn val (udp statei (inc ix)))] [statei err]
            (recur (+ ix 2)
              (update statei :locals #(conj % sym)))))))))

(defn _compile-err-catch [code state]
  "The big recursive switchyard. Lists/listoids are near the bottom."
  (let [locals (:locals state #{})
        locals (if (set? locals) locals (set locals))
        c0 (if (coll? code) (first code)) codev (if (sequential? code) (into [] code))
        path (:path state [])
        env (:env state #{})
        state1 (if (coll? code)
                 (assoc state :enclosing-arity (dec (count code))
                   :try? (= c0 'try) ; This stuff is for one level above us.
                   :fn? (= c0 'fn*)
                   :listy? (and (coll? code) (not (map? code)) (not (vector? code)) (not (set? code)))
                   :loop? (or (= c0 'loop) (= c0 'loop*))) state)
        add-path (fn [added] (assoc state1 :path (apply conj path added)))]
    ;(println "Locals:" locals "code:" code "lookup:" (get locals code))
    ;(if (symbol? code) (println "unerrorLeaf:" code))
    (cond
      (map? code) ; Errors in map keys just path to the map itself.
      (if-let [key-err (c/juice-1 #(_compile-err-catch % (assoc state1 :path [%])) (keys code))]
        (_abridged-error key-err path "somewhere in this map KEY: ")
        (c/juice-1 #(_compile-err-catch (get code %) (add-path [%])) (keys code)))
      (set? code) (c/juice-1 #(_compile-err-catch % (add-path [%])) code)
      (vector? code) (c/juice-1 #(_compile-err-catch (nth code %) (add-path [%])) (range (count code)))
      (symbol? code) (_symbol-not-found-check code path locals state)
      (not (coll? code)) false ; We aren't checking at the string level where badnumber formats, etc can mess us up.
      (= (first code) 'quote) false ; Quotes mean inside stuff isn't evaled.
      :else ; Listy collections, act recursivly in most cases.
      (if-let [spec-err (_spec-assert code state)] spec-err
        (cond
          (= c0 '.) (_compile-err-catch ; Java call = first 3 can break rules
                      (c/cassoc (cool-head code 3) 1 (second code)) state1)
          (and (= c0 'new) (not (get (:locals state) 'new))) (_compile-err-catch (cool-head code 2) state1) ; Constructor, first 2 can break rules.
          (= c0 'catch) ; Adds a single local variable.
          (let [locals1 (conj locals (nth codev 2))]
            (_compile-err-catch (cool-head code 1) (assoc state1 :locals locals1)))
          (and (contains? #{'fn 'fn*} (first code)) (vector? (second code))) ; Unusaual function format.
          (let [locals1 (set/union locals (set (second code)))]
            (_compile-err-catch (cool-head code 2) (assoc state1 :locals locals1)))
          (:fn? state) ; Adds a vector of local vars we are at the ([x y] ...) level for (fn ([z] ...) ([x y] ...))
          (let [locals1 (set/union locals (disj (set (first code)) '&))]
            (_compile-err-catch (cool-head code 1) (assoc state1 :locals locals1)))
          (get #{'let 'let* 'loop 'loop*} c0) ; Adds even elements of a the binding vector to local vars.
          (let [bindings (second code)
                s-e (_recursive-on-bindings bindings (assoc state1 :path (conj path 1)) _compile-err-catch)
                state2 (first s-e) err-report (second s-e)]
            ;(println "the err report:" err-report)
            (if err-report err-report
              (_compile-err-catch (cool-head code 2) (assoc state2 :path path))))
          :else (c/juice-1 #(_compile-err-catch (nth codev %) (add-path [%])) (range (count code))))))))


(defn compile-err-catch [code ns-sym]
  "Catches macroexpanded or compile-time errors in code,
   returns the path in the macroexpanded code if there is an error and type of error."
  (let [_err? (atom true)
        code-or-err (try (let [code-ex (langs/mexpand ns-sym code)]
                           (reset! _err? false) code-ex)
                      (catch Exception e
                        (_on-macroexpand-err-catch ns-sym code [])))]
    (if @_err? code-or-err
      (_compile-err-catch code-or-err {}))))

(defn flatten-1 [coll]
  "Turns coll into a vector of symbols that prints back to coll."
  (/ 0))

(defn label-symbol [sym]
  "Can also be a string."
  (let [sadface \u2639]
    (symbol (str sadface sadface sadface sym sadface sadface sadface))))

(defn label-code [code err-info]
  (t/cupdate-in code (:path err-info)
    (fn [broken-part] (label-symbol (pr-str broken-part)))))

(defn errprint [code err-info]
  "Lets the programmer see it in pprinted code."
  (do (println (str (:msg err-info) " (path in code=" (:path err-info) ")"))
    (blit/vpu (label-code code err-info))))

(defn errprint? [code]
  (if-let [err-info (compile-err-catch code *ns*)]
    (errprint (if (:b4expand? err-info) code (langs/mexpand code))
       err-info)
    (do (blit/vpu code)
      (println "No error caught in code [shown unqualed above].")))
  (try (do (eval (list 'fn [] code))
         (println "No vanilla error"))
    (catch Exception e
      (println "Compare to vanilla error [code shown unqualed above]: "
        (pr-error e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def java-interop-tests
  [[false '(+ 1 2)] ; Works!
   ["Cant resolve" '(+ foo bar)] ; Works!
   ["Malformed member" '(1 2 (. bar))] ; Works!
   ["Cant resolve symbol" '(+ bar.)] ; Works!
   ["Class not found" '(bar. 1)] ; Works!
   [false '(java.awt.Point 1 2)] ; Works!
   [false '(java.awt.Point 1 2 3 4 5 6 7 8 9)] ; Works (doesn't check construction arity)!
   ["No such namespace" '(Foo/bar)] ; Works!
   [false '(String/copyValueOf 1)] ; Works!
   ["No method" '(String/duplicateValueOf 1)] ; Works!
   ["Symbol not found" '(String/copyValueOf copyValueOf)] ; Works!
   ["No such match field" '(String/copyValueOf "too" "many" "args" "for" "this" "java")] ; Works!
   [false '(.bar 1)]]) ; Works!

(def basic-tests
  [[false '(if 1 2)] ; Works!
   [false '(if 1 2 3)] ; Works!
   ["Two few arguments to if" '(if 1)] ; Works!
   ["Two many arguments to if" '(if 1 2 3 4)] ; Works!
   ["Cant find symbol if" '(+ if true)] ; Works
   ["Cant find symbol if" '[if 1 2]] ; Works
   [false '(var +)] ; Works!
   ["Cant resolve" '(var +++)] ; Works!
   ["Too few to var" '(var)] ; Works!
   [false '(var + -)] ; Works (it ignores extra arguments)!
   [false '(try (/ 0) (catch Exception my-ex (.getMessage my-ex)))] ; Works!
   ["Class not found" '(try (/ 0) (catch thisDoesNotExistException my-ex (.getMessage my-ex)))] ; Works!
   ["Catch outside of try" '(catch Exception my-ex -1)] ; Works!
   [false '(try (catch Exception my-ex -1) (catch Exception my-true-ex -1))] ; Works!
   ])

(def binding-tests
  "Local symbols are fun."
  [[false '(let [a 1 b a] (+ a b))] ; Works!
   ["Must be even" '(let* [a 1 b] (+ a a))] ; Works!
   ["Bindings is not vector" '(let* (+ 1))] ; Works!
   ["Cant resolve symbol b" '(let [a b b a] (+ a b))] ; Works!
   [false '(let [a 1] (let [b 2 c 3] (+ a b c)))] ; Works!
   ["Cant resolve symbol" '(let [a (+ 1 b) b a] (+ a b))] ; Works!
   ["Cant resolve symbol" '(let [a (let [aa 1] (inc aa)) b aa] (+ a b))] ; Works!
   ])

(def fn-tests "One of the most complex compile-time bugs for us to deal with."
  [[false '(fn [foo bar] (* foo bar))]
   ["Fn args must be a list of symbols" '(fn* ([bar (+ 1 2)] (* bar bar)))]
   ["Fn args must be a list of symbols" '(fn* ([1 bar] (* bar bar)))]
   [false '(fn* [foo bar] (+ foo bar))]
   ["No arg vector" '(fn* (+ 2 3))]
   [false '(fn* )]
   ["Fn args must be symbols" '(fn* [bar 1] (+ 1 bar))]
   [false '(let* [& 1] (inc &))] ; esoteric!
   [false  '(fn [x & ys] {x ys})]
   ["Can't resolve symbol &" '(fn [x & ys] &)]])

(def loop-recur-tests "One of the most complex compile-time bugs for us to deal with."
  [[false '(loop [ix 0] (if (< ix 10) (recur (inc ix)) ix))]
   ["Not tail" '(loop [ix 0] (+ (< ix 10) (recur (inc ix)) ix))]
   ["Too many" '(loop [ix 0] (if (< ix 10) (recur (inc ix) 1) ix))]
   ["Too few" '(loop [ix 0] (if (< ix 10) (recur) ix))]
   ["false" '(loop [ix 0] (if (< ix 10) (recur) ix))]])

(def macro-tests "Error when trying to macroexpand?"
  [["Bad let form" '(+ (let [x 1 y] x) 2)]])


(def challanges "Oh boy we really want this code to fail and fail hard. TODO")


;(def tests (into [] (concat java-interop-tests)))
;(println tests)
;(let [ix 0]
;  (if (>= ix (count debugs)) (println "Cant debugtest this")) (errprint? (nth debugs 0)))