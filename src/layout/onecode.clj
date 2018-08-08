; A layout that focuses on having a single coding area, the files to the left, and the console below. 
(ns layout.onecode
  (:require [globals]
    [app.xform :as xform]
    [app.multicomp :as multicomp]
    [app.codebox :as codebox]
    [app.rtext :as rtext]
    [layout.layoutcore :as layoutcore]))

;;;;;;;;;;;;;;;;;; The layout code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def lparams {:files-x 0.2 :repl-x 0.5 :console-x 0.67 :replconsole-y 0.25 :margin 0.0})

(def y- layoutcore/y-)

(defn initial-position [s files repl console]
  "Initial positions before the codebox is opened. One code leaves a lot of room for codeboxes.
   Gensyms keys as well."
  (let [vis-xxyy (layoutcore/visible-xxyy (:camera s))
        margin (:margin lparams)
        files (layoutcore/set-unitscreen-xxyy files vis-xxyy (y- [0 (:files-x lparams) (:replconsole-y lparams) (- 1.0 margin)]))
        repl (layoutcore/set-unitscreen-xxyy repl vis-xxyy (y- [0 (:repl-x lparams) 0 (:replconsole-y lparams)]))
        console (layoutcore/set-unitscreen-xxyy console vis-xxyy (y- [(:repl-x lparams) 1 0 (:replconsole-y lparams)]))
        gk #(keyword (gensym %))]
    (update s :components
            #(assoc % (gk 'fbrowser) (assoc files :z 1) (gk 'orepl) (assoc repl :z 2)
                    (gk 'siconsole) (assoc console :z 0)))))

(defn pos-newcommer [s comp]
  "Uses s to position the new comp."
  (let [sc-free (layoutcore/maxarea-free-screenunitxxyy s) minspace 0.1
        new-unit (if (or (< (- (sc-free 1) (sc-free 0)) minspace) (< (- (sc-free 3) (sc-free 2)) minspace))
                   [0.25 0.75 0.25 0.75]
                   sc-free)]
    ;(println "pos-newcommer: " new-unit () (keys comp))
    (layoutcore/set-unitscreen-xxyy comp (layoutcore/visible-xxyy (:camera s)) new-unit)))

(defn add-component [s comp kwd & popup?]
  (let [z (+ (layoutcore/max-z s) 1.0) comp (pos-newcommer s comp)]
    (update (assoc-in s [:precompute :desync-safe-mod?] true) 
      :components #(assoc % kwd (assoc comp :z z)))))

(defn add-then [s comp kwd f]
  "Adds a component then applys f to it; f is applied after positioning."
  (let [s1 (add-component s comp kwd false)]
    (update-in s1 [:components kwd] f)))

(defn goto-code [s k key-is-file? char-ix0 char-ix1]
  "Manipulates s by going to the filename or component between char-ix0 and char-ix1.
   If there is something already open within view we can simply use it, otherwise open a new component.
   for key-is-file? we go to the real text in char-ix0 and char-ix1, which may involve opening a component.
   Otherwise we go to the component specified."
  (if key-is-file? ; the more complex one that may make a new component if need be and must go to the char-ix on the real string.
    (let [kys-loc (multicomp/who-has s k char-ix0) ;[codeboxkys real-ix-within-component char-ix-within-piece]
          kys (first kys-loc) ; only matching comps.
          loc (second kys-loc)

          comps (:components s)

          vis-xxyy (layoutcore/visible-xxyy (:camera s))
          comp-uxxyys (mapv #(layoutcore/unitscreen-xxyy vis-xxyy (layoutcore/xxyy (get comps %))) kys)
          
          threshold-on-screen 0.75 ; below this = new component.
          comp-ucenterxs (mapv #(+ (* 0.5 (% 0)) (* 0.5 (% 1))) comp-uxxyys)
          comp-ucenterys (mapv #(+ (* 0.5 (% 2)) (* 0.5 (% 3))) comp-uxxyys)
          kyix (first
                (filter
                 #(let [uxxyy (nth comp-uxxyys %) dx (max 0.001 (- (uxxyy 1) (uxxyy 0)))
                        dy (max 0.001 (- (uxxyy 3) (uxxyy 2)))
                        fracx (/ (max 0.0 (max (- (uxxyy 1) 1) (- (uxxyy 0)))) dx)
                        fracy (/ (max 0.0 (max (- (uxxyy 3) 1) (- (uxxyy 2)))) dy)]
                    (< (max fracx fracy) threshold-on-screen))
                 (range (count comp-uxxyys))))
          ky (if kyix (nth kys kyix))
          who-has? (> (count kys) 0)
          hilite #(if who-has? (codebox/select-our-who-has % loc (- char-ix1 char-ix0))
                    (codebox/select-our-who-has % [0 char-ix0] (- char-ix1 char-ix0)))] ; fresh codebox.
      (cond ky ; The comp is close enough, move to the key and adjust the key to select us.
            (update-in s [:components ky] hilite)
            who-has? ; no comps close enough, but can copy one of them (must make a copy since all exported stuff must be in agreement).
        (let [comp (get comps (first kys)) ; copy and move this comp.
              ky1 (keyword (gensym 'goto-target))]
          (add-then s comp ky1 hilite))
        :else ; no comps at all, must make them.
        (let [comp (multicomp/load-from-file comps k)
              ky1 (keyword (gensym 'goto-target))]
          (add-then s comp ky1 hilite))))
    (let [comp (get-in s [:components k])
          _ (if (not comp) (throw (Exception. "Nil component for dumb search.")))
          ; Dumb text-as-is search:
          comp1 (assoc comp :cursor-ix char-ix0 :selection-start char-ix0 :selection-end char-ix1)]
      (assoc-in s [:components k] (rtext/scroll-to-see-cursor comp1)))))

;;;;;;;;;;;;;;;;;;;;;;;;; Compile it all together ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn layout []
  {:initial-position (fn [& args] (apply initial-position args))
  :add-component (fn [& args] (apply add-component args))
   :goto (fn [& args] (apply goto-code args))})
