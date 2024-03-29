; Stack mode: adding stuff goes on top of the component of the matching type within the screen.
; The most basic layout, has some simple fns to move components.
(ns layout.spatial.lmodes.stack
  (:require [np]
    [layout.spatial.xform :as xform]
    [layout.spatial.collide :as collide]
    [app.multicomp :as multicomp]
    [app.fbrowser :as fbrowser]
    [app.codebox :as codebox]
    [app.rtext :as rtext]))

(def lparams {:files-x 0.2 :repl-x 0.5 :console-x 0.67 :replconsole-y 0.25 :margin 0.0})

(defn make-grid [comps x0 x1 y0 y1]
  "As square as possible."
  (let [n (count comps)
        nx (cond (<= n 3) 1
             (<= n 6) 2
             (<= n 12) 3
             (<= n 20) 4
             (<= n 30) 5
             (<= n 40) 6
             :else (Math/round (Math/sqrt n)))
        ny (int (+ (/ n nx) 1 -1e-9))
        x-values (np/linspace x0 x1 (inc nx))
        y-values (np/linspace y0 y1 (inc ny))
        x0-values (into [] (butlast x-values))
        x1-values (into [] (rest x-values))
        y0-values (into [] (butlast y-values))
        y1-values (into [] (rest y-values))
        xxyys (loop [acc [] ix 0 ix-x 0 ix-y 0]
                (if (= ix n) acc
                  (let [wrap? (= ix-x (dec nx))]
                    (recur (conj acc [(nth x0-values ix-x)
                                      (nth x1-values ix-x)
                                      (nth y0-values ix-y)
                                      (nth y1-values ix-y)])
                      (inc ix)
                      (if wrap? 0 (inc ix-x))
                      (if wrap? (inc ix-y) ix-y)))))
        aply (fn [c ix] (let [xxyy (nth xxyys ix)]
                          (assoc c :position [(first xxyy) (nth xxyy 2)]
                            :size [(- (second xxyy) (first xxyy))
                                   (- (nth xxyy 3) (nth xxyy 2))])))]
    (if (map? comps) (zipmap (keys comps) (mapv aply (vals comps) (range)))
      (mapv aply comps (range)))))

(defn set-unitscreen-xxyy [component visible-xxyy unit-xxyy]
  "Sets a component within the screen on a 0-1 scale."
  (xform/set-xxyy component (xform/world-xxyy visible-xxyy unit-xxyy)))

(defn snap-rects [xxyys snap-threshold-nextto snap-threshold-distanced allow-resize?]
  "Snaps nearby values together. Thresholds are absolute. This fn is not currently used or functional."
 (/ 0))

(defn initial-position [s files repl console]
  "Initial positions before the codebox is opened. One code leaves a lot of room for codeboxes.
   Gensyms keys as well."
  (let [vis-xxyy (xform/visible-xxyy (:camera s))
        margin (:margin lparams)
        files (set-unitscreen-xxyy files vis-xxyy (xform/y- [0 (:files-x lparams) (:replconsole-y lparams) (- 1.0 margin)]))
        repl (set-unitscreen-xxyy repl vis-xxyy (xform/y- [0 (:repl-x lparams) 0 (:replconsole-y lparams)]))
        console (set-unitscreen-xxyy console vis-xxyy (xform/y- [(:repl-x lparams) 1 0 (:replconsole-y lparams)]))
        gk #(keyword (gensym %))]
    (update s :components
            #(assoc % (gk 'fbrowser) (assoc files :z 1) (gk 'orepl) (assoc repl :z 2)
                    (gk 'siconsole) (assoc console :z 0)))))

(defn pos-new-comptype [s comp]
  "Only used if this type is new in the screen."
  (let [sc-free (collide/maxarea-free-screenunitxxyy s) minspace 0.1
        new-unit (if (or (< (- (sc-free 1) (sc-free 0)) minspace) (< (- (sc-free 3) (sc-free 2)) minspace))
                   [0.25 0.75 0.25 0.75]
                   sc-free)]
    (set-unitscreen-xxyy comp (xform/visible-xxyy (:camera s)) new-unit)))

(defn add-component [s comp kwd]
  "Uses s to position the new comp."
  (let [ty (:type comp)
        comp-screen (get (:components s) (collide/most-on-screen s #(= (:type %) ty)))
        zmax (collide/max-z s)
        vis-xxyy (xform/visible-xxyy (:camera s))
        comp1 (assoc (if comp-screen (merge comp (select-keys comp-screen [:position :size]))
                       (pos-new-comptype s comp))
                :z (inc zmax))]
    (assoc-in s [:components kwd] comp1)))

(defn add-then [s comp kwd add-comp-f f]
  "Adds a component then applys f to it; f is applied after positioning."
  (let [s1 (add-comp-f s comp kwd)]
    (update-in s1 [:components kwd] f)))

(defn add-grid [s comps kwds add-comp-fn]
  "Runs add-comp-fn, but replaces the new component it adds with a grid.
   Then applies f to each component that was added."
  (if (= (count comps) 0) (throw (Exception. "Empty grid.")))
  (let [s1 (add-comp-fn s (first comps) (first kwds))
        comp0 (get (:components s1) (first kwds)) pos (:position comp0) sz (:size comp0)
        xxyy [(pos 0) (+ (pos 0) (sz 0)) (pos 1) (+ (pos 1) (sz 1))]
        comp-gr (apply make-grid comps xxyy)
        new-comps (zipmap kwds comp-gr)]
    (update s1 :components #(merge % new-comps))))

(defn nearby-codebox-k [s filename relative-nearby-screen-threshold]
  "Is there a nearby codebox already open with filename?
   Returns one of said keys if it exists, or false if it doesn't.
   A threshold of 0.75 is resonable."
  (if (nil? filename) (throw (Exception. "Nil goto filename.")))
  (let [kys-curix (multicomp/who-has s filename 0)
        kys (mapv first kys-curix)
        comps (:components s)
        vis-xxyy (xform/visible-xxyy (:camera s))
        comp-uxxyys (mapv #(xform/unitscreen-xxyy vis-xxyy (xform/box-xxyy (get comps %))) kys)
        comp-ucenterxs (mapv #(+ (* 0.5 (% 0)) (* 0.5 (% 1))) comp-uxxyys)
        comp-ucenterys (mapv #(+ (* 0.5 (% 2)) (* 0.5 (% 3))) comp-uxxyys)
        kyix (first
              (filter
               #(let [uxxyy (nth comp-uxxyys %) dx (max 0.001 (- (uxxyy 1) (uxxyy 0)))
                      dy (max 0.001 (- (uxxyy 3) (uxxyy 2)))
                      fracx (/ (max 0.0 (max (- (uxxyy 1) 1) (- (uxxyy 0)))) dx)
                      fracy (/ (max 0.0 (max (- (uxxyy 3) 1) (- (uxxyy 2)))) dy)]
                  (< (max fracx fracy) relative-nearby-screen-threshold))
               (range (count comp-uxxyys))))
        ky (if kyix (nth kys kyix))]
    (if ky ky false)))

(defn goto-code [s filename char-ix0 char-ix1 threshold add-comp-function]
   "Manipulates s by going to the filename or component between char-ix0 and char-ix1.
   If there is something already open within view we can simply use it, otherwise we use add-comps-function."
  (let [k (nearby-codebox-k s filename 0.75) z-1 (+ (collide/max-z s) 1.0)
        hilite #(assoc (rtext/scroll-to-see-selection (codebox/select-on-real-string % char-ix0 char-ix1)) :z z-1)
        select #(assoc %1 :selected-comp-keys [%2])]
    (if k (-> (update-in s [:components k] hilite) (select k))
      (let [comp (fbrowser/load-from-cache-or-file (:components s) filename codebox/from-text)
            ky1 (keyword (gensym 'goto-target))]
        (-> (select s ky1)
          (add-then comp ky1 add-comp-function hilite))))))

(defn goto-codes [s filenames char-ix0s char-ix1s add-comp-fn]
  "Makes a grid of codeboxes. Always makes new boxes."
  (if (= (count filenames) 0) (throw (Exception. "Going nowhere microsecond-fast.")))
  (let [n (count filenames)
        kwds (mapv #(keyword (gensym %)) filenames)

        compvs (mapv #(fbrowser/load-from-cache-or-file (:components s) %1 codebox/from-text) filenames)
        s1 (add-grid s compvs kwds add-comp-fn)

        comps1 (select-keys (:components s1) kwds)

        z-1 (collide/max-z s)
        hilite #(assoc (rtext/scroll-to-see-selection (codebox/select-on-real-string %1 %2 %3)) :z (+ z-1 %4))

        compv1 (mapv hilite (vals comps1) char-ix0s char-ix1s (range))]
    (assoc s1 :components (merge (:components s) (zipmap kwds compv1)))))

(defn layout []
  {:initial-position (fn [s files repl console] (initial-position s files repl console))
   :add-component (fn [s comp kwd] (add-component s comp kwd))
   :goto (fn [s filename char-ix0 char-ix1] (goto-code s filename char-ix0 char-ix1 0.75 add-component))
   :gotos (fn [s filenames char-ix0s char-ix1s] (goto-codes s filenames char-ix0s char-ix1s add-component))
   :name "stack"})