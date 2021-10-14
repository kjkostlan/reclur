; Groups boxes into tabs.
(ns app.tabgroup
  (:require [clojure.string :as string] [clojure.set :as set]
    [layout.spatial.collide :as collide] [layout.spatial.xform :as xform]
    [c] [np]))

(def ^:dynamic *title-space* 17.0)
(def ^:dynamic *make-room-for-tabs* false)

(defn z-value-of [tab-group]
  (/ (reduce + (mapv #(get % :z 0.0) tab-group)) (count tab-group)))

(defn box-under? [box mouse-x mouse-y] 
  (let [[x0 x1 y0 y1] (xform/box-xxyy box)]
    (collide/hit-rect? mouse-x mouse-y x0 x1 y0 y1)))

(defn box-kys-under [boxes mouse-x mouse-y]
  (let [box-under1? (fn [k] (box-under? (get boxes k) mouse-x mouse-y))]
    (set (filter box-under1? (keys boxes)))))

(defn unique-z-vec [box-vector]
  "Assigns each component a unique z-value."
  (loop [acc (mapv #(if (:z %) % (assoc % :z 1)) box-vector)]
    (if (= (count (apply hash-set (mapv :z acc))) (count acc)) acc
      (recur (mapv (fn [c] (update c :z #(+ (* % (+ 1 (* (Math/random) 1e-10))) 1e-100))) acc)))))

(defn make-group [box-vector]
  "Averages the positions, order is based on :z
   All tab-group information is stored in the components."
  (let [box-vector (if *make-room-for-tabs*
                     (mapv (fn [box] (update-in box [:position 1] #(+ % *title-space*))) box-vector) box-vector)
        poss (mapv :position box-vector)
        szs (mapv :size box-vector)
        box-vector (sort-by :z (unique-z-vec box-vector))
        mean1 (fn [x ix] (/ (reduce + (mapv #(get % ix) x)) (count box-vector)))

        pos [(mean1 poss 0) (mean1 poss 1)]
        sz [(mean1 szs 0) (mean1 szs 1)]
        tabgroup (gensym 'tabgroup)]
    (mapv #(assoc %1 :position pos :size sz ::tabgroup tabgroup ::taborder %2)  box-vector (range))))

(defn ungroup [box-vector]
  "Removes a tab group."
  (let [box-vector (mapv (fn [box] (if *make-room-for-tabs*
                                     (update-in box [:position 1] #(+ % *title-space*)) box)) box-vector)]
    (mapv #(dissoc % ::taborder ::tabgroup) box-vector)))

(defn sort-tab-order [box-vector]
  (into [] (sort-by #(get % ::taborder 0) box-vector)))

(defn current-tab-groups [boxes]
  "Map of vectors of boxes that share like tab groups.
   Boxes not included in any tab-group will be excluded."
  (let [labels (set (filter identity (mapv ::tabgroup (vals boxes))))]
    (reduce (fn [acc box]
              (if (::tabgroup box)
                (update acc (::tabgroup box) #(conj (if % % []) box)) acc))
      {} (vals boxes))))

(defn on-tab-group-click [box-vector m-evt]
  "Selects a box within the tab group to put it on top, changes the :z values."
  (let [box-vector (unique-z-vec box-vector)
        box-vector (sort-tab-order box-vector)
        [x0 x1 y1 _] (xform/box-xxyy (first box-vector))
        y0 (- y1 *title-space*)
        x (get m-evt :X 0) y (get m-evt :Y 0)]
    (if (collide/hit-rect? x y x0 x1 y0 y1)
      (let [n (count box-vector)
            click-ix (max 0 (min (int (* (/ (- x x0) (- x1 x0)) n)) (dec n)))
            max-z-ix (np/argmax (mapv :z box-vector))
            max-z (:z (nth box-vector max-z-ix))
            cur-z (get-in box-vector [click-ix :z])]
        (-> box-vector
          (assoc-in [click-ix :z] max-z)
          (assoc-in [max-z-ix :z] cur-z)))
      box-vector)))

(defn group-boxes-under-mouse [boxes mouse-x mouse-y]
  "All boxes under the mouse's position will be tabbified."
  (let [kys (box-kys-under boxes mouse-x mouse-y)
        pre-existing (set (filter identity (map #(::tabgroup (get boxes %)) kys)))

        kys1 (filterv #(or (get kys %) (get pre-existing (::tabgroup (get boxes %)))) (keys boxes))]
    (if (<= (count kys1) 1) boxes
      (let [box-vec (mapv #(assoc (get boxes %) ::tmp-key %) kys1)
            box-vec1 (make-group box-vec)]
        (reduce #(assoc %1 (::tmp-key %2) (dissoc %2 ::tmp-key)) boxes box-vec1)))))

(defn ungroup-boxes-under-mouse [boxes mouse-x mouse-y]
  (let [kys (box-kys-under boxes mouse-x mouse-y)
        box-vec (mapv #(assoc (get boxes %) ::tmp-key %) kys)
        box-vec1 (ungroup box-vec)]
    (reduce #(assoc %1 (::tmp-key %2) (dissoc %2 ::tmp-key)) boxes box-vec1)))

(defn render-tab-group [box-vector]
  "Draws a tab-group."
  (let [box-vector (sort-tab-order box-vector) n (count box-vector)
        [x0 x1 y0 y1] (xform/box-xxyy (first box-vector))

        tspace *title-space*
        line-col [0 0 0 1]
        outline [[:drawRect [x0 (- y0 tspace) (- x1 x0) (- y1 (- y0 tspace))]
                  {:Color line-col}]]
        max-z (apply max (mapv :z box-vector))
        fill (mapv (fn [ix]
                     (let [x0i (+ x0 (/ (* (- x1 x0) ix) n))
                           col (if (= (:z (nth box-vector ix)) max-z)
                                 [0.25 1.0 0.5 1] [0.35 0.75 0.3 1])]
                       [:fillRect [x0i (- y0 tspace) (/ (- x1 x0) n) tspace] {:Color col}])) (range n))
        
        xvals (mapv #(+ x0 (* (- x1 x0) (/ % n))) (range n))
        boundaries (mapv #(vector :drawLine [(nth xvals %) (- y0 tspace) (nth xvals %) y0] {:Color line-col}) (range n))

        leaf-txts (mapv (fn [box]
                          (let [tl (get box :path "")
                                tl (if (string? tl) tl (last tl))]
                            (last (string/split tl #"/")))) box-vector)

        strings (mapv #(vector :drawString [(str %1) (+ %2 5) (- y0 5)] {:Color [0 0 0 1] :FontSize 12})
                  leaf-txts xvals)]
    (c/vcat fill outline boundaries strings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; API fns ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tab-group-global-click [boxes m-evt]
  "Tries to click every tab group."
  (let [boxes-k (zipmap (keys boxes) (mapv #(assoc %2 ::tmp-key %1) (keys boxes) (vals boxes)))
        groups (current-tab-groups boxes-k)
        groups1 (mapv on-tab-group-click (vals groups) (repeat m-evt))
        super-vec1 (apply c/vcat groups1)]
    (merge boxes (zipmap (mapv ::tmp-key super-vec1) (mapv #(dissoc % ::tmp-key) super-vec1)))))

(defn sync-on-update [boxes0 boxes1]
  "Moving any box in the tab group moves all the boxes.
   New boxes are not added to the current tab group, but instead share thier own.
   They can always be made into a tab-group."
  (let [kys0 (set (keys boxes0)) kys1 (set (keys boxes1))
        new-kys (set/difference kys1 kys0)
        common-kys (set/intersection kys1 kys0)
        need-to-rename (set (filterv identity (mapv #(::tabgroup (get boxes1 %)) new-kys)))
        rn-map (zipmap need-to-rename (mapv (fn [_] (gensym 'tabgroup)) need-to-rename))

        boxes2 (reduce #(if-let [tb (get-in %1 [%2 ::tabgroup])]
                          (assoc-in %1 [%2 ::tabgroup] (get rn-map tb tb)) %1) boxes1 new-kys)
        moved-kys (filterv #(let [b0 (get boxes0 %) b1 (get boxes1 %)]
                              (or (not= (:position b0) (:position b1))
                                (not= (:size b0) (:size b1)))) common-kys)
        k2pos-size (reduce #(let [box (get boxes1 %2)]
                               (if (::tabgroup box)
                                 (assoc %1 (::tabgroup box) [(:position box) (:size box)]) %1)) {} moved-kys)
        boxes3 (zipmap (keys boxes2) (mapv #(if-let [psz (get k2pos-size (::tabgroup %2))]
                                              (assoc %2 :position (first psz)
                                                :size (second psz)) %2)
                                       (keys boxes2) (vals boxes2)))]
    boxes3))

(defn toggle-tabs-for-boxes-under-mouse [boxes mouse-x mouse-y]
  (let [kys (box-kys-under boxes mouse-x mouse-y)
        has-tab? (get-in boxes [(first kys) ::tabgroup])]
    (if has-tab? (ungroup-boxes-under-mouse boxes mouse-x mouse-y)
      (group-boxes-under-mouse boxes mouse-x mouse-y))))

(defn render-tab-groups [boxes]
  "Returns [Map from group name to gfx, Map from group name to :z value]"
  (let [groups (current-tab-groups boxes)
        name2gfx (zipmap (keys groups) (mapv render-tab-group (vals groups)))
        name2z (zipmap (keys groups) (mapv z-value-of (vals groups)))]
    [name2gfx name2z]))
