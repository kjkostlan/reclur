; Keeps track of where stuff goes.
(ns clooj.flow 
  (:require [clooj.collections :as collections]))

; Runtime light flow tracking of a function.
; Each flow has a vector of origins and a single destination.
  ; Each origin is a path and the destination is a path to the variable in the code.
; Returns {:flow ... :result ...}
(defmacro flow-track [code]
  (throw "TODO")
)


;; A simple flag system that facilitiates keeping track of paths
;; while rearranging hierarchies.
(def _flag (keyword (str "reclur-flow-" "flag-id")))

(defn flag-leaf [flag-me id] 
  "Flags a hash-map flag-me with a unique id. 
   Flag all the leaves that need id-ing before deep-find-flags."
  (assoc flag-me _flag id))

(defn _deep-find-flags [acc x fkeys path]
  (let [deeper (reduce #(_deep-find-flags %1 (collections/gett x %2) ; from deeper levels.
                          fkeys (conj path %2)) acc (if (coll? x) (fkeys x) []))]
    (if (and (map? x) (get x _flag)) (assoc deeper (get x _flag) path) deeper)))
(defn deep-find-flags [x fkeys]
  "Creates a map of flag ids -> paths.
   fkeys is or returns a sequential or set of keys given a value.
   This function should only be called at the setup, use the fns below to modify it."
  (_deep-find-flags {} x (if (fn? fkeys) fkeys (fn [x] (if (coll? x) fkeys []))) []))

(defn add [id-map x add-me id path]
  "Adds add-me to x in path and updates id-map. add-me must be a map.
   Replaces anything in the path. 
   Both id-map and x need to be updated, returns {:id-map ... :x ...} format."
  {:id-map (assoc id-map id (into [] path))
   :x (collections/asoc-in x path (assoc add-me _flag id))})

(defn move [id-map x id dest]
  "Moves the map associated with id in x to path dest.
   Both id-map and x need to be updated, returns {:id-map ... :x ...} format.
   TODO: slow O(count ids) children-check  step can be made a bit faster."
  (let [p (get id-map id) n (count p) dest (into [] dest)]
    (if (nil? p) (throw (Exception. (str "Id not in map: " id))))
    {:id-map (assoc (zipmap (keys id-map) ; move the children as well.
                      (mapv #(if (= p (into [] (take n %)))
                               (into [] (concat dest (drop n %))) %) (vals id-map))) 
               id dest)
     :x (let [p0 (butlast p) pl (last p)
              v+ (collections/gett-in x p0) ; contains the value.
              v+- (if (map? v+) (dissoc v+ pl) (assoc v+ pl []))
              x0 (collections/asoc-in x p0 v+-)] ; remove.
          (collections/asoc-in x0 dest (get v+ pl)))}))

(defn delete [id-map x id]
  "Removes the stuff with id from x.
   Both id-map and x need to be updated, returns {:id-map ... :x ...} format.
   TODO: slow O(count ids) children-check step can be made a bit faster."
  (let [p (get id-map id) n (count p)]
    (if (not (nil? p))
      (let [p0 (butlast p) pl (last p) v+ (collections/gett-in x p0)
            v+- (if (map? v+) (dissoc v+ pl) (assoc v+ pl []))]
        {:id-map (dissoc (reduce #(if (= p (into [] (take n (get id-map %2)))) (dissoc %1 %2) %1) 
                          id-map (keys id-map)) id)
         :x (collections/asoc-in x p0 v+-)}))))

;(defmacro )
;(require '[clooj.coder.repl :as repl] '[clojure.pprint :as pprint])