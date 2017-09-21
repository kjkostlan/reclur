; Current generation of the gui in development as of at least Sept 17 2017

(ns clooj.app.gauitest.ctest
  (:require [clooj.app.gaui.cpanel :as cpanel]
    [clooj.app.gaui.rtext :as rtext]))


(defn b2p [f]
 "Converts box to panel functions.
  (f evt box) is converged to (fn [evt panel] ...).
  Only useful for the box functions."
 #(assoc %2 :box (f %1 (:box %2))))

(defn single-txt-lfns [] ; it's a function so that changes to rtext/mouse-press, etc are kept up to date.
  {:mousePressed (b2p rtext/mouse-press)
   :mouseDragged (b2p rtext/mouse-drag)
   :keyPressed (b2p rtext/key-press) 
   :keyReleased (b2p rtext/key-release)
   :mouseWheelMoved (b2p rtext/mouse-wheel)
   :everyFrame (fn [cevt panel] (assoc panel :Graphics (cpanel/grequel (rtext/render (:box panel)))))})

; Text editing with multiple pieces that can hold user data/functions.
(defn rtext1 []
  "Level 1 test or rtext: only one piece."
  (cpanel/launch-app :box rtext/empty-text (single-txt-lfns)))

(defn rtext2 []
  "Adding an extra piece to rtext adds novilty."
  (cpanel/launch-app :box (assoc rtext/empty-text :pieces [{:text "abc"} {:text "123"}]) (single-txt-lfns)))

(defn rtext3 []
  "Scrolling shift, hey it's tardis."
  (cpanel/launch-app :box (assoc rtext/empty-text :pieces (mapv #(hash-map :text (str % (if (= (mod % 25) 0) "\n" ""))) (range 800))) (single-txt-lfns)))


