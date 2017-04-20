; Test the graphics.
(ns clooj.guinew.gtest
  (:require [clooj.guinew.jui :as jui]))

(defn test0 []
  (let [window [:object :win0 {:Type :JFrame}]
        panel [:object :panl0 {:Type :JPanel :Graphics [[:drawLine [100 100 200 300]]]}]
        ch [:relation :win0 :panl0]]
    (jui/setup [window panel ch])))


(defn test1 []
  (let [window [:object :win0 {:Type :JFrame :Size [400 400]}]
        grst #(vector [:drawString [(str %) 10 20] {:Color [0 0 1] :FontSize 20}] [:drawString ["see above" 10 60]])
        panel [:object :panl0 {:Type :JPanel :Graphics (grst "Jpanel is watching")}]
        mousel [:listener :panl0 :mouseMoved :the-mouse-l [(fn [e panl0] [[:object :panl0 {:Graphics (grst (str "Mouse " (:X e) " " (:Y e)))}]]) :panl0]]
        mousecl [:listener :panl0 :mousePressed :the-mousec-l [(fn [e panl0] [[:object :panl0 {:Graphics (grst (str "Click " (:Button e) " " (:X e) " " (:Y e)))}]]) :panl0]]
        ; seems to want to go to the window:
        keyl [:listener :win0 :keyPressed :the-key-l [(fn [e panl0] [[:object :panl0 {:Graphics (grst (str "Key " (:KeyChar e)))}]]) :panl0]]
        ch [:relation :win0 :panl0]]
    (jui/setup [window panel ch keyl mousel mousecl])))

(defn test2 []
  (let [window [:object :win0 {:Type :JFrame :Size [400 400]}]
        cf #(vector 100 100 (int (+ 100 (* (Math/cos %) 100))) (int (+ 100 (* (Math/sin %) 100))))
        cmd0 [:object :panl0 {:Type :JPanel :theta 0 :Graphics [[:drawLine (cf 0)]]}]
        panel cmd0
        lfn (fn [clock-e panel] ;(println "hi")
              (let [new-theta (+ (:theta panel) 0.02)]
                [[:object :panl0 {:theta new-theta
                                  :Graphics [[:drawLine (cf new-theta)]]}]]))
        ev [:listener :Clock :everyFrame :spinner [lfn :panl0]]
        ch [:relation :win0 :panl0]]
    (jui/setup [window panel ch ev])))