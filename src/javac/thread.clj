; The top levels of the app have to deal with java threads.

(ns javac.thread
  (:import (javax.swing SwingUtilities)))

(defn edt? [] (SwingUtilities/isEventDispatchThread))

(defn assert-edt []
  "Asserts that we are on the event dispatch thread, throws an error otherwise."
  (if (not (edt?)) (throw (Exception. "A function that needed to be ran on the event dispatch thread wasn't."))))

(defmacro swing-later [body]
  `(SwingUtilities/invokeLater (fn [] ~body)))

(defmacro swing-wait [body]
  `(let [tmp-atom# (atom nil)]
     (SwingUtilities/invokeAndWait
       (fn [] (let [x# ~body]
                (reset! tmp-atom# x#))))
     @tmp-atom#))