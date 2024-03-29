; Supports Java 8-Java 1.17 (at least) and windows/max/linux (mac testing needed to be 100% sure)
; Functions in this file are either fns that query the version/os/etc, or
; functions that do different things depending on the version/os/etc.

(ns javac.crossplatform
  (:import [java.awt Graphics2D Font])
  (:require [javac.warnbox :as warnbox] [globals]))

(def ^:dynamic *disable-inertial-scroll-mode* true) ; Switching to non-inertial mode can make it too slow.

;;;;;;;;;;;;;;;;;;;;; OS, Java version, and environment ;;;;;;;;;;;;;;;;;;;

(def ^String os-name (.toLowerCase ^String (System/getProperty "os.name")))

(defn windows? []
  (.contains os-name "win"))

(defn mac? []
  (.contains os-name "mac"))

(defn linux? []
  (or (.contains os-name "linux") (.contains os-name "unix") (.contains os-name "sunos")))

(defn java12-19? []
  (let [^String ver (System/getProperty "java.version")]
    (or
      (.startsWith ver "1.12") (.startsWith ver "12.")
      (.startsWith ver "1.13") (.startsWith ver "13.")
      (.startsWith ver "1.14") (.startsWith ver "14.")
      (.startsWith ver "1.15") (.startsWith ver "15.")
      (.startsWith ver "1.16") (.startsWith ver "16.")
      (.startsWith ver "1.17") (.startsWith ver "17.")
      (.startsWith ver "1.18") (.startsWith ver "18.")
      (.startsWith ver "1.19") (.startsWith ver "19."))))

(defn java8? []
  (and (not (java12-19?))
    (let [^String ver (System/getProperty "java.version")]
      (or (.startsWith ver "1.8") (.startsWith ver "8.")))))

(defn java9? []
  (and (not (java12-19?))
    (let [^String ver (System/getProperty "java.version")]
      (or (.startsWith ver "1.9") (.startsWith ver "9.")))))

(defn java10? []
  (let [^String ver (System/getProperty "java.version")]
    (or (.startsWith ver "1.10") (.startsWith ver "10."))))

(defn java11? []
  (let [^String ver (System/getProperty "java.version")]
    (or (.startsWith ver "1.11") (.startsWith ver "11."))))

(defn _scroll-core [mevt] (select-keys mevt [:ScrollAmount :UnitsToScroll :PreciseWheelRotation :WheelRotation]))
(defn update-inertial-scroll-guess! [mevt]
  "Uses heuristics to figure out if we are on an inertial scroll or not."
  (swap! globals/external-state-atom
    #(let [evts (get % :scroll-unique-cache #{})
           p1 (conj evts (_scroll-core mevt))
           p2 (if (< (count p1) 256) p1 (set (take 256 p1)))]
       (assoc % :scroll-unique-cache p2))))
(defn _inertial-scroll-guess-core? [evts]
  (let [g11 #(dec (* 2.0 (/ 1.0 (+ 1.0 (Math/exp (- %))))))
        mean #(/ (reduce + %) (max 1 (count %)))
        mean-f (fn [f] (mean (mapv f evts)))
        ky-only (fn [evt f & ks] (if (first (remove #(get evt %) ks)) 0.0
                                   (apply f (mapv #(get evt %) ks))))
        nunique-evt-sway (* 1.0 (g11 (- (count evts) 8)))
        precise-sway (* 6.0 (g11 (mean-f (fn [evt] (ky-only evt #(- (Math/abs (- %1 %2)) 0.25) :WheelRotation :PreciseWheelRotation)))))
        units-sway (* 0.5 (g11 (let [rots (mapv (fn [evt] (Math/abs (get evt :WheelRotation 0.0))) evts)
                                     diff (- (reduce max rots) (reduce min rots))]
                                 (- diff 2.0))))]
    ;(println "Evts:" evts)
    ;(println "Sways to inertial scroll:" nunique-evt-sway precise-sway units-sway)
    (boolean (> (+ nunique-evt-sway precise-sway units-sway) 0.0))))
(defn inertial-scroll-guess? []
  "Are we on an inertial scroll or not? Best guess. Keep calling this, later guesses should be more accurate."
  (let [evts (get @globals/external-state-atom :scroll-unique-cache #{})]
    (if (or *disable-inertial-scroll-mode* (empty? evts)) false (_inertial-scroll-guess-core? evts))))

;;;;;;;;;;;;;;;;;;;;; Platform dependent code ;;;;;;;;;;;;;;;;;;;;;;;

(if (and (not (mac?)) (not (windows?)) (not (linux?)))
  (warnbox/warning "Reclur is designed for win/max/linux. If this os is not in the big three it has not been tested."))

(def import-code
  (cond (or (java9?) (java10?) (java11?) (java12-19?))
    '(do
       (import java.awt.desktop.QuitHandler)
       (import java.awt.Desktop))
    (mac?)
    '(do
       (import com.apple.eawt.QuitHandler)
       (import com.apple.eawt.Application))
    :else false))

(eval import-code)

(defmacro handler-code [quit-listener!]
  (cond (or (java9?) (java10?) (java11?) (java12-19?) (mac?))
   `(proxy [QuitHandler] []
      (handleQuitRequestWith [e# response#]
        (~quit-listener! e#)
        (.cancelQuit response#)))
    :else false))

(defmacro quit-request-listener-code [quit-listener!]
  (cond (linux?) `(fn [ignore#] true)
    (and (or (java9?) (java10?) (java11?) (java12-19?)) (or (mac?) (windows?)))
    `(let [^QuitHandler handler# (handler-code ~quit-listener!)
           ^Desktop d# (Desktop/getDesktop)]
       (.setQuitHandler d# handler#) true)
    (mac?)
    `(let [^QuitHandler handler# (handler-code ~quit-listener!)
           ^Application mac-app# (Application/getApplication)]
        (.setQuitHandler mac-app# handler#) true)
    :else `(fn [ignore#] true)))

(defn add-quit-request-listener! [quit-listener!]
  "Quit handlers are confusing, and need to be tested on multiple os's.
   calls (quit-listener! e) and cancels the quit.
   The app then should add the quit request to it's event queue to handle it later."
    (try (quit-request-listener-code quit-listener!)
      (catch Exception e
        (println "App quit handler on this version not working. App may (or may not) need to be force-quit."))))

(defn fixed-width-font! [^Graphics2D g]
  "Why is the font so much bolder on the retina display?"
  (let [^Font font (if (mac?) (Font. "Monospaced" (int Font/PLAIN) (int 11))
                     (Font. "Monospaced" (int Font/BOLD) (int 11)))]
    (.setFont g font)))
