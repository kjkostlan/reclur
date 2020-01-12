; Put the cross-platform and cross-version (java 8 and 9) code here, supporting below java 8 is not planned unless a significant market need appears.
; Note: converting files to / and \n or other code that doesn't query operating systems, even if it normalizes inputs, doesn't go here.
; This cross-platform file should handle down to Java 8.

(ns crossplatform.cp
  (:require [javac.warnbox :as warnbox]))

(def ^String os-name (.toLowerCase ^String (System/getProperty "os.name")))

(defn windows? []
  (.contains os-name "win"))

(defn mac? []
  (.contains os-name "mac"))
  
(defn linux? []
  (or (.contains os-name "linux") (.contains os-name "unix") (.contains os-name "sunos")))

(defn java8? []
  (let [^String ver (System/getProperty "java.version")]
    (or (.startsWith ver "1.8") (.startsWith ver "8."))))

(defn java9? []
  (let [^String ver (System/getProperty "java.version")]
    (or (.startsWith ver "1.9") (.startsWith ver "9."))))

; TODO: test the program on all three main PC, and mobile?
(if (and #_(not (java9?)) (not (mac?)))
  (warnbox/warning "Reclur hasn't been tested on this platform, use at your own risk."))

(def import-code
  (cond (java9?)
    '(do 
       (import java.awt.desktop.QuitHandler)
       (import java.awt.Desktop))
    (mac?)
    '(do 
       (import com.apple.eawt.QuitHandler)
       (import com.apple.eawt.Application))
    :else '(do (warnbox/warning "No save-on-quit protection is implemented (TODO) for this platform, save your work often.") false)))

(eval import-code)

(defmacro handler-code [quit-listener!]
  (cond (or (java9?) (mac?))
   `(proxy [QuitHandler] []
      (handleQuitRequestWith [e# response#]
        (~quit-listener! e#)
        (.cancelQuit response#)))
    :else false))

(defmacro quit-request-listener-code [quit-listener!]
  (cond (java9?)
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
    (quit-request-listener-code quit-listener!))