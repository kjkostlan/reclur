; Different languages will be added eventually.

(ns coder.langs
  (:require [coder.clojure :as clojure]))

(def supported-langs
  {:clojure 
    {:depth clojure/depth :tokenize clojure/grouped-tokenize}})