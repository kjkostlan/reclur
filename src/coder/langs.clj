; Different languages will be added eventually.

(ns coder.langs
  (:require [coder.clojure :as clojure]))

(def supported-langs
  {:clojure 
    {:depth clojure/depth ; quick hiliting.
     :reads-string clojure/reads-string ; like read string but better.
     :forwards clojure/forwards ; string -> x.
     :backwards clojure/backwards
     :locate-in clojure/locate-in
     :path-at clojure/path-at
     :tokenize clojure/leaf-tokenize
     }})