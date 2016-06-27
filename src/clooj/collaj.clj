(ns clooj.collaj
  (:require [clojure.edn :as edn])
  (:import (java.net URLEncoder)))

(defn url-encode
  "URL-encode a string."
  [s]
  (URLEncoder/encode s "UTF-8"))
  
(defn raw-data
  "Get a clojure data collection of raw search
   results from collaj.net, a clojure code repository."
  [terms]
  (edn/read-string (slurp (str "http://collaj.net/?format=raw&q="
                               (url-encode terms)))))

