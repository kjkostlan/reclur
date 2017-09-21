; Code automatically evaled to the repl when the app starts.
; Useful to not lose the state when you restart the app.
; TODO: implement for the new version of the repl.
(ns clooj.repl_start)

(def code '(do

 ;(require '[clooj.app.claytree :as claytree] '[clooj.guinew.jui :as jui] '[clooj.app.claytreetree :as clatre] '[clooj.app.claytreewalk :as clwalk])
 (require '[clooj.guinew.gtest :as gtest])
 (require '[clooj.coder.blitcode :as blitcode])
 (require '[clooj.coder.rcode :as rcode])
 ;(require '[clooj.coder.refactor :as refactor])
 (require '[clooj.collections :as collections])
 (defn rl [] (require '[clooj.coder.lang.clojure :as clojurelang] :reload))
 (require '[clooj.app.gauitest.ctest :as ctest])
 (intern 'clojure.core 'debug (atom []))
))