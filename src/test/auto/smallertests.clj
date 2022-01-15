(ns test.auto.smallertests
  (:require [t]
    [clojure.test :as test]))

(defn fnargpack-test []
  (test/is
    (let [;        Test, argvecs, resultvecs
          triplets '[(fn [x] :a :out) [[x]] [:out]
                     (fn ([y] :out1) ([y1] :out2)) [[y] [y1]] [:out1 :out2]
                     (defn foo [z] :a :b) [[z]] [:b]
                     (defn foo "doc" ([w] :out1) ([v u] :out2)) [[w] [v u]] [:out1 :out2]
                     (def foo (fn [arg] :out)) [[arg]] [:out]
                     (def foo (fn ([arg] :out))) [[arg]] [:out]
                     (def foo (fn ([a1] (let [out (inc a1)] out)) ([a2] :fake {5 6 7 8}))) [[a1] [a2]] [(let [out (inc a1)] out) {5 6 7 8}]]

          n3 (count triplets)
          multi-get-in (fn [x phs] (mapv #(t/cget-in x %) phs))

          fn-codes (mapv #(nth triplets %) (range 0 n3 3))
          gold-argpacks (mapv #(nth triplets %) (range 1 n3 3))
          gold-result-forms (mapv #(nth triplets %) (range 2 n3 3))

          green-argpacks (mapv #(multi-get-in % (coder.cnav/fnargpack-paths %)) fn-codes)
          green-result-forms (mapv #(multi-get-in % (coder.cnav/fnresult-paths %)) fn-codes)]
      (boolean (and (= gold-argpacks green-argpacks) (= gold-result-forms green-result-forms))))))

