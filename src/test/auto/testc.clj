; Tests c, which is our extra collections functions.
(ns test.auto.testc
  (:require [c]
    [clojure.test :as test]))

(defn _= [x y] (and (= (type x) (type y)) (= x y)))

(test/deftest foundations ;Each function here tests different sections of c. Here we test the simplest functions such as c/third.
  (test/are [x y] (_= x y)
    (c/third (list :a :b :c)) :c
    (c/third [:a :b]) nil
    (c/fourth [10 20 30 40 50]) 40
    (c/evens (seq [:even0 :odd0 [:even1] :odd1])) [:even0 [:even1]]
    (c/odds [:even0 :odd0 [:even1] :odd1]) [:odd0 :odd1]
    (let [q (c/queue) q1 (conj q 123)] (peek q1)) 123
    (let [q (-> (c/queue) (conj :first) (conj :second))] (peek q)) :first
    (let [q (-> (c/queue) (conj :first) (conj :second) pop)] (peek q)) :second
    (let [q (-> (c/queue) (conj :first) (conj :second) pop pop)] (peek q)) nil))

(test/deftest small-extractions ;All the ways to generalize get, contains?, peek
  (test/are [x y] (_= x y)
    (c/cget [:a :b :c] 1) :b
    (c/cget (list :a :b :c) 1) :b
    (c/cget [:a :b :c] -1) nil
    (c/cget (list :a :b :c) 4) nil
    (c/cget [:a :b :c :d] 2) :c
    (c/cget (list :a :b :c :d) 2) :c
    (c/cget {0 :a 1 :b 2 :c 3 :d} 2) :c
    (c/cget #{:a :b :c :d} :c) :c
    (c/cpeek [:a :b :c]) :c
    (c/cpeek (list :a :b :c)) :a
    (c/cpeek (range 10)) 0
    (c/cpeek {1 2 3 4}) [1 2]
    (c/get- [:one :two :three :four] -2) :three
    (c/get- [:one :two :three :four] 0) nil
    (c/get- [:one :two :three :four] -999 :notfound) :notfound
    (c/ccontains? [:a :b :c] 2) true
    (c/ccontains? [:a :b :c] :a) false
    (c/ccontains? {:a :b :c :d} :a) true
    (c/ccontains? (list :a :b :c) 2) true
    (c/cfind (list :a :b :c) :a) nil
    (c/cfind (list :a :b :c) 0) [0 :a]
    (c/juice-1 #(if (> % 3) (- % 3) false) [0 2 0 15 0]) 12
    (c/cfilter-1 #(> (count (str %)) 5) [:a :app :apple :apples]) :apple))

(test/deftest small-modifications ;All the ways to generalize assoc, update, conj, etc.
  (test/are [x y] (_= x y)
    (c/cpop [1 2 3]) [1 2]
    (c/cpop (list 1 2 3)) (list 2 3)
    (or (= (c/cpop #{1 2 3}) #{1 2}) (= (c/cpop #{1 2 3}) #{2 3}) (= (c/cpop #{1 2 3}) #{1 3})) true
    (c/cassoc [1 2 3] 0 10) [10 2 3]
    (c/cassoc (list 1 2 3) 0 10) (list 10 2 3)
    (c/cassoc {1 2 3 4} 0 10) {0 10 1 2 3 4}
    (c/cassoc #{1 2 3 4} 10 10) #{10 1 2 3 4}
    (c/cassoc #{1 2 3 4} 1 100) #{100 2 3 4}
    (c/cupdate [1 2 3] 1 inc) [1 3 3]
    (c/cupdate (list 1 2 3) 1 + 100) (list 1 102 3)
    (sort (vals (c/cconj {:a 1 :b 2} 3))) (sort (list 1 2 3))
    (c/entry-conj {:a [1 2 3] :b [4 5 6]} :a 4) {:a [1 2 3 4] :b [4 5 6]}
    (c/cdissoc {:a :b :c :d} :a) {:c :d}
    (c/cdissoc #{:a :b :c :d} :a) #{:b :c :d}
    (c/cdissoc #{:a :b :c :d} :a :c) #{:b :d}
    (c/cdissoc [1 2 3 4] 0) [nil 2 3 4]
    (c/cdissoc [1 2 3 4] 1) [1 nil 3 4]
    (c/cdissoc (list 1 2 3 4) 1) (list 1 nil 3 4)
    (c/assoc- [1 2 3 4] -1 40) [1 2 3 40]
    (c/update- [10 20 30 40] -1 dec) [10 20 30 39]
    (c/update- [10 20 30 40] -2 dec) [10 20 29 40]
    (c/update- [10 20 30 40] -3 + 12 12) [10 (+ 20 12 12) 30 40]
    (c/update- (list 10 20 30 40) -2 dec) (list 10 20 29 40)))

(test/deftest large-extractions ; Filter, map, etc
  (test/are [x y] (_= x y)
    (c/cselect-keys {:a 1 :b 2} [:a]) {:a 1}
    (c/cselect-keys [10 20 30 40] [1 2]) [20 30]
    (c/cselect-keys #{10 20 30 40} [10 20]) #{10 20}
    (into [] (c/ckeys [1 2 3])) [0 1 2]
    (into [] (c/ckeys (list 1 2 3))) [0 1 2]
    (into [] (c/ckeys {1 2 3 4})) [1 3]
    (into [] (sort (c/ckeys #{1 2 3 4}))) [1 2 3 4]
    (into [] (c/cvals [1 2 3])) [1 2 3]
    (into [] (c/cvals (list 1 2 3))) [1 2 3]
    (into [] (c/cvals {1 2 3 4})) [2 4]
    (into [] (sort (c/cvals #{1 2 3 4}))) [1 2 3 4]
    (c/cfilter #(> % 10) (list 10 20 30)) (list 20 30)
    (c/cfilter #(> % 10) (list 10 -20 30)) (list 30)
    (c/cfilter #(> % 10) (list 10 -20 -30)) (list)
    (c/cfilter #(> % 10) [10 20 30]) [20 30]
    (c/cfilter #(> % 10) {:a 10 :b 20 :c 30}) {:b 20 :c 30}
    (c/cfilter #(> % 10) #{10 20 30}) #{20 30}
    (c/wfilterv #(> % 10) [5 15 5 15 15] [:a :b :c :d :e]) [:b :d :e]
    (c/filter-kv (fn [k v] (or (= k :pass) (> v 10))) {:a 5 :b 15 :c 5 :d 15 :pass 3}) {:b 15 :d 15 :pass 3}
    (c/where #(> % 10) [0 100 100 0]) (list 1 2)
    (c/where #(> % 10) (list 0 100 100 0)) (list 1 2)))

(test/deftest reduction-fns ; Reduce and = and variants of.
  (test/are [x y] (_= x y)
    (c/reduce-unchecked + [1 2 3]) 6
    (c/reduce-unchecked + (list 1 2 3 4)) 10
    (= [1 2 3] (list 1 2 3)) true ; Why we need c/c=.
    (c/c= [1 2 3] (list 1 2 3)) false
    (c/c= (list 1 2 3) (list 1 2 3)) true
    (c/c= (list 1 [2] 3) (list 1 [2] 3)) true
    (c/c= (list 1 [2] 3) (list 1 (list 2) 3)) false))

(test/deftest large-modify-fns
  (test/are [x y] (_= x y)
    (c/vs2m [1 2 3 4]) {0 1 1 2 2 3 3 4}
    (c/vs2m (list 1 2 3 4)) {0 1 1 2 2 3 3 4}
    (c/vs2m #{1 2 3 4}) {1 1 4 4 3 3 2 2}
    (c/vcat [1 2 3] [10 20 30]) [1 2 3 10 20 30]
    (c/vcat (list 1 2 3) [10 20 30]) [1 2 3 10 20 30]
    (c/vcat (list 1 2 3) (list 10 20 30)) [1 2 3 10 20 30]
    (c/vcat [1 2 3] (list 10 20 30)) [1 2 3 10 20 30]
    (c/cmap inc [1 2 3]) [2 3 4]
    (c/cmap inc (list 1 2 3)) (list 2 3 4)
    (c/cmap #(inc (second %)) {:a 1 :b 2}) {:a 2 :b 3}
    (c/cmap inc #{1 2}) #{2 3}
    (c/cmap #(int (/ % 2)) #{1 2 3 4}) #{0 1 2}
    (c/rmap inc [1 2 3]) [2 3 4]
    (c/rmap inc (list 1 2 3)) (list 2 3 4)
    (c/rmap inc {10 1 20 2}) {11 2 21 3}
    (c/rmap inc #{1 2}) #{2 3}
    (c/rmap #(int (/ % 2)) #{1 2 3 4}) #{0 1 2}
    (c/vmap inc [1 2 3]) [2 3 4]
    (c/vmap inc (list 1 2 3)) (list 2 3 4)
    (c/vmap inc {:a 1 :b 2}) {:a 2 :b 3}
    (c/vmap inc #{1 2}) #{2 3}
    (c/vmap #(int (/ % 2)) #{1 2 3 4}) #{0 1 2}
    (c/creverse [:a :b :c :d]) [:d :c :b :a]
    (c/creverse (list :a :b :c :d)) (list :d :c :b :a)
    (c/creverse {:a :b :c :d}) {:a :b :c :d}
    (c/creverse #{:a :b :c :d}) #{:a :b :c :d}
    (c/cmerge [1 2 3] [4 5 6]) [4 5 6]
    (c/cmerge {:a "foo" :b "bar"} [4 5 6]) {:a "foo" :b "bar" 0 4 1 5 2 6}
    (try (c/cmerge [4 5 6] {:a "foo" :b "bar"}) (catch java.lang.ClassCastException e true)) true
    (c/cmerge {:a "foo" :b "bar"} [4 5 6] (list 7 8)) {:a "foo" :b "bar" 0 7 1 8 2 6}
    (c/cmerge-with + {:a 1 :b 2} {:a 9 :b 98 :c 0}) {:c 0, :a 10, :b 100}
    (c/cmerge-with + {0 10 1 20 2 30} [100]) {0 110 1 20 2 30}
    (c/cdifference {:a 1 :b 2 :c 3} #{:c :d}) {:a 1 :b 2}
    (c/cdifference [:a :b :c] [2]) [nil :b :c]
    (c/cintersection {:a 1 :b 2 :c 3} #{:c :d}) {:c 3}
    (c/cintersection [:a :b :c] [2]) [:a]
    (into [] (c/to-indexed-seqs [:a :b :c])) [[0 :a] [1 :b] [2 :c]]))
