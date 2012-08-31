(ns daewon.core-test
  (:use clojure.test)
  (:use daewon.core))

(prn "99 clojure problems")

(deftest test-my-last
  (is (= (my-last [:a :b :c]) :c))
  (is (= (my-last [:d]) :d))
  (is (= (my-last []) nil)))

(deftest test-my-but-last
  (is (= (my-but-last [:a :b :c]) [:a :b]))
  (is (= (my-but-last []) nil))
  (is (= (my-but-last [:a]) nil)))

(deftest test-my-element-at
  (is (= (my-element-at [:a :b :c] 0) :a))
  (is (= (my-element-at [:a :b :c] 1) :b))
  (is (= (my-element-at [:a :b :c] 2) :c)))

(deftest test-my-count
  (is (= (my-count [:a :b :c]) 3))
  (is (= (my-count []) 0)))

(deftest test-my-reverse
  (is (= (my-reverse []) []))
  (is (= (my-reverse [:a :b :c]) [:c :b :a])))

(deftest test-my-palindrome?
  (is (= (my-palindrome? [:a :b :a]) true))
  (is (= (my-palindrome? [:a :b :c]) false))
  (is (= (my-palindrome? "daead") true)  true))

(deftest test-my-flatten
  (is (= (my-flatten [:a :b [:c :d [:e [:f] :g] :h [:i]] :j])
         [:a :b :c :d :e :f :g :h :i :j])))

(deftest test-my-compress
  (is (= (my-compress [:a :a :b :b :c :c :c :c :d :e])
         [:a :b :c :d :e])))

(deftest test-my-span
  (is (= (my-span []) []))
  (is (= (my-span [:a :a :b]) [[:a :a] [:b]])))

(deftest test-my-pack
  (is (= (my-pack [:a, :a, :a, :a, :b, :c, :c, :a, :a, :d, :e, :e, :e, :e])
         [[:a, :a ,:a, :a], [:b], [:c, :c], [:a, :a], [:d], [:e, :e, :e, :e]])))

(deftest test-my-encode-run-length
  (is (= (my-encode-run-length [:a, :a, :a, :a, :b, :c, :c, :a, :a, :d, :e, :e, :e, :e])
         [[4 :a], [1 :b], [2 :c], [2 :a], [1 :d], [4 :e]])))

(deftest test-my-encode-run-length-modified
  (is (= (my-encode-run-length-modified [:a, :a, :a, :a, :b, :c, :c, :a, :a, :d, :e, :e, :e, :e])
         [[4 :a], :b, [2 :c], [2 :a], :d, [4 :e]])))

(deftest test-my-decode-run-length
  (let [ls [:a, :a, :a, :a, :b, :c, :c, :a, :a, :d, :e, :e, :e, :e]
        encoded (my-encode-run-length ls)]
    (is (= ls (my-decode-run-length encoded)))))

(deftest test-my-encode-run-length-modified-direct
  (is (= (my-encode-run-length-modified-direct [:a, :a, :a, :a, :b, :c, :c, :a, :a, :d, :e, :e, :e, :e])
         [[4 :a], :b, [2 :c], [2 :a], :d, [4 :e]]))

  (is (= (my-encode-run-length-modified-direct [:a, :a, :a, :a, :b, :c, :c, :a, :a, :d, :e, :e, :e, :e])
         (my-encode-run-length-modified [:a, :a, :a, :a, :b, :c, :c, :a, :a, :d, :e, :e, :e, :e]))))

(deftest test-my-dupli
  (is (= [:a :a :b :b :c :c] (my-dupli [:a :b :c])))
  (is (= [:a :a :a :b :b :b :c :c :c] (my-dupli [:a :b :c] 3))))

(deftest test-my-drop-nth
  (is (=  [:a, :b, :d, :e, :g, :h, :k]
          (my-drop-nth [:a, :b, :c, :d, :e, :f, :g, :h, :i, :k] 3))))

(deftest test-my-split
  (is (= [[:a, :b, :c], [:d, :e, :f, :g, :h, :i, :k]]
         (my-split [:a, :b, :c, :d, :e, :f, :g, :h, :i, :k] 3))))

(deftest test-my-slice
  (is (= [:c, :d, :e, :f, :g]
         (my-slice [:a, :b, :c, :d, :e, :f, :g, :h, :i, :k] 3 7))))

(deftest test-my-rotate
  (is (= [:d :e :f :g :h :a :b :c]
         (my-rotate '(:a :b :c :d :e :f :g :h) 3)))
  
  (is (= [:g :h :a :b :c :d :e :f]
         (my-rotate '(:a :b :c :d :e :f :g :h) -2))))

(deftest test-my-remove-at
  (is (= [:a :b :d]
         (my-remove-at '(:a :b :c :d) 2))))

(deftest test-my-insert-at
  (is (= [:T :a :b :c :d]
         (my-insert-at 0 :T [:a :b :c :d])))

  (is (= [:a :T :b :c :d]
         (my-insert-at 1 :T [:a :b :c :d])))

  (is (= [:a :b :T :c :d]
         (my-insert-at 2 :T [:a :b :c :d]))))

(deftest test-my-range
  (is (= (range 10) (my-range 10)))
  (is (= (range 2 10) (my-range 2 10))))

(deftest test-my-rand-select
  "how to test it?"
  (my-rand-select [1 2 3 4 5] 3))

(deftest test-my-lotto
  "how to test it?"
  (my-lotto 6 46))

(deftest test-my-rand-perm
  "how to test it?"
  (my-rand-perm [1 2 3 4 5]))

(deftest test-my-comb
  (is (= [[1 2] [1 3] [2 3]] (my-comb [1 2 3] 2)))
  (is (= [] (my-comb [1 2 3] 4)))
  (is (= 220 (count (my-comb [1 2 3 4 5 6 7 8 9 10 11 12] 3)))))

