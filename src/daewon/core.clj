(ns daewon.core (:gen-class))

(defmacro _mcons [a lb] `(list ~a ~@lb))
(defn mcons [a lb] (cons a lb))

(defn minterleave [as bs]
  (when-not (empty? as)
    (let [[h & r] as]
      (mcons h (minterleave bs r)))))

(minterleave [1 3 5] [2 4 6 7])

(defn mmap [f coll]
  (when-not (empty? coll)
    (let [[x & xs] coll]
      (mcons (f x) (mmap f xs)))))

(defmacro mmap2 [f coll]
  `(for [i# ~coll] (~f i#)))

(defn mconcat2 [la lb]
  (let [acc (atom [])]
    (doseq [x (rseq lb)] (reset! acc (mcons x @acc)))
    (doseq [x (rseq la)] (reset! acc (mcons x @acc)))
    @acc))

(defn mconcat [la lb]
  (let [[h & r] la]
    (if (sequential? la)
      (mcons h (mconcat r lb))
      lb)))

(defmacro _mconcat [la lb] `(list ~@la ~@lb))
(defn mconcat3 [la lb] (eval `(_mconcat ~la ~lb)))

"
P01 (*) Find the last box of a list.
    Example:
    * (mlast '(a b c d))
    (D)
"
(defn mlast [coll]
  (let [[head & rest] coll]
    (if (empty? rest)
      head
      (mlast rest))))

(defn mlast2 [coll]
  (first (drop (- (count coll) 1) coll)))

(defn mlast3 [coll]
  (let [n (- (count coll) 1)]
    (->> coll (drop n) first)))

(defn mlast3 [coll]
  (let [n (- (count coll) 1)]
    (->> coll (drop n) first)))

(defn mlast4 [coll]
  (reduce (fn [a b] (identity b)) coll))

(mlast [1 2 3 4]) 
(mlast2 [1 2 3 4])
(mlast3 [1 2 3 4])
(mlast4 [1 2 3 4])

"
P02 (*) Find the last but one box of a list.
    Example:
    * (mbut-last '(a b c d))
    (A B C)
"
(defn mbut-last [coll]
  (let [[f & r] coll]
    (when-not (empty? r)
      (lazy-seq (cons f (mbut-last r))))))

(defn mbut-last2 [coll]
  (take (- (count coll) 1) coll))

(mbut-last [1 2 3 4])
(mbut-last2 [1 2 3 4])

"
P03 (*) Find the K'th element of a list.
The first element in the list is number 0.
"
(defn mnth [coll n]
  (let [[h & r] coll]
    (if (= n 0)
      h
      (mnth r (dec n)))))

(defn mnth2 [coll n]
  (first (drop n coll)))

(mnth [1 2 3] 1)
(mnth2 [1 2 3] 1)

"  
P04 (*) Find the number of elements of a list.)
"
(defn mlength [coll]
  (let [[h & r] coll]
    (if (empty? r)
      1
      (+ 1 (mlength r)))))

(defn mlength2 [coll]
  (reduce (fn [a b] (+ 1 a)) coll))

(mlength [1 2 3])
(mlength2 [1 2 3])

"
P05 (*) Reverse a list.)
"
(defn mreverse [coll]
  (let [[h & r] coll]
    (if (empty? r)
      [h]
      (conj (mreverse r) h))))

(defn mreverse2 [coll]
  (let [[h & r] coll]
    (if (empty? r)
      [h]
      (conj (mreverse2 r) h))))

(mreverse [1 2 3])

"
P06 (**) Flatten a nested list structure.
Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
"
(defn mflatten [coll]
  (when-not (empty? coll)
    (let [[h & r] coll]      
      (if (sequential? h)
        (mconcat (mflatten h) (mflatten r))
        (mcons h (mflatten r))))))

(mflatten '(:a :b (:c) :d :e (:f :g (:h (:i))) :j))

"
P08 (**) Eliminate consecutive duplicates of list elements.
If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
Example:
 (compress '(a a a a b c c a a d e e e e))
 (A B C A D E)
"
(defn has? [n coll] (some (fn [a] (= a n)) coll))
(defn mcompress [coll]
  (when-not (empty? coll)
    (let [[h & r] coll]
      (if (= h (first r))
        (mcompress r)
        (mcons h (mcompress r))))))

(mcompress '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e))

"
P09 (**) Pack consecutive duplicates of list elements into sublists.
If a list contains repeated elements they should be placed in separate sublists.

Example:
* (pack '(a a a a b c c a a d e e e e))
 ((A A A A) (B) (C C) (A A) (D) (E E E E))
"
(defn span [coll]
  (cond 
   (empty? coll) nil
   :else (list (mtake-while #(= % (first coll)) coll)
               (mdrop-while #(= % (first coll)) coll))))

(defn span1 [a coll acc]
  (cond 
   (= a (first coll)) (span1 (first coll) (rest coll) (cons a acc))
   :else (list acc coll)))

(defn mtake-while [pred coll]
  (lazy-seq (let [[h & r] coll]
    (when (pred h) (cons h (mtake-while pred r))))))

(defn mdrop-while [pred coll]
  (when-not (empty? coll)
    (let [[h & r] coll]
    (if (pred h)
      (mdrop-while pred r)
      (cons h r)))))
        
(defn mpack [coll]
  (when-not (empty? coll)
    (let [[h & _r] (span coll)
          r (first _r)]
      (cons h (mpack r)))))

(mpack '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e))
(mmap #(list (count %) (first %)) (mpack '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e)))

(defn -main [& args] (println "Hello, World!"))

