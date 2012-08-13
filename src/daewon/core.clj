(ns daewon.core (:gen-class))

(defmacro mcons-macro [a lb]`(list ~a ~@lb))

(def mcons cons) ;; how to implement cons?

(defn mfirst [ls] (let [[h & r] ls] h))

(defn mrest [ls] (let [[h & r] ls] r))

(defn mrepeat [cnt item] (take cnt (lazy-seq (cons item (mrepeat cnt item)))))

(defn mreduce [f acc ls]
  (cond (empty? ls) acc
        :else (let [[h & r] ls
                    ret (apply f [acc h])]
                ret (mreduce f ret r))))

(defn minterleave [as bs]
  (when-not (empty? as)
    (let [[h & r] as]
      (mcons h (minterleave bs r)))))

(minterleave [1 3 5] [2 4 6 7])

(defn mmap [f ls]
  (when-not (empty? ls)
    (let [[x & xs] ls]
      (mcons (f x) (mmap f xs)))))

(defmacro mmap-macro [f ls]
  `(for [i# ~ls] (~f i#)))

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

(defmacro mconcat-macro [la lb] `(list ~@la ~@lb))
(defn mconcat3 [la lb] (eval `(mconcat-macro ~la ~lb)))

(defn mtake-while [pred ls]
  (lazy-seq (let [[h & r] ls]
              (when (pred h) (mcons h (mtake-while pred r))))))

(defn mdrop-while [pred ls]
  (when-not (empty? ls)
    (let [[h & r] ls]
      (if (pred h)
        (mdrop-while pred r)
        (mcons h r)))))
"
P01 (*) Find the last box of a list.
    Example:
    * (mlast '(a b c d))
    (D)
"
(defn mlast [ls]
  (let [[head & r] ls]
    (if (empty? r)
      head
      (mlast r))))

(defn mlast2 [ls]
  (mfirst (drop (- (count ls) 1) ls)))

(defn mlast3 [ls]
  (let [n (- (count ls) 1)]
    (->> ls (drop n) first)))

(defn mlast3 [ls]
  (let [n (- (count ls) 1)]
    (->> ls (drop n) first)))

(defn mlast4 [ls]
  (reduce (fn [a b] (identity b)) ls))

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
(defn mbut-last [ls]
  (let [[f & r] ls]
    (when-not (empty? r)
      (lazy-seq (cons f (mbut-last r))))))

(defn mbut-last2 [ls]
  (take (- (count ls) 1) ls))

(mbut-last [1 2 3 4])
(mbut-last2 [1 2 3 4])

"
P03 (*) Find the K'th element of a list.
The first element in the list is number 0.
"
(defn mnth [ls n]
  (let [[h & r] ls]
    (if (= n 0)
      h
      (mnth r (dec n)))))

(defn mnth2 [ls n]
  (first (drop n ls)))

(mnth [1 2 3] 1)
(mnth2 [1 2 3] 1)

"  
P04 (*) Find the number of elements of a list.)
"
(defn mcount [ls]
  (let [[h & r] ls]
    (if (empty? r)
      1
      (+ 1 (mcount r)))))

(defn mcount2 [ls]
  (reduce (fn [a b] (+ 1 a)) ls))

(mcount[1 2 3])
(mcount2 [1 2 3])

"
P05 (*) Reverse a list.)
"
(defn mreverse [ls]
  (let [[h & r] ls]
    (if (empty? r)
      [h]
      (conj (mreverse r) h))))

(defn mreverse2 [ls acc]
  (cond 
   (empty? ls) acc
   :else (let [[h & r] ls]
           (mreverse2 r (mcons h acc)))))

(mreverse2 [1 2 3 4 5] [])

"
P06 (**) Flatten a nested list structure.
Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
"
(defn mflatten [ls]
  (when-not (empty? ls)
    (let [[h & r] ls]      
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
(defn has? [n ls] (some (fn [a] (= a n)) ls))
(defn mcompress [ls]
  (when-not (empty? ls)
    (let [[h & r] ls]
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
(defn span [ls]
  (cond 
   (empty? ls) nil
   :else (list (mtake-while #(= % (first ls)) ls)
               (mdrop-while #(= % (first ls)) ls))))

(defn span1 [a ls acc]
  (cond 
   (= a (mfirst ls)) (span1 (mfirst ls) (mrest ls) (mcons a acc))
   :else (list acc ls)))

(defn mpack [ls]
  (when-not (empty? ls)
    (let [[h & _r] (span ls)
          r (first _r)]
      (cons h (mpack r)))))

(mpack '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e))

(defn encode-run-length [ls]
  (mmap #(list (mcount %) (mfirst %)) (mpack ls)))

(encode-run-length '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e))

(defn encode-run-length-modified [ls]
  (mmap #(if (= 1 (mcount %))
           (mfirst %)
           (list (mcount %) (mfirst %))) (mpack ls)))

(encode-run-length-modified '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e))

(defn decode-run-length [ls]
  (when-not (empty? ls)
    (let [[h & r] ls
          len (first h)
          item (last h)]
      (mconcat (mrepeat len item) (decode-run-length r)))))

(defn decode-run-length-modified [ls]
  (when-not (empty? ls)
    (let [[h & r] ls]
      (if (sequential? h)
        (mconcat (mrepeat (first h) (last h)) (decode-run-length-modified r))
        (cons h (decode-run-length-modified r))))))

(decode-run-length-modified (encode-run-length-modified '(:a :a :a :a :b :c :c :a :a :d :e :e :e :e)))

(defn mreplicate [ls n]
  (when-not (empty? ls)
    (let [[h & r] ls]
      (mconcat (mrepeat n h) (mreplicate r n)))))

(mreplicate [1 2 3] 3)

(defn msplit [_n _ls]
  (letfn [(ms [n ls left]
            (when-not (empty? ls)
              (let [[h & r] ls]
                (if (= n 0)
                  `(~left ~ls)
                  (ms (dec n) r (concat left [h]))
                  ))))]
    (ms _n _ls [])))

(msplit 3 [1 2 3 4 5 6 7 8 9 10])

(defn msplit2 [n ls]
  (list (take n ls) (drop n ls)))

(msplit2 3 [1 2 3 4 5 6 7 8 9 10])

(defn mslice [ls s e];
  (when-not (empty? ls)
    (let [[h & r] ls]
      (cond
       (> e 1) (if (> s 1)
                 (mslice r (dec s) (dec e)) ;; drop previous
                 (cons h (mslice r s (dec e)))) 
       :else `(~h)))))

(mslice [1 2 3 4 5] 2 3)
(mslice '(:a :b :c :d :e :f :g :h :i :k) 3 7)


(defn mrotate [_ls _n]
  (letfn [(ms [n ls left]
            (when-not (empty? ls)
              (let [[h & r] ls]
                (if (= n 0)
                  (concat ls left)
                  (ms (dec n) r (concat left [h]))
                  ))))]
    (if (> _n 0)
      (ms _n _ls [])
      (ms (- (count _ls) (* -1 _n)) _ls [])
      )))

(mrotate '(:a :b :c :d :e :f :g :h) 3)
;;(D E F G H A B C)

(mrotate '(:a :b :c :d :e :f :g :h) -2)
;;(G H A B C D E F)

(defn mremove-at [n ls]
  (when-not (empty? ls)
    (let [[h & r] ls]
      (if (= n 0)
        (mremove-at (dec n) r)
        (cons h (mremove-at (dec n) r))))))

(mremove-at 2 [1 2 3 4 5 6])

(defn minsert-at [n item ls]
  (when-not (empty? ls)
    (let [[h & r] ls]
      (if (= n 0)
        (cons h (cons item  (minsert-at (dec n) item r)))
        (cons h (minsert-at (dec n) item r))))))

(minsert-at 2 0 [1 2 3 4 5 6])

(defn mrange
  ([e] (mrange 1 e))
  ([s e]
     (if (not= e s)
       (cons s (mrange (inc s) e))
       `(~s))))

(mrange 4 9)
(mrange 10)

(defn mrand-select [coll n]
  (when-not (empty? coll)
  (let [p (rand (count coll))
        el (nth coll p)]
    (cons el (mrand-select (rest coll) n)))))

(mrand-select [:a :b :c :d :e :f :g :h :i] 3)

(defn -main [& args] (println "Hello, World!"))

