(ns daewon.core (:gen-class))
(require '[clojure.string :as str])

;; P-99: Ninety-Nine Prolog Problems
;; https://sites.google.com/site/prologsite/prolog-problems/

(defn my-last [[x & xs]]
  "1.01 (*) Find the last element of a list."
  (if (empty? xs) x
      (recur xs)))

(defn my-but-last [[x & xs]]
  "1.02 (*) Find the last but one element of a list."
  (when-not (empty? xs)
    (cons x (my-but-last xs))))

(defn my-element-at [xss n]
  "1.03 (*) Find the K'th element of a list."
  (when-let [[x & xs] xss]
    (cond (zero? n) x
          :else (my-element-at xs (dec n)))))

(defn my-count [yss]
  "1.04 (*) Find the number of elements of a list."
  (loop [[x & xs :as xss] yss, cnt 0]
    (cond (empty? xss) cnt
          :else (recur xs (inc cnt)))))

(defn my-reverse [[x & xs :as xss]]
  "1.05 (*) Reverse a list."
  (cond (empty? xss) []
        :else (conj (my-reverse xs) x)));

(defn my-palindrome? [xss]
  "1.06 (*) Find out whether a list is a palindrome."
  (let [seq-xss (seq xss)] (= (reverse seq-xss) seq-xss)))

(defn my-flatten [[x & xs :as xss]]
  "1.07 (**) Flatten a nested list structure."
  (cond (empty? xss) nil
        (sequential? x) (concat (my-flatten x) (my-flatten xs))
        :else (cons x (my-flatten xs))))

(defn my-compress [[x & xs :as xss]]
  "1.08 (**) Eliminate consecutive duplicates of list elements."
  (cond (empty? xss) nil
        (= x (first xs)) (my-compress xs)
        :else (cons x (my-compress xs))))

(defn my-span [xss]
  "span list"
  (loop [[x & xs] xss, acc []]
    (cond
     (empty? xss) []
     (= x (first xs)) (recur xs (cons x acc))
     :else (list (cons x acc) xs))))

(defn my-pack [xss]
  "1.09 (**) Pack consecutive duplicates of list elements into sublists."
  (when-not (empty? xss)
    (let [[h r & _] (my-span xss)]
      (cons h (my-pack r)))))

(defn my-encode-run-length [xss]
  "1.10 (*) Run-length encoding of a list."
  (map #(list (count %) (first %)) (my-pack xss)))

(defn my-encode-run-length-modified [xss]
  "1.11 (*) Modified run-length encoding."
  (map #(if (= 1 (count %))
          (first %)
          [(count %) (first %)]) (my-pack xss)))

(defn my-decode-run-length [xss]
  "1.12 (**) Decode a run-length encoded list."  
  (when-not (empty? xss)
    (let [[x & xs] xss, len (first x), item (last x)]
      (concat (repeat len item) (my-decode-run-length xs)))))

(defn my-decode-run-length-modified [xss]
  (when-let [[x & xs] xss]
    (if (sequential? x)
      (concat (repeat (first x) (last x)) (my-decode-run-length-modified xs))
      (cons x (my-decode-run-length-modified xs)))))

(defn my-encode-run-length-modified-direct [[x & xs :as xss]]
  "1.13 (**) Run-length encoding of a list (direct solution)."
  (cond (empty? xss) nil
        (not= x (first xs)) (cons x (my-encode-run-length-modified-direct xs))
        :else (let [[a b & _] (my-span xss), size-a [(count a) x]]
                (cons size-a (my-encode-run-length-modified-direct b)))))

(defn my-dupli 
  "1.14 (*) Duplicate the elements of a list. 1.15 (**) Duplicate the elements of a list a given number of times."
  ([xss] (my-dupli xss 2))
  ([[x & xs :as xss] n]
     (when-not (empty? xss)
       (concat (repeat n x) (my-dupli xs n)))))

(defn my-drop-nth [yss n]
  "1.16 (**) Drop every N'th element from a list."
  (loop [[x & xs :as xss] yss, idx 1, acc []]
    (cond (empty? xss) acc
          (zero? (mod idx n)) (recur xs (inc idx), acc)
          :else (recur xs (inc idx) (conj acc x)))))

(defn my-split [yss n]
  "1.17 (*) Split a list into two parts; the length of the first part is given."
  (loop [[x & xs :as xss] yss, idx 0, acc []]
    (cond (= n idx) [acc xss]
          :else (recur xs (inc idx) (conj acc x)))))

(defn my-slice [yss s e]
  "1.18 (**) Extract a slice from a list."
  (loop [[x & xs] yss, idx 1, acc []]
    (cond (> idx e) acc
          (< idx s) (recur xs (inc idx) acc)          
          :else (recur xs (inc idx) (conj acc x)))))

(defn my-rotate [xss n]
  "1.19 (**) Rotate a list N places to the left."
  (let [n-split (if (> n 0) n (+ (count xss) n))
        [left right & _] (my-split xss n-split)]
    (concat right left)))

(defn my-remove-at [yss n]
  "1.20 (*) Remove the K'th element from a list."
  (when-let [[x & xs :as xss] yss]
    (if (= n 0)
      (my-remove-at xs (dec n))
      (cons x (my-remove-at xs (dec n))))))

(defn my-insert-at [n item yss]
  "1.21 (*) Insert an element at a given position into a list."
  (when-let [[x & xs] yss]
    (if (= n 0)
      (cons item (cons x (my-insert-at (dec n) item xs)))
      (cons x (my-insert-at (dec n) item xs)))))

(defn my-range
  "1.22 (*) Create a list containing all integers within a given range."
  ([e] (my-range 0 e))
  ([s e] (if (not= (dec e) s)
           (cons s (my-range (inc s) e))
           `(~s))))

(defn my-rand [n] (Math/round (rand (dec n))))
(defn my-rand-select [xss n]
  "1.23 (**) Extract a given number of randomly selected elements from a list."  
  (let [[x & xs] xss, rnd (my-rand (count xss))]
    (cond
     (= (count xss) n) xss
     (< rnd n) (cons x (my-rand-select xs (dec n)))
     :else (my-rand-select xs n))))

(defn my-lotto [n limit]
  "1.24 (*) Lotto: Draw N different random numbers from the set 1..M."
  (let [xss (my-range 1 (inc limit))]
    (my-rand-select xss n)))

(defn my-rand-perm [xss]
  "1.25 (*) Generate a random permutation of the elements of a list."
  (shuffle xss))

(defn my-comb [[x & xs :as xss] n]
  "(**) Generate the combinations of K distinct objects chosen from the N elements of a list"
  (cond (> n (count xss)) []
        (= (count xss) n) [xss]
        (empty? xss) nil
        :else (concat (map #(cons x %) (my-comb xs (dec n))) (my-comb xs n))))

(def ls [1 2 3 4 5 6 7 8])
(defn slice [ls s e k];
  (slice ls s e))

(defn -main [& args] (println "99 problem clojure"))