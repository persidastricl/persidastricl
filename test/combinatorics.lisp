;;; -----
;;;
;;;  Copyright (c) 2019-2023 Michael D Pendergrass, pupcus.org
;;;
;;;  This program and the accompanying materials are made
;;;  available under the terms of the Eclipse Public License 2.0
;;;  which is available at https://www.eclipse.org/legal/epl-2.0/
;;;
;;;  SPDX-License-Identifier: EPL-2.0
;;;
;;; -----

;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;; test/combinatorics.lisp
;;;
;;; -----

(in-package #:persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(def-suite :combinatorics-tests
  :description "testing the combinatorcis library/functions"
  :in master-suite)

(in-suite :combinatorics-tests)

(test t-combinations
  (is (== (c:combinations (range 3) 2) '((0 1) (0 2) (1 2))))
  (is (== (c:combinations [1 2 3] 2) '((1 2) (1 3) (2 3))))
  (is (== (c:combinations '(:a :b :c) 2) '((:a :b) (:a :c) (:b :c)))))

(test t-simple-subsets
  (is (== (c:subsets []) '(())))
  (is (== (c:subsets [1 2 3]) '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))))
  (is (== (c:subsets [3 2 1]) '(() (3) (2) (1) (3 2) (3 1) (2 1) (3 2 1))))
  (is (== (c:subsets [1 2 3 4]) '(() (1) (2) (3) (4) (1 2) (1 3) (1 4) (2 3) (2 4) (3 4) (1 2 3) (1 2 4) (1 3 4) (2 3 4) (1 2 3 4))))
  (is (== (c:subsets [1 1 2]) (c:subsets [1 2 1])))
  (is (== (c:subsets [1 3 2 3]) (c:subsets [1 3 3 2])))
  (is (== (c:subsets [:a :b :c]) '(() (:a) (:b) (:c) (:a :b) (:a :c) (:b :c) (:a :b :c))))
  (is (== (c:subsets [:c :b :a]) '(() (:c) (:b) (:a) (:c :b) (:c :a) (:b :a) (:c :b :a))))
  (is (== (c:subsets [:a :b :c :d])
          '(() (:a) (:b) (:c) (:d) (:a :b) (:a :c) (:a :d) (:b :c) (:b :d) (:c :d) (:a :b :c) (:a :b :d) (:a :c :d) (:b :c :d) (:a :b :c :d))))
  (is (== (c:subsets [:a :a :b]) (c:subsets [:a :b :a])))
  (is (== (c:subsets [:a :c :b :c]) (c:subsets [:a :c :c :b]))))

(test t-cartesian-product
  (is (== (c:cartesian-product [1 2] [3 4]) '((1 3) (1 4) (2 3) (2 4))))
  (is (== (c:cartesian-product [:a :b] [:c :d]) '((:a :c) (:a :d) (:b :c) (:b :d)))))

(test t-selections
  (is (== (c:selections [1 2] 3) '((1 1 1) (1 1 2) (1 2 1) (1 2 2) (2 1 1) (2 1 2) (2 2 1) (2 2 2))))
  (is (== (c:selections [:a :b] 3) '((:a :a :a) (:a :a :b) (:a :b :a) (:a :b :b) (:b :a :a) (:b :a :b) (:b :b :a) (:b :b :b)))))

(test t-permutations
  (is (== (c:permutations [1 2 3]) '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))))
  (is (== (c:permutations [2 3 1]) '((2 3 1) (2 1 3) (3 2 1) (3 1 2) (1 2 3) (1 3 2))))
  (is (== (c:permutations [1 1 2]) (c::lex-permutations [1 1 2])))
  (is (== (c:permutations [:a :b]) [[:a :b] [:b :a]]))
  (is (== (c:permutations [2 1 1]) [[2 1 1] [1 2 1] [1 1 2]])))

(test t-lex-permutations
  (is (== (c::lex-permutations [1 1 2]) [[1 1 2] [1 2 1] [2 1 1]])))

(test t-permuted-combinations
  (is (== (c:permuted-combinations [1 2 3] 2) [[1 2] [2 1] [1 3] [3 1] [2 3] [3 2]]))
  (is (== (c:permuted-combinations [1 2 2] 2) [[1 2] [2 1] [2 2]])))

(test t-sorted-numbers?
  (is-true  (c::sorted-numbers? [1 2 3]))
  (is-true  (c::sorted-numbers? [1 1 2]))
  (is-true  (c::sorted-numbers? []))
  (is-false (c::sorted-numbers? [1 4 2]))
  (is-false (c::sorted-numbers? [1 :a 2])))

(test t-factorial-numbers
  (is (== (c::factorial-numbers 463) '(3 4 1 0 1 0)))
  (is (== (c::factorial-numbers 0) '()))
  (is (== (c::factorial-numbers 1) '(1 0)))
  (is (== (c::factorial-numbers 2) '(1 0 0))))

(test t-nth-permutation-distinct
  (let ((perms (c:permutations (range 4))))
    (mapv
     (lambda (i)
       (is (== (nth perms i) (c::nth-permutation-distinct (range 4) i))))
     (range 24))))

(test t-nth-permutation-duplicates
  (let ((perms (c:permutations [1 1 2 2 2 3])))
    (mapv
     (lambda (i)
       (is (== (nth perms i) (c::nth-permutation-duplicates [1 1 2 2 2 3] i))))
     (range 60))))

(test t-count-permutations
  (is (== (c:count-permutations (range 4)) (count (c:permutations (range 4)))))
  (is (== (c:count-permutations [1 1 2]) (count (c:permutations [1 1 2]))))
  (is (== (c:count-permutations [1 1 2 2]) (count (c:permutations [1 1 2 2]))))
  (is (== (c:count-permutations [1 1 1 2 2 3]) (count (c:permutations [1 1 1 2 2 3])))))

(test t-nth-permutation
  (let ((sortedDistinctNumbers (range 4))
        (sortedDuplicateNumbers [1 1 1 2 3 3])
        (distinctChars [#\a #\b #\c #\d])
        (duplicates [#\a #\a #\b #\c #\c])
        (duplicates2 [1 3 1 2 1 2]))
    (mapv
     (lambda (collection)
       (let* ((perms (c:permutations collection))
              (c (count perms)))
         (mapv
          (lambda (i)
            (is (== (nth perms i) (c:nth-permutation collection i)))
            (is (== c (c:count-permutations collection))))
          (range c))))
     [sortedDistinctNumbers
      sortedDuplicateNumbers
      distinctChars
      duplicates
      duplicates2])))

(test t-drop-permutations
  (mapv
   (lambda (items)
     (let ((c (c:count-permutations items)))
       (mapv
        (lambda (i)
          (is (== (c:drop-permutations items i) (drop i (c:permutations items)))))
        (range c))))
   [[1 2 3]
    [1 1 2]
    [#\a #\b #\c]
    [#\a #\a #\b #\c #\c]
    [1 3 1 2 1 2]]))

(test t-permutation-index
  (let ((sortedDistinctNumbers (range 4))
        (sortedDuplicateNumbers [1 1 1 2 3 3])
        (distinctChars [#\a #\b #\c #\d])
        (duplicates [#\a #\a #\b #\c #\c])
        (duplicates2 [1 3 1 2 1 2]))
    (mapv
     (lambda (collection)
       (let ((perms (c:permutations collection)))
         (mapv
          (lambda (perm)
            (is (== (c:nth-permutation (sort (->list collection) #'c::less-than) (c:permutation-index perm)) perm)))
          perms)))
     [sortedDistinctNumbers
      sortedDuplicateNumbers
      distinctChars
      duplicates
      duplicates2])))

;;(5am:run! :combinatorics-tests)
