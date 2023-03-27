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
;;; test/lazy-sequence.lisp
;;;
;;; -----

(in-package #:persidastricl)

(named-readtables:in-readtable persidastricl:syntax)

(def-suite :lazy-sequence-tests
  :description "testing lazy sequences"
  :in master-suite)

(in-suite :lazy-sequence-tests)

(test create-lazy-sequence-with-lseq
  :description "test making a lazy sequence via the lseq macro"
  (let* ((l (lseq 1 (inc 1))))
    (is (= 1 (:head l)))
    (is (instance? 'thunk (:tail l)))))

(test lazy-execution
  :description "test that the tail of a lazy-sequence is not evaluated until needed"
  (let ((a (atom 10)))
    (labels ((next* (v)
               (declare (ignore v))
               (let ((i (deref a)))
                 (when (pos? i)
                   (lseq i (next* (swap! a #'dec)))))))
      (let ((l (next* a)))
        (is (= 10 (head l)))
        (is (= 10 (deref a)))
        (tail l)
        (is (= 9 (deref a)))
        (drop 2 l)
        (is (= 8 (deref a)))
        (drop 3 l)
        (is (= 7 (deref a)))
        (drop 4 l)
        (is (= 6 (deref a)))
        (drop 5 l)
        (is (= 5 (deref a)))
        (drop 6 l)
        (is (= 4 (deref a)))
        (drop 7 l)
        (is (= 3 (deref a)))
        (drop 8 l)
        (is (= 2 (deref a)))
        (drop 9 l)
        (is (= 1 (deref a)))
        (let ((v (into [] l)))
          (is (== v [10 9 8 7 6 5 4 3 2 1]))
          (is (= 0 (deref a))))))))

(test bounded-count-test
  :description "check a bounded count on an infinite sequence"
  (is (= 100 (bounded-count 100 (integers)))))

(test empty-lazy-sequence
  (let ((l (lseq 1 nil)))
    (is (= 1 (head l)))
    (is (eq nil (head (tail l))))
    (is (empty? (tail l)))))
