;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   test/transient-hash-set.lisp
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :transient-hash-set-tests
  :description "testing transient hash set operations"
  :in master-suite)

(in-suite :transient-hash-set-tests)

(named-readtables:in-readtable persidastricl:syntax)

(test transient-hash-set-test
  (let ((s @#{:k1 :v1 :k2 :v2 :k3 :v3 :k4 :v4}))
    (is (typep s 'transient-hash-set))
    (is (= 8 (count s)))
    (is (contains? s :k1))))

;;(5am:run! :transient-hash-set-tests)
