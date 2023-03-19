;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   test/transient-hash-set.lisp
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :hash-set-tests
  :description "testing hash set operations"
  :in master-suite)

(in-suite :hash-set-tests)

(named-readtables:in-readtable persidastricl:syntax)

(test transient-hash-set-test
  (let ((s @#{:k1 :v1 :k2 :v2 :k3 :v3 :k4 :v4}))
    (is (typep s 'transient-hash-set))
    (is (= 8 (count s)))
    (is (contains? s :k1))))

(test persistent-hash-set-test
  (let ((s #{:k1 :v1 :k2 :v2 :k3 :v3 :k4 :v4}))
    (is (typep s 'persistent-hash-set))
    (is (= 8 (count s)))
    (is (contains? s :k1))))

;;(5am:run! :hash-set-tests)
