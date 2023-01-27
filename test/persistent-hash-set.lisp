;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   test/persistent-hash-set.lisp
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :persistent-hash-set-tests
  :description "testing persistent hash set operations"
  :in master-suite)

(in-suite :persistent-hash-set-tests)

(named-readtables:in-readtable persidastricl:syntax)

(test persistent-hash-set-test
  (let ((s #{:k1 :v1 :k2 :v2 :k3 :v3 :k4 :v4}))
    (is (typep s 'persistent-hash-set))
    (is (= 8 (count s)))
    (is (contains? s :k1))))

;;(5am:run! :persistent-hash-set-tests)
