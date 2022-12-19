;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;; test/bitmap-vector.lisp
;;;
;;;  testing bitmap-vectors
;;;
;;; -----

(in-package #:bitmap-vector-test)

(def-suite bitmap-vector-tests
  :description ""
  :in master-suite)

(in-suite bitmap-vector-tests)

(defun bv-equal-p (bv1 bv2)
  (and (equalp (:bitmap bv1) (:bitmap bv2))
       (equalp (:data bv1) (:data bv2))))

(test create-bitmap-vector-test
  :description "creating empty bitmap-vector is as expected (EMPTY)"
  (let ((bv (make-instance 'bitmap-vector)))
    (is (bv-equal-p bv EMPTY))))

(test simple-insert-test
  :description "insert item in bitmap-vector"
  (let ((bv (bv:insert EMPTY 31 "test")))
    (is (and (equalp (:bitmap bv) #b10000000000000000000000000000000)
             (equalp (:data bv) #("test"))))))


(test simple-insert-test
  :description "insert item in bitmap-vector"
  (let ((bv (bv:insert EMPTY 31 "test")))
    (is (and (equalp (:bitmap bv) #b10000000000000000000000000000000)
             (equalp (:data bv) #("test"))))))

(defparameter *bv* (arrow-macros:-> EMPTY
                     (bv:insert 31 :a)
                     (bv:insert 0  :b)
                     (bv:insert 15 :c)))

(test multiple-insert-test
  :description ""
  (let ((bv *bv*))
    (is (and (equalp (:bitmap bv) #b10000000000000001000000000000001)
             (equalp (:data bv) #(:b :c :a))))))

(test update-test
  :description ""
  (let ((bv *bv*)
        (expected (make-instance 'bitmap-vector
                                 :bitmap #b10000000000000001000000000000001
                                 :data #(:b :c :updated))))
    (is (bv-equal-p expected (bv:update bv 31 :updated)))))

(test delete-test
  :description ""
  (let ((bv *bv*)
        (expected (make-instance 'bitmap-vector
                                 :bitmap #b00000000000000001000000000000001
                                 :data #(:b :c))))
    (is (bv-equal-p expected (bv:delete bv 31)))))

(test set?-test
  :description ""
  (let ((bv *bv*))
    (is-true  (bv:set? bv 15))
    (is-false (bv:set? bv 2))))

(test get-test
  :description ""
  (let ((bv *bv*))
    (is (equalp :c  (bv:get bv 15)))
    (is (equalp nil (bv:get bv 2)))))


(test count-test
  :description ""
  (let ((bv *bv*))
    (is (= 3 (c:count bv)))))
