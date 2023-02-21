;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;; test/bitmap-vector.lisp
;;;
;;;  testing bitmap-vector related things
;;;
;;; -----

(in-package #:persidastricl)

(def-suite :bitmap-vector-tests
  :description ""
  :in master-suite)

(in-suite :bitmap-vector-tests)

(defun bv-equal-p (bv1 bv2)
  (and (== (:bitmap bv1) (:bitmap bv2))
       (== (:data bv1) (:data bv2))))

(test create-bitmap-node-vector-test
      :description "creating empty bitmap-vector is as expected (EMPTY)"
      (let ((bv (make-instance 'persistent-node-bitmap-vector)))
        (is (bv-equal-p bv (EMPTY-PERSISTENT-NODE-BITMAP-VECTOR)))))

(test create-bitmap-key-value-vector-test
      :description "creating empty bitmap-vector is as expected (EMPTY)"
      (let ((bv (make-instance 'persistent-key-value-bitmap-vector)))
        (is (bv-equal-p bv (EMPTY-PERSISTENT-KEY-VALUE-BITMAP-VECTOR)))))

(test simple-insert-key-value-test
      :description "insert map-entry in key/value bitmap-vector"
      (let ((bv (ins (EMPTY-PERSISTENT-KEY-VALUE-BITMAP-VECTOR) 31 (e:map-entry "k1" "v1"))))
        (is (and (== (:bitmap bv) #b10000000000000000000000000000000)
                 (== (:data bv) #("k1" "v1"))))))

(defparameter *pkvbv* (-> (EMPTY-PERSISTENT-KEY-VALUE-BITMAP-VECTOR)
                          (ins 31 (e:map-entry :k1 :v1))
                          (ins 0  (e:map-entry :k2 :v2))
                          (ins 15 (e:map-entry :k3 :v3))))

(test multiple-insert-key-value-test
  :description ""
  (let ((bv *pkvbv*))
    (is (and (== (:bitmap bv) #b10000000000000001000000000000001)
             (== (:data bv) #(:k2 :v2 :k3 :v3 :k1 :v1))))))

(test update-key-value-test
      :description ""
      (let ((bv *pkvbv*)
            (expected (make-instance 'persistent-key-value-bitmap-vector
                                     :bitmap #b10000000000000001000000000000001
                                     :data #(:k2 :v2 :k3 :v3 :k1 :updated))))
        (is (bv-equal-p expected (upd bv 31 (e:map-entry :k1  :updated))))))

(test delete-key-value-test
      :description ""
      (let ((bv *pkvbv*)
            (expected (make-instance 'persistent-key-value-bitmap-vector
                                     :bitmap #b00000000000000001000000000000001
                                     :data #(:k2 :v2 :k3 :v3))))
        (is (bv-equal-p expected (del bv 31)))))

(test is-set-key-value-test
  :description ""
  (let ((bv *pkvbv*))
    (is-true  (is-set bv 15))
    (is-false (is-set bv 2))))

(test at-position-key-value-test
  :description ""
  (let ((bv *pkvbv*))
    (is (== (e:map-entry :k3 :v3) (at-position bv 15)))
    (is (== nil (at-position bv 2)))))

(test count-test
      :description ""
      (let ((bv *pkvbv*))
        (is (== 3 (count bv)))))


(defparameter sub-node-1 (make-instance 'persistent-node-bitmap-vector :bitmap #b0001 :data #(1)))
(defparameter sub-node-2 (make-instance 'persistent-node-bitmap-vector :bitmap #b0010 :data #(2)))
(defparameter sub-node-3 (make-instance 'persistent-node-bitmap-vector :bitmap #b0100 :data #(4)))
(defparameter updated-node (make-instance 'persistent-node-bitmap-vector :bitmap #b1000 :data #(8)))

(defparameter *pnbv* (-> (EMPTY-PERSISTENT-NODE-BITMAP-VECTOR)
                         (ins 31 sub-node-1)
                         (ins 0  sub-node-2)
                         (ins 15 sub-node-3)))

(defun new-array (&rest elements)
  (make-array (length elements) :initial-contents elements))

(test multiple-insert-node-test
  :description ""
  (let ((bv *pnbv*))
    (is (and (== (:bitmap bv) #b10000000000000001000000000000001)
             (== (:data bv) (new-array sub-node-2 sub-node-3 sub-node-1))))))

(test update-node-test
      :description ""
      (let ((bv *pnbv*)
            (expected (make-instance 'persistent-node-bitmap-vector
                                     :bitmap #b10000000000000001000000000000001
                                     :data (new-array sub-node-2 sub-node-3 updated-node))))
        (is (bv-equal-p expected (upd bv 31 updated-node)))))

(test delete-key-value-test
      :description ""
      (let ((bv *pnbv*)
            (expected (make-instance 'persistent-node-bitmap-vector
                                     :bitmap #b00000000000000001000000000000001
                                     :data (new-array sub-node-2 sub-node-3))))
        (is (bv-equal-p expected (del bv 31)))))

(test is-set-node-test
      :description ""
      (let ((bv *pnbv*))
        (is-true  (is-set bv 15))
        (is-false (is-set bv 2))))

(test at-position-node-test
      :description ""
      (let ((bv *pnbv*))
        (is (== sub-node-3 (at-position bv 15)))
        (is (== nil (at-position bv 2)))))
