;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   vector-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defparameter *items-per-node* 32)

;; -----
;;  vector-node
;;
;;  base class for transient/persistent dynamic vector nodes

(defclass vector-node ()
  ((level :initarg :level :reader :level)
   (data :initarg :data :reader :data))
  (:default-initargs :level 0 :data (make-array 0)))

(define-immutable-class persistent-vector-node (vector-node) ())

(defmethod cons :before (item (node vector-node))
  ;; cannot add more items than we have bits in the hash
  (assert (< (length (:data node)) *items-per-node*)))

(defmethod cons (item (node persistent-vector-node))
  (make-instance (type-of node) :level (:level node) :data (v:append item (:data node))))
