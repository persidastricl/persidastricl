;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; transient-vector-leaf-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defclass transient-vector-leaf-node (vector-leaf-node) ()
  (:default-initargs :data (make-array 32 :adjustable t :fill-pointer 0 :initial-element nil)))

(defmethod cons (item (node transient-vector-leaf-node))
  (with-slots (data) node
    (vector-push item data))
  node)

(defmethod add ((node transient-vector-leaf-node) item &key index)
  (with-slots (data) node
    (let ((i (b:bits index 0)))
      (setf (elt data i) item)))
  node)

(defmethod pop ((node transient-vector-leaf-node))
  (with-slots (data) node
    (vector-pop data))
  node)
