;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; persistent-vector-tail-node.lisp
;;;
;;; -----

(in-package :node)

(defparameter *items-per-node* 32)

(define-immutable-class persistent-vector-tail-node (persistent-vector-leaf-node) ())

(defmethod cons :before (item (node persistent-vector-tail-node))
  ;; cannot add more items than we have bits in the hash
  (assert (< (length (:data node)) *items-per-node*)))

(defmethod cons (item (node persistent-vector-tail-node))
  (make-instance (type-of node) :level 0 :data (v:append (:data node) item)))
