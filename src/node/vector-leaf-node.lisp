;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; vector-leaf-node.lisp
;;;
;;; -----

(in-package :node)

(defparameter *items-per-node* 32)

(defclass vector-leaf-node (vector-node) ()
  (:default-initargs :level 0))

(defmethod cons :before (item (node vector-leaf-node))
  ;; cannot add more items than we have bits in the hash
  (assert (< (length (:data node)) *items-per-node*)))

;; get value at level 0 index

(defmethod get ((node vector-leaf-node) index context)
  (destructuring-bind (default) context
    (let* ((i (b:bits index 0))
           (v (elt (:data node) i)))
      (or v default))))
