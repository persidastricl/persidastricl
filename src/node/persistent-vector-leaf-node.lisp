;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; persistent-vector-leaf-node.lisp
;;;
;;; -----

(in-package :node)

(define-immutable-class persistent-vector-leaf-node (persistent-vector-node) ()
  (:default-initargs :level 0))

;; get value at level 0 index

(defmethod get ((node persistent-vector-leaf-node) index context)
  (destructuring-bind (default) context
    (let* ((i (b:bits index 0))
           (v (elt (:data node) i)))
      (or v default))))
