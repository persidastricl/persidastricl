;;; -------
;;; -*- mode: Lisp; -*-
;;;
;;; vector-leaf-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defparameter *items-per-node* 32)

(defclass vector-leaf-node (vector-node) ()
  (:default-initargs :level 0))

(defmethod cons :before (item (node vector-leaf-node))
  ;; cannot add more items than we have bits in the hash
  (assert (< (length (data node)) *items-per-node*)))

;; locate value at level 0 index

(defmethod loc ((node vector-leaf-node) index &key (default nil))
  (let* ((i (b:bits index 0))
         (v (elt (data node) i)))
    (or v default)))
