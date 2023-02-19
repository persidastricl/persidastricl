;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-set-overflow-node.lisp
;;;
;;; -----

(in-package #:node)

;; -----
;;  transient-hash-set-overflow-node
;;
;; -----

(defclass transient-hash-set-overflow-node (transient-overflow-node hash-set-overflow-node) ())

(defmethod put ((node transient-hash-set-overflow-node) item context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (first context))))
    (when-not hash (setf hash (first context)))
    (setf data (adjoin item data :test #'==)))
  node)

(defmethod delete ((node transient-hash-set-overflow-node) item context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (first context))))
    (when-not hash (setf hash (first context)))
    (setf data (remove-if (lambda (e) (== item e)) data)))
  node)
