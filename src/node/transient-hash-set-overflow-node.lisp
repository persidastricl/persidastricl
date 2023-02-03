;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   transient-hash-set-overflow-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  transient-hash-set-overflow-node
;;
;; -----

(defclass transient-hash-set-overflow-node (hash-set-overflow-node) ())

(defmethod put ((node transient-hash-set-overflow-node) item context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (cl:first context))))
    (when-not hash (setf hash (cl:first context)))
    (setf data (adjoin item data :test #'==)))
  node)

(defmethod del ((node transient-hash-set-overflow-node) item context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (cl:first context))))
    (when-not hash (setf hash (cl:first context)))
    (setf data (remove-if (lambda (e) (== item e)) data)))
  node)
