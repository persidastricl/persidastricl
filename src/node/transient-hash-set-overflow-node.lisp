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

(defmethod put-it ((node transient-hash-set-overflow-node) item context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (first context))))
    (when-not hash (setf hash (first context)))
    (setf data (adjoin item data :test #'==)))
  node)

(defmethod del-it ((node transient-hash-set-overflow-node) item context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (first context))))
    (when-not hash (setf hash (first context)))
    (setf data (remove-if (lambda (e) (== item e)) data)))
  node)
