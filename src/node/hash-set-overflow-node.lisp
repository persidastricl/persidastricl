;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash-set-overflow-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  hash-set-overflow-node
;;
;; -----

(defclass hash-set-overflow-node (overflow-node) ())

(defmethod single-remaining-data ((node hash-set-overflow-node))
  (first (:data node)))

(defmethod loc ((node hash-set-overflow-node) item &key hash (default nil) &allow-other-keys)
  (when (:hash node) (assert (eq (:hash node) hash)))
  (if-let ((target (member item data :test #'==)))
    (first target)
    default))
