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
  (cl:first (:data node)))

(defmethod get ((node hash-set-overflow-node) item context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (cl:first context))))
    (if-let (target (member item data :test #'==))
      (cl:first target)
      (cl:first (cl:last context)))))
