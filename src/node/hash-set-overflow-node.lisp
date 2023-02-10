;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hash-set-overflow-node.lisp
;;;
;;; -----

(in-package #:node)

;; -----
;;  hash-set-overflow-node
;;
;; -----

(defclass hash-set-overflow-node (overflow-node) ())

(defmethod single-remaining-data ((node hash-set-overflow-node))
  (first (:data node)))

(defmethod get ((node hash-set-overflow-node) item context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (first context))))
    (if-let (target (member item data :test #'==))
      (first target)
      (first (last context)))))
