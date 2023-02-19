;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-set-overflow-node.lisp
;;;
;;; -----

(in-package #:node)

;; -----
;;  persistent-hash-set-overflow-node
;;
;; -----

(define-immutable-class persistent-hash-set-overflow-node (persistent-overflow-node hash-set-overflow-node) ())

(defmethod put ((node persistent-hash-set-overflow-node) item context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (first context))))
    (make-instance 'persistent-hash-set-overflow-node :hash (or hash (first context)) :data (adjoin item data :test #'==))))

(defmethod delete ((node persistent-hash-set-overflow-node) item context)
  (with-slots (hash data) node
    (when hash (assert (eq hash (first context))))
    (make-instance 'persistent-hash-set-overflow-node :hash (or hash (first context)) :data (remove-if (lambda (e) (== item e)) data))))
