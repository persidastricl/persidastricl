;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   persistent-hash-set-overflow-node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  persistent-hash-set-overflow-node
;;
;; -----

(define-immutable-class persistent-hash-set-overflow-node (persistent-overflow-node hash-set-overflow-node) ())

(defmethod add ((node persistent-hash-set-overflow-node) item &key hash &allow-other-keys)
  (when (hash node) (assert (eq (hash node) hash)))
  (make-instance 'persistent-hash-set-overflow-node :hash (or (hash node) hash) :data (adjoin item (data node) :test #'==)))

(defmethod remove ((node persistent-hash-set-overflow-node) item &key hash &allow-other-keys)
  (when (hash node) (assert (eq (hash node) hash)))
  (make-instance 'persistent-hash-set-overflow-node hash (or (hash node) hash) :data (remove-if (lambda (e) (== item e)) (data node))))
