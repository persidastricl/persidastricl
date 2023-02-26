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

(defclass transient-hash-set-overflow-node (transient-overflow-node hash-set-overflow-node) ())

(defmethod add ((node transient-hash-set-overflow-node) item &key hash &allow-other-keys)
  (when (hash node) (assert (eq (hash node) hash)))
  (when-not (hash node) (setf (hash node) hash))
  (setf (data node) (adjoin item (data node) :test #'==))
  node)

(defmethod remove ((node transient-hash-set-overflow-node) item &key hash &allow-other-keys)
  (when (hash node) (assert (eq (hash node) hash)))
  (when-not (hash node) (setf (hash node) hash))
  (setf (data node) (remove-if (lambda (e) (== item e)) (data node)))
  node)
