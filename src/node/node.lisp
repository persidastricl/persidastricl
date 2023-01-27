;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   node.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defgeneric put (target item context))
(defgeneric del (target item context))
(defgeneric get (target item context))

;; -----
;;  base class for all node objects
;;
;; -----

(defun empty-overflow-node (node)
  "create an overflow node for the type of node we are currently using xxxx-node --> xxx-overflow-node"
  (-> (type-of node)
      symbol-name
      (s:replace "(?i)node" "overflow-node")
      read-from-string
      make-instance))

(defun empty-node (node context)
  "given the current node and the context of the caller, determine if we
 need a new node or a new overflow node (ie. the max depth has been
 reached) "
  (destructuring-bind (hash depth bit-size) context
                      (declare (ignore hash))
                      (let ((max-depth (floor (/ (h:size) bit-size))))
                        (if (< depth max-depth)
                            (empty node)
                          (empty-overflow-node node)))))

(defclass node () ())
