;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   node.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;;  base class for all node objects
;;
;; -----

(defclass node () ())

(defun empty-overflow-node (node)
  "create an overflow node for the type of node we are currently using xxxx-node --> xxx-overflow-node"
  (-> (type-of node)
      s:str
      (s:replace "(?i)node$" "overflow-node")
      read-from-string
      make-instance))

(defun empty-node (node &key hash depth)
  "given the current node and the context of the caller, determine if we
 need a new node or a new overflow node (ie. the max depth has been
 reached) "
  (declare (ignore hash))
  (let ((max-depth (floor (/ (h:size) b::*default-hash-slice-bit-size*))))
    (if (< depth max-depth)
        (empty node)
        (empty-overflow-node node))))
