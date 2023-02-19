;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   node.lisp
;;;
;;; -----

(in-package #:node)
;;
;; TODO: make a util package and put common fns like this for all other packages there
;;
(defgeneric empty (object)
  (:documentation "return an empty data-object of the same type as the original object argument")
  (:method ((object t)) (make-instance (type-of object))))

(defgeneric put (target item context))
(defgeneric get (target item context))
(defgeneric delete (target item context))

(defgeneric count (object)
  (:method (object) (cl:length object)))

(defgeneric cons (se1 se2)
  (:method (se1 se2) (cl:cons se1 se2)))

(defgeneric at-index (node index))

;; -----
;;  base class for all node objects
;;
;; -----

(defun empty-overflow-node (node)
  "create an overflow node for the type of node we are currently using xxxx-node --> xxx-overflow-node"
  (-> (type-of node)
    s:str
    (s:replace "(?i)node$" "overflow-node")
    read-from-string
    make-instance))

(defun empty-node (node context)
  "given the current node and the context of the caller, determine if we
 need a new node or a new overflow node (ie. the max depth has been
 reached) "
  (destructuring-bind (hash depth) context
    (declare (ignore hash))
    (let ((max-depth (floor (/ (h:size) b::*default-hash-slice-bit-size*))))
      (if (< depth max-depth)
          (empty node)
          (empty-overflow-node node)))))

(defclass node () ())
