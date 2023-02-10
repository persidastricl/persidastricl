;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   overflow-node.lisp
;;;
;;; -----

(in-package #:node)

;; -----
;;  overflow-node
;;
;;  base class for overflow nodes
;; -----

(defclass overflow-node (node)
  ((hash :reader :hash :initarg :hash)
   (data :reader :data :initarg :data))
  (:default-initargs :hash nil :data '()))

(defmethod sub-nodes ((node overflow-node))
  (declare (ignore node))
  '())

(defmethod single-value-node? ((node overflow-node))
  (= (length (:data node)) 1))

(defmethod at-index ((node overflow-node) index)
  (elt (:data node) index))

(defmethod count ((node overflow-node))
  (length (:data node)))
