;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   node-iterator.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defclass node-iterator (iterator)
  ((node :initarg :node :accessor :node)
   (index :initarg :index :accessor :index)))

(defmethod iterator ((target node))
  (make-instance 'node-iterator :node target :index 0))

(defmethod iterator ((target overflow-node))
  (make-instance 'node-iterator :node target :index 0))

(defmethod has-next? ((iterator node-iterator))
  (with-slots (index node) iterator
    (when (and index node)
      (< index (count node)))))

(defmethod current ((iterator node-iterator))
  (with-slots (node index) iterator
    (at-index node index)))

(defmethod next ((iterator node-iterator))
  (with-slots (node index) iterator
    (when (< index (count node))
      (let ((item (at-index node index)))
        (incf index)
        item))))
