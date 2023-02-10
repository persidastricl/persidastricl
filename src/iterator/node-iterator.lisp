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

(defmethod iterator ((target n:node))
  (make-instance 'node-iterator :node target :index 0))

(defmethod iterator ((target n:overflow-node))
  (make-instance 'node-iterator :node target :index 0))

(defmethod has-next? ((iterator node-iterator))
  (with-slots (index node) iterator
    (when (and index node)
      (< index (n:count node)))))

(defmethod current ((iterator node-iterator))
  (with-slots (node index) iterator
    (n:at-index node index)))

(defmethod next ((iterator node-iterator))
  (with-slots (node index) iterator
    (when (< index (n:count node))
      (let ((item (n:at-index node index)))
        (incf index)
        item))))
