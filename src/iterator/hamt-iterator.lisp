;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   hamt-iterator.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defclass hamt-iterator (iterator)
  ((stack :initarg :stack :accessor :stack)
   (current :initarg :current :accessor :current)))

(defmethod iterator ((target hamt))
  (let ((start (iterator (:root target)))
        (stack (coerce (sub-nodes (:root target)) 'list)))
    (multiple-value-bind (current stack) (if (has-next? start)
                                             (values start stack)
                                             (next-iterator stack))
      (make-instance 'hamt-iterator :stack stack :current current))))

(defmethod has-next? ((iterator hamt-iterator))
  (when-let ((current (:current iterator)))
    (has-next? (:current iterator))))

(defun next-iterator (stack)
  (if (empty? stack)
      (values nil nil)
      (let ((node (first stack)))
        (if (> (count node) 0)
            (values (iterator node) (concatenate 'list (sub-nodes node) (rest stack)))
            (next-iterator (concatenate 'list (sub-nodes node) (rest stack)))))))

(defmethod current ((iterator hamt-iterator))
  (with-slots (current) iterator
    (current current)))

(defmethod next ((iterator hamt-iterator))
  (with-slots (current stack) iterator
    (let ((entry (next current)))
      (when-not (has-next? current)
        (multiple-value-bind (next-current new-stack) (next-iterator stack)
          (when next-current
            (setf current next-current)
            (setf stack new-stack))))
      entry)))
