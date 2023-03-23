;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   thunk.lisp
;;;
;;; -----

(in-package #:persidastricl)

;; -----
;; thunk
;;
;; -----

(defclass thunk ()
  ((fn :initarg :fn)
   (r  :initarg :r))
  (:default-initargs :r nil))

(defmacro delay (&rest body)
  `(make-instance 'thunk :fn (lambda () ,@body)))

(defgeneric force (obj)
  (:method (obj) obj))

(defmethod force ((thunk thunk))
  (when (slot-value thunk 'fn)
    (setf (slot-value thunk 'r) (funcall (slot-value thunk 'fn)))
    (setf (slot-value thunk 'fn) nil))
  (slot-value thunk 'r))
