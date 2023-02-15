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
  ((fn :initarg :fn :accessor :fn)
   (r  :initarg :r  :accessor :r))
  (:default-initargs :r nil))

(defmacro delay (&rest body)
  `(make-instance 'thunk :fn (lambda () ,@body)))

(defgeneric force (obj)
  (:method (obj) obj))

(defmethod force ((thunk thunk))
  (if (and (slot-boundp thunk 'fn) (functionp (:fn thunk)))
      (let ((values (multiple-value-list (funcall (:fn thunk)))))
        (setf (:r thunk) values)
        (slot-makunbound thunk 'fn)))
  (values-list (:r thunk)))
