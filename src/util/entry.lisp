;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   entry.lisp
;;;
;;; class for tracking a bitmap and an associated vector of data
;;;
;;; -----

(in-package #:entry)

(defgeneric key (target))

(defgeneric value (target))

(defclass entry ()
  ((key :initarg :key :reader :key)
   (value :initarg :value :reader :value)))

(defun map-entry (k v)
  (make-instance 'entry :key k :value v))

(defmethod key ((entry entry))
  (:key entry))

(defmethod value ((entry entry))
  (:value entry))

(defun ->vec (entry)
  (vector (key entry) (value entry)))

(defun ->list (entry)
  (list (key entry) (value entry)))

(defun ->cons (entry)
  (cons (key entry) (value entry)))

(defmethod print-object ((obj Entry) stream)
  (if (eq 'persidastricl:syntax (named-readtables:readtable-name *readtable*))
      (format stream "[~s ~s]" (key obj) (value obj))
      (format stream "(entry:map-entry ~s ~s)" (key obj) (value obj))))

(defmethod make-load-form ((obj Entry) &optional env)
  (declare (ignore env))
  `(entry:map-entry ,(key Entry) ,(value Entry)))
