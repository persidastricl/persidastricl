;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   counted.lisp
;;;
;;; CLOS class for tracking a count of items
;;;
;;; -----

(in-package #:persidastricl)

(defclass counted ()
  ((count :type integer :initarg :count :reader :count))
  (:default-initargs :count 0))

(defgeneric count (thing))

(defmethod count (thing)
  (length thing))
