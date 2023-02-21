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

(defmethod count ((thing counted))
  (:count thing))

(defmethod length ((thing counted))
  (:count thing))

(defmethod empty? ((thing counted))
  (zerop (:count thing)))
