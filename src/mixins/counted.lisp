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
  ((count :type integer :initarg :count))
  (:default-initargs :count 0))

(defmethod count ((thing counted))
  (slot-value thing 'count))

(defmethod length ((thing counted))
  (count thing))

(defmethod bounded-count (n (thing counted))
  (:method (n (thing counted)) (count thing)))

(defmethod empty? ((thing counted))
  (zerop (count thing)))
