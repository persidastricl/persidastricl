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

(defun Entry. (k v)
  (make-instance 'entry :key k :value v))

(defun map-entry (k v)
  (make-instance 'entry :key k :value v))

(defmethod key ((item entry))
  (:key item))

(defmethod value ((item entry))
  (:value item))
