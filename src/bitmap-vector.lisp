;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   bitmap-vector.lisp
;;;
;;; class for tracking a bitmap and an associated vector of data
;;;
;;; -----

(in-package #:bitmap-vector)

(defclass bitmap-vector ()
  ((bitmap :initarg :bitmap :reader :bitmap :type unsigned-integer)
   (data :initarg :data :reader :data))
  (:default-initargs :bitmap 0 :data (make-array 0)))

(defparameter EMPTY (make-instance 'bitmap-vector))

(defun insert (bv index item)
  (let ((new-bitmap (b:set index (:bitmap bv))))
    (make-instance 'bitmap-vector
                   :bitmap new-bitmap
                   :data (v:insert (:data bv) (b:index index new-bitmap) item))))

(defun update (bv index item)
  (let ((new-bitmap (b:set index (:bitmap bv))))
    (make-instance 'bitmap-vector
                   :bitmap new-bitmap
                   :data (v:update (:data bv) (b:index index new-bitmap) item))))

(defun delete (bv index)
  (let ((new-bitmap (b:clear index (:bitmap bv))))
    (make-instance 'bitmap-vector
                   :bitmap new-bitmap
                   :data (v:delete (:data bv) (b:index index new-bitmap)))))

(defun set? (bv index)
  (b:set? index (:bitmap bv)))

(defun get (bv index)
  (when (set? bv index)
    (elt (:data bv) (b:index index (:bitmap bv)))))

(defmethod counted:count ((bv bitmap-vector))
  (logcount (:bitmap bv)))
