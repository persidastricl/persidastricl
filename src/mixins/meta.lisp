;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   meta.lisp
;;;
;;; CLOS class for metadata
;;;
;;; -----

(in-package #:persidastricl)

(defclass metadata ()
  ((meta :initarg :meta :reader :meta :documentation "map of metadata"))
  (:default-initargs :meta nil))

(defgeneric with-meta (object meta))

(defgeneric meta (object)
  (:method ((object metadata)) (with-slots (meta) object meta)))

(defun vary-meta (obj f & args)
  (with-meta obj (apply f (meta obj) args)))
