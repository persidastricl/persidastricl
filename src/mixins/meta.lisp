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
