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
