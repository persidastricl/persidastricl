;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   meta.lisp
;;;
;;; CLOS class for metadata
;;;
;;; -----

(in-package #:metadata)

(defclass metadata ()
  ((meta :initarg :meta :reader :meta :documentation "map of metadata"))
  (:default-initargs :meta nil))
