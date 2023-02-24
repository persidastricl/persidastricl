;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   iterator.lisp
;;;
;;; -----

(in-package #:persidastricl)

(defclass iterator () ())

(defmethod iterator (target))
(defgeneric current (object))
(defgeneric has-next? (object))
