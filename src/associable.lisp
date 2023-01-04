;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   associable.lisp
;;;
;;; marker class for an associable data structure
;;;
;;; -----

(in-package #:persidastricl)

(defclass asscciable () ())

(defgeneric assoc (associable &rest kv-pairs))
(defgeneric dissoc (associable &rest keys))
(defgeneric lookup (associable k &optional default))
