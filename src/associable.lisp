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

(defgeneric assoc (associable k v))
(defgeneric dissoc (associable k))
(defgeneric lookup (associable k &optional default))
