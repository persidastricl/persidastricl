;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   associable.lisp
;;;
;;; marker class for an associable data structure
;;;
;;; -----

(in-package #:persidastricl)

(defclass associable () ())

(defgeneric assoc (associable k v &rest kv-pairs))
(defgeneric dissoc (associable &rest keys))
(defgeneric lookup (associable k &optional default))
(defgeneric get (associable k &optional default))
