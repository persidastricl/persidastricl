;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   entry.lisp
;;;
;;; map entry helper functions
;;;
;;; -----

(in-package #:entry)

(defun entry (k v)
  (vector k v))

(defun key (entry)
  (elt entry 0))

(defun value (entry)
  (elt entry 1))
