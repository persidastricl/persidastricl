;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   seqable.lisp
;;;
;;; CLOS marker class for a sequence of items
;;;
;;; -----

(in-package #:persidastricl)

(defclass seqable () ())

(defgeneric cons (seqable item))
(defgeneric first (seqable))
(defgeneric rest (seqable))
