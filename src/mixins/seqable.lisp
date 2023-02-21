;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   seqable.lisp
;;;
;;; CLOS marker class for collections that are capable of becoming sequences of items
;;;
;;; -----

(in-package #:persidastricl)

(defclass seqable () ())

(defun sequential? (x)
  (or (typep x 'sequence)
      (typep x 'seqable)))
