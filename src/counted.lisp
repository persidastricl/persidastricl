;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   counted.lisp
;;;
;;; CLOS class for tracking a count of items
;;;
;;; -----

(in-package #:counted)

(defclass counted ()
  ((count :type integer :initarg :count :reader :count))
  (:default-initargs :count 0))
