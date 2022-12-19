;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   count.lisp
;;;
;;; generic function for getting a count of something
;;;
;;; -----

(in-package #:counted)

(defgeneric count (thing))

(defmethod count (thing)
  (length thing))
