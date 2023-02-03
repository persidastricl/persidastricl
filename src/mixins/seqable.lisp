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

(defgeneric seq (seqable)
  (:method ((object sequence)) object)
  (:method ((object seqable)) (let ((seq '())
                                    (it (iterator object)))
                                (while (has-next? it) (setf seq (cl:cons (next it) seq)))
                                (nreverse seq))))
