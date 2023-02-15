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

(defmethod seq ((object seqable)) (let ((seq '())
                                        (it (iterator object)))
                                    (while (has-next? it) (setf seq (cons (next it) seq)))
                                    (nreverse seq)))
