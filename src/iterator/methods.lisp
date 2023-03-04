;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   seqable.lisp
;;;
;;; CLOS marker class for collections that are capable of becoming sequences of items
;;;
;;; -----

(in-package #:persidastricl)

(defmethod seq ((it iterator))
  (labels ((seq* (it)
             (when (has-next? it)
               (lseq (next it) (seq* it)))))
    (seq* it)))

(defmethod seq ((object seqable))
  (seq (iterator object)))
