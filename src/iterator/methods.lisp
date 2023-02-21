;;; -----
;;; -*- mode: Lisp; -*-
;;;
;;;   seqable.lisp
;;;
;;; CLOS marker class for collections that are capable of becoming sequences of items
;;;
;;; -----

(in-package #:persidastricl)

;; (defmethod seq ((object seqable)) (let ((seq '())
;;                                         (it (iterator object)))
;;                                     (while (has-next? it) (setf seq (cons (next it) seq)))
;;                                     (nreverse seq)))

(defmethod seq ((it iterator))
  (labels ((seq* (it)
             (when (has-next? it)
               (lseq (next it) (seq* it)))))
    (seq* it)))

(defmethod seq ((object seqable))
  (seq (iterator object)))
